{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
-- |

module Engine.Monad where

import           Lib.Prelude                  hiding (Selector)

import           Control.Lens                 hiding (op, (&))
import           Control.Monad.Trans.Class

import qualified Data.Map                     as Map
import           Data.Maybe                   (fromJust)
import qualified Data.Set                     as Set

import           Database.MongoDB             (Selector, (=:))

import qualified Lib.Api.Schema.Compiler      as Api
import           Lib.Compiler
import           Lib.Compiler.Check.Types
import           Lib.Compiler.Type
import           Lib.Model
import           Lib.Model.Cell
import           Lib.Model.Class
import           Lib.Model.Column
import           Lib.Model.Common
import           Lib.Model.Dependencies
import           Lib.Model.Dependencies.Types
import           Lib.Model.Project
import           Lib.Model.Row
import           Lib.Model.Table
import           Lib.Types

import           Monads

--------------------------------------------------------------------------------

data ChangeOrCached
  = Change ChangeOp
  | Cached

type StoreMap a = Map (Id a) (ChangeOrCached, a)

data Store = Store
  { _storeCells   :: StoreMap Cell
  , _storeColumns :: StoreMap Column
  , _storeRows    :: StoreMap Row
  , _storeTables  :: StoreMap Table
  }

makeLenses ''Store

type What a = Lens' Store (Map (Id a) (ChangeOrCached, a))

emptyChanges :: Store
emptyChanges = Store Map.empty Map.empty Map.empty Map.empty

data Cache = Cache
  { _cacheCellByCoord :: Map (Id Column, Id Row) (Entity Cell)
  , _cacheColumnCells :: Map (Id Column) [Entity Cell]
  }

makeLenses ''Cache

emptyCache :: Cache
emptyCache = Cache Map.empty Map.empty

data EvalTargets
  = CompleteColumn
  | SpecificRows (Set (Id Row))
  deriving (Eq)

data EngineState = EngineState
  { _engineStore          :: Store
  , _engineCache          :: Cache
  , _engineProject        :: Project
  , _engineCompileTargets :: Set (Id Column)
  , _engineEvalTargets    :: Map (Id Column) EvalTargets
  }

makeLenses ''EngineState

newEngineState :: Project -> EngineState
newEngineState p =
  EngineState emptyChanges emptyCache p Set.empty Map.empty

--------------------------------------------------------------------------------

class MonadError AppError m => MonadEngine m where
  askProjectId :: m (Id Project)
  useProject :: Lens' Project a -> m a
  modifyProject :: (Project -> Project) -> m ()

  -- Operations on dependency graph

  graphGetsM   :: (DependencyGraph -> m a) -> m a
  graphModify :: (DependencyGraph -> DependencyGraph) -> m ()
  graphSetCodeDependencies :: Id Column -> CodeDependencies -> m Bool
  graphSetTypeDependencies :: Id Column -> TypeDependencies -> m Bool

  -- Misc database queries

  makeDefaultValue :: DataType -> m Value

  -- Perform changes to DB objects. Guiding principles:
  -- * Functions cascade
  -- * Changes are stored in cache
  -- * Changes will be collected and commited / broadcasted at the end

  createTable :: Table -> m (Id Table)
  modifyTable :: Id Table -> (Table -> Table) -> m ()
  deleteTable :: Id Table -> m ()
  getTable :: Id Table -> m Table

  createColumn :: Column -> m (Id Column)
  modifyColumn :: Id Column -> (Column -> Column) -> m ()
  deleteColumn :: Id Column -> m ()
  getColumn    :: Id Column -> m Column

  createRow :: Row -> m (Id Row)
  deleteRow :: Id Row -> m ()
  getRow    :: Id Row -> m Row

  setCellContent :: Id Column -> Id Row -> CellContent -> m ()

  -- Complex Queries

  -- Cached
  getCellByCoord :: Id Column -> Id Row -> m (Entity Cell)
  getColumnCells :: Id Column -> m [Entity Cell]

  -- Not cached (still need to combine with store)
  getProjectTables       :: m [Entity Table]
  getTableRows           :: Id Table -> m [Entity Row]
  getTableColumns        :: Id Table -> m [Entity Column]
  getRowRecord           :: Id Row -> m (Maybe (Map Text Value))
  getTableByName         :: Ref Table -> m (Maybe (Entity Table))
  getColumnOfTableByName :: Id Table -> Ref Column -> m (Maybe (Entity Column))

  -- Prepare compilation and propagation

  -- | Also schedules evaluation of the column.
  scheduleCompileColumn :: Id Column -> m ()
  scheduleEvalColumn    :: Id Column -> m ()
  scheduleEvalCell      :: Id Column -> Id Row -> m ()

  getCompileTargets :: m [Id Column]
  getEvalRoots      :: m [Id Column]
  getEvalTargets    :: Id Column -> m [Id Row]

--------------------------------------------------------------------------------

-- | MonadHexl interpreter of MonadEngine
newtype EngineT m a = EngineT
  { unEngineT :: ReaderT (Id Project) (StateT EngineState m) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadState EngineState
             , MonadReader (Id Project)
             )

runEngineT :: Entity Project -> EngineT m a -> m (a, EngineState)
runEngineT (Entity projectId p) action =
  runStateT (runReaderT (unEngineT action) projectId) (newEngineState p)

--------------------------------------------------------------------------------

instance MonadTrans EngineT where
  lift = EngineT . lift . lift

instance MonadError AppError m => MonadError AppError (EngineT m) where
  throwError = lift . throwError
  catchError a h = EngineT $ unEngineT a `catchError` (unEngineT . h)

instance MonadHexl m => MonadEngine (EngineT m) where

  askProjectId = ask

  useProject l = use (engineProject . l)

  modifyProject f = engineProject %= f

  graphGetsM f = use (engineProject . projectDependencyGraph) >>= f

  graphModify f = (engineProject . projectDependencyGraph) %= f

  graphSetCodeDependencies columnId deps = do
    let getTableId c = _columnTableId <$> lift (getById' c)
    graph <- use (engineProject . projectDependencyGraph)
    setCodeDependencies getTableId columnId deps graph >>= \case
      Nothing -> pure True
      Just graph' -> (engineProject . projectDependencyGraph .= graph') $> False

  graphSetTypeDependencies columnId deps = do
    graph <- use (engineProject . projectDependencyGraph)
    let getTableId c = view columnTableId <$> getColumn c
    setTypeDependencies columnId getTableId deps graph >>= \case
      Nothing     -> pure True
      Just graph' -> (engineProject . projectDependencyGraph .= graph') $> False

  --

  makeDefaultValue dt = do
    result <- useProject projectModule
    let
      projTycons = case result of
        CompileResultOk modu -> resultTycons $ Api.moduleToCheckResult modu
        _                    -> Map.empty
      tycons = projTycons `Map.union` preludeTycons
      defaultValue i dt' = if i == 0 then pure VUndefined else case dt' of
        DataAlgebraic "Boolean" [] ->
          pure $ VBool False
        DataAlgebraic "String" [] ->
          pure $ VString ""
        DataAlgebraic "Number" [] ->
          pure $ VNumber 0
        DataAlgebraic "Integer" [] ->
          pure $ VInteger 0
        DataAlgebraic "DateTime" [] ->
          VTime <$> lift getCurrentTime
        DataAlgebraic "Row" [sub] ->
          defaultValue (i-1) sub
        DataAlgebraic "Record" [sub] ->
          defaultValue (i-1) sub
        DataAlgebraic "List" [_] ->
          pure $ VList []
        DataAlgebraic "Maybe" [_] ->
          pure $ VMaybe Nothing
        DataAlgebraic tycon args ->
          let
            cs = instConstructors tycon args
            compare' (_, as) (_, as') = compare (length as) (length as')
          in
            case cs of
              [] -> pure VUndefined
              _ ->
                let (vcon, vargs) = minimumBy compare' cs in
                VData vcon <$> traverse (defaultValue (i-1)) vargs
        DataRecord dts ->
          VRecord <$> traverse (\(f, dty) -> (f,) <$> defaultValue (i-1) dty) dts
        DataTable t -> do
          res <- lift $ getOneByQuery [ "tableId" =: toObjectId t ]
          pure $ VRowRef $ case res of
            Left _                   -> Nothing
            Right (Entity tableId _) -> Just tableId
      instConstructors :: Text -> [DataType] -> [(Text, [DataType])]
      instConstructors tycon dataTypes =
        map goConstr (tyconValueConstrs tyconInfo)
        where
        tyconInfo = fromJust $ Map.lookup tycon tycons
        goConstr (c, args) = (c, map goType args)
        goType (Fix ty) = case ty of
          TypeVar v -> resolveParam v
          TypeConstructor c -> DataAlgebraic c []
          TypeApp f arg -> case goType f of
            DataAlgebraic c f' -> DataAlgebraic c (snoc f' $ goType arg)
          TypeTable (InId t) -> DataTable t
          TypeRecord r -> DataRecord $ Map.toList $ map goType r
        resolveParam :: Text -> DataType
        resolveParam p = fromJust $ Map.lookup p paramMap
        paramMap = Map.fromList $ zip (tyconParams tyconInfo) dataTypes

    defaultValue (10 :: Int) dt

  --

  createTable = storeCreate storeTables

  modifyTable tableId f = do
    table <- lift $ getById' tableId
    storeUpdate storeTables tableId $ f table

  deleteTable tableId = do
    let query = [ "tableId" =: toObjectId tableId ]
    lift $ deleteByQuery (Proxy :: Proxy Column) query
    lift $ deleteByQuery (Proxy :: Proxy Cell) query
    lift $ deleteByQuery (Proxy :: Proxy Row) query
    storeDelete storeTables tableId

  getTable = storeGetById' storeTables

  createColumn column = do
    columnId <- storeCreate storeColumns column
    case column ^. columnKind of
      ColumnReport _ -> pure ()
      ColumnData dataCol -> do
        let tableId = column ^. columnTableId
        rows <- getTableRows tableId
        for_ rows $ \(Entity rowId _) -> do
          def <- makeDefaultValue (dataCol ^. dataColType)
          let cell = newCell tableId columnId rowId (CellValue def)
          storeCreate storeCells cell
    pure columnId

  modifyColumn columnId f = do
    column <- getColumn columnId
    let newColumn = f column
    storeUpdate storeColumns columnId newColumn

  deleteColumn columnId = do
    -- Cascade delete cells
    lift $ deleteByQuery (Proxy :: Proxy Cell)
      [ "columnId" =: toObjectId columnId ]
    storeDelete storeColumns columnId

  getColumn = storeGetById' storeColumns

  createRow row = do
    rowId <- storeCreate storeRows row
    let tableId = row ^. rowTableId
    columns <- getTableColumns tableId
    for_ columns $ \(Entity columnId column) -> do
      -- Invalidate cache
      engineCache . cacheColumnCells . at columnId .= Nothing
      -- Create cells for every data column
      case column ^. columnKind of
        ColumnReport _ -> pure ()
        ColumnData dataCol -> do
          def <- makeDefaultValue (dataCol ^. dataColType)
          let cell = newCell tableId columnId rowId (CellValue def)
          void $ storeCreate storeCells cell
    pure rowId

  deleteRow rowId = do
    -- Cascade delete cells
    lift $ deleteByQuery (Proxy :: Proxy Cell)
      [ "rowId" =: toObjectId rowId ]
    -- Invalidate cache
    row <- getRow rowId
    columns <- getTableColumns $ row ^. rowTableId
    for_ columns $ \(Entity columnId _) -> do
      engineCache . cacheCellByCoord . at (columnId, rowId) .= Nothing
      engineCache . cacheColumnCells . at columnId .= Nothing
    -- Log
    storeDelete storeRows rowId

  getRow = storeGetById' storeRows

  setCellContent columnId rowId content = do
    Entity cellId cell <- getCellByCoord columnId rowId
    let cell' = cell & cellContent .~ content
    -- Update / Invalidate cache
    engineCache . cacheCellByCoord . at (columnId, rowId) .=
      Just (Entity cellId cell')
    engineCache . cacheColumnCells . at columnId .= Nothing
    storeUpdate storeCells cellId cell'

  --

  getCellByCoord columnId rowId =
    use (engineCache . cacheCellByCoord . at (columnId, rowId)) >>= \case
      Just c -> pure c
      Nothing -> do
        cell <- storeGetByQuery' storeCells
          [ "columnId" =: toObjectId columnId
          , "rowId"    =: toObjectId rowId ]
          (\cell -> cell ^. cellColumnId == columnId
                 && cell ^. cellRowId == rowId)
        engineCache . cacheCellByCoord . at (columnId, rowId) .= Just cell
        pure cell

  getColumnCells columnId =
    use (engineCache . cacheColumnCells . at columnId) >>= \case
      Just cells -> pure cells
      Nothing -> do
        result <- storeListByQuery storeCells
          [ "columnId" =: toObjectId columnId ]
          (\cell -> cell ^. cellColumnId == columnId)
        engineCache . cacheColumnCells . at columnId .= Just result
        pure result

  --

  getProjectTables = do
    projectId <- ask
    storeListByQuery storeTables
      [ "projectId" =: toObjectId projectId ]
      (\table -> table ^. tableProjectId == projectId)

  getTableRows tableId = storeListByQuery storeRows
    [ "tableId" =: toObjectId tableId ]
    (\row -> row ^. rowTableId == tableId)

  getTableColumns tableId = storeListByQuery storeColumns
    [ "tableId" =: toObjectId tableId ]
    (\column -> column ^. columnTableId == tableId)

  getRowRecord rowId = do
    row <- storeGetById' storeRows rowId
    columns <- getTableColumns (row ^. rowTableId)
    vals <- for columns $ \(Entity columnId col) -> do
      Entity _ cell <- getCellByCoord columnId rowId
      let field = col ^. columnName
      let val = case cell ^. cellContent of
            CellError _ -> Nothing
            CellValue v -> Just v
      pure (field, val)
    pure (sequence $ Map.fromList vals)

  getTableByName name = do
    i <- askProjectId
    storeGetByQuery storeTables
      [ "name" =: name, "projectId" =: toObjectId i ]
      (\table -> table ^. tableName == unRef name
              && table ^. tableProjectId == i)

  getColumnOfTableByName tableId name = storeGetByQuery storeColumns
    [ "name" =: name
    , "tableId" =: toObjectId tableId ]
    (\column -> column ^. columnName == unRef name
             && column ^. columnTableId == tableId)

  --

  scheduleCompileColumn columnId = do
    engineCompileTargets . at columnId .= Just ()
    scheduleEvalColumn columnId

  scheduleEvalColumn columnId =
    engineEvalTargets . at columnId .= Just CompleteColumn

  scheduleEvalCell columnId rowId =
    engineEvalTargets . at columnId . non (SpecificRows Set.empty) %= \case
      SpecificRows rows -> SpecificRows $ Set.insert rowId rows
      CompleteColumn    -> CompleteColumn

  getCompileTargets = Set.toList <$> use engineCompileTargets

  getEvalRoots = Map.keys <$> use engineEvalTargets

  getEvalTargets columnId =
    use (engineEvalTargets . at columnId . non (SpecificRows Set.empty)) >>=
      \case
        SpecificRows rs -> pure $ Set.toList rs
        CompleteColumn -> do
          col <- getColumn columnId
          let tableId = col ^. columnTableId
          rows <- storeListByQuery storeRows
            [ "tableId" =: toObjectId tableId ]
            (\row -> row ^. rowTableId == tableId)
          pure $ map entityId rows

--------------------------------------------------------------------------------

-- | First looks in the store, then in the DB
storeGetById :: (MonadHexl m, Model a) => What a -> Id a -> EngineT m (Maybe a)
storeGetById what i = use (engineStore . what . at i) >>= \case
  Just (x, a) -> case x of
    Change Create -> pure $ Just a
    Change Update -> pure $ Just a
    Change Delete -> pure Nothing
    Cached        -> pure $ Just a
  Nothing -> lift (getById i) >>= \case
    Left _  -> pure Nothing
    Right a -> do
      engineStore . what . at i .= Just (Cached, a)
      pure $ Just a

storeGetById' :: (MonadHexl m, Model a) => What a -> Id a -> EngineT m a
storeGetById' what i = storeGetById what i >>= \case
  Just a -> pure a
  Nothing -> lift $ throwError $
    ErrBug $ "storeGetById': could not find id " <> show i

storeGetChangedList :: (MonadHexl m) => What a -> EngineT m [Entity a]
storeGetChangedList what = do
  m <- use (engineStore . what)
  let p (i, (x, a)) = case x of
        Change Create -> Just (Entity i a)
        Change Update -> Just (Entity i a)
        Change Delete -> Nothing
        Cached        -> Nothing
  pure $ mapMaybe p (Map.toList m)

storeGetByQuery :: (MonadHexl m, Model a)
                => What a -> Selector -> (a -> Bool)
                -> EngineT m (Maybe (Entity a))
storeGetByQuery what query p = do
  changes <- storeGetChangedList what
  case find (p . entityVal) changes of
    Just e  -> pure $ Just e
    Nothing -> lift (getOneByQuery query) >>= \case
      Left _ -> pure Nothing
      Right e -> do
        -- If e is found in the DB, we need to make sure that a perhaps
        -- updated version of it in the store satisfies `p`.
        use (engineStore . what . at (entityId e)) >>= \case
          Nothing          -> pure $ Just e
          Just (Cached, _) -> pure $ Just e
          Just (Change op, a') -> case op of
            Delete -> pure Nothing
            _      -> pure $ if p a' then Just e
                                     else Nothing

storeGetByQuery' :: (MonadHexl m, Model a)
                 => What a -> Selector -> (a -> Bool)
                 -> EngineT m (Entity a)
storeGetByQuery' what query p = storeGetByQuery what query p >>= \case
  Nothing -> lift $ throwError $ ErrBug "storeGetByQuery': not found"
  Just e -> pure e

storeListByQuery :: (MonadHexl m, Model a)
                 => What a -> Selector -> (a -> Bool)
                 -> EngineT m [Entity a]
storeListByQuery what query p = do
    lst <- lift $ listByQuery query
    changes <- use (engineStore . what)
    pure $ foldl' apply lst (Map.toList changes)
  where
    updateList :: (a -> Bool) -> a -> [a] -> [a]
    updateList _ _ [] = []
    updateList p' a (x:xs) = if p' x then a : updateList p' a xs
                                     else x : updateList p' a xs
    apply as (i, (x, a)) = if p a
      then case x of
        Cached        -> as
        Change Create -> as -- We don't add these here since "Created" entries
                            -- have actually been created in the DB.
        Change Update -> updateList (\(Entity i' _) -> i' == i)
                                    (Entity i a) as
        Change Delete -> filter (\(Entity i' _) -> i' /= i) as
      else as

-- | Also actually creates the entity, because otherwise we wouldn't be able
-- to get the id...
storeCreate :: (MonadHexl m, Model a) => What a -> a -> EngineT m (Id a)
storeCreate what a = do
  i <- lift $ create a
  engineStore . what . at i .= Just (Change Create, a)
  pure i

storeUpdate :: MonadHexl m => What a -> Id a -> a -> EngineT m ()
storeUpdate what i a = engineStore . what . at i .= Just (Change Update, a)

storeDelete :: (MonadHexl m, Model a) => What a -> Id a -> EngineT m ()
storeDelete what i = do
  a <- storeGetById' what i
  engineStore . what . at i .= Just (Change Delete, a)
