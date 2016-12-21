{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
-- |

module Engine.Monad where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State

import           Data.Foldable                (for_)
import           Data.Functor                 (($>))
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Proxy
import           Data.Set                     (Set)
import qualified Data.Set                     as Set

import           Database.MongoDB             ((=:))

import           Lib.Model
import           Lib.Model.Cell
import           Lib.Model.Class
import           Lib.Model.Column
import           Lib.Model.Dependencies
import           Lib.Model.Dependencies.Types
import           Lib.Model.Row
import           Lib.Model.Table
import           Lib.Types

import           Monads

--------------------------------------------------------------------------------

type ChangeMap a = Map (Id a) (ChangeOp a)

data Changes = Changes
  { _changesCells   :: ChangeMap Cell
  , _changesColumns :: ChangeMap Column
  , _changesRows    :: ChangeMap Row
  , _changesTables  :: ChangeMap Table
  }

makeLenses ''Changes

type What a = Lens' Changes (Map (Id a) (ChangeOp a))

emptyChanges :: Changes
emptyChanges = Changes Map.empty Map.empty Map.empty Map.empty

-- | Note: cache does not have a notion that columns of cells have been deleted
-- earlier in the transaction.
data Cache = Cache
  { _cacheCells       :: Map (Id Column, Id Row) (Entity Cell)
  , _cacheColumnCells :: Map (Id Column) [Entity Cell]
  , _cacheColumns     :: Map (Id Column) Column
  }

makeLenses ''Cache

emptyCache :: Cache
emptyCache = Cache Map.empty Map.empty Map.empty

data EvalTargets
  = CompleteColumn
  | SpecificRows (Set (Id Row))
  deriving (Eq)

data EngineState = EngineState
  { _engineChanges        :: Changes
  , _engineCache          :: Cache
  , _engineGraph          :: DependencyGraph
  , _engineCompileTargets :: Set (Id Column)
  , _engineEvalTargets    :: Map (Id Column) EvalTargets
  }

makeLenses ''EngineState

newEngineState :: DependencyGraph -> EngineState
newEngineState graph =
  EngineState emptyChanges emptyCache graph Set.empty Map.empty

--------------------------------------------------------------------------------

class MonadError AppError m => MonadEngine m where
  -- Operations on dependency graph

  graphGets   :: (DependencyGraph -> a) -> m a
  graphModify :: (DependencyGraph -> DependencyGraph) -> m ()
  graphSetCodeDependencies :: Id Column -> CodeDependencies -> m Bool
  graphSetTypeDependencies :: Id Column -> TypeDependencies -> m Bool

  -- Misc database queries

  getTableByName :: Ref Table -> m (Maybe (Entity Table))
  getColumnOfTableByName :: Id Table -> Ref Column -> m (Maybe (Entity Column))
  makeDefaultValue :: DataType -> m Value

  -- Perform changes to DB objects. Guiding principles:
  -- * Functions cascade
  -- * Changes are stored in cache
  -- * Changes will be collected and commited / broadcasted at the end

  createTable :: Table -> m (Id Table)
  modifyTable :: Id Table -> (Table -> Table) -> m ()
  deleteTable :: Id Table -> m ()

  createColumn :: Column -> m (Id Column)
  modifyColumn :: Id Column -> (Column -> Column) -> m ()
  deleteColumn :: Id Column -> m ()

  createRow :: Row -> m (Id Row)
  deleteRow :: Id Row -> m ()

  createCell :: Cell -> m (Id Cell)
  setCellContent :: Id Column -> Id Row -> CellContent -> m ()

  -- Get cached values

  getCell         :: Id Column -> Id Row -> m (Entity Cell)
  getColumn       :: Id Column -> m Column
  getColumnCells  :: Id Column -> m [Entity Cell]
  getTableRows    :: Id Table -> m [Entity Row]
  getTableColumns :: Id Table -> m [Entity Column]
  getRow          :: Id Row -> m Row
  getRowField     :: Id Row -> Ref Column -> m (Maybe Value)

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
  { unEngineT :: StateT EngineState m a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadState EngineState
             )

runEngineT :: DependencyGraph -> EngineT m a -> m (a, EngineState)
runEngineT graph action = runStateT (unEngineT action) (newEngineState graph)

--------------------------------------------------------------------------------

instance MonadTrans EngineT where
  lift = EngineT . lift

instance MonadError AppError m => MonadError AppError (EngineT m) where
  throwError = lift . throwError
  catchError a h = EngineT $ unEngineT a `catchError` (unEngineT . h)

instance MonadHexl m => MonadEngine (EngineT m) where

  graphGets f = f <$> use engineGraph

  graphModify f = engineGraph %= f

  graphSetCodeDependencies columnId deps = do
    let getTableId c = _columnTableId <$> lift (getById' c)
    graph <- use engineGraph
    setCodeDependencies getTableId columnId deps graph >>= \case
      Nothing -> pure True
      Just graph' -> (engineGraph .= graph') $> False

  graphSetTypeDependencies columnId deps = do
    graph <- use engineGraph
    case setTypeDependencies columnId deps graph of
      Nothing     -> pure True
      Just graph' -> (engineGraph .= graph') $> False

  --

  getTableByName name =
    let query = [ "name" =: name ]
    in  eitherToMaybe <$> lift (getOneByQuery query)

  getColumnOfTableByName tableId name =
    let query = [ "name" =: name
                , "tableId" =: toObjectId tableId ]
    in  eitherToMaybe <$> lift (getOneByQuery query)

  makeDefaultValue = \case
    DataBool     -> pure $ VBool False
    DataString   -> pure $ VString ""
    DataNumber   -> pure $ VNumber 0
    DataTime     -> VTime <$> lift getCurrentTime
    DataRowRef t -> do
      res <- lift $ getOneByQuery [ "tableId" =: toObjectId t ]
      pure $ VRowRef $ case res of
        Left _             -> Nothing
        Right (Entity i _) -> Just i
    DataList _   -> pure $ VList []
    DataMaybe _  -> pure $ VMaybe Nothing

  --

  createTable = logCreate changesTables

  modifyTable tableId f = do
    table <- lift $ getById' tableId
    logUpdate changesTables tableId $ f table

  deleteTable tableId = do
    let query = [ "tableId" =: toObjectId tableId ]
    lift $ deleteByQuery (Proxy :: Proxy Column) query
    lift $ deleteByQuery (Proxy :: Proxy Cell) query
    lift $ deleteByQuery (Proxy :: Proxy Row) query
    logDelete changesTables tableId

  createColumn column = do
    columnId <- logCreate changesColumns column
    engineCache . cacheColumns . at columnId .= Just column
    pure columnId

  modifyColumn columnId f = do
    column <- getColumn columnId
    let newColumn = f column
    logUpdate changesColumns columnId newColumn
    engineCache . cacheColumns . at columnId .= Just column

  deleteColumn columnId = do
    -- Cascade delete cells
    lift $ deleteByQuery (Proxy :: Proxy Cell)
      [ "columnId" =: toObjectId columnId ]
    logDelete changesColumns columnId

  createRow row = do
    -- FIXME: We invalidate the columnCells cache. This should work since the
    -- next time a column is queried, the cells will have been created.
    -- Still really ugly.
    columns <- getTableColumns $ row ^. rowTableId
    for_ columns $ \(Entity columnId _) ->
      engineCache . cacheColumnCells . at columnId .= Nothing
    logCreate changesRows row

  deleteRow rowId = do
    -- Cascade delete cells
    lift $ deleteByQuery (Proxy :: Proxy Cell)
      [ "rowId" =: toObjectId rowId ]
    -- Update cache
    row <- getRow rowId
    columns <- getTableColumns $ row ^. rowTableId
    for_ columns $ \(Entity columnId _) -> do
      engineCache . cacheCells . at (columnId, rowId) .= Nothing
      engineCache . cacheColumnCells . at columnId . _Just %= filter
        (\(Entity _ cell) -> cell ^. cellRowId /= rowId)
    -- Log
    logDelete changesRows rowId

  createCell cell = do
    cellId <- logCreate changesCells cell
    engineCache . cacheCells . at (cell ^. cellColumnId, cell ^. cellRowId) .=
      Just (Entity cellId cell)
    pure cellId

  setCellContent columnId rowId content = do
    Entity cellId cell <- getCell columnId rowId
    let cell' = cell & cellContent .~ content
    engineCache . cacheCells . at (columnId, rowId) .=
      Just (Entity cellId cell')
    -- FIXME: Need also to update the column lists, but should be safe for now.
    logUpdate changesCells cellId cell'

  --

  -- TODO: cache functions

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
    use (engineEvalTargets . at columnId . non (SpecificRows Set.empty)) >>= \case
      SpecificRows rs -> pure $ Set.toList rs
      CompleteColumn -> do
        col <- getColumn columnId
        rows <- lift $ listByQuery [ "tableId" =: toObjectId (_columnTableId col) ]
        pure $ map entityId rows

--------------------------------------------------------------------------------

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _)  = Nothing
eitherToMaybe (Right b) = Just b

-- | Also actually creates the entity, because otherwise we wouldn't be able
-- to get the id...
logCreate :: (MonadHexl m, Model a) => What a -> a -> EngineT m (Id a)
logCreate what a = do
  i <- lift $ create a
  engineChanges . what . at i .= Just (Create a)
  pure i

logUpdate :: MonadHexl m => What a -> Id a -> a -> EngineT m ()
logUpdate what i a = engineChanges . what . at i .= Just (Update a)

logDelete :: MonadHexl m => What a -> Id a -> EngineT m ()
logDelete what i = engineChanges . what . at i .= Just Delete
