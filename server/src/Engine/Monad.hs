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

data ChangeOp a
  = Created a
  | Update a
  | Delete

data Changes = Changes
  { _changesCells   :: Map (Id Cell) (ChangeOp Cell)
  , _changesColumns :: Map (Id Column) (ChangeOp Column)
  , _changesRows    :: Map (Id Row) (ChangeOp Row)
  , _changesTables  :: Map (Id Table) (ChangeOp Table)
  }

makeLenses ''Changes

type What a = Lens' Changes (Map (Id a) (ChangeOp a))

emptyChanges :: Changes
emptyChanges = Changes Map.empty Map.empty Map.empty Map.empty

data Cache = Cache
  { _cacheCell        :: !(Map (Id Column, Id Row) (Entity Cell))
  , _cacheColumnCells :: !(Map (Id Column) [CellContent])
  , _cacheColumn      :: !(Map (Id Column) Column)
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

  graphGets :: (DependencyGraph -> a) -> m a
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
  scheduleEvalColumn :: Id Column -> m ()
  scheduleEvalCell :: Id Column -> Id Row -> m ()

  getEvalRoots :: m [Id Column]
  getEvalTargets :: Id Column -> m [Id Row]

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

  getColumnOfTableByName tableId name = do
    let colQuery =
          [ "name" =: name
          , "tableId" =: toObjectId tableId
          ]
    eitherToMaybe <$> lift (getOneByQuery colQuery)

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

  deleteTable tableId = do
    let query = [ "tableId" =: toObjectId tableId ]
    lift $ deleteByQuery (Proxy :: Proxy Column) query
    lift $ deleteByQuery (Proxy :: Proxy Cell) query
    lift $ deleteByQuery (Proxy :: Proxy Row) query
    opDelete changesTables tableId

  deleteColumn columnId = do
    lift $ deleteByQuery (Proxy :: Proxy Cell)
      [ "columnId" =: toObjectId columnId ]
    opDelete changesColumns columnId

  scheduleCompileColumn columnId = do
    engineCompileTargets . at columnId .= Just ()
    scheduleEvalColumn columnId

  scheduleEvalColumn columnId =
    engineEvalTargets . at columnId .= Just CompleteColumn

  scheduleEvalCell columnId rowId =
    engineEvalTargets . at columnId . non (SpecificRows Set.empty) %= \case
      SpecificRows rows -> SpecificRows $ Set.insert rowId rows
      CompleteColumn    -> CompleteColumn

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

opDelete :: (MonadHexl m, Model a) => What a -> Id a -> EngineT m ()
opDelete what i = do
  engineChanges . what . at i .= Just Delete
  lift $ delete i
