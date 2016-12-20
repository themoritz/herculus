{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
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
  { _cacheCell          :: !(Map (Id Column, Id Row) Cell)
  , _cacheCellsModified :: !(Map (Id Column, Id Row) (Entity Cell))
  , _cacheColumnCells   :: !(Map (Id Column) [CellContent])
  , _cacheColumn        :: !(Map (Id Column) Column)
  }

makeLenses ''Cache

emptyCache :: Cache
emptyCache = Cache Map.empty Map.empty Map.empty Map.empty

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

class (MonadError AppError m, MonadHexl h) => MonadEngine h m | m -> h where
  graphGets :: (DependencyGraph -> a) -> m a
  graphModify :: (DependencyGraph -> DependencyGraph) -> m ()
  graphSetCodeDependencies :: Id Column -> CodeDependencies -> m Bool
  graphSetTypeDependencies :: Id Column -> TypeDependencies -> m Bool

  -- | Fallback (to be eliminated)
  liftDB :: h a -> m a

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

  -- Get cached values

  getCellValue    :: Id Column -> Id Row -> m (Maybe Value)
  getColumnCells  :: Id Column -> m [Entity Cell]
  getTableRows    :: Id Table -> m [Id Row]
  getTableColumns :: Id Table -> m [Entity Column]
  getRowField     :: Id Row -> Ref Column -> m (Maybe Value)
  getColumn       :: Id Column -> m Column
  getRow          :: Id Row -> m Row

  setCellContent :: Id Column -> Id Row -> CellContent -> m ()

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

instance MonadHexl m => MonadEngine m (EngineT m) where
  graphGets f = f <$> use engineGraph

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

opDelete :: (MonadHexl m, Model a) => What a -> Id a -> EngineT m ()
opDelete what i = do
  engineChanges . what . at i .= Just Delete
  lift $ delete i
