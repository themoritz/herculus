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

import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Proxy

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

data EngineState = EngineState
  { _engineChanges :: Changes
  , _engineCache   :: Cache
  , _engineGraph   :: DependencyGraph
  }

makeLenses ''EngineState

newEngineState :: DependencyGraph -> EngineState
newEngineState graph = EngineState emptyChanges emptyCache graph

--------------------------------------------------------------------------------

class (MonadError AppError m, MonadHexl h) => MonadEngine h m | m -> h where
  graphGets :: (DependencyGraph -> a) -> m a
  graphModify :: (DependencyGraph -> DependencyGraph) -> m ()
  graphSetDependencies :: Id Column
    -> ([(Id Column, ColumnDependency)], [(Id Table, TableDependency)]) -> m Bool

  -- Fallback (to be eliminated)
  liftDB :: h a -> m a

  tableCreate :: Table -> m (Id Table)
  tableModify :: Id Table -> (Table -> Table) -> m ()
  tableDelete :: Id Table -> m ()

  columnCreate :: Column -> m (Id Column)

  cellCreate :: Cell -> m (Id Cell)

  -- Cached
  getCellValue    :: Id Column -> Id Row -> m (Maybe Value)
  setCellContent  :: Id Column -> Id Row -> CellContent -> m ()
  getColumnValues :: Id Column -> m [Maybe Value]
  getTableRows    :: Id Table -> m [Id Row]
  getRowField     :: Id Row -> Ref Column -> m (Maybe Value)
  getColumn       :: Id Column -> m Column

--------------------------------------------------------------------------------

-- | MonadHexl interpreter of MonadEngine
newtype EngineT m a = EngineT
  { unEngineT :: StateT EngineState m a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadState EngineState
             )

runEngine :: MonadHexl m => DependencyGraph -> EngineT m () -> m EngineState
runEngine graph action = execStateT (unEngineT action) (newEngineState graph)

--------------------------------------------------------------------------------

instance MonadTrans EngineT where
  lift = EngineT . lift

instance MonadError AppError m => MonadError AppError (EngineT m) where
  throwError = lift . throwError
  catchError a h = EngineT $ unEngineT a `catchError` (unEngineT . h)

instance MonadHexl m => MonadEngine m (EngineT m) where
  graphGets f = f <$> use engineGraph

  tableDelete tableId = do
    let query = [ "tableId" =: toObjectId tableId ]
    lift $ deleteByQuery (Proxy :: Proxy Column) query
    lift $ deleteByQuery (Proxy :: Proxy Cell) query
    lift $ deleteByQuery (Proxy :: Proxy Row) query
    opDelete changesTables tableId

--------------------------------------------------------------------------------

opDelete :: (MonadHexl m, Model a) => What a -> Id a -> EngineT m ()
opDelete what i = do
  engineChanges . what . at i .= Just Delete
  lift $ delete i
