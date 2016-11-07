{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Propagate.Monad where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State  (MonadState, StateT, evalStateT)

import           Data.Map             (Map)
import qualified Data.Map             as Map

import           Database.MongoDB     ((=:))

import           Lib.Model
import           Lib.Model.Cell
import           Lib.Model.Column
import           Lib.Model.Record
import           Lib.Types

import           Cache
import           Monads

data AddTarget
  = CompleteColumn
  | OneRecord (Id Record)

class MonadCache m => MonadPropagate m where
  addTargets :: Id Column -> AddTarget -> m ()
  getTargets :: Id Column -> m [Id Record]

data State = State
  { _stateTargets :: Map (Id Column) (Maybe [Id Record]) -- Nothing = All
  }

makeLenses ''State

newtype PropT m a = PropT
  { unPropT :: StateT State (CacheT m) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadState State
             )

instance MonadTrans PropT where
  lift = PropT . lift . lift

instance MonadError AppError m => MonadError AppError (PropT m) where
  throwError = lift . throwError
  catchError a h = PropT $ unPropT a `catchError` (unPropT . h)

runPropT :: MonadHexl m => PropT m a -> m (a, [Entity Cell])
runPropT action = runCacheT $ evalStateT (unPropT action) (State Map.empty)

instance MonadHexl m => MonadCache (PropT m) where
  getCellValue c     = PropT . lift . getCellValue c
  setCellContent c r = PropT . lift . setCellContent c r
  getColumnValues    = PropT . lift . getColumnValues
  getTableRecords    = PropT . lift . getTableRecords
  getRecordValue c   = PropT . lift . getRecordValue c
  getCompileResult   = PropT . lift . getCompileResult


instance MonadHexl m => MonadPropagate (PropT m) where

  addTargets c prop = stateTargets . at c . non (Just []) %=
    \col -> case (col, prop) of
      (Just rs, OneRecord r) -> Just $ r:rs
      _                      -> Nothing

  getTargets c = use (stateTargets . at c . non (Just [])) >>= \case
    Just rs -> pure rs
    Nothing -> do
      col <- lift $ getById' c
      records <- lift $ listByQuery [ "tableId" =: toObjectId (_columnTableId col) ]
      pure $ map entityId records
