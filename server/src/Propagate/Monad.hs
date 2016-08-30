{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Propagate.Monad where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State  (MonadState, StateT, gets, runStateT)

import           Data.Map             (Map)
import qualified Data.Map             as Map

import           Database.MongoDB     ((=:))

import           Lib.Api.WebSocket
import           Lib.Model
import           Lib.Model.Cell
import           Lib.Model.Column
import           Lib.Model.Types
import           Lib.Types

import           Monads
import           Propagate.Cache

data AddTarget
  = CompleteColumn
  | OneRecord (Id Record)

class (MonadError AppError m, Monad m) => MonadPropagate m where
  getCellValue :: Id Column -> Id Record -> m (Maybe Value)
  setCellContent :: Id Column -> Id Record -> CellContent -> m ()
  getColumnValues :: Id Column -> m [Maybe Value]
  getTableRecords :: Id Table -> m [Id Record]
  getRecordValue  :: Id Record -> Ref Column -> m (Maybe Value)
  addTargets :: Id Column -> AddTarget -> m ()
  getTargets :: Id Column -> m [Id Record]
  getCompileResult :: Id Column -> m CompileResult

data State = State
  { _stateCache   :: Cache
  , _stateTargets :: Map (Id Column) (Maybe [Id Record]) -- Nothing = All
  }

makeLenses ''State

newtype PropT m a = PropT
  { unPropT :: StateT State m a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadState State
             )

instance MonadTrans PropT where
  lift = PropT . lift

instance MonadError AppError m => MonadError AppError (PropT m) where
  throwError = lift . throwError
  catchError a h = PropT $ (unPropT a) `catchError` (unPropT . h)

runPropagate :: MonadHexl m => PropT m a -> m a
runPropagate action = do
  (a, st) <- runStateT (unPropT action) (State empty Map.empty)
  sendWS $ WsDownCellsChanged $ getAllCells $ _stateCache st
  pure a

cacheCell :: Monad m => Id Column -> Id Record -> Cell -> PropT m ()
cacheCell c r result = stateCache %= storeCell c r result

instance MonadHexl m => MonadPropagate (PropT m) where
  getCellValue c r = do
    result <- gets (getCell c r . _stateCache) >>= \case
      Just cell -> pure $ cellContent cell
      Nothing -> do
        Entity _ cell <- lift $ getOneByQuery'
          [ "aspects.columnId" =: toObjectId c
          , "aspects.recordId" =: toObjectId r
          ]
        cacheCell c r cell
        pure $ cellContent cell
    case result of
      CellError _ -> pure Nothing
      CellValue val -> pure $ Just val

  setCellContent c r content = do
    Entity i cell <- lift $ getOneByQuery'
      [ "aspects.columnId" =: toObjectId c
      , "aspects.recordId" =: toObjectId r
      ]
    let updatedCell = cell { cellContent = content }
    cacheCell c r updatedCell
    lift $ update i $ const updatedCell

  getColumnValues c = do
    results <- gets (getColumn c . _stateCache) >>= \case
      Just results -> pure results
      Nothing -> do
        let query = [ "aspects.columnId" =: toObjectId c ]
        cells <- lift $ listByQuery query
        let results = map (cellContent . entityVal) cells
        stateCache %= storeColumn c results
        pure results
    let go = \case
          CellError _ -> pure Nothing
          CellValue val -> pure $ Just val
    traverse go results

  getTableRecords t = do
    -- TODO: cache
    rs <- lift $ listByQuery [ "tableId" =: toObjectId t ]
    pure $ map entityId rs

  getRecordValue r colRef = do
    -- TODO: cache
    record <- lift $ getById' r
    col <- lift $ getOneByQuery'
      [ "name" =: unRef colRef
      , "tableId" =: toObjectId (recordTableId record)
      ]
    getCellValue (entityId col) r

  addTargets c prop = stateTargets . at c . non (Just []) %=
    \col -> case (col, prop) of
      (Just rs, OneRecord r) -> Just $ r:rs
      _                      -> Nothing

  getTargets c = use (stateTargets . at c . non (Just [])) >>= \case
    Just rs -> pure rs
    Nothing -> do
      col <- lift $ getById' c
      records <- lift $ listByQuery [ "tableId" =: toObjectId (columnTableId col) ]
      pure $ map entityId records

  getCompileResult c = gets (getCode c . _stateCache) >>= \case
    Just result -> pure result
    Nothing -> do
      col <- lift $ getById' c
      let compileResult = columnCompileResult col
      stateCache %= storeCode c compileResult
      pure compileResult
