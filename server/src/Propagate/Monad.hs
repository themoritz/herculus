{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

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

data Propagate
  = CompleteColumn
  | OneRecord (Id Record)

class Monad m => MonadPropagate m where
  getCellValue :: ExtractValue a => Id Column -> Id Record -> m (Maybe a)
  setCellContent :: Id Column -> Id Record -> CellContent -> m ()
  getColumnValues :: ExtractValue a => Id Column -> m [Maybe a]
  addTargets :: Id Column -> Propagate -> m ()
  getTargets :: Id Column -> m [Id Record]
  getCompiledCode :: Id Column -> m ATExpr

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

runPropagate :: MonadHexl m => PropT m a -> m a
runPropagate action = do
  (a, st) <- runStateT (unPropT action) (State empty Map.empty)
  sendWS $ WsDownCellsChanged $ getAllCells $ _stateCache st
  pure a

cacheCellContent :: Monad m => Id Column -> Id Record -> CellContent -> PropT m ()
cacheCellContent c r result = stateCache %= storeCell c r result

instance MonadHexl m => MonadPropagate (PropT m) where
  getCellValue c r = do
    result <- gets (getCell c r . _stateCache) >>= \case
      Just result -> pure result
      Nothing -> do
        let cellQuery =
              [ "aspects.columnId" =: toObjectId c
              , "aspects.recordId" =: toObjectId r
              ]
        cell <- lift $ getOneByQuery' cellQuery
        let result = cellContent $ entityVal cell
        cacheCellContent c r result
        pure result
    case result of
      CellNothing -> pure Nothing
      CellEvalError _ -> pure Nothing
      CellValue val -> case extractValue val of
        Just x -> pure $ Just x
        Nothing -> lift $ throwError $ ErrBug "wrong type in value"

  setCellContent c r content = do
    cacheCellContent c r content
    let query =
          [ "aspects.columnId" =: toObjectId c
          , "aspects.recordId" =: toObjectId r
          ]
    lift $ updateByQuery' query $ \cell -> cell { cellContent = content }

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
          CellNothing -> pure Nothing
          CellEvalError _ -> pure Nothing
          CellValue val -> case extractValue val of
            Just x -> pure $ Just x
            Nothing -> lift $ throwError $ ErrBug "wrong type in value"
    traverse go results

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

  getCompiledCode c = gets (getCode c . _stateCache) >>= \case
    Just atexpr -> pure atexpr
    Nothing -> do
      col <- lift $ getById' c
      case columnCompileResult col of
        CompileResultCode atexpr@(_ ::: ttype) ->
          case checkSig (columnDataType col) ttype of
            Just Ok -> do
              stateCache %= storeCode c atexpr
              pure atexpr
            Nothing -> lift $ throwError $
              ErrBug "getCompiledCode: signature mismatch"
        _ -> lift $ throwError $ ErrBug "getCompiledCode: no atexpr found"
