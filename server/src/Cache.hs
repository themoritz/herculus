{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Cache
  ( runCacheT
  , CacheT
  , MonadCache (..)
  ) where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.State  (MonadState, StateT, gets, modify,
                                       runStateT)

import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as Map

import           Database.MongoDB     ((=:))

import           Lib.Model
import           Lib.Model.Cell
import           Lib.Model.Column
import           Lib.Model.Record
import           Lib.Model.Table
import           Lib.Types

import           Monads

data Cache = Cache
  { _cacheCell          :: !(Map (Id Column, Id Record) Cell)
  , _cacheCellsModified :: !(Map (Id Column, Id Record) (Entity Cell))
  , _cacheColumn        :: !(Map (Id Column) [CellContent])
  , _cacheCompileResult :: !(Map (Id Column) DataCompileResult)
  }

makeLenses ''Cache

empty :: Cache
empty = Cache Map.empty Map.empty Map.empty Map.empty

--

storeCell :: Id Column -> Id Record -> Cell -> Cache -> Cache
storeCell c r v = set (cacheCell . at (c, r)) (Just v)

setCellModified :: Id Column -> Id Record -> Entity Cell -> Cache -> Cache
setCellModified c r v = set (cacheCellsModified . at (c, r)) (Just v)

getCell :: Id Column -> Id Record -> Cache -> Maybe Cell
getCell c r = view (cacheCell . at (c, r))

getCellsModified :: Cache -> [Entity Cell]
getCellsModified = Map.elems . _cacheCellsModified

--

storeColumn :: Id Column -> [CellContent] -> Cache -> Cache
storeColumn c vs = set (cacheColumn . at c) (Just vs)

getColumn :: Id Column -> Cache -> Maybe [CellContent]
getColumn c = view (cacheColumn . at c)

--

storeCode :: Id Column -> DataCompileResult -> Cache -> Cache
storeCode c expr = set (cacheCompileResult . at c) (Just expr)

getCode :: Id Column -> Cache -> Maybe DataCompileResult
getCode c = view (cacheCompileResult . at c)

--

class (MonadError AppError m, Monad m) => MonadCache m where
  getCellValue :: Id Column -> Id Record -> m (Maybe Value)
  setCellContent :: Id Column -> Id Record -> CellContent -> m ()
  getColumnValues :: Id Column -> m [Maybe Value]
  getTableRecords :: Id Table -> m [Id Record]
  getRecordValue  :: Id Record -> Ref Column -> m (Maybe Value)
  getCompileResult :: Id Column -> m (Maybe DataCompileResult)

newtype CacheT m a = CacheT
  { unCacheT :: StateT Cache m a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadState Cache
             )

runCacheT :: MonadHexl m => CacheT m a -> m (a, [Entity Cell])
runCacheT action = do
  (a, st) <- runStateT (unCacheT action) empty
  pure (a, getCellsModified st)

instance MonadTrans CacheT where
  lift = CacheT . lift

instance MonadError AppError m => MonadError AppError (CacheT m) where
  throwError = lift . throwError
  catchError a h = CacheT $ unCacheT a `catchError` (unCacheT . h)

instance MonadHexl m => MonadCache (CacheT m) where
  getCellValue c r = do
    result <- gets (getCell c r) >>= \case
      Just cell -> pure $ cellContent cell
      Nothing -> do
        Entity _ cell <- lift $ getOneByQuery'
          [ "aspects.columnId" =: toObjectId c
          , "aspects.recordId" =: toObjectId r
          ]
        modify $ storeCell c r cell
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
    modify $ storeCell c r updatedCell . setCellModified c r (Entity i updatedCell)

  getColumnValues c = do
    results <- gets (getColumn c) >>= \case
      Just results -> pure results
      Nothing -> do
        let query = [ "aspects.columnId" =: toObjectId c ]
        cells <- lift $ listByQuery query
        let results = map (cellContent . entityVal) cells
        modify $ storeColumn c results
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

  getCompileResult c = gets (getCode c) >>= \case
    Just result -> pure $ Just result
    Nothing -> do
      col <- lift $ getById' c
      case col ^? columnKind . _ColumnData . dataColCompileResult of
        Nothing -> pure Nothing
        Just compileResult -> do
          modify $ storeCode c compileResult
          pure $ Just compileResult
