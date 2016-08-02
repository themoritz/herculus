{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs                      #-}

module Eval
  ( runEval
  ) where

import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.Monoid
import           Data.Text (Text)

import           Database.MongoDB     ((=:))

import           Lib.Types
import           Lib.Model.Types
import           Lib.Model.Column
import           Lib.Model.Cell
import           Lib.Model

import           Propagate.Cache
import           Monads

data EvalEnv = EvalEnv
  { evalRecordId  :: Id Record
  , evalCellCache :: CellCache
  }

newtype EvalT m a = EvalT
  { unEvalT :: ReaderT EvalEnv (ExceptT Text m) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadReader EvalEnv
             , MonadError Text
             )

instance MonadTrans EvalT where
  lift = EvalT . lift . lift

runEval :: MonadHexl m => CellCache -> Id Record
        -> TExpr a -> m (Either Text a)
runEval cache r expr =
  let env = EvalEnv r cache
      action = unEvalT $ eval expr
  in runExceptT $ runReaderT action env

eval :: MonadHexl m => TExpr a -> EvalT m a
eval expr = case expr of
  TExprLitString txt -> pure txt
  TExprLitNumber num -> pure num
  TExprColumnRefString col -> getCellValue col
  TExprColumnRefNumber col -> getCellValue col
  TExprColumnRefStrings col -> getColumnValues col
  TExprColumnRefNumbers col -> getColumnValues col
  TExprStringAppend l r -> (<>) <$> eval l <*> eval r
  TExprNumberAdd l r -> (+) <$> eval l <*> eval r
  TExprSum sub -> sum <$> eval sub

getCellValue :: (MonadHexl m, ExtractValue a) => Id Column -> EvalT m a
getCellValue colId = do
  recId <- asks evalRecordId
  cellCache <- asks evalCellCache
  val <- case retrieve colId recId cellCache of
    Just val -> pure val
    Nothing -> do
      let cellQuery =
            [ "aspects.columnId" =: toObjectId colId
            , "aspects.recordId" =: toObjectId recId
            ]
      cell <- lift $ getOneByQuery' cellQuery
      case cellResult $ entityVal cell of
        CellParseError _ -> throwError "dependent cell not parsed"
        CellEvalError _ -> throwError "dependent cell not evaluated"
        CellOk val -> pure val
  case extractValue val of
    Just x -> pure x
    Nothing -> throwError "wrong type in value"

getColumnValues :: (MonadHexl m, ExtractValue a) => Id Column -> EvalT m [a]
getColumnValues colId = do
  let query = [ "aspects.columnId" =: toObjectId colId ]
  cells <- lift $ listByQuery query
  let go cell = case cellResult $ entityVal cell of
        CellParseError _ -> throwError "dependent cell not parsed"
        CellEvalError _ -> throwError "dependent cell not evaluated"
        CellOk val -> case extractValue val of
          Just x -> pure x
          Nothing -> throwError "Cannot parse value"
  traverse go cells
