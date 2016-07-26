{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Eval where

import           Control.Monad
import           Control.Monad.Except

import           Data.Text
import Data.Maybe
import Data.Monoid

import           Database.MongoDB     ((=:))
import qualified Database.MongoDB     as Mongo

import           Lib
import           Lib.Expression

import           Monads

import           Control.Monad.Reader

data EvalEnv = EvalEnv
  { evalTableId  :: Id Table
  , evalColumnId :: Id Column
  , evalRecordId :: Id Record
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

runEval :: MonadHexl m => Id Table -> Id Column -> Id Record
        -> Expr -> m (Either Text Value)
runEval t c r expr =
  let env = EvalEnv t c r
      action = unEvalT $ eval expr
  in runExceptT $ runReaderT action env

eval :: MonadHexl m => Expr -> EvalT m Value
eval (ExprBinOp Append l r) = (<>) <$> eval l <*> eval r
eval (ExprStringLit txt) = pure $ Value txt
eval (ExprColumnRef colName) = do
  ownTblId <- asks evalTableId
  let colQuery =
        [ "name" =: colName
        , "tableId" =: toObjectId ownTblId
        ]
  mColumn <- lift $ runMongo $ Mongo.findOne (Mongo.select colQuery "columns")
  ownRecId <- asks evalRecordId
  case mColumn >>= Mongo.lookup "_id" of
    Nothing -> throwError "Column not found on same table"
    Just colId -> do
      let cellQuery =
            [ "aspects" =:
              [ "columnId" =: (colId :: Mongo.ObjectId)
              , "recordId" =: toObjectId ownRecId
              , "tableId"  =: toObjectId ownTblId
              ]
            ]
      mCell <- lift $ runMongo $ Mongo.findOne (Mongo.select cellQuery "cells")
      pure $ fromMaybe "" $ mCell >>= Mongo.lookup "value"
