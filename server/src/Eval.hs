{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module Eval
  ( runEval
  , collectDependencies
  ) where

import           Control.Monad.Except

import           Data.Maybe
import           Data.Monoid
import           Data.Text

import           Database.MongoDB     ((=:))
import qualified Database.MongoDB     as Mongo

import           Lib
import           Lib.Expression

import           CellCache
import           Dependencies
import           Monads

import           Control.Monad.Reader

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
        -> Expr -> m (Either Text Value)
runEval cache r expr =
  let env = EvalEnv r cache
      action = unEvalT $ eval expr
  in runExceptT $ runReaderT action env

eval :: MonadHexl m => Expr -> EvalT m Value
eval (ExprBinOp Append l r) = (<>) <$> eval l <*> eval r
eval (ExprStringLit txt) = pure $ Value txt
eval (ExprColumnRef colName) = do
  ownRecId <- asks evalRecordId
  lift (resolveColumnRef colName) >>= \case
    Nothing -> throwError "Column not found on same table"
    Just colId -> do
      cellCache <- asks evalCellCache
      case retrieve colId ownRecId cellCache of
        Just val -> pure val
        Nothing -> do
          let cellQuery =
                  [ "aspects" =:
                  [ "columnId" =: toObjectId colId
                  , "recordId" =: toObjectId ownRecId
                  ]
                  ]
          mCell <- lift $ runMongo $ Mongo.findOne (Mongo.select cellQuery "cells")
          pure $ fromMaybe "" $ mCell >>= Mongo.lookup "value"

collectDependencies :: MonadHexl m => Expr -> m [(Id Column, DependencyType)]
collectDependencies (ExprBinOp _ l r) =
  (<>) <$> collectDependencies l
       <*> collectDependencies r
collectDependencies (ExprStringLit _) = pure []
collectDependencies (ExprColumnRef colName) = do
  mCol <- resolveColumnRef colName
  pure $ maybe [] (\col -> [(col, OneToOne)]) mCol


resolveColumnRef :: MonadHexl m => Ref Column -> m (Maybe (Id Column))
resolveColumnRef colName = do
  let colQuery =
        [ "name" =: colName
        ]
  mColumn <- runMongo $ Mongo.findOne (Mongo.select colQuery "columns")
  pure $ mColumn >>= Mongo.lookup "_id"
