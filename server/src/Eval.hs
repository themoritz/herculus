{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE FlexibleContexts           #-}

module Eval
  ( EvalEnv (..)
  , eval
  ) where

import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.Monoid
import           Data.Text (Text)

import           Lib.Types
import           Lib.Model.Column
import           Lib.Model.Cell

import           Propagate.Monad

data EvalEnv m = EvalEnv
  { envGetCellValue :: forall a. ExtractValue a => Id Column -> m (Maybe a)
  , envGetColumnValues :: forall a. ExtractValue a => Id Column -> m [(Maybe a)]
  }

newtype EvalT m a = EvalT
  { unEvalT :: ReaderT (EvalEnv m) (ExceptT Text m) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadReader (EvalEnv m)
             , MonadError Text
             )

instance MonadTrans EvalT where
  lift = EvalT . lift . lift

eval :: (MonadPropagate m, MakeValue a) => (EvalEnv m) -> TExpr a -> m CellContent
eval env expr = do
  let action = unEvalT $ eval' expr
  runExceptT (runReaderT action env) >>= \case
    Left msg -> pure $ CellEvalError msg
    Right a -> case makeValue a of
      Nothing -> pure $ CellEvalError "cannot make value"
      Just val -> pure $ CellValue val

eval' :: MonadPropagate m => TExpr a -> EvalT m a
eval' expr = case expr of
    TExprLitString txt        -> pure txt
    TExprLitNumber num        -> pure num
    TExprColumnRefString col  -> getCellValue' col
    TExprColumnRefNumber col  -> getCellValue' col
    TExprColumnRefStrings col -> getColumnValues' col
    TExprColumnRefNumbers col -> getColumnValues' col
    TExprStringAppend l r     -> (<>) <$> eval' l <*> eval' r
    TExprNumberAdd l r        -> (+) <$> eval' l <*> eval' r
    TExprSum sub              -> sum <$> eval' sub
  where
    getCellValue' c = do
      f <- asks envGetCellValue
      lift (f c) >>= \case
        Nothing -> throwError "dependent cell not ready"
        Just v -> pure v
    getColumnValues' c = do
      f <- asks envGetColumnValues
      mVals <- lift $ f c
      case sequenceA mVals of
        Nothing -> throwError "dependent cell not ready"
        Just vs -> pure vs
