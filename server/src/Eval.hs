{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Eval where

import Lib
import Lib.Expression

import Monads

import Control.Monad.Reader

data EvalEnv = EvalEnv
  { tableId :: Id Table
  , columnId :: Id Column
  , recordId :: Id Record
  }

newtype EvalT m a = EvalT
  { unEvalT :: ReaderT EvalEnv m a
  } deriving (Functor, Applicative, Monad, MonadReader EvalEnv)

eval :: MonadHexl m => Expression -> EvalT m a
eval = undefined
