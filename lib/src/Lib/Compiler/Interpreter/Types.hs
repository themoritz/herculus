{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Compiler.Interpreter.Types where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State

import           Data.Text            (Text)

import           Data.Map             (Map)

import           Lib.Compiler.Types
import           Lib.Model.Cell
import           Lib.Model.Column
import           Lib.Model.Row
import           Lib.Model.Table
import           Lib.Types

type TermEnv m = Map Name (Result m)

data Result m
  = RValue Value
  | RClosure Name CExpr (TermEnv m)
  | RInstanceDict (Map Name (Result m))
  | RPrelude (TermEnv m -> Result m -> InterpretT m (Result m))

type EvalError = Text

data EvalEnv m = EvalEnv
  { envGetCellValue    :: Id Column -> m (Maybe Value)
  , envGetColumnValues :: Id Column -> m [Maybe Value]
  , envGetTableRows    :: Id Table -> m [Id Row]
  , envGetRowField     :: Id Row -> Ref Column -> m (Maybe Value)
  }

newtype InterpretT m a = InterpretT
  { unInterpretT :: StateT Int (ReaderT (EvalEnv m) (ExceptT EvalError m)) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadError EvalError
             , MonadReader (EvalEnv m)
             , MonadState Int
             )

instance MonadTrans InterpretT where
  lift = InterpretT . lift . lift . lift

runInterpretT :: Monad m => Int -> EvalEnv m
              -> InterpretT m a -> m (Either EvalError a)
runInterpretT gas env action =
  runExceptT $ runReaderT (evalStateT (unInterpretT action) gas) env
