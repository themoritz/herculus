{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Compiler.Interpreter.Types where

import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.Text            (Text)

import           Data.Map             (Map)

import           Lib.Compiler.Types
import           Lib.Model.Cell
import           Lib.Model.Column
import           Lib.Model.Record
import           Lib.Model.Table
import           Lib.Types

type TermEnv m = Map Name (Result m)

data Result m
  = RValue Value
  | RClosure Name CExpr (TermEnv m)
  | RPrelude (TermEnv m -> Result m -> InterpretT m (Result m))

type EvalError = Text

data EvalEnv m = EvalEnv
  { envGetCellValue    :: Id Column -> m (Maybe Value)
  , envGetColumnValues :: Id Column -> m [Maybe Value]
  , envGetTableRecords :: Id Table -> m [Id Record]
  , envGetRecordValue  :: Id Record -> Ref Column -> m (Maybe Value)
  }

newtype InterpretT m a = InterpretT
  { unInterpretT :: ReaderT (EvalEnv m) (ExceptT EvalError m) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadError EvalError
             , MonadReader (EvalEnv m)
             )

instance MonadTrans InterpretT where
  lift = InterpretT . lift . lift

runInterpretT :: EvalEnv m -> InterpretT m a -> m (Either EvalError a)
runInterpretT env action = runExceptT $ runReaderT (unInterpretT action) env
