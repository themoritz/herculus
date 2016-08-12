{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Compiler.Types where

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader

import           Data.Text              (Text, pack, unpack)

import           Data.Map               (Map)

import           Lib.Model
import           Lib.Model.Cell
import           Lib.Model.Column
import           Lib.Model.Types
import           Lib.Types

import           Lib.Model.Cell
import           Lib.Model.Column
import           Lib.Types

type TermEnv m = Map String (Result m)

data Result m
  = RValue Value
  | RClosure String (Expr Id) (TermEnv m)
  | RPrelude (TermEnv m -> Result m -> InterpretT m (Result m))

type EvalError = Text

data EvalEnv m = EvalEnv
  { getCellValue    :: Id Column -> m (Maybe Value)
  , getColumnValues :: Id Column -> m [(Maybe Value)]
  , getTableRecords :: Id Table -> m [Id Record]
  , getRecordValue  :: Id Record -> Ref Column -> m (Maybe Value)
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