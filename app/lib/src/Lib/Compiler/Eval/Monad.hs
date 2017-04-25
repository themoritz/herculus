{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeOperators             #-}
-- |

module Lib.Compiler.Eval.Monad where

import           Lib.Prelude

import           Control.Monad.Free

import           Lib.Model.Cell
import           Lib.Model.Column
import           Lib.Model.Row
import           Lib.Model.Table
import           Lib.Types

type Eval' = EvalF :+: GetF

type Eval = Free Eval'

liftEval :: (f :<: Eval') => f a -> Eval a
liftEval = liftF . inj

--------------------------------------------------------------------------------

data EvalF a
  = ConsumeGas a
  | forall b. EvalError Text (b -> a)

deriving instance Functor EvalF

consumeGas :: Eval ()
consumeGas = liftEval $ ConsumeGas ()

evalError :: Text -> Eval b
evalError e = liftEval $ EvalError e id

internalError :: Text -> Eval a
internalError msg = evalError $ "Internal: " <> msg

--------------------------------------------------------------------------------

type Getter m = forall x. GetF x -> m x

data GetF a
  = GetCellValue (Id Column) (Maybe Value -> a)
  | GetColumnValues (Id Column) ([Maybe Value] -> a)
  | GetTableRows (Id Table) ([Id Row] -> a)
  | GetRowField (Id Row) (Ref Column) (Maybe Value -> a)
  deriving (Functor)

getCellValue :: Id Column -> Eval (Maybe Value)
getCellValue c = liftEval $ GetCellValue c id

getColumnValues :: Id Column -> Eval [Maybe Value]
getColumnValues c = liftEval $ GetColumnValues c id

getTableRows :: Id Table -> Eval [Id Row]
getTableRows t = liftEval $ GetTableRows t id

getRowField :: Id Row -> Ref Column -> Eval (Maybe Value)
getRowField r c = liftEval $ GetRowField r c id

--------------------------------------------------------------------------------

type EvalInterpT m = StateT Integer (ExceptT Text m)

runEvalInterpT :: Monad m => Integer -> EvalInterpT m a -> m (Either Text a)
runEvalInterpT gas m = runExceptT $ evalStateT m gas

runEval
  :: forall m a. Monad m
  => Integer -> Getter m
  -> Eval a -> m (Either Text a)
runEval gas goGet =
  runEvalInterpT gas .
  foldFree go
  where
  go :: Eval' b -> EvalInterpT m b
  go = coproduct goEval (lift . lift . goGet)
  goEval :: EvalF b -> EvalInterpT m b
  goEval = \case
    ConsumeGas next -> do
      g <- get
      when (g == 0) $ throwError
        "Computation exceeded the maximum allowed number of operations."
      put (g - 1)
      pure next
    EvalError e _ -> throwError e
