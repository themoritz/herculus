{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |

module Lib.Compiler.Eval.Monad where

import           Lib.Prelude

import           Lib.Model.Cell
import           Lib.Model.Column
import           Lib.Model.Row
import           Lib.Model.Table
import           Lib.Types

data Getter m = Getter
  { getCellValue    :: Id Column -> m (Maybe Value)
  , getColumnValues :: Id Column -> m [Maybe Value]
  , getTableRows    :: Id Table -> m [Id Row]
  , getRowRecord    :: Id Row -> m (Maybe (Map Text Value))
  }

newtype Eval m a = Eval
  { unEval :: Getter m -> Int -> m (Either Text (Int, a))
  } deriving (Functor)

instance Monad m => Applicative (Eval m) where
  pure a = Eval $ \_ gas -> pure (Right (gas, a))
  mf <*> ma = Eval $ \env gas ->
    unEval mf env gas >>= \case
      Left e -> pure (Left e)
      Right (gas', f) ->
        unEval ma env gas' >>= \case
          Left e -> pure (Left e)
          Right (gas'', a) -> pure (Right (gas'', f a))

instance Monad m => Monad (Eval m) where
  return = pure
  ma >>= f = Eval $ \env gas ->
    unEval ma env gas >>= \case
      Left e -> pure (Left e)
      Right (gas', a) ->
        unEval (f a) env gas' >>= \case
          Left e -> pure (Left e)
          Right (gas'', b) -> pure (Right (gas'', b))

runEval :: Monad m => Int -> Getter m -> Eval m a -> m (Either Text a)
runEval gas env action = map snd <$> unEval action env gas

--------------------------------------------------------------------------------

withGetter :: Monad m => (Getter m -> m a) -> Eval m a
withGetter f = Eval $ \env gas -> do
  a <- f env
  pure (Right (gas, a))

consumeGas :: Monad m => Eval m ()
consumeGas = Eval $ \_ gas -> pure $ if gas == 0
  then Left "Computation exceeeded the maximum allowed number of operations."
  else Right (gas - 1, ())

evalError :: Monad m => Text -> Eval m a
evalError e = Eval $ \_ _ -> pure (Left e)

internalError :: Monad m => Text -> Eval m a
internalError msg = evalError $ "Internal: " <> msg
