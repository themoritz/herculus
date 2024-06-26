module Lib.Template.Eval
  ( evalTemplate
  ) where

import           Lib.Prelude

import qualified Data.HashMap.Strict     as HashMap

import           Lib.Compiler.Eval
import           Lib.Compiler.Eval.Monad
import           Lib.Compiler.Eval.Types
import           Lib.Template.Core

evalTemplate :: Monad m => TermEnv m -> [TplChunk] -> Eval m Text
evalTemplate env = map mconcat . traverse (evalTemplateChunk env)

evalTemplateChunk :: Monad m => TermEnv m -> TplChunk -> Eval m Text
evalTemplateChunk env = \case
  TplText t -> pure t
  TplFor iterator expr body -> do
    rs <- eval env expr
    let
      goList = \case
        RData "Cons" [a, as] -> (:) <$> pure a <*> goList as
        RData "Nil" [] -> pure []
        _ -> internalError "Inconsistent list data type."
    rs' <- goList rs
    texts <- for rs' $ \r -> case matchValue r iterator of
      Nothing   -> evalError "Pattern match failure."
      Just env' -> evalTemplate (env' `HashMap.union` env) body
    pure $ mconcat texts
  TplIf cond thenTpl elseTpl -> do
    r <- eval env cond
    case r of
      RData "True" []  -> evalTemplate env thenTpl
      RData "False" [] -> evalTemplate env elseTpl
      _ -> internalError "If condition did not evaluate to boolean data type."
  TplPrint expr -> do
    eval env expr >>= \case
      RString str -> pure str
      _ -> error "Pattern match failure in evalTemplateChunk/TplPrint."
