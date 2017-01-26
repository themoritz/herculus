module Lib.Template.Interpreter
  ( runEvalTemplate
  ) where

import qualified Data.Map                       as Map
import           Data.Text                      (Text)
import           Data.Traversable

import           Lib.Compiler.Interpreter
import           Lib.Compiler.Interpreter.Types

import           Lib.Model.Cell

import           Lib.Template.Types

runEvalTemplate :: Monad m => EvalEnv m -> CTemplate -> m (Either Text Text)
runEvalTemplate env tpl = runInterpretT 1000000 env (evalTpl prelude tpl)

evalTpl :: Monad m => TermEnv m -> CTemplate -> InterpretT m Text
evalTpl env (CTemplate tpl) = mconcat <$> traverse (evalTplExpr env) tpl

evalTplExpr :: Monad m => TermEnv m -> CTplExpr -> InterpretT m Text
evalTplExpr env tplExpr = case tplExpr of
  CTplText t -> pure t
  CTplFor iterator expr body -> do
    RValue (VList xs) <- eval env expr
    texts <- for xs $ \x -> evalTpl (Map.insert iterator (RValue x) env) body
    pure $ mconcat texts
  CTplIf cond thenTpl elseTpl -> do
    RValue (VBool bVal) <- eval env cond
    if bVal then evalTpl env thenTpl else evalTpl env elseTpl
  CTplShow expr -> do
    RValue (VString str) <- eval env expr
    pure str
