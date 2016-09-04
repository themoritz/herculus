module Lib.Template.Typechecker
 ( runInferTpl
 , TTemplate
 , TTplExpr (..)
 ) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State

import           Lib.Compiler.Typechecker
import           Lib.Compiler.Typechecker.Types
import           Lib.Template.Types

runInferTpl :: Monad m => TypecheckEnv m -> PTemplate -> m (Either TypeError TTemplate)
runInferTpl env tpl =
  runExceptT $ evalStateT (runReaderT (unInferT $ inferTpl tpl) env) newInferState

inferTpl :: Monad m => PTemplate -> InferT m TTemplate
inferTpl (PTemplate tpl) = TTemplate <$> traverse inferTplExpr tpl

inferTplExpr :: Monad m => PTplExpr -> InferT m TTplExpr
inferTplExpr expr = case expr of
  PTplText t -> pure $ TTplText t
  PTplFor x e tpl -> do
    e' ::: te <- infer e
    tx <- fresh
    (tpl', Forall [] tx') <- inEnv (x, Forall [] tx) $ inferTpl tpl
    _ <- unify
      [ (te, TyUnary TyList tx')
      ]
    pure $ TTplFor x e' tpl'
  PTplIf cond thenTpl elseTpl -> do
    cond' ::: tcond <- infer cond
    thenTpl' <- inferTpl thenTpl
    elseTpl' <- inferTpl elseTpl
    _ <- unify
      [ (tcond, TyNullary TyBool) ]
    pure $ TTplIf cond' thenTpl' elseTpl'
  PTplShow e -> do
    e' ::: te <- infer e
    _ <- unify [ (te, TyNullary TyString) ]
    pure $ TTplShow e'
