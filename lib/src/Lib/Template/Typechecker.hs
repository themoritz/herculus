module Lib.Template.Typechecker
 ( runInferTpl
 ) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State

import           Lib.Compiler.Types
import           Lib.Compiler.Typechecker
import           Lib.Compiler.Typechecker.Prim
import           Lib.Compiler.Typechecker.Types
import           Lib.Template.Types

runInferTpl :: Monad m => TypecheckEnv m -> PTemplate -> m (Either TypeError CTemplate)
runInferTpl env tpl =
  runExceptT $ evalStateT (runReaderT (unInferT $ inferTpl tpl) env) newInferState

inferTpl :: Monad m => PTemplate -> InferT m CTemplate
inferTpl (PTemplate tpl) = CTemplate <$> traverse inferTplExpr tpl

inferTplExpr :: Monad m => PTplExpr -> InferT m CTplExpr
inferTplExpr expr = case expr of
  PTplText t -> pure $ CTplText t
  PTplFor x e tpl -> do
    (e', ePoint) <- inferAndDesugar e
    xPoint <- freshPoint
    tpl' <- inLocalContext (x, ForAll [] [] xPoint) $ inferTpl tpl
    listPoint <- mkList xPoint
    unify ePoint listPoint
    pure $ CTplFor x e' tpl'
  PTplIf cond thenTpl elseTpl -> do
    (cond', condPoint) <- inferAndDesugar cond
    thenTpl' <- inferTpl thenTpl
    elseTpl' <- inferTpl elseTpl
    boolPoint <- mkPoint tyBool
    unify condPoint boolPoint
    pure $ CTplIf cond' thenTpl' elseTpl'
  PTplShow e -> do
    (e', ePoint) <- inferAndDesugar e
    strPoint <- mkPoint tyString
    unify ePoint strPoint
    pure $ CTplShow e'
