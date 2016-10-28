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
  let action = do
        loadPrelude
        ttpl <- inferTpl tpl
        ttpl' <- tplReplaceTypeClassDicts ttpl
        toCoreTpl ttpl'
  in runExceptT $ evalStateT (runReaderT (unInferT action) env) newInferState

inferTpl :: Monad m => PTemplate -> InferT m TTemplate
inferTpl (PTemplate tpl) = TTemplate <$> traverse inferTplExpr tpl

inferTplExpr :: Monad m => PTplExpr -> InferT m TTplExpr
inferTplExpr expr = case expr of
  PTplText t -> pure $ TTplText t
  PTplFor x e tpl -> do
    e' ::: (_, ePoint) <- infer e
    xPoint <- freshPoint
    tpl' <- inLocalContext (x, ForAll [] [] xPoint) $ inferTpl tpl
    listPoint <- mkList xPoint
    unify ePoint listPoint
    pure $ TTplFor x e' tpl'
  PTplIf cond thenTpl elseTpl -> do
    cond' ::: (_, condPoint) <- infer cond
    thenTpl' <- inferTpl thenTpl
    elseTpl' <- inferTpl elseTpl
    boolPoint <- mkPoint tyBool
    unify condPoint boolPoint
    pure $ TTplIf cond' thenTpl' elseTpl'
  PTplShow e -> do
    e' ::: (_, ePoint) <- infer e
    strPoint <- mkPoint tyString
    unify ePoint strPoint
    pure $ TTplShow e'

tplReplaceTypeClassDicts :: Monad m => TTemplate -> InferT m TTemplate
tplReplaceTypeClassDicts (TTemplate tpl) = TTemplate <$> mapM replace tpl
  where
    replace = \case
      TTplText t -> pure $ TTplText t
      TTplFor x e t -> TTplFor x <$> replaceTypeClassDicts e <*> tplReplaceTypeClassDicts t
      TTplIf c t e -> TTplIf <$> replaceTypeClassDicts c <*> tplReplaceTypeClassDicts t <*> tplReplaceTypeClassDicts e
      TTplShow e -> TTplShow <$> replaceTypeClassDicts e
