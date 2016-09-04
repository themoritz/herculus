module Lib.Template.Typechecker
 ( runInferTpl
 , TTemplate
 , TTplExpr (..)
 ) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State

import           Data.Text                      (Text)

import           Lib.Model.Column

import           Lib.Compiler.Typechecker
import           Lib.Compiler.Typechecker.Types
import           Lib.Template.Parser

type TTemplate = [TTplExpr]

data TTplExpr
  = TTplText Text
  | TTplFor Name TExpr TTemplate
  | TTplIf TExpr TTemplate TTemplate
  | TTplShow TExpr
  deriving (Show)

runInferTpl :: Monad m => TypecheckEnv m -> Template -> m (Either TypeError TTemplate)
runInferTpl env tpl =
  runExceptT $ evalStateT (runReaderT (unInferT $ inferTpl tpl) env) newInferState

inferTpl :: Monad m => Template -> InferT m TTemplate
inferTpl = traverse inferTplExpr

inferTplExpr :: Monad m => TplExpr -> InferT m TTplExpr
inferTplExpr expr = case expr of
  TplText t -> pure $ TTplText t
  TplFor x e tpl -> do
    e' ::: te <- infer e
    tx <- fresh
    (tpl', Forall [] tx') <- inEnv (x, Forall [] tx) $ inferTpl tpl
    _ <- unify
      [ (te, TyUnary TyList tx')
      ]
    pure $ TTplFor x e' tpl'
  TplIf cond thenTpl elseTpl -> do
    cond' ::: tcond <- infer cond
    thenTpl' <- inferTpl thenTpl
    elseTpl' <- inferTpl elseTpl
    _ <- unify
      [ (tcond, TyNullary TyBool) ]
    pure $ TTplIf cond' thenTpl' elseTpl'
  TplShow e -> do
    e' ::: te <- infer e
    _ <- unify [ (te, TyNullary TyString) ]
    pure $ TTplShow e'
