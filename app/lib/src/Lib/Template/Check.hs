{-# LANGUAGE NoImplicitPrelude #-}

module Lib.Template.Check
 ( checkTemplate
 , compileTemplate
 ) where

import           Lib.Prelude

import           Control.Comonad.Cofree

import           Data.Functor.Foldable
import qualified Data.Map                 as Map

import           Lib.Compiler.AST
import           Lib.Compiler.AST.Common
import           Lib.Compiler.Check
import           Lib.Compiler.Check.Monad
import           Lib.Compiler.Env
import           Lib.Compiler.Type
import           Lib.Template.AST
import           Lib.Template.Core        (TplChunk, toCore)

compileTemplate :: [TplIntermed] -> Check [TplChunk]
compileTemplate = map toCore . traverse go
  where
    go :: TplIntermed -> Check TplCompiled
    go (Fix i) = case i of
      TplText t ->
        pure $ tplText t
      TplFor b e body ->
        tplFor <$> cleanUpIntermed b <*> cleanUpIntermed e <*> traverse go body
      TplIf c th el ->
        tplIf <$> cleanUpIntermed c <*> traverse go th <*> traverse go el
      TplPrint e ->
        tplPrint <$> cleanUpIntermed e

checkTemplate :: [SourceTplChunk] -> Check [TplIntermed]
checkTemplate = traverse checkTemplateChunk

checkTemplateChunk :: SourceTplChunk -> Check TplIntermed
checkTemplateChunk (span :< chunk) = case chunk of
  TplText t -> pure $ tplText t
  TplFor (hoistCofree unsafePrj -> binder) e body -> do
    (e', eType, cs) <- inferExpr e
    -- TODO: Check `cs` is empty
    argType <- freshType
    binderDict <- inferBinder argType binder
    body' <- inExtendedTypeEnv binderDict $ checkTemplate body
    unifyTypes' span (typeApp tyList argType) eType
    pure $ tplFor (injFix $ stripAnn binder) e' body'
  TplIf cond th el -> do
    (cond', condType, cs) <- inferExpr cond
    unifyTypes' span tyBoolean condType
    th' <- checkTemplate th
    el' <- checkTemplate el
    pure $ tplIf cond' th' el'
  TplPrint e -> do
    (e', eType, cs) <- inferExpr e
    let printFn = methodPlaceholder span (IsIn "Print" $ injFix eType) "print"
    e'' <- resolvePlaceholders Map.empty $ app printFn e'
    unifyTypes' span eType tyString
    pure $ tplPrint e''
