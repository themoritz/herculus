{-# LANGUAGE NoImplicitPrelude #-}

module Lib.Template.Check
 ( checkTemplate
 ) where

import           Lib.Prelude

import           Control.Comonad.Cofree

import           Data.Functor.Foldable
import           Data.List                (unzip)
import qualified Data.Map                 as Map

import           Lib.Compiler.AST
import           Lib.Compiler.AST.Common
import           Lib.Compiler.Check
import           Lib.Compiler.Check.Monad
import           Lib.Compiler.Env
import           Lib.Compiler.Type
import           Lib.Template.AST
import           Lib.Template.Core        (TplChunk, toCore)

checkTemplate :: [SourceTplChunk] -> Check [TplChunk]
checkTemplate chunks = do
  (i, cs) <- checkTemplate' chunks
  cleanToplevelConstraints cs Nothing
  i' <- tplResolvePlaceholders i
  map toCore $ traverse go i'
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

tplResolvePlaceholders :: [TplIntermed] -> Check [TplIntermed]
tplResolvePlaceholders = traverse chunkResolvePlaceholders

chunkResolvePlaceholders :: TplIntermed -> Check TplIntermed
chunkResolvePlaceholders (Fix i) = case i of
  TplText t -> pure $ tplText t
  TplFor b e body -> tplFor b
    <$> resolvePlaceholders Map.empty e
    <*> tplResolvePlaceholders body
  TplIf c th el -> tplIf
    <$> resolvePlaceholders Map.empty c
    <*> tplResolvePlaceholders th
    <*> tplResolvePlaceholders el
  TplPrint e -> tplPrint <$> resolvePlaceholders Map.empty e

checkTemplate' :: [SourceTplChunk] -> Check ([TplIntermed], [ConstraintToSolve])
checkTemplate' chunks =
  (id *** join) . unzip <$> traverse checkTemplateChunk chunks

checkTemplateChunk :: SourceTplChunk -> Check (TplIntermed, [ConstraintToSolve])
checkTemplateChunk (span :< chunk) = case chunk of
  TplText t -> pure (tplText t, [])
  TplFor (hoistCofree unsafePrj -> binder) e body -> do
    (e', eType, cs) <- inferExpr e
    -- TODO: Check `cs` is empty
    argType <- freshType
    binderDict <- checkBinder argType binder
    (body', bodyCs) <- inExtendedTypeEnv binderDict $ checkTemplate' body
    unifyTypes' span (typeApp tyList argType) eType
    pure (tplFor (injFix $ stripAnn binder) e' body', cs <> bodyCs)
  TplIf cond th el -> do
    (cond', condType, cs) <- inferExpr cond
    unifyTypes' span tyBoolean condType
    (th', thCs) <- checkTemplate' th
    (el', elCs) <- checkTemplate' el
    pure (tplIf cond' th' el', cs <> thCs <> elCs)
  TplPrint e -> do
    (e', eType, cs) <- inferExpr e
    let printFn = methodPlaceholder span "Print" (injFix eType) "print"
    pure (tplPrint (app printFn e'), cs)
