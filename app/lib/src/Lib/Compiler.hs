{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Lib.Compiler where

import           Lib.Prelude

import           Data.FileEmbed            (embedStringFile,
                                            makeRelativeToProject)
import qualified Data.Map                  as Map

import           Text.Show.Pretty          (ppShow)

import           Lib.Model.Cell
import           Lib.Model.Column
import           Lib.Types

import           Lib.Compiler.AST.Common
import           Lib.Compiler.AST.Position
import           Lib.Compiler.Check
import           Lib.Compiler.Check.Monad
import           Lib.Compiler.Core
import           Lib.Compiler.Env
import           Lib.Compiler.Error
import           Lib.Compiler.Eval
import           Lib.Compiler.Eval.Monad
import           Lib.Compiler.Eval.Types
import           Lib.Compiler.Parse
import           Lib.Compiler.Parse.State
import           Lib.Compiler.Pretty
import           Lib.Compiler.Type

compileFormula
  :: Monad m => Text -> Resolver m -> CheckEnv
  -> m (Either Error (Expr, Type))
compileFormula src resolver env = runExceptT $ do
  e <- hoistError $ parse src (mkCheckEnvOpTable env) parseFormula
  ExceptT $ runCheck env resolver $ checkFormula e

compileModule
  :: Monad m => Text -> Resolver m -> CheckEnv
  -> m (Either Error (CheckEnv, Map Text Expr))
compileModule src resolver env = runExceptT $ do
  e <- hoistError $ parse src (mkCheckEnvOpTable env) parseModule
  ExceptT $ runCheck env resolver $ checkModule e

evalFormula
  :: Monad m => Expr -> Getter m -> TermEnv
  -> m (Either Text Value)
evalFormula expr getter env = runEval 5000 getter $ do
  r <- eval env expr
  storeValue r

--------------------------------------------------------------------------------

preludeCheckEnv :: CheckEnv
preludeCheckEnv = fst prelude

preludeTermEnv :: TermEnv
preludeTermEnv = snd prelude

prelude :: (CheckEnv, TermEnv)
prelude =
  case runIdentity $ compileModule preludeText voidResolver primCheckEnv of
    Left err -> error $ displayError preludeText err
    Right (env, code) ->
      ( primCheckEnv `unionCheckEnv` env
      , primTermEnv `Map.union` loadModule code )

preludeText :: Text
preludeText =
  $(makeRelativeToProject "src/Lib/Compiler/Prelude.hexl" >>= embedStringFile)

--------------------------------------------------------------------------------

testDataCol :: DataCol
testDataCol = DataCol
  DataBool
  NotDerived
  ""
  CompileResultNone

testResolveInterp :: Monad m => Resolver m
testResolveInterp = \case
  GetTableRecordType _ reply ->
    pure $ reply $ typeApp tyRecord $ recordCons "A" tyNumber recordNil
  ResolveColumnOfTableRef _ _ reply ->
    pure $ reply $ Just (nullObjectId, nullObjectId, testDataCol)
  ResolveColumnRef _ reply ->
    pure $ reply $ Just (nullObjectId, testDataCol)
  ResolveTableRef _ reply ->
    pure $ reply $ Just nullObjectId

testGetInterp :: Monad m => Getter m
testGetInterp = \case
  GetCellValue _ reply ->
    pure $ reply $ Just $ VBool True
  GetColumnValues _ reply ->
    pure $ reply [Just $ VBool False]
  GetTableRows _  reply ->
    pure $ reply [nullObjectId]
  GetRowField _ _ reply ->
    pure $ reply $ Just $ VBool True

voidResolver :: Monad m => Resolver m
voidResolver = \case
  GetTableRecordType _ reply ->
    pure $ reply $ typeApp tyRecord recordNil
  ResolveColumnOfTableRef _ _ reply ->
    pure $ reply Nothing
  ResolveColumnRef _ reply ->
    pure $ reply Nothing
  ResolveTableRef _ reply ->
    pure $ reply Nothing

voidGetter :: Monad m => Getter m
voidGetter = \case
  GetCellValue _ reply ->
    pure $ reply Nothing
  GetColumnValues _ reply ->
    pure $ reply []
  GetTableRows _  reply ->
    pure $ reply []
  GetRowField _ _ reply ->
    pure $ reply Nothing

--------------------------------------------------------------------------------

withParsed :: Text -> OpTable -> Parser a -> (a -> IO ()) -> IO ()
withParsed src opTable p m = case parse src opTable p of
  Left err    -> putStrLn $ displayError src err
  Right decls -> m decls

testParsePretty :: Text -> IO ()
testParsePretty src = withParsed src testOpTable parseModule $ \decls ->
  mapM_ (putStrLn . (<> "\n") . prettyAst) (map stripAnn decls)

testParseSpans :: Text -> IO ()
testParseSpans src = withParsed src testOpTable parseModule $ \decls ->
  mapM_ (putStrLn . ppShow . map (flip (highlightSpan True) src)) decls

testCheck :: Text -> IO ()
testCheck src = compileModule src testResolveInterp primCheckEnv >>= \case
  Left err -> putStrLn $ displayError src err
  Right (_, code) -> do
    void $ flip Map.traverseWithKey code $ \n core -> do
      putStrLn $ n <> ": " <> prettyCore core <> "\n"

testEval :: Text -> IO ()
testEval src = do
  res <- runExceptT $ do
    (expr, t) <- ExceptT $ (map (mapLeft errMsg)) $
      compileFormula src testResolveInterp preludeCheckEnv
    val <- ExceptT $ evalFormula expr testGetInterp preludeTermEnv
    pure (t, val)
  case res of
    Left err -> putStrLn err
    Right (t, val) -> do
      putStrLn $ "Type: " <> prettyType t
      print val
