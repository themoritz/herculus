{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

module Lib.Compiler where

import           Lib.Prelude

import           Data.FileEmbed               (embedStringFile,
                                               makeRelativeToProject)
import qualified Data.HashMap.Strict          as HashMap
import qualified Data.Map                     as Map

import           Text.PrettyPrint.Leijen.Text (Doc)
import           Text.Show.Pretty             (ppShow)

import           Lib.Model.Cell
import           Lib.Model.Column
import           Lib.Types

import           Lib.Compiler.AST.Common
import           Lib.Compiler.AST.Position
import           Lib.Compiler.Check
import           Lib.Compiler.Check.Monad
import           Lib.Compiler.Core
import           Lib.Compiler.Docs
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
  ExceptT $ runCheck env resolver $ inferFormula e

compileFormulaWithType
  :: Monad m => Text -> Type -> Resolver m -> CheckEnv
  -> m (Either Error Expr)
compileFormulaWithType src t resolver env = runExceptT $ do
  e <- hoistError $ parse src (mkCheckEnvOpTable env) parseFormula
  ExceptT $ runCheck env resolver $ checkFormula t e

compileModule
  :: Monad m => Text -> Resolver m -> CheckEnv
  -> m (Either Error CheckResult)
compileModule src resolver env = runExceptT $ do
  e <- hoistError $ parse src (mkCheckEnvOpTable env) parseModule
  ExceptT $ runCheck env resolver $ checkModule e

evalFormula
  :: Monad m => Expr -> Getter m -> TermEnv m
  -> m (Either Text Value)
evalFormula expr getter env = runEval 1000000 getter $ do
  r <- eval env expr
  storeValue r

documentModule
  :: Monad m => Text -> Resolver m -> CheckEnv
  -> m (Either Error Doc)
documentModule src resolver env = runExceptT $ do
  e <- hoistError $ parse src (mkCheckEnvOpTable env) parseModule
  _ <- ExceptT $ runCheck env resolver $ checkModule e
  pure $ moduleDoc e

--------------------------------------------------------------------------------

preludeCheckEnv :: CheckEnv
preludeCheckEnv = let (x, _, _) = prelude @Identity in x

preludeTermEnv :: Monad m => TermEnv m
preludeTermEnv = let (_, x, _) = prelude in x

preludeTycons :: Map Text TyconInfo
preludeTycons = let (_, _, x) = prelude @Identity in x

prelude :: Monad m => (CheckEnv, TermEnv m, Map Text TyconInfo)
prelude =
  case runIdentity $ compileModule preludeText voidResolver primCheckEnv of
    Left err -> error $ displayError preludeText err
    Right CheckResult {..} ->
      ( primCheckEnv `unionCheckEnv` resultCheckEnv
      , primTermEnv `HashMap.union` loadModule resultTermEnv
      , primTycons `Map.union` resultTycons
      )

preludeText :: Text
preludeText =
  $(makeRelativeToProject "src/Lib/Compiler/Prelude.hexl" >>= embedStringFile)

--------------------------------------------------------------------------------

testDataCol :: DataCol
testDataCol = DataCol
  (DataAlgebraic "Number" [])
  NotDerived
  ""
  CompileResultNone

testResolveInterp :: Monad m => Resolver m
testResolveInterp = \case
  GetTableRecordType _ reply ->
    pure $ reply $ Map.singleton "A" tyNumber
  GetTableName _ reply ->
    pure $ reply "Table"
  ResolveColumnOfTableRef _ _ reply ->
    pure $ reply $ Just (nullObjectId, nullObjectId, testDataCol)
  ResolveColumnRef _ reply ->
    pure $ reply $ Just (nullObjectId, testDataCol)
  ResolveTableRef _ reply ->
    pure $ reply $ Just nullObjectId

testGetInterp :: Monad m => Getter m
testGetInterp = Getter
  { getCellValue = \_ ->
      pure $ Just $ VNumber (Number 4.0)
  , getColumnValues = \_ ->
      pure $ [Just $ VNumber (Number 4.0)]
  , getTableRows = \_ ->
      pure $ [nullObjectId]
  , getRowRecord = \_ ->
      pure $ Just $ Map.singleton "A" $ VNumber (Number 4.0)
  }

voidResolver :: Monad m => Resolver m
voidResolver = \case
  GetTableRecordType _ reply ->
    pure $ reply Map.empty
  GetTableName _ reply ->
    pure $ reply "SomeTable"
  ResolveColumnOfTableRef _ _ reply ->
    pure $ reply Nothing
  ResolveColumnRef _ reply ->
    pure $ reply Nothing
  ResolveTableRef _ reply ->
    pure $ reply Nothing

voidGetter :: Monad m => Getter m
voidGetter = Getter
  { getCellValue = \_ ->
      pure Nothing
  , getColumnValues = \_ ->
      pure []
  , getTableRows = \_ ->
      pure []
  , getRowRecord = \_ ->
      pure $ Just Map.empty
  }

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
  Right CheckResult {..} ->
    void $ flip HashMap.traverseWithKey resultTermEnv $ \n core ->
      putStrLn $ show n <> ": " <> prettyCore core <> "\n"

testEval :: Text -> IO ()
testEval src = do
  res <- runExceptT $ do
    (expr, t) <- ExceptT $ compileFormula src testResolveInterp preludeCheckEnv
    val <- ExceptT $ (map (mapLeft (\msg -> Error msg voidSpan))) $
      evalFormula expr testGetInterp preludeTermEnv
    pure (t, val)
  case res of
    Left err -> putStrLn $ displayError src err
    Right (t, val) -> do
      putStrLn $ "Type: " <> prettyType t
      print val

testDocs :: Text -> IO ()
testDocs src = documentModule src voidResolver primCheckEnv >>= \case
  Left err -> putStrLn $ displayError src err
  Right doc -> print doc
