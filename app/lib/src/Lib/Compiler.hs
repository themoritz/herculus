{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Lib.Compiler where

import           Lib.Prelude

import qualified Data.Map                  as Map
import           Data.Text                 (Text, pack, unpack)

import           NeatInterpolation
import           Text.Show.Pretty

import           Lib.Model.Cell
import           Lib.Model.Column
import           Lib.Types

import           Lib.Compiler.AST.Common
import           Lib.Compiler.AST.Position
import           Lib.Compiler.Checker
import           Lib.Compiler.Core
import           Lib.Compiler.Error
-- import           Lib.Compiler.Interpreter
import           Lib.Compiler.Env
import           Lib.Compiler.Eval
import           Lib.Compiler.Eval.Types
import           Lib.Compiler.Type
-- import           Lib.Compiler.Interpreter.Types
import           Lib.Compiler.Parser
import           Lib.Compiler.Pretty

-- import           Lib.Compiler.Typechecker
-- import           Lib.Compiler.Typechecker.Prim
-- import           Lib.Compiler.Typechecker.Types
-- import           Lib.Compiler.Types

-- compile :: Monad m => Text -> TypecheckEnv m
--         -> m (Either Text (CExpr, Type))
-- compile inp env = case parseExpr inp of
--   Left e   -> pure $ Left e
--   Right e' -> runInfer env e'

-- testDataCol :: DataCol
-- testDataCol = DataCol
--   DataNumber
--   NotDerived
--   ""
--   CompileResultNone

-- testTypecheckEnv :: Monad m => TypecheckEnv m
-- testTypecheckEnv = TypecheckEnv
--   { envResolveColumnRef        = \_ -> pure $ Just (nullObjectId, testDataCol)
--   , envResolveColumnOfTableRef = \_ _ -> pure $ Just (nullObjectId, nullObjectId, testDataCol)
--   , envResolveTableRef         = \_ -> pure $ Just nullObjectId
--   , envGetTableRowType         = \_ -> pure $ Type $ TyRecordCons (Ref "A") (Type tyNumber) (Type TyRecordNil)
--   , envOwnTableId              = nullObjectId
--   }

-- testEvalEnv :: Monad m => EvalEnv m
-- testEvalEnv = EvalEnv
--   { envGetCellValue    = \_ -> pure $ Just $ VNumber 1
--   , envGetColumnValues = \_ -> pure [Just $ VNumber 1]
--   , envGetTableRows    = \_ -> pure [nullObjectId]
--   , envGetRowField     = \_ _ -> pure $ Just $ VNumber 1
--   }

-- test :: String -> IO ()
-- test inp = compile (pack inp) testTypecheckEnv >>= \case
--   Left e -> putStrLn $ unpack e
--   Right (e, typ) -> do
--     putStrLn $ "Type: " ++ show typ
--     case interpret e testEvalEnv of
--       Left e'   -> putStrLn $ unpack e'
--       Right val -> putStrLn $ "Val: " ++ show val

--------------------------------------------------------------------------------

prelude :: Text
prelude = [text|
data Boolean
  = True
  | False

data List a
  = Nil
  | Cons a (List a)

foldr :: forall a b. (a -> b -> b) -> b -> List a -> b
foldr f a xs = case xs of
  Nil -> a
  Cons x xs -> f x (foldr f a xs)

class Eq a where
  eq :: a -> a -> Boolean

elem :: forall a b. Eq a => a -> List a -> b
elem x xs = case xs of
  Nil -> False
  Cons a as -> if eq x a
    then True
    else elem x as
|]

withParsed :: Text -> Parser a -> (a -> IO ()) -> IO ()
withParsed src p m = case parse src p of
  Left err    -> putStrLn $ displayError src err
  Right decls -> m decls

testParsePretty :: Text -> IO ()
testParsePretty src = withParsed src parseModule $ \decls ->
  mapM_ (putStrLn . (<> "\n") . prettyAst) (map stripAnn decls)

testParseSpans :: Text -> IO ()
testParseSpans src = withParsed src parseFormula $ \decls ->
  mapM_ (putStrLn . ppShow . map (flip (highlightSpan True) src)) decls

testCheck :: Text -> IO ()
testCheck src = withParsed src parseModule $ \decls ->
  case runCheck primCheckEnv (checkModule decls) of
    Left err -> putStrLn $ displayError src err
    Right _  -> pure ()

testEval :: Text -> IO ()
testEval src =
  withParsed prelude parseModule $ \decls ->
  withParsed src parseFormula $ \formula ->
  let
    go :: Check (Expr, PolyType, TermEnv)
    go = do
      (kinds, polys, env) <- checkModule decls
      inExtendedKindEnv kinds $ inExtendedTypeEnv polys $ do
        (code, poly) <- checkFormula formula
        pure (code, poly, primTermEnv `Map.union` loadModule env)
  in
  case runCheck primCheckEnv go of
    Left err -> putStrLn $ displayError src err
    Right (code, poly, env) -> do
      putStrLn $ "Type: " <> prettyPolyType poly
      case runExcept (eval env code) of
        Left err -> putStrLn err
        Right r  -> putStrLn $ prettyResult r
