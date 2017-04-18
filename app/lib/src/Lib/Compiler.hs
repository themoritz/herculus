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
data Unit = Unit

data Void

data Boolean
  = True
  | False

conj :: Boolean -> Boolean -> Boolean
conj a b = if a then b else False

disj :: Boolean -> Boolean -> Boolean
disj a b = if a then True else b

not :: Boolean -> Boolean
not a = case a of
  True -> False
  False -> True

data List a
  = Nil
  | Cons a (List a)

sum :: List Number -> Number
sum xs = case xs of
  Nil -> 0.0
  Cons h tl -> h + sum tl

length :: forall a. List a -> Number
length xs = case xs of
  Nil -> 0.0
  Cons h tl -> 1.0 + length tl

map :: forall a b. (a -> b) -> List a -> List b
map f xs = case xs of
  Nil -> Nil
  Cons a as -> Cons (f a) (map f as)

filter :: forall a. (a -> Boolean) -> List a -> List a
filter p xs = case xs of
  Nil -> Nil
  Cons a as -> if p a then Cons a as else filter p as

find :: forall a. (a -> Boolean) -> List a -> Maybe a
find p xs = case xs of
  Nil -> Nothing
  Cons a as -> if p a then Just a else find p as

const :: forall a b. a -> b -> a
const a b = a

data Maybe a
  = Nothing
  | Just a

maybe :: forall a b. b -> (a -> b) -> Maybe a -> b
maybe def f ma = case ma of
  Nothing -> def
  Just a -> f a

fromMaybe :: forall a. a -> Maybe a -> a
fromMaybe def = maybe def id

data Either a b
  = Left a
  | Right b

id :: forall a. a -> a
id x = x

foldr :: forall a b. (a -> b -> b) -> a -> List b -> b
foldr f a xs = case xs of
  Nil -> a
  Cons x xs -> f x (foldr f a xs)

class Eq a where
  eq :: a -> a -> Boolean

instance Eq Boolean where
  eq a b = case a of
    True -> case b of
      True -> True
      False -> False
    False -> case b of
      True -> False
      False -> True
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

