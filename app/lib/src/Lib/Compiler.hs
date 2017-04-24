{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Lib.Compiler where

import           Lib.Prelude

import qualified Data.Map                  as Map

import           NeatInterpolation
import           Text.Show.Pretty

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
import           Lib.Compiler.Pretty
import           Lib.Compiler.Type

-- compile :: Monad m => Text -> TypecheckEnv m
--         -> m (Either Text (CExpr, Type))
-- compile inp env = case parseExpr inp of
--   Left e   -> pure $ Left e
--   Right e' -> runInfer env e'

testDataCol :: DataCol
testDataCol = DataCol
  DataNumber
  NotDerived
  ""
  CompileResultNone

testResolveInterp :: Monad m => ResolveF a -> m a
testResolveInterp = \case
  GetTableRecordType _ reply ->
    pure $ reply $ typeApp tyRecord $ recordCons "A" tyNumber recordNil
  ResolveColumnOfTableRef _ _ reply ->
    pure $ reply $ Just (nullObjectId, nullObjectId, testDataCol)
  ResolveColumnRef _ reply ->
    pure $ reply $ Just (nullObjectId, testDataCol)
  ResolveTableRef _ reply ->
    pure $ reply $ Just nullObjectId

testGetInterp :: Monad m => GetF a -> m a
testGetInterp = \case
  GetCellValue _ reply ->
    pure $ reply $ Just $ VNumber 1
  GetColumnValues _ reply ->
    pure $ reply [Just $ VNumber 1]
  GetTableRows _  reply ->
    pure $ reply [nullObjectId]
  GetRowField _ _ reply ->
    pure $ reply $ Just $ VNumber 1

--------------------------------------------------------------------------------

prelude :: Text
prelude = [text|
data Boolean
  = True
  | False

data Tuple a b
  = Tuple a b

data List a
  = Nil
  | Cons a (List a)

class Functor f where
  map : forall a b. (a -> b) -> f a -> f b

instance Functor List where
  map f xs = case xs of
    Nil -> Nil
    Cons a as -> Cons (f a) (map f as)

conj : Boolean -> Boolean -> Boolean
conj x y = if x then False else y

disj : Boolean -> Boolean -> Boolean
disj x y = if x then True else y

not : Boolean -> Boolean
not x = if x then False else True

append : forall a. List a -> List a -> List a
append xs ys = case xs of
  Nil -> ys
  Cons a as -> Cons a (append as ys)

join : forall a. List (List a) -> List a
join xss = case xss of
  Nil -> Nil
  Cons as ass -> append as (join ass)

class Eq a where
  eq : a -> a -> Boolean

instance Eq Boolean where
  eq a b = case Tuple a b of
    Tuple True True -> True
    Tuple False False -> True
    other -> False

member : forall a. Eq a => a -> List a -> Boolean
member x xs = case xs of
  Nil -> False
  Cons a as -> if eq x a then True else member x as

class Print a where
  print : a -> List Boolean

instance Print Boolean where
  print x = Cons x Nil

instance Print a => Print (List a) where
  print xs = join (map print xs)

instance Print a => Print b => Print (Tuple a b) where
  print (Tuple a b) = append (print a) (print b)
|]

withParsed :: Text -> Parser a -> (a -> IO ()) -> IO ()
withParsed src p m = case parse src p of
  Left err    -> putStrLn $ displayError src err
  Right decls -> m decls

testParsePretty :: Text -> IO ()
testParsePretty src = withParsed src parseModule $ \decls ->
  mapM_ (putStrLn . (<> "\n") . prettyAst) (map stripAnn decls)

testParseSpans :: Text -> IO ()
testParseSpans src = withParsed src parseModule $ \decls ->
  mapM_ (putStrLn . ppShow . map (flip (highlightSpan True) src)) decls

testCheck :: Text -> IO ()
testCheck src = withParsed src parseModule $ \decls ->
  case runCheck primCheckEnv testResolveInterp $ checkModule decls of
    Left err -> putStrLn $ displayError src err
    Right _  -> pure ()

testEval :: Text -> IO ()
testEval src =
  withParsed prelude parseModule $ \decls ->
  withParsed src parseFormula $ \formula ->
  let
    go :: Either Error (Expr, PolyType, TermEnv)
    go = runIdentity $ runExceptT $ do
      (preludeHeader, preludeCode) <- ExceptT $
        runCheck primCheckEnv testResolveInterp $ checkModule decls
      (code, poly) <- ExceptT $
        runCheck (unionCheckEnv preludeHeader primCheckEnv) testResolveInterp $
        checkFormula formula
      pure (code, poly, primTermEnv `Map.union` loadModule preludeCode)
  in
  case go of
    Left err -> putStrLn $ displayError src err
    Right (code, poly, env) -> do
      putStrLn $ "Type: " <> prettyPolyType poly
      runEval 10000 testGetInterp (eval env code) >>= \case
        Left err -> putStrLn err
        Right r  -> putStrLn $ prettyResult r
