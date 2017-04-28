{-# LANGUAGE NoImplicitPrelude #-}
-- |

module Lib.Compiler.Eval
  ( matchValue
  , eval
  ) where

import           Lib.Prelude

import qualified Data.Map                as Map
import           Data.Maybe              (fromJust)
import           Data.Text               (unlines)

import           Lib.Model.Cell
import           Lib.Types

import           Lib.Compiler.Core
import           Lib.Compiler.Eval.Monad
import           Lib.Compiler.Eval.Types

eval :: TermEnv -> Expr -> Eval Result
eval env e = consumeGas *> case e of
  Literal lit -> evalLit env lit
  Var v -> case Map.lookup v env of
    Nothing -> internalError $ unlines
      [ "Variable not found: " <> v
      , termEnvPretty env ]
    Just r -> case r of
      RContinuation expr cl -> eval (env `Map.union` cl) expr
      _                     -> pure r
  Reference ref -> evalRef ref
  Constructor c -> pure $ RData c []
  Abs b expr -> pure $ RClosure b expr env
  App f arg -> do
    argRes <- eval env arg
    eval env f >>= \case
      RClosure b body cl -> case matchValue argRes b of
        Just env' -> eval (env' `Map.union` cl) body
        Nothing   -> internalError "Eval `App`: pattern match failure"
      RPrimFun primF -> primF argRes
      RData name args ->
        pure $ RData name (args <> [argRes])
      _ -> internalError "Eval `App`: expected closure"
  Let bs body -> do
    let
      conts = map (\(name, expr) -> (name, RContinuation expr env)) bs
      env' = Map.fromList conts `Map.union` env
    vals <- for bs $ \(name, expr) -> do
      result <- eval env' expr
      pure (name, result)
    eval (Map.fromList vals `Map.union` env) body
  Case scrut alts -> do
    res <- eval env scrut
    let
      tryAlts [] = evalError "Eval `Case`: pattern match failure"
      tryAlts ((binder, expr):as) = case matchValue res binder of
        Nothing   -> tryAlts as
        Just env' -> eval (env' `Map.union` env) expr
    tryAlts alts
  Accessor e field -> do
    v <- eval env e
    case v of
      RRecord r -> pure $ fromJust $ Map.lookup field r
      -- Row references are automatically dereferenced here.
      RRowRef mR -> case mR of
        Nothing -> evalError "Invalid row reference."
        Just r -> getRowField r (Ref field) >>= \case
          Nothing -> evalError "Dependent cell not ready."
          Just val -> pure $ loadValue val

matchValue :: Result -> Binder -> Maybe TermEnv
matchValue res = \case
  VarBinder x ->
    Just $ Map.singleton x res
  WildcardBinder -> Just Map.empty
  ConstructorBinder name args
    | RData label results <- res
    , label == name -> map Map.unions $ zipWithM matchValue results args
    | otherwise -> Nothing

evalLit :: TermEnv -> Literal -> Eval Result
evalLit env = \case
  NumberLit n -> pure $ RNumber (Number n)
  IntegerLit i -> pure $ RInteger i
  StringLit s -> pure $ RString s
  RecordLit fields -> RRecord <$> traverse (eval env) fields

evalRef :: Reference -> Eval Result
evalRef = \case
  ColumnRef c -> getCellValue c >>= \case
    Nothing -> evalError "Dependent cell not ready."
    Just v -> pure $ loadValue v
  TableRef t -> do
    rows <- getTableRows t
    pure $ loadValue $ VList $ map (VRowRef . Just) rows
  ColumnOfTableRef _ c -> do
    mVals <- getColumnValues c
    case sequence mVals of
      Nothing   -> evalError "Dependent cell not ready."
      Just vals -> pure $ loadValue $ VList vals
