{-# LANGUAGE NoImplicitPrelude #-}
-- |

module Lib.Compiler.Eval where

import           Lib.Prelude

import qualified Data.Map                as Map
import           Data.Maybe              (fromJust)
import           Data.Text               (unlines)

import           Lib.Compiler.Core
import           Lib.Compiler.Eval.Types

eval :: TermEnv -> Expr -> Eval Result
eval env = \case
  Literal lit -> evalLit env lit
  Var v -> case Map.lookup v env of
    Nothing -> throwError $ unlines
      [ "Variable not found: " <> v
      , termEnvPretty env ]
    Just r -> case r of
      RContinuation expr cl -> eval (env `Map.union` cl) expr
      _                     -> pure r
  Constructor c -> pure $ RValue $ VData c []
  Abs b expr -> pure $ RClosure b expr env
  App f arg -> do
    argRes <- eval env arg
    eval env f >>= \case
      RClosure b body cl -> case matchValue argRes b of
        Just env' -> eval (env' `Map.union` cl) body
        Nothing   -> throwError "Eval `App`: pattern match failure"
      RPrimFun primF -> primF argRes
      RValue (VData name args) ->
        pure $ RValue $ VData name (args <> [argRes])
      _ -> throwError "Eval `App`: expected closure"
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
      tryAlts [] = throwError "Eval `Case`: pattern match failure"
      tryAlts ((binder, expr):as) = case matchValue res binder of
        Nothing   -> tryAlts as
        Just env' -> eval (env' `Map.union` env) expr
    tryAlts alts
  Accessor e field -> do
    RValue (VRecord r) <- eval env e
    pure $ fromJust $ Map.lookup field r

matchValue :: Result -> Binder -> Maybe TermEnv
matchValue res = \case
  VarBinder x ->
    Just $ Map.singleton x res
  ConstructorBinder name args
    | RValue (VData label results) <- res
    , label == name -> map Map.unions $ zipWithM matchValue results args
    | otherwise -> Nothing

evalLit :: TermEnv -> Literal -> Eval Result
evalLit env = \case
  NumberLit n -> pure $ RValue $ VNumber n
  IntegerLit i -> pure $ RValue $ VInt i
  StringLit s -> pure $ RValue $ VString s
  RecordLit fields -> RValue . VRecord <$> traverse (eval env) fields
