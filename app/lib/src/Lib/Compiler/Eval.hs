{-# LANGUAGE NoImplicitPrelude #-}
-- |

module Lib.Compiler.Eval where

import           Lib.Prelude

import qualified Data.Map          as Map

import           Lib.Compiler.Core

type TermEnv = Map Text Result

data Value
  = VInt Integer
  | VNumber Double
  | VString Text
  | VData Text [Result]
  deriving (Show)

data Result
  = RValue Value
  | RClosure Text Expr TermEnv
  | RContinuation Expr TermEnv
  deriving (Show)

type Eval = Except Text

eval :: TermEnv -> Expr -> Eval Result
eval env = \case
  Literal lit -> evalLit lit
  Var v -> case Map.lookup v env of
    Nothing -> throwError "Eval `Var`: not found"
    Just r -> case r of
      RContinuation expr cl -> eval cl expr
      _                     -> pure r
  Constructor c -> pure $ RValue $ VData c []
  Abs x expr -> pure $ RClosure x expr env
  App f arg -> do
    argRes <- eval env arg
    eval env f >>= \case
      RClosure x body cl -> eval (Map.insert x argRes cl) body
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

matchValue :: Result -> Binder -> Maybe TermEnv
matchValue res = \case
  VarBinder x ->
    Just $ Map.singleton x res
  ConstructorBinder name args
    | RValue (VData label results) <- res
    , label == name -> Map.unions <$> zipWithM matchValue results args
    | otherwise -> Nothing

evalLit :: Literal -> Eval Result
evalLit = \case
  NumberLit n -> pure $ RValue $ VNumber n
  IntLit i -> pure $ RValue $ VInt i
  StringLit s -> pure $ RValue $ VString s
