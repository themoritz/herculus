-- |

module Lib.Compiler.Eval
  ( matchValue
  , eval
  ) where

import           Lib.Prelude

import qualified Data.HashMap.Strict     as HashMap
import qualified Data.Map                as Map
import           Data.Maybe              (fromJust)

import           Lib.Model.Cell
import           Lib.Types

import           Lib.Compiler.Core
import           Lib.Compiler.Eval.Monad
import           Lib.Compiler.Eval.Types

eval :: Monad m => TermEnv m -> Expr -> Eval m (Result m)
eval env e = consumeGas *> case e of
  Literal lit -> evalLit env lit
  Var v -> case HashMap.lookup v env of
    Nothing -> internalError $ unlines
      [ "Variable not found: " <> show v
      , termEnvPretty env ]
    Just r -> case r of
      RContinuation expr cl -> eval (env `HashMap.union` cl) expr
      _                     -> pure r
  Reference ref -> evalRef ref
  Constructor c -> pure $ RData c []
  Abs b expr -> pure $ RClosure b expr env
  App f arg -> do
    argRes <- eval env arg
    eval env f >>= \case
      RClosure b body cl -> case matchValue argRes b of
        Just env' -> eval (env' `HashMap.union` cl) body
        Nothing   -> internalError "Eval `App`: pattern match failure"
      RPrimFun primF -> primF argRes
      RData name args ->
        pure $ RData name (args <> [argRes])
      _ -> internalError "Eval `App`: expected closure"
  Let bs body -> do
    let
      conts = map (\(name, expr) -> (name, RContinuation expr env)) bs
      env' = HashMap.fromList conts `HashMap.union` env
    vals <- for bs $ \(name, expr) -> do
      result <- eval env' expr
      pure (name, result)
    eval (HashMap.fromList vals `HashMap.union` env) body
  Case scrut alts -> do
    res <- eval env scrut
    let
      tryAlts [] = evalError $ "Eval `Case`: pattern match failure: " <> prettyResult res
      tryAlts ((binder, expr):as) = case matchValue res binder of
        Nothing   -> tryAlts as
        Just env' -> eval (env' `HashMap.union` env) expr
    tryAlts alts
  Access e' field -> do
    r <- eval env e' >>= \case
      RRecord r -> pure r
      _ -> error "Pattern match failure in eval/Access."
    f <- eval env field >>= \case
      RString f -> pure f
      _ -> error "Pattern match failure in eval/Access."
    pure $ fromJust $ Map.lookup f r
  Deref e' -> do
    mR <- eval env e' >>= \case
      RRowRef mR -> pure mR
      _ -> error "Pattern match failure in eval/Deref."
    case mR of
      Nothing -> evalError "Invalid row reference."
      Just r  -> withGetter (\g -> getRowRecord g r) >>= \case
        Just record -> pure $ RRecord $ map loadValue record
        Nothing -> evalError "Dependent cell not ready."

matchValue :: Result m -> Binder -> Maybe (TermEnv m)
matchValue res = \case
  VarBinder x ->
    Just $ HashMap.singleton x res
  WildcardBinder ->
    Just HashMap.empty
  ConstructorBinder name args
    | RData label results <- res
    , label == name ->
        map HashMap.unions $ zipWithM matchValue results args
    | otherwise ->
        Nothing

evalLit :: Monad m => TermEnv m -> Literal -> Eval m (Result m)
evalLit env = \case
  NumberLit n -> pure $ RNumber (Number n)
  IntegerLit i -> pure $ RInteger i
  StringLit s -> pure $ RString s
  RecordLit fields -> RRecord <$> traverse (eval env) fields

evalRef :: Monad m => Reference -> Eval m (Result m)
evalRef = \case
  ColumnRef c -> withGetter (\g -> getCellValue g c) >>= \case
    Nothing -> evalError "Dependent cell not ready."
    Just v -> pure $ loadValue v
  TableRef t -> do
    rows <- withGetter (\g -> getTableRows g t)
    pure $ loadValue $ VList $ map (VRowRef . Just) rows
  ColumnOfTableRef _ c -> do
    mVals <- withGetter (\g -> getColumnValues g c)
    case sequence mVals of
      Nothing   -> evalError "Dependent cell not ready."
      Just vals -> pure $ loadValue $ VList vals
