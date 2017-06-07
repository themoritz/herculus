-- |

module Lib.Compiler.Eval
  ( matchValue
  , eval
  ) where

import           Lib.Prelude

import qualified Data.IntMap             as IntMap
import qualified Data.Map                as Map
import           Data.Maybe              (fromJust)
import           Data.Text               (unlines)

import           Lib.Model.Cell
import           Lib.Types

import           Lib.Compiler.Core
import           Lib.Compiler.Eval.Monad
import           Lib.Compiler.Eval.Types

eval :: Monad m => TermEnv m -> Expr -> Eval m (Result m)
eval env e = consumeGas *> case e of
  Literal lit -> evalLit env lit
  Var v -> case IntMap.lookup v env of
    Nothing -> internalError $ unlines
      [ "Variable not found: " <> show v
      , termEnvPretty env ]
    Just r -> case r of
      RContinuation expr cl -> eval (env `IntMap.union` cl) expr
      _                     -> pure r
  Reference ref -> evalRef ref
  Constructor c -> pure $ RData c []
  Abs b expr -> pure $ RClosure b expr env
  App f arg -> do
    argRes <- eval env arg
    eval env f >>= \case
      RClosure b body cl -> case matchValue argRes b of
        Just env' -> eval (env' `IntMap.union` cl) body
        Nothing   -> internalError "Eval `App`: pattern match failure"
      RPrimFun primF -> primF argRes
      RData name args ->
        pure $ RData name (args <> [argRes])
      _ -> internalError "Eval `App`: expected closure"
  Let bs body -> do
    let
      conts = map (\(name, expr) -> (name, RContinuation expr env)) bs
      env' = IntMap.fromList conts `IntMap.union` env
    vals <- for bs $ \(name, expr) -> do
      result <- eval env' expr
      pure (name, result)
    eval (IntMap.fromList vals `IntMap.union` env) body
  Case scrut alts -> do
    res <- eval env scrut
    let
      tryAlts [] = evalError $ "Eval `Case`: pattern match failure: " <> prettyResult res
      tryAlts ((binder, expr):as) = case matchValue res binder of
        Nothing   -> tryAlts as
        Just env' -> eval (env' `IntMap.union` env) expr
    tryAlts alts
  Access e' field -> do
    RRecord r <- eval env e'
    RString f <- eval env field
    pure $ fromJust $ Map.lookup f r
  Deref e' -> do
    RRowRef mR <- eval env e'
    case mR of
      Nothing -> evalError "Invalid row reference."
      Just r  -> withGetter (\g -> getRowRecord g r) >>= \case
        Just record -> pure $ RRecord $ map loadValue record
        Nothing -> evalError "Dependent cell not ready."

matchValue :: Result m -> Binder -> Maybe (TermEnv m)
matchValue res = \case
  VarBinder x ->
    Just $ IntMap.singleton x res
  WildcardBinder ->
    Just IntMap.empty
  ConstructorBinder name args
    | RData label results <- res
    , label == name ->
        map IntMap.unions $ zipWithM matchValue results args
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
