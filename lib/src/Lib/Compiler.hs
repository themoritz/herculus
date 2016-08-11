module Lib.Compiler where

import           Control.Monad.Identity

import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Text                  (Text, pack)

import           Lib.Compiler.Parser
import           Lib.Compiler.Typechecker

compile :: String -> Either Text (Expr, Type)
compile inp = do
  expr <- parseExpr $ pack inp
  typ <- runInfer expr
  pure (expr, typ)

-- Interpreter

data Value
  = VInt Int
  | VBool Bool
  | VString Text
  | VClosure String Expr TermEnv
  deriving (Show)

type TermEnv = Map String Value

type Interpret a = Identity a

eval :: TermEnv -> Expr -> Interpret Value
eval env expr = case expr of
  Lam x body -> do
    pure $ VClosure x body env
  App f arg -> do
    VClosure x body cl <- eval env f
    argVal <- eval env arg
    eval (Map.insert x argVal cl) body
  Let x e body -> do
    eVal <- eval env e
    eval (Map.insert x eVal env) body
  If cond e1 e2 -> do
    VBool bVal <- eval env cond
    if bVal then eval env e1 else eval env e2
  Var x -> do
    let Just v = Map.lookup x env
    pure v
  Lit l -> case l of
    LInt v -> pure $ VInt v
    LBool v -> pure $ VBool v
    LString v -> pure $ VString v
  Binop op l r -> do
    VInt a <- eval env l
    VInt b <- eval env r
    let res = case op of
          Add -> a + b
          Sub -> a - b
          Mul -> a * b
    pure $ VInt res

runEval :: String -> Either Text Value
runEval inp = case compile inp of
  Left e -> Left e
  Right (expr, _) -> pure $ runIdentity (eval Map.empty expr)
