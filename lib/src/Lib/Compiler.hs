module Lib.Compiler where

import           Control.Monad.Trans.Reader

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

type Interpret a = Reader TermEnv a

eval :: Expr -> Interpret Value
eval expr = case expr of
  Lam x body -> do
    env <- ask
    pure $ VClosure x body env
  App f arg -> do
    VClosure x body cl <- eval f
    argVal <- eval arg
    local (Map.insert x argVal) $ eval body
  Let x e body -> do
    eVal <- eval e
    local (Map.insert x eVal) $ eval body
  If cond e1 e2 -> do
    VBool bVal <- eval cond
    if bVal then eval e1 else eval e2
  Var x -> do
    Just v <- asks (Map.lookup x)
    pure v
  Lit l -> case l of
    LInt v -> pure $ VInt v
    LBool v -> pure $ VBool v
    LString v -> pure $ VString v
  Binop op l r -> do
    VInt a <- eval l
    VInt b <- eval r
    let res = case op of
          Add -> a + b
          Sub -> a - b
          Mul -> a * b
    pure $ VInt res

runEval :: String -> Either Text Value
runEval inp = case compile inp of
  Left e -> Left e
  Right (expr, _) -> pure $ runReader (eval expr) Map.empty
