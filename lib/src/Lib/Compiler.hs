module Lib.Compiler where

import           Control.Monad.Identity

import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Text                  (Text, pack)

import           Lib.Types hiding (Value)
import           Lib.Model.Types
import           Lib.Compiler.Parser
import           Lib.Compiler.Typechecker
import           Lib.Compiler.Typechecker.Types

compile :: String -> Id Table -> Either Text (Expr Id, Type)
compile inp tblId = do
  expr <- parseExpr $ pack inp
  expr' ::: typ <- runInfer tblId expr
  pure (expr', typ)

-- Interpreter

data Value
  = VNumber Number
  | VBool Bool
  | VString Text
  | VClosure String (Expr Id) TermEnv
  | VList [Value]
  deriving (Show)

type TermEnv = Map String Value

type Interpret a = Identity a

eval :: TermEnv -> (Expr Id) -> Interpret Value
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
    LNumber v -> pure $ VNumber v
    LBool v -> pure $ VBool v
    LString v -> pure $ VString v
  Binop op l r -> do
    VNumber a <- eval env l
    VNumber b <- eval env r
    let res = case op of
          Add -> a + b
          Sub -> a - b
          Mul -> a * b
    pure $ VNumber res
  PrjRecord e name -> do
    undefined
    -- e should evaluate to a record
    -- get column by name
    -- return value according to cell content
  ColumnRef colId -> do
    undefined
    -- return value according to cell content
  ColumnOfTableRef _ colId -> do
    undefined
    -- return list of values according to cell contents
  TableRef tblId -> do
    undefined
    -- return list of records

runEval :: String -> Either Text Value
runEval inp = case compile inp of
  Left e -> Left e
  Right (expr, _) -> pure $ runIdentity (eval Map.empty expr)
