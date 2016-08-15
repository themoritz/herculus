{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Compiler.Interpreter where

import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.Text                      (Text)
import           Data.Traversable

import           Lib.Model.Cell
import           Lib.Model.Column
import           Lib.Types

import           Lib.Compiler.Interpreter.Types

-- Prelude

prelude :: Monad m => Map String (Result m)
prelude = Map.fromList
  [ ( "zero"
    , RValue $ VNumber $ Number 0
    )
  , ( "double"
    , RPrelude $ \_ (RValue (VNumber x)) -> pure $ RValue $ VNumber $ 2 * x
    )
  , ( "sum"
    , RPrelude $ \_ (RValue (VList xs)) -> pure $
        RValue $ VNumber $ sum $ map (\(VNumber v) -> v) xs
    )
  , ( "map"
    , RPrelude $ \env arg -> pure $ RPrelude $ \_ (RValue (VList xs)) -> do
        let f x = case arg of
              RClosure name body cl -> do
                RValue v <- eval (Map.insert name (RValue x) cl) body
                pure v
              RPrelude f' -> do
                RValue v <- f' env (RValue x)
                pure v
        RValue . VList <$> traverse f xs
    )
  , ( "filter"
    , RPrelude $ \env arg -> pure $ RPrelude $ \_ (RValue (VList xs)) -> do
        let p x = case arg of
              RClosure name body cl -> do
                RValue (VBool b) <- eval (Map.insert name (RValue x) cl) body
                pure b
              RPrelude f' -> do
                RValue (VBool b) <- f' env (RValue x)
                pure b
        RValue . VList <$> filterM p xs
    )
  , ( "find"
    , RPrelude $ \env arg -> pure $ RPrelude $ \_ (RValue (VList xs)) -> do
        let p x = case arg of
              RClosure name body cl -> do
                RValue (VBool b) <- eval (Map.insert name (RValue x) cl) body
                pure b
              RPrelude f' -> do
                RValue (VBool b) <- f' env (RValue x)
                pure b
            findM _ [] = pure Nothing
            findM p' (x:xs') = do
              b <- p' x
              if b then pure (Just x) else findM p' xs'
        RValue . VMaybe <$> findM p xs
    )
  ]

-- Interpreter

eval :: Monad m => TermEnv m -> Expr Id -> InterpretT m (Result m)
eval env expr = case expr of
  Lam x body -> do
    pure $ RClosure x body env
  App f arg -> do
    argVal <- eval env arg
    eval env f >>= \case
      RClosure x body cl -> eval (Map.insert x argVal cl) body
      RPrelude f' -> f' env argVal
  Let x e body -> do
    eVal <- eval env e
    eval (Map.insert x eVal env) body
  If cond e1 e2 -> do
    RValue (VBool bVal) <- eval env cond
    if bVal then eval env e1 else eval env e2
  Var x -> do
    let Just v = Map.lookup x env
    pure v
  Lit l -> do
    let v = case l of
          LNumber v' -> VNumber v'
          LBool v' -> VBool v'
          LString v' -> VString v'
    pure $ RValue v
  Binop op l r -> do
    RValue (VNumber a) <- eval env l
    RValue (VNumber b) <- eval env r
    let res = case op of
          Add -> a + b
          Sub -> a - b
          Mul -> a * b
    pure $ RValue $ VNumber res
  PrjRecord e name -> do
    RValue (VRecord recId) <- eval env e
    f <- asks envGetRecordValue
    lift (f recId name) >>= \case
      Nothing -> throwError "dependent cell not ready"
      Just val -> pure $ RValue val
  ColumnRef colId -> do
    f <- asks envGetCellValue
    lift (f colId) >>= \case
      Nothing -> throwError "dependent cell not ready"
      Just val -> pure $ RValue val
  ColumnOfTableRef _ colId -> do
    f <- asks envGetColumnValues
    mVals <- lift $ f colId
    fmap (RValue . VList) $ for mVals $ \case
      Nothing -> throwError "dependent cell not ready"
      Just val -> pure val
  TableRef tblId -> do
    f <- asks envGetTableRecords
    records <- lift $ f tblId
    pure $ RValue $ VList $ map VRecord records

interpret :: Monad m => Expr Id -> EvalEnv m -> m (Either Text Value)
interpret expr env = do
  result <- runInterpretT env (eval prelude expr)
  case result of
    Left e -> pure $ Left e
    Right (RValue v) -> pure $ Right v
    Right (RClosure _ _ _) -> pure $ Left "did not expect closure"
    Right (RPrelude _) -> pure $ Left "did not expect prelude function"
