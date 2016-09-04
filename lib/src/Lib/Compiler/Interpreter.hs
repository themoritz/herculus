{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Compiler.Interpreter where

import           Control.Monad.Except
import           Control.Monad.Reader

import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.Text                      (Text, pack)
import           Data.Traversable

import           Lib.Model.Cell
import           Lib.Types

import           Lib.Compiler.Interpreter.Types
import           Lib.Compiler.Types

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
  , ( "show"
    , RPrelude $ \_ (RValue (VNumber n)) -> pure $ RValue $ VString $ pack $ show n
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

eval :: Monad m => TermEnv m -> TExpr -> InterpretT m (Result m)
eval env expr = case expr of
  TLam x body -> do
    pure $ RClosure x body env
  TApp f arg -> do
    argVal <- eval env arg
    eval env f >>= \case
      RClosure x body cl -> eval (Map.insert x argVal cl) body
      RPrelude f' -> f' env argVal
  TLet x e body -> do
    eVal <- eval env e
    eval (Map.insert x eVal env) body
  TIf cond e1 e2 -> do
    RValue (VBool bVal) <- eval env cond
    if bVal then eval env e1 else eval env e2
  TVar x -> do
    let Just v = Map.lookup x env
    pure v
  TLit l -> do
    let v = case l of
          LNumber v' -> VNumber v'
          LBool v' -> VBool v'
          LString v' -> VString v'
    pure $ RValue v
  TBinop op l r -> do
    let numOp o = do
          RValue (VNumber a) <- eval env l
          RValue (VNumber b) <- eval env r
          pure $ RValue $ VNumber $ a `o` b
        timOp o = do
          RValue (VTime a) <- eval env l
          RValue (VTime b) <- eval env r
          pure $ RValue $ VBool $ a `o` b
        bolOp o = do
          RValue (VBool a) <- eval env l
          RValue (VBool b) <- eval env r
          pure $ RValue $ VBool $ a `o` b
    case op of
      Add       -> numOp (+)
      Sub       -> numOp (-)
      Mul       -> numOp (*)
      -- Div TODO: catch div by 0 error
      LessEq    -> timOp (<=)
      GreaterEq -> timOp (>=)
      Less      -> timOp (<)
      Greater   -> timOp (>)
      And       -> bolOp (&&)
      Or        -> bolOp (||)
  TPrjRecord e name -> do
    RValue (VRecord mRecId) <- eval env e
    case mRecId of
      Nothing -> throwError "dependent cell not ready (invalid reference))"
      Just recId -> do
        f <- asks envGetRecordValue
        lift (f recId name) >>= \case
          Nothing -> throwError "dependent cell not ready"
          Just val -> pure $ RValue val
  TColumnRef colId -> do
    f <- asks envGetCellValue
    lift (f colId) >>= \case
      Nothing -> throwError "dependent cell not ready"
      Just val -> pure $ RValue val
  TWholeColumnRef colId -> do
    f <- asks envGetColumnValues
    mVals <- lift $ f colId
    fmap (RValue . VList) $ for mVals $ \case
      Nothing -> throwError "dependent cell not ready"
      Just val -> pure val
  TTableRef tblId _ -> do
    f <- asks envGetTableRecords
    records <- lift $ f tblId
    pure $ RValue $ VList $ map (VRecord . Just) records

interpret :: Monad m => TExpr -> EvalEnv m -> m (Either Text Value)
interpret expr env = do
  result <- runInterpretT env (eval prelude expr)
  case result of
    Left e -> pure $ Left e
    Right (RValue v) -> pure $ Right v
    Right (RClosure _ _ _) -> pure $ Left "did not expect closure"
    Right (RPrelude _) -> pure $ Left "did not expect prelude function"
