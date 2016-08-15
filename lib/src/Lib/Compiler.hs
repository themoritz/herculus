{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Compiler where

import           Control.Monad.Except
import           Control.Monad.Identity
import           Control.Monad.Reader

import           Data.Traversable
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.Text                      (Text, pack, unpack)

import           Lib.Model
import           Lib.Model.Cell
import           Lib.Model.Column
import           Lib.Model.Types
import           Lib.Types

import           Lib.Compiler.Types
import           Lib.Compiler.Parser
import           Lib.Compiler.Typechecker
import           Lib.Compiler.Typechecker.Types

compile :: MonadTypecheck m => Text -> Id Table
        -> m (Either Text TypedExpr)
compile inp tblId = case parseExpr inp of
  Left e -> pure $ Left e
  Right expr -> do
    expr' <- runInfer tblId expr
    pure expr'

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
        (RValue . VList) <$> traverse f xs
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
      RPrelude f -> f env argVal
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
          LNumber v -> VNumber v
          LBool v -> VBool v
          LString v -> VString v
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
    f <- asks getRecordValue
    lift (f recId name) >>= \case
      Nothing -> throwError "dependent cell not ready"
      Just val -> pure $ RValue val
  ColumnRef colId -> do
    f <- asks getCellValue
    lift (f colId) >>= \case
      Nothing -> throwError "dependent cell not ready"
      Just val -> pure $ RValue val
  ColumnOfTableRef _ colId -> do
    f <- asks getColumnValues
    mVals <- lift $ f colId
    fmap (RValue . VList) $ for mVals $ \case
      Nothing -> throwError "dependent cell not ready"
      Just val -> pure val
  TableRef tblId -> do
    f <- asks getTableRecords
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

--

newtype Test a = Test
  { unTest :: Identity a
  } deriving ( Functor
             , Applicative
             , Monad
             )

runTest :: Test a -> a
runTest = runIdentity . unTest

testColumn :: Entity Column
testColumn = Entity nullObjectId col
  where col = Column nullObjectId
                     "A"
                     DataNumber
                     ColumnInput
                     ""
                     CompileResultNone

instance MonadTypecheck Test where
  resolveColumnRef t c = pure $ Just testColumn
  resolveColumnOfTableRef table c = pure $ Just (nullObjectId, testColumn)
  resolveTableRef table = pure $ Just ( nullObjectId
                                      , Map.singleton (Ref "A")
                                                      (TBase "Number")
                                      )

testEvalEnv :: EvalEnv Test
testEvalEnv = EvalEnv
  { getCellValue = \c -> pure $ Just $ VNumber 1
  , getColumnValues = \c -> pure [Just $ VNumber 1]
  , getTableRecords = \t -> pure [nullObjectId]
  , getRecordValue = \r column -> pure $ Just $ VNumber 1
  }

test :: String -> IO ()
test inp = case runTest $ compile (pack inp) nullObjectId of
  Left e -> putStrLn $ unpack e
  Right (expr ::: typ) -> do
    putStrLn $ "Type: " ++ show typ
    case runTest $ interpret expr testEvalEnv of
      Left e -> putStrLn $ unpack e
      Right val -> putStrLn $ "Val: " ++ show val
