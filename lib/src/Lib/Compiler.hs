{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module Lib.Compiler where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Identity

import           Data.Traversable
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.Text                      (Text, pack, unpack)

import           Lib.Compiler.Parser
import           Lib.Compiler.Typechecker
import           Lib.Compiler.Typechecker.Types
import           Lib.Model
import           Lib.Model.Cell
import           Lib.Model.Column
import           Lib.Model.Types
import           Lib.Types

compile :: MonadTypecheck m => Text -> Id Table
        -> m (Either Text TypedExpr)
compile inp tblId = case parseExpr inp of
  Left e -> pure $ Left e
  Right expr -> do
    expr' <- runInfer tblId expr
    pure expr'

-- Interpreter

data Prelude
  = PreSum

data Body
  = BodyPrelude Prelude
  | BodyExpr (Expr Id)

data Result
  = RValue Value
  | RClosure String (Expr Id) TermEnv
  deriving (Show)

type TermEnv = Map String Result

type EvalError = Text

data EvalEnv m = EvalEnv
  { getCellValue :: Id Column -> m (Maybe Value)
  , getColumnValues :: Id Column -> m [(Maybe Value)]
  , getTableRecords :: Id Table -> m [Id Record]
  , getRecordValue :: Id Record -> Ref Column -> m (Maybe Value)
  }

newtype InterpretT m a = InterpretT
  { unInterpretT :: ReaderT (EvalEnv m) (ExceptT EvalError m) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadError EvalError
             , MonadReader (EvalEnv m)
             )

instance MonadTrans InterpretT where
  lift = InterpretT . lift . lift

runInterpretT :: EvalEnv m -> InterpretT m a -> m (Either EvalError a)
runInterpretT env action = runExceptT $ runReaderT (unInterpretT action) env

eval :: Monad m => TermEnv -> Expr Id -> InterpretT m Result
eval env expr = case expr of
  Lam x body -> do
    pure $ RClosure x body env
  App f arg -> do
    RClosure x body cl <- eval env f
    argVal <- eval env arg
    eval (Map.insert x argVal cl) body
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
  result <- runInterpretT env (eval Map.empty expr)
  case result of
    Left e -> pure $ Left e
    Right (RValue v) -> pure $ Right v
    Right (RClosure _ _ _) -> pure $ Left "did not expect closure"

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
                                      , TR $ Map.singleton (Ref "A")
                                                           (nullObjectId, TBase "Number")
                                      )

testEvalEnv :: EvalEnv Test
testEvalEnv = EvalEnv
  { getCellValue = \c -> pure $ Just $ VNumber 1
  , getColumnValues = \c -> pure [Just $ VNumber 1]
  , getTableRecords = \t -> pure [nullObjectId]
  , getRecordValue = \r column -> pure $ Just $ VNumber 1
  }

test :: String -> IO ()
test inp = do
  let res = runTest $ do
        compile (pack inp) nullObjectId >>= \case
          Left e -> pure $ Left e
          Right (expr ::: typ) ->
            interpret expr testEvalEnv >>= \case
              Left e -> pure $ Left e
              Right val -> pure $ Right (val, typ)
  case res of
    Left e -> putStrLn $ unpack e
    Right (val, typ) -> do
      putStrLn $ "Type: " ++ show typ
      putStrLn $ "Val: " ++ show val
