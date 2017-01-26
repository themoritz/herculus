{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Compiler where

import           Data.Text                      (Text, pack, unpack)

import           Lib.Model.Cell
import           Lib.Model.Column
import           Lib.Types

import           Lib.Compiler.Interpreter
import           Lib.Compiler.Interpreter.Types
import           Lib.Compiler.Parser
import           Lib.Compiler.Typechecker
import           Lib.Compiler.Typechecker.Prim
import           Lib.Compiler.Typechecker.Types
import           Lib.Compiler.Types

compile :: Monad m => Text -> TypecheckEnv m
        -> m (Either Text (CExpr, Type))
compile inp env = case parseExpr inp of
  Left e   -> pure $ Left e
  Right e' -> runInfer env e'

testDataCol :: DataCol
testDataCol = DataCol
  DataNumber
  NotDerived
  ""
  CompileResultNone

testTypecheckEnv :: Monad m => TypecheckEnv m
testTypecheckEnv = TypecheckEnv
  { envResolveColumnRef        = \_ -> pure $ Just (nullObjectId, testDataCol)
  , envResolveColumnOfTableRef = \_ _ -> pure $ Just (nullObjectId, nullObjectId, testDataCol)
  , envResolveTableRef         = \_ -> pure $ Just nullObjectId
  , envGetTableRowType         = \_ -> pure $ Type $ TyRecordCons (Ref "A") (Type tyNumber) (Type TyRecordNil)
  , envOwnTableId              = nullObjectId
  }

testEvalEnv :: Monad m => EvalEnv m
testEvalEnv = EvalEnv
  { envGetCellValue    = \_ -> pure $ Just $ VNumber 1
  , envGetColumnValues = \_ -> pure [Just $ VNumber 1]
  , envGetTableRows    = \_ -> pure [nullObjectId]
  , envGetRowField     = \_ _ -> pure $ Just $ VNumber 1
  }

test :: String -> IO ()
test inp = compile (pack inp) testTypecheckEnv >>= \case
  Left e -> putStrLn $ unpack e
  Right (e, typ) -> do
    putStrLn $ "Type: " ++ show typ
    case interpret e testEvalEnv of
      Left e'   -> putStrLn $ unpack e'
      Right val -> putStrLn $ "Val: " ++ show val
