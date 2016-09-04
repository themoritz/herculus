{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Compiler where

import           Data.Text                      (pack, unpack)
import qualified Data.Text.IO                   as Text

import           Lib.Model.Cell
import           Lib.Model.Column
import           Lib.Types

import           Lib.Compiler.Interpreter
import           Lib.Compiler.Interpreter.Types
import           Lib.Compiler.Parser
import           Lib.Compiler.Typechecker
import           Lib.Compiler.Typechecker.Types

testDataCol :: DataCol
testDataCol = DataCol
  DataNumber
  NotDerived
  ""
  CompileResultNone

testTypecheckEnv :: Monad m => TypecheckEnv m
testTypecheckEnv = TypecheckEnv
  { envResolveColumnRef = \_ -> pure $ Just (nullObjectId, testDataCol)
  , envResolveColumnOfTableRef = \_ _ -> pure $ Just (nullObjectId, testDataCol)
  , envResolveTableRef = \_ -> pure $ Just (nullObjectId
                                           , [ (nullObjectId, testDataCol)
                                             ]
                                           )
  , envGetTableRows = \_ -> pure $ TyRow (Ref "A") (TyNullary TyNumber) TyNoRow
  , envOwnTableId = nullObjectId
  }

testEvalEnv :: Monad m => EvalEnv m
testEvalEnv = EvalEnv
  { envGetCellValue = \_ -> pure $ Just $ VNumber 1
  , envGetColumnValues = \_ -> pure [Just $ VNumber 1]
  , envGetTableRecords = \_ -> pure [nullObjectId]
  , envGetRecordValue = \_ _ -> pure $ Just $ VNumber 1
  }

test :: String -> IO ()
test inp = case parseExpr (pack inp) of
  Left e -> Text.putStrLn e
  Right pe -> do
    inferRes <- runInfer testTypecheckEnv pe
    case inferRes of
      Left e -> putStrLn $ unpack e
      Right (te ::: typ) -> do
        putStrLn $ "Type: " ++ show typ
        case interpret te testEvalEnv of
          Left e -> putStrLn $ unpack e
          Right val -> putStrLn $ "Val: " ++ show val
