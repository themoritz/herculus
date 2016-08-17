{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Compiler where

import qualified Data.Map                       as Map
import           Data.Text                      (Text, pack, unpack)

import           Lib.Model
import           Lib.Model.Cell
import           Lib.Model.Column
import           Lib.Types

import           Lib.Compiler.Parser
import           Lib.Compiler.Typechecker
import           Lib.Compiler.Typechecker.Types
import           Lib.Compiler.Interpreter
import           Lib.Compiler.Interpreter.Types

compile :: Monad m => Text -> TypecheckEnv m
        -> m (Either Text TypedExpr)
compile inp env = case parseExpr inp of
  Left e -> pure $ Left e
  Right expr -> do
    expr' <- runInfer env expr
    pure expr'

--

testColumn :: Entity Column
testColumn = Entity nullObjectId col
  where col = Column nullObjectId
                     "A"
                     DataNumber
                     ColumnInput
                     ""
                     CompileResultNone

testTypecheckEnv :: Monad m => TypecheckEnv m
testTypecheckEnv = TypecheckEnv
  { envResolveColumnRef = \_ -> pure $ Just testColumn
  , envResolveColumnOfTableRef = \_ _ -> pure $ Just testColumn
  , envResolveTableRef = \_ -> pure $ Just (nullObjectId
                                           , [ Entity nullObjectId
                                                      (Column nullObjectId
                                                              "A"
                                                              DataNumber
                                                              ColumnInput
                                                              ""
                                                              CompileResultNone
                                                      )
                                             ]
                                           )
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
test inp = compile (pack inp) testTypecheckEnv >>= \case
  Left e -> putStrLn $ unpack e
  Right (expr ::: typ) -> do
    putStrLn $ "Type: " ++ show typ
    case interpret expr testEvalEnv of
      Left e -> putStrLn $ unpack e
      Right val -> putStrLn $ "Val: " ++ show val
