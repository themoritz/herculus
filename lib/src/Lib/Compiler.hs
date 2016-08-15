{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Compiler where

import           Control.Monad.Identity

import qualified Data.Map                       as Map
import           Data.Text                      (Text, pack, unpack)

import           Lib.Model
import           Lib.Model.Cell
import           Lib.Model.Column
import           Lib.Model.Types
import           Lib.Types

import           Lib.Compiler.Parser
import           Lib.Compiler.Typechecker
import           Lib.Compiler.Typechecker.Types
import           Lib.Compiler.Interpreter
import           Lib.Compiler.Interpreter.Types

compile :: MonadTypecheck m => Text -> Id Table
        -> m (Either Text TypedExpr)
compile inp tblId = case parseExpr inp of
  Left e -> pure $ Left e
  Right expr -> do
    expr' <- runInfer tblId expr
    pure expr'

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
  resolveColumnRef _ _ = pure $ Just testColumn
  resolveColumnOfTableRef _ _ = pure $ Just (nullObjectId, testColumn)
  resolveTableRef _ = pure $ Just ( nullObjectId
                                      , Map.singleton (Ref "A")
                                                      (TNullary TNumber)
                                      )

testEvalEnv :: EvalEnv Test
testEvalEnv = EvalEnv
  { getCellValue = \_ -> pure $ Just $ VNumber 1
  , getColumnValues = \_ -> pure [Just $ VNumber 1]
  , getTableRecords = \_ -> pure [nullObjectId]
  , getRecordValue = \_ _ -> pure $ Just $ VNumber 1
  }

test :: String -> IO ()
test inp = case runTest $ compile (pack inp) nullObjectId of
  Left e -> putStrLn $ unpack e
  Right (expr ::: typ) -> do
    putStrLn $ "Type: " ++ show typ
    case runTest $ interpret expr testEvalEnv of
      Left e -> putStrLn $ unpack e
      Right val -> putStrLn $ "Val: " ++ show val
