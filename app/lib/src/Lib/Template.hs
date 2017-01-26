{-# LANGUAGE LambdaCase #-}

module Lib.Template where

import           Data.Text                      (Text, pack)
import           Data.Text.IO                   as Text

import           Lib.Compiler
import           Lib.Compiler.Typechecker.Types
import           Lib.Template.Interpreter
import           Lib.Template.Parser
import           Lib.Template.Typechecker
import           Lib.Template.Types

compileTemplate :: Monad m => Text -> TypecheckEnv m -> m (Either Text CTemplate)
compileTemplate inp env = case parseTemplate inp of
  Left e -> pure $ Left e
  Right tpl -> do
    inferResult <- runInferTpl env tpl
    case inferResult of
      Left e -> pure $ Left e
      Right ttpl -> pure $ Right ttpl

test :: String -> IO ()
test inp = compileTemplate (pack inp) testTypecheckEnv >>= \case
  Left e -> Text.putStrLn e
  Right ttpl -> do
    evalResult <- runEvalTemplate testEvalEnv ttpl
    case evalResult of
      Left e -> Text.putStrLn e
      Right txt -> Text.putStrLn txt
