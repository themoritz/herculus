module Lib.Template where

import           Data.Text                (pack)
import           Data.Text.IO             as Text

import           Lib.Compiler
import           Lib.Template.Interpreter
import           Lib.Template.Parser
import           Lib.Template.Typechecker

test :: String -> IO ()
test inp = case parseTemplate (pack inp) of
  Left e -> Text.putStrLn e
  Right tpl -> do
    inferResult <- runInferTpl testTypecheckEnv tpl
    case inferResult of
      Left e -> Text.putStrLn e
      Right ttpl -> do
        evalResult <- runEvalTemplate testEvalEnv ttpl
        case evalResult of
          Left e -> Text.putStrLn e
          Right txt -> Text.putStrLn txt
