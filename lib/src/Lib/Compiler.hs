module Lib.Compiler where

import Data.Text (Text, pack)

import Lib.Compiler.Parser
import Lib.Compiler.Typechecker

compile :: String -> Either Text Type
compile inp = do
  expr <- parseExpr $ pack inp
  typecheckExpr expr