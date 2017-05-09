-- |

module Main where

import           Lib.Prelude
import           Options

import qualified Data.Text.IO             as T

import           Lib.Compiler
import           Lib.Compiler.Check.Monad
import           Lib.Compiler.Error

main :: IO ()
main = do
  opts <- getOptions

  input <- if optGenPrelude opts
    then pure preludeText
    else T.getContents

  documentModule input voidResolver primCheckEnv >>= \case
    Left err  -> T.hPutStrLn stderr $ displayError input err
    Right doc -> print doc
