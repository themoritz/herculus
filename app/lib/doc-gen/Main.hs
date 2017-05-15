{-# LANGUAGE NoImplicitPrelude #-}
-- |

module Main where

import           Lib.Prelude
import           Options

import           Data.Text                as T
import qualified Data.Text.IO             as T

import           Lib.Compiler
import           Lib.Compiler.Check.Monad
import           Lib.Compiler.Error

main :: IO ()
main = do
  -- i <- T.getLine
  -- testEval $ T.unlines
  --   [ "range i = if i == 0 then Nil else Cons i (range (i-1))"
  --   , "sum (range " <> i <> ")"
  --   ]
  opts <- getOptions

  input <- if optGenPrelude opts
    then pure preludeText
    else T.getContents

  documentModule input voidResolver primCheckEnv >>= \case
    Left err  -> do
      T.hPutStrLn stderr $ displayError input err
      exitFailure
    Right doc -> print doc
