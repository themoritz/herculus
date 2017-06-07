{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}
-- |

module Lib.Template.AST where

import           Lib.Prelude

import           Data.Functor.Foldable

import           Lib.Compiler.AST
import           Lib.Compiler.AST.Common

data TplChunkF e a
  = TplText Text
  -- Binder, expression, body
  | TplFor e e [a]
  -- Condition, then part, else part
  | TplIf e [a] [a]
  | TplPrint e
  deriving (Functor, Show)

type SourceTplChunk = WithSpan (TplChunkF SourceAst)

type TplIntermed = Fix (TplChunkF Intermed)
type TplCompiled = Fix (TplChunkF Compiled)

tplText :: Text -> Fix (TplChunkF e)
tplText = Fix . TplText

tplFor :: e -> e -> [Fix (TplChunkF e)] -> Fix (TplChunkF e)
tplFor b e body = Fix $ TplFor b e body

tplIf :: e -> [Fix (TplChunkF e)] -> [Fix (TplChunkF e)] -> Fix (TplChunkF e)
tplIf e th el = Fix $ TplIf e th el

tplPrint :: e -> Fix (TplChunkF e)
tplPrint = Fix . TplPrint
