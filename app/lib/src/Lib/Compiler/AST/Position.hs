{-# LANGUAGE NoImplicitPrelude #-}
-- |

module Lib.Compiler.AST.Position where

import           Lib.Prelude

import           Text.Megaparsec

data SourceSpan = SourceSpan
  { spanStart :: SourcePos
  , spanStop  :: SourcePos
  } deriving (Eq, Ord, Show)

sourceUnion :: SourceSpan -> SourceSpan -> SourceSpan
sourceUnion (SourceSpan s1 e1) (SourceSpan s2 e2) =
  SourceSpan (min s1 s2) (max e1 e2)
