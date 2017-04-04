{-# LANGUAGE NoImplicitPrelude #-}
-- |

module Lib.Compiler.AST.Position where

import           Lib.Prelude

import qualified Data.Text       as T

import           Text.Megaparsec as P

data SourceSpan = SourceSpan
  { spanStart :: SourcePos
  , spanEnd   :: SourcePos
  } deriving (Eq, Ord, Show)

sourceUnion :: SourceSpan -> SourceSpan -> SourceSpan
sourceUnion (SourceSpan s1 e1) (SourceSpan s2 e2) =
  SourceSpan (min s1 s2) (max e1 e2)

prevColumn :: SourcePos -> SourcePos
prevColumn (SourcePos n line col) = SourcePos n line (subPos col)
  where
  subPos p = unsafePos $ max 1 (unPos p - 1)

highlightSpan :: SourceSpan -> Text -> [Text]
highlightSpan (SourceSpan start end) = fill . injStart . injEnd . T.lines
  where
  fill :: [Text] -> [Text]
  fill ts =
    let l = maximum (map T.length ts)
    in  map (\t -> t <> T.replicate (l - T.length t) " ") ts

  injStart :: [Text] -> [Text]
  injStart = inj (mkMarker 'v' startCol) (startLine - 1)
  injEnd :: [Text] -> [Text]
  injEnd = inj (mkMarker '^' endCol) endLine

  inj :: a -> Int -> [a] -> [a]
  inj x i xs = take i xs <> (x : drop i xs)
  mkMarker :: Char -> Int -> Text
  mkMarker c i = T.snoc (T.replicate (i - 1) " ") c

  startCol  = fromIntegral $ unPos $ P.sourceColumn start
  startLine = fromIntegral $ unPos $ P.sourceLine start
  endCol    = fromIntegral $ unPos $ P.sourceColumn end
  endLine   = fromIntegral $ unPos $ P.sourceLine end
