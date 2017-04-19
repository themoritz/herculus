{-# LANGUAGE NoImplicitPrelude #-}
-- |

module Lib.Compiler.AST.Position where

import           Lib.Prelude

import qualified Data.Text       as T

import           Text.Megaparsec as P

data Span = Span
  { spanStart :: SourcePos
  , spanEnd   :: SourcePos
  } deriving (Eq, Ord, Show)

spanUnion :: Span -> Span -> Span
spanUnion (Span s1 e1) (Span s2 e2) =
  Span (min s1 s2) (max e1 e2)

prevColumn :: SourcePos -> SourcePos
prevColumn (SourcePos n line col) = SourcePos n line (subPos col)
  where
  subPos p = unsafePos $ max 1 (unPos p - 1)

highlightSpan :: Bool -> Span -> Text -> [Text]
highlightSpan doFill (Span start end) =
  (if doFill then fill else id) . crop . insStart . insEnd . T.lines
  where
  fill :: [Text] -> [Text]
  fill ts =
    let l = maximum (map T.length ts)
    in  map (\t -> t <> T.replicate (l - T.length t) " ") ts

  crop :: [Text] -> [Text]
  crop = drop (startLine - 1) . take (endLine + 2)

  insStart :: [Text] -> [Text]
  insStart = ins (mkMarker 'v' startCol) (startLine - 1)
  insEnd :: [Text] -> [Text]
  insEnd = ins (mkMarker '^' endCol) endLine

  ins :: a -> Int -> [a] -> [a]
  ins x i xs = take i xs <> (x : drop i xs)
  mkMarker :: Char -> Int -> Text
  mkMarker c i = T.snoc (T.replicate (i - 1) " ") c

  startCol  = fromIntegral $ unPos $ P.sourceColumn start
  startLine = fromIntegral $ unPos $ P.sourceLine start
  endCol    = fromIntegral $ unPos $ P.sourceColumn end
  endLine   = fromIntegral $ unPos $ P.sourceLine end
