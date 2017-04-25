{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |

module Lib.Compiler.AST.Position where

import           Lib.Prelude

import           Data.Aeson
import qualified Data.Text       as T

import qualified Text.Megaparsec as P

data Pos = Pos
  { posLine :: Int
  , posCol  :: Int
  } deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

fromSourcePos :: P.SourcePos -> Pos
fromSourcePos p = Pos
  (fromIntegral $ P.unPos $ P.sourceLine p)
  (fromIntegral $ P.unPos $ P.sourceColumn p)

voidPos :: Pos
voidPos = Pos 1 1

data Span = Span
  { spanStart :: Pos
  , spanEnd   :: Pos
  } deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

spanUnion :: Span -> Span -> Span
spanUnion (Span s1 e1) (Span s2 e2) =
  Span (min s1 s2) (max e1 e2)

voidSpan :: Span
voidSpan = Span voidPos voidPos

prevColumn :: Pos -> Pos
prevColumn (Pos line col) = Pos line (subPos col)
  where
  subPos p = max 1 (p - 1)

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

  startCol  = posCol start
  startLine = posLine start
  endCol    = posCol end
  endLine   = posLine end
