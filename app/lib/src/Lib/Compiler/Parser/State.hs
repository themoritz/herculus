{-# LANGUAGE NoImplicitPrelude #-}
-- |

module Lib.Compiler.Parser.State where

import           Lib.Prelude

data ParseState = ParseState
  { parserOperators   :: [OpSpec]
  , parserIndentation :: Word
  }

initialParseState :: ParseState
initialParseState = ParseState
  [ OpSpec (Infix AssocL 7) "*"
  , OpSpec (Infix AssocL 7) "/"
  , OpSpec (Infix AssocL 6) "+"
  , OpSpec (Infix AssocL 6) "-"
  , OpSpec (Infix AssocR 5) "<>"
  , OpSpec (Infix AssocL 4) "<="
  , OpSpec (Infix AssocL 4) ">="
  , OpSpec (Infix AssocL 4) "<"
  , OpSpec (Infix AssocL 4) ">"
  , OpSpec (Infix AssocN 4) "=="
  , OpSpec (Infix AssocN 4) "/="
  , OpSpec (Infix AssocR 3) "&&"
  , OpSpec (Infix AssocR 2) "||"
  ]
  0

data OpSpec = OpSpec
  { opFixity :: Fixity
  , opName   :: Text
  } deriving (Eq, Ord, Show)

data Fixity
  = Infix Assoc Int
  deriving (Eq, Ord, Show)

data Assoc
  = AssocL
  | AssocR
  | AssocN
  deriving (Eq, Ord, Show)
