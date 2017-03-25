{-# LANGUAGE NoImplicitPrelude #-}
-- |

module Lib.Compiler.Parser.State where

import           Lib.Prelude

data ParseState = ParseState
  { parserOperators :: [OpSpec]
  }

initialParseState :: ParseState
initialParseState = ParseState
  [
  ]

data OpSpec = OpSpec
  { opFixity :: Fixity
  , opName   :: Text
  }

data Fixity
  = Infix Assoc Int
  deriving (Eq, Ord)

data Assoc
  = AssocL
  | AssocR
  | AssocN
  deriving (Eq, Ord)
