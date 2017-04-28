{-# LANGUAGE NoImplicitPrelude #-}
-- |

module Lib.Compiler.Parse.State where

import           Lib.Prelude

import           Lib.Compiler.AST.Position

data ParseState = ParseState
  { parserOperators     :: OpTable
  , parserTypeOperators :: OpTable
  , parserIndentation   :: Word
  , parserLastTokenEnd  :: Pos
  }

type OpTable = [(Text, Fixity)]

testOpTable :: OpTable
testOpTable =
  [ ("*" , Infix AssocL 7)
  , ("/" , Infix AssocL 7)
  , ("+" , Infix AssocL 6)
  , ("-" , Infix AssocL 6)
  , ("<>", Infix AssocR 5)
  , ("<=", Infix AssocL 4)
  , (">=", Infix AssocL 4)
  , ("<" , Infix AssocL 4)
  , (">" , Infix AssocL 4)
  , ("==", Infix AssocN 4)
  , ("/=", Infix AssocN 4)
  , ("&&", Infix AssocR 3)
  , ("||", Infix AssocR 2)
  ]

initialParseState :: OpTable -> ParseState
initialParseState opTable = ParseState
  opTable
  [ ("->", Infix AssocR 2) ]
  1
  voidPos

addOpSpec :: Text -> Fixity -> ParseState -> ParseState
addOpSpec name fixity st =
  st { parserOperators = (name, fixity) : parserOperators st }

data Fixity
  = Infix Assoc Int
  deriving (Eq, Ord, Show)

data Assoc
  = AssocL
  | AssocR
  | AssocN
  deriving (Eq, Ord, Show)
