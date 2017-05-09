{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
-- |

module Lib.Compiler.Parse.State where

import           Lib.Prelude

import           Control.Lens

import           Lib.Compiler.AST.Position

type OpTable = [(Text, Fixity)]

data Fixity
  = Infix Assoc Int
  deriving (Eq, Ord, Show)

data Assoc
  = AssocL
  | AssocR
  | AssocN
  deriving (Eq, Ord, Show)

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

data ParseState = ParseState
  { _parserOperators     :: OpTable
  , _parserTypeOperators :: OpTable
  , _parserIndentation   :: Word
  , _parserLastTokenEnd  :: Pos
  , _parserLastDocString :: Text
  }

makeLenses ''ParseState

initialParseState :: OpTable -> ParseState
initialParseState opTable = ParseState
  opTable
  [ ("->", Infix AssocR 2) ]
  1
  voidPos
  ""

addOpSpec :: MonadState ParseState m => Text -> Fixity -> m ()
addOpSpec name fixity =
  parserOperators %= (:) (name, fixity)
