{-# LANGUAGE DeriveGeneric #-}

module Lib.Expression
  ( BinOp (..)
  , Expr (..)
  , parseExpression
  ) where

import Control.Monad.Identity

import Data.Aeson
import Data.Text (Text, pack, unpack)

import Lib.Types
import Lib.Model.Types

import GHC.Generics hiding (Infix)

import           Text.Parsec hiding (Column)
import           Text.Parsec.Expr
import           Text.Parsec.Language   (emptyDef)
import           Text.Parsec.String     (Parser)
import qualified Text.Parsec.Token      as P

data BinOp
  = Append
  deriving (Show, Generic)

instance ToJSON BinOp
instance FromJSON BinOp

data Expr
  = ExprColumnRef (Ref Column)
  | ExprBinOp BinOp Expr Expr
  | ExprStringLit Text
  deriving (Show, Generic)

instance ToJSON Expr
instance FromJSON Expr

-- Parser

lexer :: P.TokenParser ()
lexer = P.makeTokenParser emptyDef
  { P.reservedOpNames = ["<>"]
  , P.identStart = letter
  , P.identLetter = alphaNum <|> oneOf "_."
  }

colRef :: Parser Expr
colRef = ExprColumnRef . Ref . pack <$> (char '$' *> P.identifier lexer)

stringLit :: Parser Expr
stringLit = ExprStringLit . pack <$>
  P.lexeme lexer (char '"' *> many (noneOf "\"") <* char '"')

binary :: String -> BinOp -> Operator String () Identity Expr
binary name op = Infix (P.reservedOp lexer name *> pure (ExprBinOp op)) AssocLeft

expression :: Parser Expr
expression = buildExpressionParser table terms
  where
    table =
      [ [ binary "<>" Append ]
      ]
    terms = P.parens lexer expression
      <|> colRef
      <|> stringLit
      <?> "term"

parseExpression :: Text -> Either ParseError Expr
parseExpression = parse (P.whiteSpace lexer *> expression <* eof) "" . unpack
