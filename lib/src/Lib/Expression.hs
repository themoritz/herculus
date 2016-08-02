{-# LANGUAGE DeriveGeneric              #-}

module Lib.Expression
  ( Expr (..)
  , BinOp (..)
  , UnOp (..)
  , parseExpression
  ) where

import           Control.Monad.Identity

import           Data.Aeson             (FromJSON (..), ToJSON (..))
import           Data.Monoid            ((<>))
import           Data.Text              (Text, pack, unpack)

import           Text.Read              (readMaybe)

import           Lib.Model.Dependencies
import           Lib.Model.Types
import           Lib.Types

import           GHC.Generics           hiding (Infix, Prefix)

import           Text.Parsec            hiding (Column, Ok)
import           Text.Parsec.Expr
import           Text.Parsec.Language   (emptyDef)
import           Text.Parsec.String     (Parser)
import qualified Text.Parsec.Token      as P

import           Lib.Model.Column

data UnOp
  = Sum
  deriving (Show, Generic)

instance ToJSON UnOp
instance FromJSON UnOp

data BinOp
  = Append
  | Add
  deriving (Show, Generic)

instance ToJSON BinOp
instance FromJSON BinOp


data Expr
  = ExprColumnRef (Ref Column)
  | ExprWholeColumnRef (Ref Table) (Ref Column)
  | ExprBinOp BinOp Expr Expr
  | ExprUnOp UnOp Expr
  | ExprStringLit Text
  | ExprNumberLit Number
  deriving (Show, Generic)

instance ToJSON Expr
instance FromJSON Expr

-- Parser

lexer :: P.TokenParser ()
lexer = P.makeTokenParser emptyDef
  { P.reservedOpNames = ["<>", "whole"]
  , P.identStart = letter
  , P.identLetter = alphaNum <|> oneOf "_"
  }

colRef :: Parser Expr
colRef = ExprColumnRef . Ref . pack <$> (char '$' *> P.identifier lexer)

wholeColRef :: Parser Expr
wholeColRef = do
  char '$'
  tbl <- P.identifier lexer
  char '.'
  col <- P.identifier lexer
  pure $ ExprWholeColumnRef (Ref $ pack tbl) (Ref $ pack col)

stringLit :: Parser Expr
stringLit = ExprStringLit . pack <$>
  P.lexeme lexer (char '"' *> many (noneOf "\"") <* char '"')

numberLit :: Parser Expr
numberLit = P.lexeme lexer $ do
  raw <- many $ oneOf "0123456789-+."
  case readMaybe raw of
    Nothing -> fail "expected decimal number"
    Just dec -> pure $ ExprNumberLit $ Number dec

binary :: String -> BinOp -> Operator String () Identity Expr
binary name op = Infix (P.reservedOp lexer name *> pure (ExprBinOp op)) AssocLeft

unary :: String -> UnOp -> Operator String () Identity Expr
unary name op = Prefix (P.reservedOp lexer name *> pure (ExprUnOp op))

expression :: Parser Expr
expression = buildExpressionParser table terms
  where
    table =
      [ [ unary "sum" Sum ]
      , [ binary "<>" Append, binary "+" Add ]
      ]
    terms = P.parens lexer expression
      <|> try wholeColRef
      <|> try colRef
      <|> try numberLit
      <|> try stringLit
      <?> "term"

parseExpression :: Text -> Either Text Expr
parseExpression e =
  case parse (P.whiteSpace lexer *> expression <* eof) "" $ unpack e of
    Left msg -> Left $ pack $ show msg
    Right x -> Right x
