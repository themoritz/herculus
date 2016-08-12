{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}

module Lib.Compiler.Parser
  ( Expr (..)
  , Name
  , Lit (..)
  , Binop (..)
  , parseExpr
  ) where

import           Control.Monad.Identity

import           Data.Functor           (($>))
import           Data.Monoid            ((<>))
import           Data.Text              (Text, pack, unpack)

import           Text.Read              (readMaybe)

import           Text.Parsec            hiding (Column)
import           Text.Parsec.Expr
import           Text.Parsec.Language   (emptyDef)
import           Text.Parsec.String     (Parser)
import qualified Text.Parsec.Token      as P

import           Lib.Model.Column
import           Lib.Model.Types
import           Lib.Types

--

lexer :: P.TokenParser ()
lexer = P.makeTokenParser emptyDef
  { P.reservedOpNames = ["+", "-", "*", "/", "==", "="]
  , P.reservedNames = ["if", "then", "else", "True", "False", "let", "in"]
  , P.identStart = letter
  , P.identLetter = alphaNum <|> oneOf "_"
  }

binary :: String -> Binop -> Operator String () Identity (Expr Ref)
binary name op = Infix (P.reservedOp lexer name *> pure (Binop op)) AssocLeft

expr :: Parser (Expr Ref)
expr = buildExpressionParser table terms
  where
    table =
      [ [ binary "*" Mul ]
      , [ binary "+" Add, binary "-" Sub ]
      ]
    terms =
          try app
      <|> try let'
      <|> try lam
      <|> try ifThenElse
      <|> try prjRecord
      <|> try aExpr
      <?> "expression"

aExpr :: Parser (Expr Ref)
aExpr =
      try var
  <|> try tblRef
  <|> try colOfTblRef
  <|> try colRef
  <|> try lit
  <|> try (P.parens lexer expr)
  <?> "atomic expression"

app :: Parser (Expr Ref)
app = mkAppChain =<< many1 aExpr
  where mkAppChain exprs =
          let (h:t) = reverse exprs
          in case go t of
            Just ex -> pure $ App ex h
            Nothing -> fail "application"
        go [] = Nothing
        go (h:t) = case go t of
          Just ex -> Just $ App ex h
          Nothing -> Just h

let' :: Parser (Expr Ref)
let' = Let
  <$> (P.reserved lexer "let" *> P.identifier lexer)
  <*> (P.reservedOp lexer "=" *> expr <* P.lexeme lexer (char ';'))
  <*> expr

lam :: Parser (Expr Ref)
lam = Lam
  <$> (char '\\' *> P.identifier lexer)
  <*> (P.lexeme lexer (string "->") *> expr)

var :: Parser (Expr Ref)
var = Var <$> P.identifier lexer

lit :: Parser (Expr Ref)
lit = Lit <$> (stringLit <|> numberLit <|> boolLit)
  where
    stringLit = P.lexeme lexer $ LString . pack <$>
                  P.lexeme lexer (char '"' *> many (noneOf "\"") <* char '"')
    numberLit = P.lexeme lexer $ do
      pref <- many $ oneOf "+-"
      raw <- many $ oneOf "0123456789."
      case readMaybe (pref <> raw) of
        Nothing -> fail "expected number"
        Just dec -> pure $ LNumber $ Number dec
    boolLit =
          try (P.reserved lexer "True"  $> LBool True)
      <|> try (P.reserved lexer "False" $> LBool False)
      <?> "True or False"

ifThenElse :: Parser (Expr Ref)
ifThenElse = If
  <$> (P.reserved lexer "if"   *> expr)
  <*> (P.reserved lexer "then" *> expr)
  <*> (P.reserved lexer "else" *> expr)

prjRecord :: Parser (Expr Ref)
prjRecord = PrjRecord
  <$> aExpr
  <*> (char '.' *> ((Ref . pack) <$> P.identifier lexer))

tblRef :: Parser (Expr Ref)
tblRef = TableRef . Ref . pack <$> (char '#' *> P.identifier lexer)

colRef :: Parser (Expr Ref)
colRef = ColumnRef . Ref . pack <$> (char '$' *> P.identifier lexer)

colOfTblRef :: Parser (Expr Ref)
colOfTblRef = do
  char '$'
  tbl <- P.identifier lexer
  char '.'
  col <- P.identifier lexer
  pure $ ColumnOfTableRef (Ref $ pack tbl) (Ref $ pack col)

parseExpr :: Text -> Either Text (Expr Ref)
parseExpr e =
  case parse (P.whiteSpace lexer *> expr <* eof) "" $ unpack e of
    Left msg -> Left $ pack $ show msg
    Right x -> Right x
