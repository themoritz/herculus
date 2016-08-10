module Lib.Compiler.Parser
  ( Expr (..)
  , Name
  , Lit (..)
  , Binop (..)
  , parseExpr
  ) where

import           Control.Monad.Identity

import           Data.Monoid            ((<>))
import           Data.Text              (Text, pack, unpack)
import           Data.Functor           (($>))

import           Text.Read              (readMaybe)

import           Text.Parsec
import           Text.Parsec.Expr
import           Text.Parsec.Language   (emptyDef)
import           Text.Parsec.String     (Parser)
import qualified Text.Parsec.Token      as P

--

type Name = String

data Expr
  = Lam Name Expr
  | App Expr Expr
  | Let Name Expr Expr
  -- | Fix Expr
  | If Expr Expr Expr
  | Var Name
  | Lit Lit
  | Binop Binop Expr Expr
  deriving (Eq, Show)

data Lit
  = LInt Int
  | LBool Bool
  | LString Text
  deriving (Show, Eq, Ord)

data Binop = Add | Sub | Mul | Eql
  deriving (Eq, Ord, Show)

--

lexer :: P.TokenParser ()
lexer = P.makeTokenParser emptyDef
  { P.reservedOpNames = ["+", "-", "*", "/", "==", "="]
  , P.reservedNames = ["if", "then", "else", "True", "False", "let", "in"]
  , P.identStart = letter
  , P.identLetter = alphaNum <|> oneOf "_"
  }

binary :: String -> Binop -> Operator String () Identity Expr
binary name op = Infix (P.reservedOp lexer name *> pure (Binop op)) AssocLeft

expr :: Parser Expr
expr = buildExpressionParser table terms
  where
    table =
      [ [ binary "*" Mul ]
      , [ binary "+" Add, binary "-" Sub ]
      , [ binary "==" Eql ]
      ]
    terms =
          try app
      <|> try let'
      <|> try lam
      <|> try ifThenElse
      <|> try var
      <|> try aExpr
      <?> "expression"

aExpr :: Parser Expr
aExpr =
      try var
  <|> try lit
  <|> try (P.parens lexer expr)
  <?> "atomic expression"

app :: Parser Expr
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

let' :: Parser Expr
let' = Let
  <$> (P.reserved lexer "let" *> P.identifier lexer)
  <*> (P.reservedOp lexer "=" *> expr <* P.lexeme lexer (char ';'))
  <*> expr

lam :: Parser Expr
lam = Lam
  <$> (char '\\' *> P.identifier lexer)
  <*> (P.lexeme lexer (string "->") *> expr)

var :: Parser Expr
var = Var <$> P.identifier lexer

lit :: Parser Expr
lit = Lit <$> (stringLit <|> numberLit <|> boolLit)
  where
    stringLit = P.lexeme lexer $ LString . pack <$>
                  P.lexeme lexer (char '"' *> many (noneOf "\"") <* char '"')
    numberLit = P.lexeme lexer $ do
      pref <- many $ oneOf "+-"
      raw <- many $ oneOf "0123456789."
      case readMaybe (pref <> raw) of
        Nothing -> fail "expected number"
        Just dec -> pure $ LInt dec
    boolLit =
          try (P.reserved lexer "True"  $> LBool True)
      <|> try (P.reserved lexer "False" $> LBool False)
      <?> "True or False"

ifThenElse :: Parser Expr
ifThenElse = If
  <$> (P.reserved lexer "if"   *> expr)
  <*> (P.reserved lexer "then" *> expr)
  <*> (P.reserved lexer "else" *> expr)

parseExpr :: Text -> Either Text Expr
parseExpr e =
  case parse (P.whiteSpace lexer *> expr <* eof) "" $ unpack e of
    Left msg -> Left $ pack $ show msg
    Right x -> Right x
