module Lib.Compiler.Parser
  ( parseExpr
  , expr
  ) where

import           Control.Monad.Identity

import           Data.Foldable          (foldl')
import           Data.Functor           (($>))
import           Data.Monoid            ((<>))
import           Data.Text              (Text, pack, unpack)

import           Text.Read              (readMaybe)

import           Text.Parsec            hiding (Column)
import           Text.Parsec.Expr
import           Text.Parsec.Language   (emptyDef)
import           Text.Parsec.String     (Parser)
import qualified Text.Parsec.Token      as P

import           Lib.Compiler.Types
import           Lib.Types

--

lexer :: P.TokenParser ()
lexer = P.makeTokenParser emptyDef
  { P.reservedOpNames = ["+", "-", "*", "/", "==", "=", "<=", ">=", "<", ">", "&&", "||"]
  , P.reservedNames = ["if", "then", "else", "True", "False", "let"]
  , P.identStart = letter
  , P.identLetter = alphaNum <|> oneOf "_"
  }

binary :: String -> Operator String () Identity PExpr
binary name = Infix (P.reservedOp lexer name *> pure (\l r -> PApp (PApp (PVar $ pack name) l) r)) AssocLeft

expr :: Parser PExpr
expr = buildExpressionParser table terms
  where
    table =
      [ [ binary "*" ]
      , [ binary "+"
        , binary "-"
        ]
      , [ binary "<="
        , binary ">="
        , binary "<"
        , binary ">"
        , binary "=="
        , binary "!="
        ]
      , [ binary "&&" ]
      , [ binary "||" ]
      ]
    terms =
          try ifThenElse
      <|> try let'
      <|> try lam
      <|> try app
      <|> try aExpr
      <?> "expression"

aExpr :: Parser PExpr
aExpr =
      try prjRecord
  <|> try bExpr
  <?> "a-expression"

bExpr :: Parser PExpr
bExpr =
      try var
  <|> try colOfTblRef
  <|> try tblRef
  <|> try colRef
  <|> try lit
  <|> try (P.parens lexer expr)
  <?> "b-expression"

app :: Parser PExpr
app = do
  start <- aExpr
  args <- many1 aExpr
  pure $ foldl' PApp start args

prjRecord :: Parser PExpr
prjRecord = do
  e <- bExpr
  refs <- many1 $ char '.' *> ((Ref . pack) <$> P.identifier lexer)
  pure $ foldl' PPrjRecord e refs

let' :: Parser PExpr
let' = do
  _ <- P.reserved lexer "let"
  name <- P.identifier lexer
  args <- many $ P.identifier lexer
  _ <- P.reservedOp lexer "="
  e <- expr
  _ <- P.lexeme lexer (char ';')
  body <- expr
  pure $ PLet (pack name) (foldr PLam e (map pack args)) body

lam :: Parser PExpr
lam = do
  _ <- P.lexeme lexer $ char '\\'
  vars <- many1 $ P.identifier lexer
  _ <- P.lexeme lexer $ string "->"
  body <- expr
  pure $ foldr PLam body (map pack vars)

var :: Parser PExpr
var = PVar . pack <$> P.identifier lexer

lit :: Parser PExpr
lit = PLit <$> (stringLit <|> numberLit <|> boolLit)
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

ifThenElse :: Parser PExpr
ifThenElse = PIf
  <$> (P.reserved lexer "if"   *> expr)
  <*> (P.reserved lexer "then" *> expr)
  <*> (P.reserved lexer "else" *> expr)

tblRef :: Parser PExpr
tblRef = PTableRef . Ref . pack <$> (char '#' *> P.identifier lexer)

colRef :: Parser PExpr
colRef = PColumnRef . Ref . pack <$> (char '$' *> P.identifier lexer)

colOfTblRef :: Parser PExpr
colOfTblRef = do
  tbl <- char '#' *> P.identifier lexer
  col <- char '.' *> P.identifier lexer
  pure $ PColumnOfTableRef (Ref $ pack tbl) (Ref $ pack col)

parseExpr :: Text -> Either Text PExpr
parseExpr e =
  case parse (P.whiteSpace lexer *> expr <* eof) "" $ unpack e of
    Left msg -> Left $ pack $ show msg
    Right x -> Right x
