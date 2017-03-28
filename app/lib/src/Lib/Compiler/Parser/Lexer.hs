{-# LANGUAGE NoImplicitPrelude #-}
-- |

module Lib.Compiler.Parser.Lexer where

import           Lib.Prelude

import qualified Data.Char                 as C
import qualified Data.Text                 as T

import qualified Text.Megaparsec           as P
import qualified Text.Megaparsec.Lexer     as L

import           Lib.Compiler.Parser.State

type Parser = P.ParsecT P.Dec Text (State ParseState)

testParser :: Show a => Parser a -> Text -> IO ()
testParser p s =
  case flip evalState initialParseState $ P.runParserT (p <* P.eof) "" s of
    Left msg -> putStrLn $ P.parseErrorPretty msg
    Right a  -> print a

--------------------------------------------------------------------------------

spaceConsumer :: Parser ()
spaceConsumer = L.space
  (void $ P.oneOf (" \t" :: [Char]))
  (L.skipLineComment "-- ")
  (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme spaceConsumer

match :: Text -> Parser Text
match s = T.pack <$> L.symbol spaceConsumer (T.unpack s)

--------------------------------------------------------------------------------

lparen :: Parser ()
lparen = void $ match "("

rparen :: Parser ()
rparen = void $ match ")"

parens :: Parser a -> Parser a
parens = P.between lparen rparen

lbrace :: Parser ()
lbrace = void $ match "{"

rbrace :: Parser ()
rbrace = void $ match "}"

braces :: Parser a -> Parser a
braces = P.between lbrace rbrace

rArrow :: Parser ()
rArrow = void $ match "->"

rfatArrow :: Parser ()
rfatArrow = void $ match "=>"

doubleColon :: Parser ()
doubleColon = void $ match "::"

dollarSign :: Parser ()
dollarSign = void $ match "$"

hashSign :: Parser ()
hashSign = void $ match "#"

equals :: Parser ()
equals = void $ match "="

dot :: Parser ()
dot = void $ match "."

comma :: Parser ()
comma = void $ match ","

semicolon :: Parser ()
semicolon = void $ match ";"

backslash :: Parser ()
backslash = void $ match "\\"

reserved :: Text -> Parser ()
reserved s = go P.<?> show s where
  go = do
    s' <- lname
    guard (s == s')

tyname :: Parser Text
tyname = uname P.<?> "type name"

dconsname :: Parser Text
dconsname = uname P.<?> "data constructor name"

dconsname' :: Text -> Parser ()
dconsname' c = do
  c' <- dconsname
  guard (c == c')

identifier :: Parser Text
identifier = go P.<?> "identifier" where
  go = do
    s <- lname
    guard (s `notElem` reservedNames)
    pure s

anySymbol :: Parser Text
anySymbol = lexeme (T.pack <$> some (P.satisfy isSymbolChar) P.<?> "symbol")
  where
    isSymbolChar c = (c `elem` (":!#$%&*+./<=>?@\\^|-~" :: [Char])) ||
                     (not (C.isAscii c) && C.isSymbol c)

symbol :: Text -> Parser ()
symbol s = do
  s' <- anySymbol
  guard (s == s')

stringLit :: Parser Text
stringLit = go P.<?> "string" where
  go = T.pack <$> (P.char '"' *> P.manyTill L.charLiteral (P.char '"'))

numberLit :: Parser Double
numberLit = L.float P.<?> "number"

integerLit :: Parser Integer
integerLit = L.integer P.<?> "integer"

--------------------------------------------------------------------------------

lname :: Parser Text
lname = lexeme (T.cons <$> identStart <*> (T.pack <$> many identLetter))

identStart :: Parser Char
identStart = P.lowerChar <|> P.oneOf ("_" :: [Char])

identLetter :: Parser Char
identLetter = P.alphaNumChar <|> P.oneOf ("_'" :: [Char])

uname :: Parser Text
uname = lexeme (T.cons <$> P.upperChar <*> (T.pack <$> many identLetter))

reservedNames :: [Text]
reservedNames =
  [ "data"
  , "newtype"
  , "type"
  , "forall"
  , "foreign"
  , "import"
  , "infixl"
  , "infixr"
  , "infix"
  , "class"
  , "instance"
  , "derive"
  , "module"
  , "case"
  , "of"
  , "if"
  , "then"
  , "else"
  , "do"
  , "let"
  , "in"
  , "where"
  ]
