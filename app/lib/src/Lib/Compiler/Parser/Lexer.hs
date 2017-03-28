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
  case evalState (P.runParserT (p <* P.eof) "" s) initialParseState of
    Left msg -> putStrLn $ P.parseErrorPretty msg
    Right a  -> print a

--------------------------------------------------------------------------------

scn :: Parser ()
scn = L.space
  (void P.spaceChar)
  (L.skipLineComment "-- ")
  (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme p = p <* scn

text :: Text -> Parser ()
text s = void $ lexeme $ P.string (T.unpack s)

--------------------------------------------------------------------------------

mark :: Parser a -> Parser a
mark p = do
  current <- gets parserIndentation
  pos <- P.unPos . P.sourceColumn <$> P.getPosition
  modify $ \st -> st { parserIndentation = pos }
  a <- p
  modify $ \st -> st { parserIndentation = current }
  pure a

checkIndentation :: (Word -> Text) -> (Word -> Word -> Bool) -> Parser ()
checkIndentation mkMsg rel = do
  pos <- P.unPos . P.sourceColumn <$> P.getPosition
  current <- gets parserIndentation
  guard (pos `rel` current) P.<?> T.unpack (mkMsg current)

indented :: Parser ()
indented = checkIndentation (\c -> "indentation past column " <> show c) (>)

same :: Parser ()
same = checkIndentation (\c -> "indentation at column " <> show c) (==)

--------------------------------------------------------------------------------

lparen :: Parser ()
lparen = text "("

rparen :: Parser ()
rparen = text ")"

parens :: Parser a -> Parser a
parens = P.between lparen rparen

lbrace :: Parser ()
lbrace = text "{"

rbrace :: Parser ()
rbrace = text "}"

braces :: Parser a -> Parser a
braces = P.between lbrace rbrace

rArrow :: Parser ()
rArrow = text "->"

rfatArrow :: Parser ()
rfatArrow = text "=>"

doubleColon :: Parser ()
doubleColon = text "::"

dollarSign :: Parser ()
dollarSign = text "$"

hashSign :: Parser ()
hashSign = text "#"

equals :: Parser ()
equals = text "="

dot :: Parser ()
dot = text "."

comma :: Parser ()
comma = text ","

semicolon :: Parser ()
semicolon = text ";"

backslash :: Parser ()
backslash = text "\\"

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
anySymbol = lexeme go P.<?> "symbol"
  where
    go = T.pack <$> some (P.satisfy isSymbolChar)
    isSymbolChar c = (c `elem` (":!#$%&*+./<=>?@\\^|-~" :: [Char])) ||
                     (not (C.isAscii c) && C.isSymbol c)

symbol :: Text -> Parser ()
symbol s = do
  s' <- anySymbol
  guard (s == s')

stringLit :: Parser Text
stringLit = lexeme go P.<?> "string" where
  go = T.pack <$> (P.char '"' *> P.manyTill L.charLiteral (P.char '"'))

numberLit :: Parser Double
numberLit = lexeme L.float P.<?> "number"

integerLit :: Parser Integer
integerLit = lexeme L.integer P.<?> "integer"

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
