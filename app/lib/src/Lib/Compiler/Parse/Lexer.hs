{-# LANGUAGE NoImplicitPrelude #-}
-- |

module Lib.Compiler.Parse.Lexer where

import           Lib.Prelude

import qualified Data.Char                 as C
import qualified Data.Text                 as T

import qualified Text.Megaparsec           as P
import qualified Text.Megaparsec.Lexer     as L

import           Lib.Compiler.AST.Position
import           Lib.Compiler.Parse.State

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
lexeme p = do
  x <- p
  pos <- getPosition
  modify $ \st -> st { parserLastTokenEnd = prevColumn pos }
  scn
  pure x

text :: Text -> Parser ()
text = lexeme . text'

text' :: Text -> Parser ()
text' s = void $ P.string (T.unpack s)

--------------------------------------------------------------------------------

getPosition :: Parser Pos
getPosition = fromSourcePos <$> P.getPosition

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

colon :: Parser ()
colon = text ":"

doubleColon :: Parser ()
doubleColon = text "::"

dollarSign' :: Parser ()
dollarSign' = text' "$"

hashSign' :: Parser ()
hashSign' = text' "#"

equals :: Parser ()
equals = text "="

dot :: Parser ()
dot = text "."

dot' :: Parser ()
dot' = text' "."

comma :: Parser ()
comma = text ","

semicolon :: Parser ()
semicolon = text ";"

backslash :: Parser ()
backslash = text "\\"

pipe :: Parser ()
pipe = text "|"

reserved :: Text -> Parser ()
reserved s = P.try go P.<?> show s where
  go = do
    s' <- lname
    guard (s == s')

tyname :: Parser Text
tyname = P.try uname P.<?> "type name"

dconsname :: Parser Text
dconsname = P.try uname P.<?> "data constructor name"

identifier :: Parser Text
identifier = P.try go P.<?> "identifier" where
  go = do
    s <- lname
    guard (s `notElem` reservedNames)
    pure s

anySymbol :: Parser Text
anySymbol = lexeme (P.try go) P.<?> "symbol"
  where
    go = T.pack <$> some (P.satisfy isSymbolChar)
    isSymbolChar c = (c `elem` (":!#$%&*+./<=>?@\\^|-~" :: [Char])) ||
                     (not (C.isAscii c) && C.isSymbol c)

symbol :: Text -> Parser ()
symbol s = P.try $ do
  s' <- anySymbol
  guard (s == s')

stringLit :: Parser Text
stringLit = lexeme stringLit'

stringLit' :: Parser Text
stringLit' = P.try go P.<?> "string" where
  go = T.pack <$> (P.char '"' *> P.manyTill L.charLiteral (P.char '"'))

numberLit :: Parser Double
numberLit = lexeme L.float P.<?> "number"

integerLit :: Parser Integer
integerLit = lexeme L.integer P.<?> "integer"

reference :: Parser Text
reference = lexeme reference'

reference' :: Parser Text
reference' = P.choice
  [ stringLit'
  , uname'
  , lname'
  ] P.<?> "reference"

--------------------------------------------------------------------------------

lname :: Parser Text
lname = lexeme lname'

lname' :: Parser Text
lname' = T.cons <$> identStart <*> (T.pack <$> many identLetter)

identStart :: Parser Char
identStart = P.lowerChar <|> P.oneOf ("_" :: [Char])

identLetter :: Parser Char
identLetter = P.alphaNumChar <|> P.oneOf ("_'" :: [Char])

uname' :: Parser Text
uname' = T.cons <$> P.upperChar <*> (T.pack <$> many identLetter)

uname :: Parser Text
uname = lexeme uname'

reservedNames :: [Text]
reservedNames =
  [ "type"
  , "forall"
  , "import"
  , "infixl"
  , "infixr"
  , "infix"
  , "class"
  , "instance"
  , "derive"
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
