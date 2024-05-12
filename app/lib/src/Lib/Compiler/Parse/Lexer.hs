-- |

module Lib.Compiler.Parse.Lexer where

import           Lib.Prelude

import           Control.Lens

import qualified Data.Char                  as C
import qualified Data.Text                  as T

import qualified Text.Megaparsec            as P
import qualified Text.Megaparsec.Char       as P
import qualified Text.Megaparsec.Char.Lexer as L

import           Lib.Compiler.AST.Position
import           Lib.Compiler.Parse.State

type Parser = P.ParsecT Void Text (State ParseState)

testParser :: Show a => Parser a -> Text -> IO ()
testParser p s =
  let
    st = initialParseState testOpTable
  in
    case evalState (P.runParserT (p <* P.eof) "" s) st of
      Left msg -> putStrLn $ P.errorBundlePretty msg
      Right a  -> print a

--------------------------------------------------------------------------------

whitespace :: Parser ()
whitespace = P.skipMany (P.satisfy C.isSpace)

comment :: Parser Text
comment = blockComment <|> lineComment
  where
  blockComment :: Parser Text
  blockComment = P.try $ do
    text' "{-"
    T.pack <$> P.manyTill P.anySingle (P.try (text' "-}"))

  lineComment :: Parser Text
  lineComment = P.try $ do
    text' "--"
    T.pack <$> P.manyTill P.anySingle (P.try (void (P.single '\n') <|> P.eof))

-- Skips whitespace and collects docstrings
scn :: Parser ()
scn = do
  whitespace
  dss <- map join $ many $ do
    (T.stripPrefix " | " -> mDocString) <- comment
    whitespace
    pure $ case mDocString of
      Nothing        -> []
      Just docString -> [docString]
  when (length dss > 0) $ parserLastDocString .= (T.intercalate "\n" dss)

lexeme :: Parser a -> Parser a
lexeme p = do
  x <- p
  pos <- getPosition
  parserLastTokenEnd .= prevColumn pos
  scn
  pure x

text :: Text -> Parser ()
text = lexeme . text'

text' :: Text -> Parser ()
text' s = void $ P.string s

--------------------------------------------------------------------------------

getPosition :: Parser Pos
getPosition = fromSourcePos <$> P.getSourcePos

mark :: Parser a -> Parser a
mark p = do
  current <- use parserIndentation
  pos <- P.unPos . P.sourceColumn <$> P.getSourcePos
  parserIndentation .= pos
  a <- p
  parserIndentation .= current
  pure a

checkIndentation :: (Int -> Text) -> (Int -> Int -> Bool) -> Parser ()
checkIndentation mkMsg rel = do
  pos <- P.unPos . P.sourceColumn <$> P.getSourcePos
  current <- use parserIndentation
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

underscore :: Parser ()
underscore = text "_"

reserved :: Text -> Parser ()
reserved s = P.try go P.<?> show s where
  go = do
    s' <- lname
    guard (s == s') P.<?> T.unpack s

tyname :: Parser Text
tyname = P.try uname P.<?> "type name"

dconsname :: Parser Text
dconsname = P.try uname P.<?> "data constructor name"

identifier :: Parser Text
identifier = P.try go P.<?> "identifier" where
  go = do
    s <- lname
    guard (s `notElem` reservedNames)
      P.<?> "none of the reserved names in " <> show reservedNames
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
  guard (s == s') P.<?> T.unpack s

stringLit :: Parser Text
stringLit = lexeme stringLit'

stringLit' :: Parser Text
stringLit' = P.try go P.<?> "string" where
  go = T.pack <$> (P.char '"' *> P.manyTill L.charLiteral (P.char '"'))

numberLit :: Parser Double
numberLit = lexeme L.float P.<?> "number"

integerLit :: Parser Integer
integerLit = lexeme L.decimal P.<?> "integer"

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
  , "interface"
  , "implements"
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
