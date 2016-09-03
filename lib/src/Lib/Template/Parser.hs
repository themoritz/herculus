{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Template.Parser
  ( parseTemplate
  ) where

import           Control.Applicative  (empty)
import           Control.Monad        (void)

import           Data.Text            (Text, pack, unpack)
import           Data.Text            as Text (null)

import           Text.Parsec          hiding (Column)
import           Text.Parsec.Expr
import           Text.Parsec.Language (emptyDef)
import           Text.Parsec.String   (Parser)
import qualified Text.Parsec.Token    as P

import           Lib.Compiler.Parser  (expr)
import           Lib.Model.Column

type Template = [TplExpr]

data TplExpr
  = TplText Text
  | TplFor Text PExpr Template
  | TplIf PExpr Template Template
  | TplShow PExpr
  deriving (Show)

lexer :: P.TokenParser ()
lexer = P.makeTokenParser emptyDef
  { P.reservedNames = ["for", "in", "endfor", "if", "else", "endif"]
  , P.identStart = letter
  , P.identLetter = alphaNum <|> oneOf "_"
  }

parseTemplate :: Text -> Either Text Template
parseTemplate e =
  case parse (template <* eof) "" $ unpack e of
    Left msg -> Left $ pack $ show msg
    Right x -> Right x

template :: Parser Template
template = many tpl

tpl :: Parser TplExpr
tpl =
      try tplFor
  <|> try tplIf
  <|> try tplShow
  <|> try tplText
  <?> "template"

startInstr :: Parser ()
startInstr = P.lexeme lexer $ () <$ string "{%"

endInstr :: Parser ()
endInstr = () <$ string "%}"

instr :: Text -> Parser a -> Parser a
instr i p = startInstr *> P.reserved lexer (unpack i) *> p <* endInstr

startShow :: Parser ()
startShow = P.lexeme lexer $ () <$ string "{{"

endShow :: Parser ()
endShow = () <$ string "}}"

tplFor :: Parser TplExpr
tplFor = do
  (name, hexlExpr) <- instr "for" $ do
    name <- P.identifier lexer
    P.reserved lexer "in"
    hexlExpr <- expr
    pure (name, hexlExpr)
  body <- template
  instr "endfor" $ pure ()
  pure $ TplFor (pack name) hexlExpr body

tplIf :: Parser TplExpr
tplIf = do
  cond <- instr "if" expr
  thenTpl <- template
  instr "else" $ pure ()
  elseTpl <- template
  instr "endif" $ pure ()
  pure $ TplIf cond thenTpl elseTpl

tplShow :: Parser TplExpr
tplShow = TplShow <$> (startShow *> expr <* endShow)

tplText :: Parser TplExpr
tplText = text >>= \case
  ""  -> fail "empty TplText"
  txt -> pure $ TplText txt

text :: Parser Text
text = pack <$> manyUntil anyChar (try (string "{{") <|> string "{%")

manyUntil :: Parser a -> Parser b -> Parser [a]
manyUntil p end = scan
  where scan =     try (lookAhead (void end <|> eof)) *> pure []
               <|> ((:) <$> p <*> scan)
