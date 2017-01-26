{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Template.Parser
  ( parseTemplate
  ) where

import           Control.Monad        (void)

import           Data.Text            (Text, pack, unpack)

import           Text.Parsec          hiding (Column)
import           Text.Parsec.Language (emptyDef)
import           Text.Parsec.String   (Parser)
import qualified Text.Parsec.Token    as P

import           Lib.Compiler.Parser  (expr)
import           Lib.Template.Types

lexer :: P.TokenParser ()
lexer = P.makeTokenParser emptyDef
  { P.reservedNames = ["for", "in", "endfor", "if", "else", "endif"]
  , P.identStart = letter
  , P.identLetter = alphaNum <|> oneOf "_"
  }

parseTemplate :: Text -> Either Text PTemplate
parseTemplate e =
  case parse (template <* eof) "" $ unpack e of
    Left msg -> Left $ pack $ show msg
    Right x -> Right x

template :: Parser PTemplate
template = PTemplate <$> many tpl

tpl :: Parser PTplExpr
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

tplFor :: Parser PTplExpr
tplFor = do
  (name, hexlExpr) <- instr "for" $ do
    name <- P.identifier lexer
    P.reserved lexer "in"
    hexlExpr <- expr
    pure (name, hexlExpr)
  body <- template
  instr "endfor" $ pure ()
  pure $ PTplFor (pack name) hexlExpr body

tplIf :: Parser PTplExpr
tplIf = do
  cond <- instr "if" expr
  thenTpl <- template
  instr "else" $ pure ()
  elseTpl <- template
  instr "endif" $ pure ()
  pure $ PTplIf cond thenTpl elseTpl

tplShow :: Parser PTplExpr
tplShow = PTplShow <$> (startShow *> expr <* endShow)

tplText :: Parser PTplExpr
tplText = text >>= \case
  ""  -> fail "empty TplText"
  txt -> pure $ PTplText txt

text :: Parser Text
text = pack <$> manyUntil anyChar (try (string "{{") <|> string "{%")

manyUntil :: Parser a -> Parser b -> Parser [a]
manyUntil p end = scan
  where scan =     try (lookAhead (void end <|> eof)) *> pure []
               <|> ((:) <$> p <*> scan)
