{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Template.Parse
  ( parseTemplate
  ) where

import           Lib.Prelude

import           Control.Comonad.Cofree
import           Control.Monad             (void)

import qualified Data.List.NonEmpty        as NE
import           Data.Text                 (pack)

import qualified Text.Megaparsec           as P

import           Lib.Compiler.AST
import           Lib.Compiler.Parse        (parseBinder, parseExpr)
import           Lib.Compiler.Parse.Common
import           Lib.Compiler.Parse.Lexer
import           Lib.Template.AST
import           Lib.Template.Parse.Lexer

parseTemplate :: Parser [SourceTplChunk]
parseTemplate = many parseChunk

parseChunk :: Parser SourceTplChunk
parseChunk = P.choice
  [ P.try parseFor
  , P.try parseIf
  , P.try parsePrint
  , P.try parseText
  ] P.<?> "template chunk"

instr :: Text -> Parser a -> Parser a
instr i p = startInstr *> reserved i *> p <* endInstr

parseFor :: Parser SourceTplChunk
parseFor = withSource $ do
  (binder, hexlExpr) <- instr "for" $ do
    binder <- parseBinder
    reserved "in"
    hexlExpr <- parseExpr
    pure (binder, hexlExpr)
  body <- parseTemplate
  instr "endfor" $ pure ()
  pure $ inj $ TplFor (hoistCofree inj binder) hexlExpr body

parseIf :: Parser SourceTplChunk
parseIf = withSource $ do
  cond <- instr "if" parseExpr
  thenTpl <- parseTemplate
  instr "else" $ pure ()
  elseTpl <- parseTemplate
  instr "endif" $ pure ()
  pure $ inj $ TplIf cond thenTpl elseTpl

parsePrint :: Parser SourceTplChunk
parsePrint = withSource $
  inj . TplPrint <$> (startPrint *> parseExpr <* endPrint)

parseText :: Parser SourceTplChunk
parseText = withSource $ do
  t <- manyUntil P.anyChar (P.try (text' "{{") <|> text' "{%")
  case t of
    "" -> P.unexpected $ P.Label $ NE.fromList "empty TplText"
    _  -> pure $ inj (TplText (pack t) :: TplChunkF SourceAst SourceTplChunk)

manyUntil :: Parser a -> Parser b -> Parser [a]
manyUntil p end = scan
  where scan =     P.try (P.lookAhead (void end <|> P.eof)) *> pure []
               <|> ((:) <$> p <*> scan)
