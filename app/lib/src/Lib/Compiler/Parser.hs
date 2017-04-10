{-# LANGUAGE NoImplicitPrelude #-}

module Lib.Compiler.Parser
  ( parse
  , parseExpr
  , parseModule
  , parseFormula
  ) where

import           Lib.Prelude

import           Control.Comonad.Cofree

import           Data.Foldable              (foldl')

import qualified Text.Megaparsec            as P
import           Text.Megaparsec.Expr

import           Lib.Compiler.AST
import           Lib.Compiler.AST.Position
import           Lib.Compiler.Error
import           Lib.Compiler.Parser.Common
import           Lib.Compiler.Parser.Lexer
import           Lib.Compiler.Parser.State
import           Lib.Compiler.Type
import           Lib.Types

--------------------------------------------------------------------------------

parse :: Text -> Either Error [SourceAst]
parse e =
  flip evalState initialParseState $
  mapLeft convertParseError <$>
  P.runParserT (scn *> parseModule <* P.eof) "" e

--------------------------------------------------------------------------------

parseModule :: Parser [SourceAst]
parseModule = many (same *> parseDeclaration)

parseFormula :: Parser ([SourceAst], SourceAst)
parseFormula = (,)
  <$> many (same *> parseDeclaration)
  <*> (same *> parseExpr)

--------------------------------------------------------------------------------

parseDeclaration :: Parser SourceAst
parseDeclaration = P.choice
  [ P.try parseDataDecl
  , P.try parseTypeDecl
  , P.try parseValueDecl
  ]
  P.<?> "declaration"

parseDataDecl :: Parser SourceAst
parseDataDecl = withSource $ do
  reserved "data"
  name <- indented *> tyname
  tyArgs <- many (indented *> identifier)
  constructors <- P.option [] $ do
    indented *> equals
    let
      constructor = (,)
        <$> dconsname
        <*> many (indented *> (hoistCofree inj <$> parseType'))
    P.sepBy1 constructor pipe
  pure $ inj (DataDecl name tyArgs constructors)

parseTypeDecl :: Parser SourceAst
parseTypeDecl = withSource $ inj <$> (TypeDecl
  <$> (identifier <* doubleColon)
  <*> (map (hoistCofree inj) <$> parsePolyType)
                                         )

parseValueDecl :: Parser SourceAst
parseValueDecl = withSource $ inj <$> (ValueDecl
  <$> identifier
  <*> (many (hoistCofree inj <$> parseBinder) <* equals)
  <*> parseExpr
                                          )

--------------------------------------------------------------------------------

parseExpr :: Parser SourceAst
parseExpr = do
  opSpecs <- gets parserOperators
  makeExprParser terms (mkOpTable spanVar spanApp opSpecs)
  where
    terms = P.choice
      [ P.try parseApp
      , P.try parseExpr'
      ]
      P.<?> "expression"

parseExpr' :: Parser SourceAst
parseExpr' = P.choice
  [ P.try parseAccessor
  , P.try parseExpr''
  ]

parseExpr'' :: Parser SourceAst
parseExpr'' = P.choice
  [ P.try parseLit
  , P.try parseAbs
  , P.try parseConstructor
  , P.try parseVar
  , P.try parseCase
  , P.try parseIfThenElse
  , P.try parseLet
  , P.try parseColOfTblRef
  , P.try parseTblRef
  , P.try parseColRef
  , P.try (parens parseExpr)
  ]

parseApp :: Parser SourceAst
parseApp = do
  start <- parseExpr'
  args <- some (indented *> parseExpr')
  pure $ foldl' spanApp start args

parseAccessor :: Parser SourceAst
parseAccessor = do
  e <- parseExpr''
  refs <- some $ withSpan $ dot *> (Ref <$> identifier)
  pure $ foldl' spanAccessor e refs

parseConstructor :: Parser SourceAst
parseConstructor = withSource $ inj . Constructor <$> dconsname

parseCase :: Parser SourceAst
parseCase = withSource $ do
  reserved "case"
  scrut <- parseExpr
  indented *> reserved "of"
  alts <- indented *> mark (some (same *> parseAlternative))
  pure $ inj $ Case scrut alts
  where
  parseAlternative :: Parser (SourceAst, SourceAst)
  parseAlternative =
    (,) <$> (hoistCofree inj <$> parseBinder) <*> (indented *> rArrow *> parseExpr)
    P.<?> "case alternative"

parseLet :: Parser SourceAst
parseLet = withSource $ do
  reserved "let"
  indented
  bindings <- mark $ some $ do
    name <- same *> identifier
    args <- many parseBinder
    equals
    e <- parseExpr
    pure (name, args, e)
  indented
  reserved "in"
  body <- parseExpr
  let
    bindings' = flip map bindings $ \(name, args, e) ->
      (name, foldr spanAbs e (map (hoistCofree inj) args))
  pure $ inj $ Let bindings' body

parseAbs :: Parser SourceAst
parseAbs = do
  backslash
  binders <- some (indented *> parseBinder)
  indented *> rArrow
  body <- parseExpr
  pure $ foldr spanAbs body (map (hoistCofree inj) binders)

parseVar :: Parser SourceAst
parseVar = withSource $ inj . Var <$> identifier

parseLit :: Parser SourceAst
parseLit = withSource $ inj . Literal <$> P.choice
    [ P.try parseString
    , P.try parseNumber
    , P.try parseInteger
    , P.try parseRecord
    ]
  where
    parseString = StringLit <$> (lexeme stringLit)
    parseNumber = NumberLit <$> (lexeme numberLit)
    parseInteger = IntegerLit <$> (lexeme integerLit)
    parseRecord = braces $ do
      fields <- P.sepBy1 ((,) <$> identifier <* equals <*> parseExpr) comma
      pure $ RecordLit fields

parseIfThenElse :: Parser SourceAst
parseIfThenElse = do
  start <- P.getPosition
  cond <-              reserved "if"   *> indented *> parseExpr
  true@(tspan :< _) <-  indented *> reserved "then" *> indented *> parseExpr
  false@(fspan :< _) <- indented *> reserved "else" *> indented *> parseExpr
  end <- P.getPosition
  pure $ Span start end :< inj (Case cond
    [ (tspan :< inj (ConstructorBinder "True" []), true)
    , (fspan :< inj (ConstructorBinder "False" []), false)
    ])

parseTblRef :: Parser SourceAst
parseTblRef = withSource $ inj . TableRef . Ref <$> (hashSign *> identifier)

parseColRef :: Parser SourceAst
parseColRef = map (hoistCofree inj) $
  withSource $ ColumnRef . Ref <$> (dollarSign *> identifier)

parseColOfTblRef :: Parser SourceAst
parseColOfTblRef = map (hoistCofree inj) $ withSource $ do
  tbl <- hashSign *> identifier
  col <- dot *> identifier
  pure $ ColumnOfTableRef (Ref tbl) (Ref col)

--------------------------------------------------------------------------------

parseBinder :: Parser SourceBinder
parseBinder = P.choice
  [ withSource (VarBinder <$> P.try identifier)
  , withSource (ConstructorBinder <$> P.try dconsname <*> many parseBinder)
  , parens parseBinder
  ]
  P.<?> "binder"

--------------------------------------------------------------------------------

parseType :: Parser SourceType
parseType = do
  opSpecs <- gets parserTypeOperators
  makeExprParser terms (mkOpTable spanTypeConstructor spanTypeApp opSpecs)
  where
    terms = P.choice
      [ P.try parseTypeApp
      , P.try parseType'
      ]
      P.<?> "type"

parseTypeApp :: Parser SourceType
parseTypeApp = do
  start <- parseType'
  args <- some (indented *> parseType')
  pure $ foldl' spanTypeApp start args

parseType' :: Parser SourceType
parseType' = P.choice
  [ withSource (TypeVar <$> identifier)
  , withSource (TypeConstructor <$> tyname)
  , P.try $ parseRecordType
  , parens parseType
  ]

parseRecordType :: Parser SourceType
parseRecordType = parens $ do
  (span, rows) <- withSpan parseRows
  pure $ spanTypeApp (spanTypeConstructor (span, "Record")) rows
  where
  parseRows = do
    rows <- P.sepBy1 ((,) <$> identifier <* doubleColon <*> parseType) comma
    (endSpan, end) <- withSpan $ P.option RecordNil $ do
      _ :< t <- indented *> pipe *> indented *> parseType
      pure t
    pure $ foldr (\(ref, ty) rest -> spanRecordCons ref ty rest) (endSpan :< end) rows

parsePolyType :: Parser (PolyType SourceType)
parsePolyType = do
  (vars, preds) <- P.option ([], []) $ P.try $ do
    vs <- reserved "forall" *> some (indented *> identifier) <* indented <* dot
    ps <- many (P.try parseConstraint)
    pure (vs, ps)
  ty <- parseType
  pure $ ForAll vars preds ty

parseConstraint :: Parser (Constraint SourceType)
parseConstraint =
  (IsIn <$> (indented *> tyname) <*> parseType) <* indented <* rfatArrow
