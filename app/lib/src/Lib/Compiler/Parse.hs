{-# LANGUAGE NoImplicitPrelude #-}

module Lib.Compiler.Parse
  ( parse
  , parseExpr
  , parseBinder
  , parseModule
  , parseFormula
  , Parser
  ) where

import           Lib.Prelude

import           Control.Comonad.Cofree

import           Data.Foldable             (foldl')
import qualified Data.Map                  as Map

import qualified Text.Megaparsec           as P
import           Text.Megaparsec.Expr

import           Lib.Compiler.AST
import           Lib.Compiler.AST.Position
import           Lib.Compiler.Error
import           Lib.Compiler.Parse.Common
import           Lib.Compiler.Parse.Lexer
import           Lib.Compiler.Parse.State
import           Lib.Compiler.Type
import           Lib.Types

--------------------------------------------------------------------------------

parse :: Text -> OpTable -> Parser a -> Either Error a
parse e ops p =
  flip evalState (initialParseState ops) $
  mapLeft convertParseError <$>
  P.runParserT (scn *> p <* P.eof) "" e

--------------------------------------------------------------------------------

parseModule :: Parser Module
parseModule = many (same *> parseDeclaration)

parseFormula :: Parser Formula
parseFormula = (,)
  <$> many (same *> parseDeclaration)
  <*> (same *> parseExpr)

--------------------------------------------------------------------------------

parseDeclaration :: Parser SourceAst
parseDeclaration = P.choice
  [ parseDataDecl
  , parseClassDecl
  , parseInstanceDecl
  , parseFixityDecl
  , parseTypeDecl
  , parseValueDecl
  ]
  P.<?> "declaration"

parseDataDecl :: Parser SourceAst
parseDataDecl = withSource $ do
  P.try $ reserved "type"
  name <- indented *> tyname
  tyArgs <- many (indented *> identifier)
  constructors <- P.option [] $ do
    indented *> equals
    let
      constr = (,)
        <$> dconsname
        <*> many (indented *> (hoistCofree inj <$> parseType'))
    P.sepBy1 constr pipe
  pure $ inj (DataDecl name tyArgs constructors)

parseClassDecl :: Parser SourceAst
parseClassDecl = withSource $ do
  P.try $ reserved "class"
  supers <- many (P.try parseSuper)
  cls <- tyname
  param <- identifier
  reserved "where"
  indented
  decls <- mark $ many (same *> parseTypeDecl)
  pure $ inj $ ClassDecl (cls, param) supers decls
  where
  parseSuper = (,) <$> (indented *> tyname) <*> identifier <* rfatArrow

parseInstanceDecl :: Parser SourceAst
parseInstanceDecl = withSource $ do
  P.try $ reserved "instance"
  cs <- many (P.try parseConstraint)
  cls <- tyname
  ty <- parseType
  reserved "where"
  decls <- mark $ many (same *> parseValueDecl)
  pure $ inj $ InstanceDecl (IsIn cls (hoistCofree inj ty))
                            (map (map (hoistCofree inj)) cs)
                            decls

parseTypeDecl :: Parser SourceAst
parseTypeDecl = withSource $ inj <$> (TypeDecl
  <$> P.try (identifier <* colon)
  <*> (map (hoistCofree inj) <$> parsePolyType)
                                     )

parseValueDecl :: Parser SourceAst
parseValueDecl = withSource $ do
  (name, binders) <- P.try $ (,)
    <$> identifier
    <*> (many (hoistCofree inj <$> parseBinder) <* equals)
  expr <- parseExpr
  pure $ inj $ ValueDecl name binders expr

parseFixityDecl :: Parser SourceAst
parseFixityDecl = withSource $ do
  assoc <- P.choice
    [ P.try (reserved "infixl") $> AssocL
    , P.try (reserved "infixr") $> AssocR
    , P.try (reserved "infix") $> AssocN
    ]
  fixity <- integerLit
  x <- identifier
  reserved "as"
  op <- anySymbol
  let opSpec = Infix assoc $ fromIntegral fixity
  modify $ addOpSpec op opSpec
  pure $ inj $ FixityDecl x op opSpec

--------------------------------------------------------------------------------

parseExpr :: Parser SourceAst
parseExpr = do
  opSpecs <- gets parserOperators
  makeExprParser terms (mkOpTable spanVar spanApp opSpecs)
    P.<?> "expression"
  where
    terms = P.choice
      [ P.try parseApp
      , parseExpr'
      ]

parseExpr' :: Parser SourceAst
parseExpr' = P.choice
  [ parseAccessor
  , parseExpr''
  ]

parseExpr'' :: Parser SourceAst
parseExpr'' = P.choice
  [ parseCase
  , parseIfThenElse
  , parseLet
  , parseAbs
  , parseColOfTblRef
  , parseTblRef
  , parseColRef
  , parseLit
  , parseConstructor
  , parseVar
  , (parens parseExpr)
  ]

parseApp :: Parser SourceAst
parseApp = do
  start <- parseExpr'
  args <- some (indented *> parseExpr')
  pure $ foldl' spanApp start args

parseAccessor :: Parser SourceAst
parseAccessor = do
  e <- P.try $ parseExpr'' <* dot
  ref <- withSpan identifier
  refs <- many $ withSpan $ dot *> identifier
  pure $ foldl' spanAccessor e (ref:refs)

parseConstructor :: Parser SourceAst
parseConstructor = withSource $ inj . Constructor <$> dconsname

parseCase :: Parser SourceAst
parseCase = withSource $ do
  P.try $ reserved "case"
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
  P.try $ reserved "let"
  indented
  bindings <- mark $ some $ do
    name <- same *> identifier
    args <- many parseBinder
    equals
    e <- parseExpr
    pure (name, args, e)
  same <|> indented
  reserved "in"
  body <- parseExpr
  let
    bindings' = flip map bindings $ \(name, args, e) ->
      (name, foldr spanAbs e (map (hoistCofree inj) args))
  pure $ inj $ Let bindings' body

parseAbs :: Parser SourceAst
parseAbs = do
  P.try backslash
  binders <- some (indented *> parseBinder)
  indented *> rArrow
  body <- parseExpr
  pure $ foldr spanAbs body (map (hoistCofree inj) binders)

parseVar :: Parser SourceAst
parseVar = withSource $ inj . Var <$> identifier

parseLit :: Parser SourceAst
parseLit = withSource $ inj . Literal <$> (P.choice
    [ parseString
    , P.try parseNumber
    , parseInteger
    , parseRecord
    ]
    P.<?> "literal")
  where
    parseString = StringLit <$> (lexeme stringLit)
    parseNumber = NumberLit <$> (lexeme numberLit)
    parseInteger = IntegerLit <$> (lexeme integerLit)
    parseRecord = braces $ do
      fields <- P.sepBy1 ((,) <$> identifier <* colon <*> parseExpr) comma
      pure $ RecordLit $ Map.fromList fields

parseIfThenElse :: Parser SourceAst
parseIfThenElse = do
  start <- getPosition
  cond <-                    P.try (reserved "if")  *> indented *> parseExpr
  true@(tspan :< _) <-  indented *> reserved "then" *> indented *> parseExpr
  false@(fspan :< _) <- indented *> reserved "else" *> indented *> parseExpr
  end <- getPosition
  pure $ Span start end :< inj (Case cond
    [ (tspan :< inj (ConstructorBinder "True" []), true)
    , (fspan :< inj (ConstructorBinder "False" []), false)
    ])

parseTblRef :: Parser SourceAst
parseTblRef = withSource $ do
  ref <- TableRef . Ref <$> (P.try hashSign' *> reference)
  pure $ inj (ref :: RefTextF SourceAst)

parseColRef :: Parser SourceAst
parseColRef = withSource $ do
  ref <- ColumnRef . Ref <$> (P.try dollarSign' *> reference)
  pure $ inj (ref :: RefTextF SourceAst)

parseColOfTblRef :: Parser SourceAst
parseColOfTblRef = withSource $ do
  tbl <- P.try (hashSign' *> reference' <* dot')
  col <- reference
  pure $ inj (ColumnOfTableRef (Ref tbl) (Ref col) :: RefTextF SourceAst)

--------------------------------------------------------------------------------

parseBinder :: Parser SourceBinder
parseBinder = P.choice
  [ withSource (ConstructorBinder <$> dconsname <*> many parseBinder')
  , withSource (underscore $> WildcardBinder)
  , parseBinder'
  ]
  P.<?> "binder"

parseBinder' :: Parser SourceBinder
parseBinder' = P.choice
  [ withSource (VarBinder <$> P.try identifier)
  , withSource (ConstructorBinder <$> dconsname <*> pure [])
  , parens parseBinder
  ]

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
  , parseRecordType
  , parens parseType
  ]

parseRecordType :: Parser SourceType
parseRecordType = braces $ do
  (span, rows) <- withSpan parseRows
  pure $ spanTypeApp (spanTypeConstructor (span, "Record")) rows
  where
  parseRows = do
    rows <- P.sepBy1 ((,) <$> identifier <* colon <*> parseType) comma
    (endSpan, end) <- withSpan $ P.option RecordNil $ do
      _ :< t <- indented *> pipe *> indented *> parseType
      pure t
    pure $ foldr (\(ref, ty) rest -> spanRecordCons ref ty rest) (endSpan :< end) rows

parsePolyType :: Parser SourcePolyType
parsePolyType = do
  (vars, preds) <- P.option ([], []) $ P.try $ do
    vs <- reserved "forall" *> some (indented *> identifier) <* indented <* dot
    ps <- many (P.try parseConstraint)
    pure (vs, ps)
  ty <- parseType
  pure $ ForAll vars preds ty

parseConstraint :: Parser SourceConstraint
parseConstraint =
  (IsIn <$> (indented *> tyname) <*> parseType) <* indented <* rfatArrow
