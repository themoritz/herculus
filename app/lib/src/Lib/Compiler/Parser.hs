{-# LANGUAGE NoImplicitPrelude #-}

module Lib.Compiler.Parser
  ( parseExpr
  , parseModule
  , parseFormula
  , parse
  , testParsePretty
  , testParseSpans
  ) where

import           Lib.Prelude

import           Control.Comonad.Cofree

import           Data.Foldable              (foldl')
import           Data.Text                  (Text, pack)

import qualified Text.Megaparsec            as P
import           Text.Megaparsec.Expr
import           Text.Show.Pretty           (ppShow)

import           Lib.Compiler.AST
import           Lib.Compiler.AST.Common
import           Lib.Compiler.AST.Position
import           Lib.Compiler.Env
import           Lib.Compiler.Parser.Common
import           Lib.Compiler.Parser.Lexer
import           Lib.Compiler.Parser.State
import           Lib.Compiler.Pretty
import           Lib.Compiler.Type
import           Lib.Types

--------------------------------------------------------------------------------

parse :: Text -> Either Text [SourceAst]
parse e =
  flip evalState initialParseState $
  P.runParserT (scn *> parseModule <* P.eof) "" e >>= \case
    Left msg -> pure $ Left $ pack $ P.parseErrorPretty msg
    Right x  -> pure $ Right x

testParsePretty :: Text -> IO ()
testParsePretty e = case parse e of
  Left msg    -> putStrLn msg
  Right decls -> mapM_ (putStrLn . prettyAst) (map stripAnn decls)

testParseSpans :: Text -> IO ()
testParseSpans e = case parse e of
  Left msg    -> putStrLn msg
  Right decls -> mapM_ (putStrLn . ppShow . map (flip highlightSpan e)) decls


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
        <*> many (indented *> (mapCofree InjType <$> parseType))
    P.sepBy1 constructor pipe
  pure $ InjDecl (DataDecl name tyArgs constructors)

parseTypeDecl :: Parser SourceAst
parseTypeDecl = withSource $ InjDecl <$> (TypeDecl
  <$> (identifier <* doubleColon)
  <*> (map (mapCofree InjType) <$> parsePolyType)
                                         )

parseValueDecl :: Parser SourceAst
parseValueDecl = withSource $ InjDecl <$> (ValueDecl
  <$> identifier
  <*> (many (mapCofree InjBinder <$> parseBinder) <* equals)
  <*> parseExpr
                                          )

--------------------------------------------------------------------------------

parseExpr :: Parser SourceAst
parseExpr = do
  opSpecs <- gets parserOperators
  makeExprParser terms (mkOpTable mkSourceVar mkSourceApp opSpecs)
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
  pure $ foldl' mkSourceApp start args

parseAccessor :: Parser SourceAst
parseAccessor = do
  e <- parseExpr''
  refs <- some $ withSpan $ dot *> (Ref <$> identifier)
  pure $ foldl' mkSourceAccessor e refs

parseConstructor :: Parser SourceAst
parseConstructor = withSource $ InjExpr . Constructor <$> dconsname

parseCase :: Parser SourceAst
parseCase = withSource $ do
  reserved "case"
  scrut <- parseExpr
  indented *> reserved "of"
  alts <- indented *> mark (some (same *> parseAlternative))
  pure $ InjExpr $ Case scrut alts
  where
  parseAlternative :: Parser (SourceAst, SourceAst)
  parseAlternative =
    (,) <$> (mapCofree InjBinder <$> parseBinder) <*> (indented *> rArrow *> parseExpr)
    P.<?> "case alternative"

parseLet :: Parser SourceAst
parseLet = withSource $ do
  reserved "let"
  indented
  (name, args, e) <- mark $ do
    name <- identifier
    args <- many parseBinder
    equals
    e <- parseExpr
    pure (name, args, e)
  indented
  reserved "in"
  body <- parseExpr
  pure $ InjExpr $
    Let name (foldr mkSourceAbs e (map (mapCofree InjBinder) args)) body

parseAbs :: Parser SourceAst
parseAbs = do
  backslash
  binders <- some (indented *> parseBinder)
  indented *> rArrow
  body <- parseExpr
  pure $ foldr mkSourceAbs body (map (mapCofree InjBinder) binders)

parseVar :: Parser SourceAst
parseVar = withSource $ InjExpr . Var <$> identifier

parseLit :: Parser SourceAst
parseLit = withSource $ InjExpr . Literal <$> P.choice
    [ P.try parseString
    , P.try parseNumber
    , P.try parseInteger
    , P.try parseBool
    , P.try parseRecord
    ]
  where
    parseString = StringLit <$> (lexeme stringLit)
    parseNumber = NumberLit <$> (lexeme numberLit)
    parseInteger = IntegerLit <$> (lexeme integerLit)
    parseBool =
          P.try (dconsname' "True"  $> BoolLit True)
      <|> P.try (dconsname' "False" $> BoolLit False)
      P.<?> "True or False"
    parseRecord = braces $ do
      fields <- P.sepBy1 ((,) <$> identifier <* doubleColon <*> parseExpr) comma
      pure $ RecordLit fields

parseIfThenElse :: Parser SourceAst
parseIfThenElse = do
  start <- P.getPosition
  cond <-              reserved "if"   *> indented *> parseExpr
  true@(tspan :< _) <-  indented *> reserved "then" *> indented *> parseExpr
  false@(fspan :< _) <- indented *> reserved "else" *> indented *> parseExpr
  end <- P.getPosition
  pure $ SourceSpan start end :< InjExpr (Case cond
    [ (tspan :< InjBinder (ConstructorBinder "True" []), true)
    , (fspan :< InjBinder (ConstructorBinder "False" []), false)
    ])

parseTblRef :: Parser SourceAst
parseTblRef = map (mapCofree InjExpr) $
  withSource $ TableRef . Ref <$> (hashSign *> identifier)

parseColRef :: Parser SourceAst
parseColRef = map (mapCofree InjExpr) $
  withSource $ ColumnRef . Ref <$> (dollarSign *> identifier)

parseColOfTblRef :: Parser SourceAst
parseColOfTblRef = map (mapCofree InjExpr) $ withSource $ do
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
  makeExprParser terms (mkOpTable mkSourceTypeConstructor mkSourceTypeApp opSpecs)
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
  pure $ foldl' mkSourceTypeApp start args

parseType' :: Parser SourceType
parseType' = P.choice
  [ withSource (TypeVar <$> identifier)
  , withSource (TypeConstructor <$> tyname)
  , parseRecordType
  ]

parseRecordType :: Parser SourceType
parseRecordType = braces $ do
  (span, rows) <- withSpan parseRows
  pure $ mkSourceTypeApp (span :< tyRecord) rows
  where
  parseRows = do
    rows <- P.sepBy1 ((,) <$> identifier <* doubleColon <*> parseType) comma
    (endSpan, end) <- withSpan $ P.option RecordNil $ do
      _ :< t <- indented *> pipe *> indented *> parseType
      pure t
    pure $ foldr (\(ref, ty) rest -> mkRecordCons ref ty rest) (endSpan :< end) rows

parsePolyType :: Parser (PolyType SourceType)
parsePolyType = do
  (vars, preds) <- P.option ([], []) $ P.try $ do
    vs <- reserved "forall" *> some (indented *> identifier) <* indented <* dot
    ps <- many (P.try parsePredicate)
    pure (vs, ps)
  ty <- parseType
  pure $ ForAll vars preds ty

parsePredicate :: Parser (Predicate SourceType)
parsePredicate =
  (IsIn <$> (indented *> tyname) <*> parseType) <* indented <* rfatArrow
