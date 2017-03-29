{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Lib.Compiler.Parser
  ( parseExpr
  , parseModule
  , parseFormula
  , parse
  , testParse
  ) where

import           Lib.Prelude

import           Data.Foldable             (foldl')
import           Data.Functor.Foldable
import           Data.List                 (groupBy)
import           Data.Text                 (Text, pack)

import qualified Text.Megaparsec           as P
import           Text.Megaparsec.Expr

import           Lib.Compiler.AST
import           Lib.Compiler.Env
import           Lib.Compiler.Parser.Lexer
import           Lib.Compiler.Parser.State
import           Lib.Compiler.Type
import           Lib.Types

--------------------------------------------------------------------------------

parseModule :: Parser Module
parseModule = Module <$> many (same *> parseDeclaration)

parseFormula :: Parser Formula
parseFormula = Formula
  <$> many (same *> parseDeclaration)
  <*> (same *> parseExpr)

parseDeclaration :: Parser Declaration
parseDeclaration = P.choice
  [ P.try parseDataDecl
  , P.try parseTypeDecl
  , P.try parseValueDecl
  ]
  P.<?> "declaration"

parseDataDecl :: Parser Declaration
parseDataDecl = do
  reserved "data"
  name <- indented *> tyname
  tyArgs <- many (indented *> identifier)
  constructors <- P.option [] $ do
    indented *> equals
    let
      constructor = (,) <$> dconsname <*> many (indented *> parseType)
    P.sepBy1 constructor pipe
  pure $ DataDecl name tyArgs constructors

parseTypeDecl :: Parser Declaration
parseTypeDecl = TypeDecl <$> (identifier <* doubleColon) <*> parsePolyType

parseValueDecl :: Parser Declaration
parseValueDecl = ValueDecl
  <$> identifier
  <*> (many parseBinder <* equals)
  <*> parseExpr

--------------------------------------------------------------------------------

parseExpr :: Parser Expr
parseExpr = do
  opSpecs <- gets parserOperators
  makeExprParser terms (mkOpTable (Fix . Var) mkApp opSpecs)
  where
    terms = P.choice
      [ P.try parseApp
      , P.try parseExpr'
      ]
      P.<?> "expression"

parseExpr' :: Parser Expr
parseExpr' = P.choice
  [ P.try parseAccessor
  , P.try parseExpr''
  ]

parseExpr'' :: Parser Expr
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

parseApp :: Parser Expr
parseApp = do
  start <- parseExpr'
  args <- some (indented *> parseExpr')
  pure $ foldl' mkApp start args

parseAccessor :: Parser Expr
parseAccessor = do
  e <- parseExpr''
  refs <- some $ dot *> (Ref <$> identifier)
  pure $ foldl' mkAccessor e refs

parseConstructor :: Parser Expr
parseConstructor = Fix . Constructor <$> dconsname

parseCase :: Parser Expr
parseCase = do
  reserved "case"
  scrut <- parseExpr
  indented *> reserved "of"
  alts <- indented *> mark (some (same *> parseAlternative))
  pure $ Fix $ Case scrut alts
  where
  parseAlternative =
    (,) <$> parseBinder <*> (indented *> rArrow *> parseExpr)
    P.<?> "case alternative"

parseLet :: Parser Expr
parseLet = do
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
  pure $ Fix $ Let name (foldr mkAbs e args) body

parseAbs :: Parser Expr
parseAbs = do
  backslash
  binders <- some (indented *> parseBinder)
  indented *> rArrow
  body <- parseExpr
  pure $ foldr mkAbs body binders

parseBinder :: Parser Binder
parseBinder = P.choice
  [ VarBinder <$> P.try identifier
  , ConstructorBinder <$> P.try dconsname <*> many parseBinder
  , parens parseBinder
  ]
  P.<?> "binder"

parseVar :: Parser Expr
parseVar = Fix . Var <$> identifier

parseLit :: Parser Expr
parseLit = Fix . Literal <$> P.choice
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

parseIfThenElse :: Parser Expr
parseIfThenElse = do
  cond <-              reserved "if"   *> indented *> parseExpr
  true <-  indented *> reserved "then" *> indented *> parseExpr
  false <- indented *> reserved "else" *> indented *> parseExpr
  pure $ Fix $ Case cond
    [ (ConstructorBinder "True" [], true)
    , (ConstructorBinder "False" [], false)
    ]

parseTblRef :: Parser Expr
parseTblRef = Fix . TableRef . Ref <$> (hashSign *> identifier)

parseColRef :: Parser Expr
parseColRef = Fix . ColumnRef . Ref <$> (dollarSign *> identifier)

parseColOfTblRef :: Parser Expr
parseColOfTblRef = do
  tbl <- hashSign *> identifier
  col <- dot *> identifier
  pure $ Fix $ ColumnOfTableRef (Ref tbl) (Ref col)

--------------------------------------------------------------------------------

parse :: Text -> Either Text Module
parse e =
  flip evalState initialParseState $
  P.runParserT (scn *> parseModule <* P.eof) "" e >>= \case
    Left msg -> pure $ Left $ pack $ P.parseErrorPretty msg
    Right x  -> pure $ Right x

testParse :: Text -> IO ()
testParse e = case parse e of
  Left msg -> putStrLn msg
  Right x  -> putStrLn $ prettyModule x

--------------------------------------------------------------------------------

parseType :: Parser Type
parseType = do
  opSpecs <- gets parserTypeOperators
  makeExprParser terms (mkOpTable (Fix . TypeConstructor) mkTypeApp opSpecs)
  where
    terms = P.choice
      [ P.try parseTypeApp
      , P.try parseType'
      ]
      P.<?> "type"

parseTypeApp :: Parser Type
parseTypeApp = do
  start <- parseType'
  args <- some (indented *> parseType')
  pure $ foldl' mkTypeApp start args

parseType' :: Parser Type
parseType' = P.choice
  [ Fix . TypeVar <$> identifier
  , Fix . TypeConstructor <$> tyname
  , parseRecordType
  ]

parseRecordType :: Parser Type
parseRecordType = braces $ mkTypeApp tyRecord <$> parseRows
  where
  parseRows = do
    rows <- P.sepBy1 ((,) <$> identifier <* doubleColon <*> parseType) comma
    end <- P.option (Fix RecordNil) $ indented *> pipe *> indented *> parseType
    pure $ foldr (\(ref, ty) rest -> mkRecordCons ref ty rest) end rows

parsePolyType :: Parser PolyType
parsePolyType = do
  (vars, preds) <- P.option ([], []) $ P.try $ do
    vs <- reserved "forall" *> some (indented *> identifier) <* indented <* dot
    ps <- many (P.try parsePredicate)
    pure (vs, ps)
  ty <- parseType
  pure $ ForAll vars preds ty

parsePredicate :: Parser Predicate
parsePredicate =
  (IsIn <$> (indented *> tyname) <*> parseType) <* indented <* rfatArrow

--------------------------------------------------------------------------------

mkOpTable
  :: forall a
   . (Text -> a)
  -> (a -> a -> a)
  -> [OpSpec] -> [[Operator Parser a]]
mkOpTable embedOp combine = (map.map) binary . groupBy f . sortBy g
  where
  g (opFixity -> Infix _ x) (opFixity -> Infix _ y) = compare y x

  f (opFixity -> Infix _ x) (opFixity -> Infix _ y) = x == y

  binary :: OpSpec -> Operator Parser a
  binary (OpSpec (Infix assoc _) name) =
    let
      p = P.try (symbol name) $> (\l r -> combine (combine (embedOp name) l) r)
    in
      case assoc of
        AssocL -> InfixL p
        AssocN -> InfixN p
        AssocR -> InfixR p
