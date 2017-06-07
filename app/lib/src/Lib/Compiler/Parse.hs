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
import           Control.Lens              hiding ((:<), op)

import           Data.Foldable             (foldl')
import qualified Data.Map                  as Map

import qualified Text.Megaparsec           as P
import           Text.Megaparsec.Expr

import           Lib.Compiler.AST
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
  docString <- use parserLastDocString
  P.try $ reserved "type"
  -- We reset the docstring state after we've committed to this declaration
  parserLastDocString .= ""
  name <- indented *> declName tyname
  tyArgs <- many (indented *> declName identifier)
  constructors <- P.option [] $ do
    indented *> equals
    let
      constr = (,,)
        <$> (use parserLastDocString <* (parserLastDocString .= ""))
        <*> declName dconsname
        <*> many (indented *> (hoistCofree inj <$> parseType'))
    P.sepBy1 constr pipe
  pure $ inj (DataDecl docString name tyArgs constructors)

parseClassDecl :: Parser SourceAst
parseClassDecl = withSource $ do
  docString <- use parserLastDocString
  P.try $ reserved "interface"
  parserLastDocString .= ""
  supers <- many (P.try parseSuper)
  cls <- indented *> declName tyname
  param <- indented *> declName identifier
  indented *> reserved "where"
  indented
  decls <- mark $ many (same *> parseTypeDecl)
  pure $ inj $ ClassDecl docString (cls, param) supers decls
  where
  parseSuper = (,) <$> (indented *> declName tyname)
                   <*> (indented *> declName identifier <* rfatArrow)

parseInstanceDecl :: Parser SourceAst
parseInstanceDecl = withSource $ do
  P.try $ reserved "implements"
  cs <- many (P.try constraint)
  cls <- indented *> declName tyname
  ty <- indented *> parseType
  indented *> reserved "where"
  decls <- mark $ many (same *> parseValueDecl)
  pure $ inj $ InstanceDecl (cls, (hoistCofree inj ty))
                            (map (id *** hoistCofree inj) cs)
                            decls
  where
  constraint =
    ((,) <$> (indented *> declName tyname) <*> parseType)
    <* indented <* rfatArrow

parseTypeDecl :: Parser SourceAst
parseTypeDecl = withSource $ do
  docString <- use parserLastDocString
  name <- P.try (declName identifier <* indented <* colon)
  parserLastDocString .= ""
  poly <- indented *> parsePolyType
  pure $ inj $ TypeDecl docString name (hoistCofree inj <$> poly)

parseValueDecl :: Parser SourceAst
parseValueDecl = withSource $ do
  (name, binders) <- P.try $ (,)
    <$> declName identifier
    <*> (many binder <* (indented *> equals))
  expr <- indented *> parseExpr
  pure $ inj $ ValueDecl name binders expr
  where
  binder = hoistCofree inj <$> (indented *> parseBinder)

parseFixityDecl :: Parser SourceAst
parseFixityDecl = withSource $ do
  docString <- use parserLastDocString
  assoc <- P.choice
    [ P.try (reserved "infixl") $> AssocL
    , P.try (reserved "infixr") $> AssocR
    , P.try (reserved "infix") $> AssocN
    ]
  parserLastDocString .= ""
  fixity <- indented *> integerLit
  x <- indented *> declName identifier
  indented *> reserved "as"
  (span, op) <- indented *> withSpan anySymbol
  let opSpec = Infix assoc $ fromIntegral fixity
  addOpSpec op opSpec
  pure $ inj $ FixityDecl docString x (span :< inj (DeclName op)) opSpec

declName :: Parser Text -> Parser SourceAst
declName p = withSource $ inj . DeclName <$> p

--------------------------------------------------------------------------------

parseExpr :: Parser SourceAst
parseExpr = do
  opSpecs <- use parserOperators
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
  e <- P.try $ parseExpr'' <* dot'
  let parseRef = withSource $ map (inj . Literal . StringLit) reference
  ref <- parseRef
  refs <- many $ dot' *> parseRef
  pure $ foldl' spanAccess e (ref:refs)

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
    (,) <$> (hoistCofree inj <$> parseBinder)
        <*> (indented *> rArrow *> parseExpr)
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
    parseString = StringLit <$> stringLit
    parseNumber = NumberLit <$> numberLit
    parseInteger = IntegerLit <$> integerLit
    parseRecord = braces $ do
      fields <- P.sepBy ((,) <$> reference <* colon <*> parseExpr) comma
      pure $ RecordLit $ Map.fromList fields

parseIfThenElse :: Parser SourceAst
parseIfThenElse = withSource $ do
  cond <-                    P.try (reserved "if")  *> indented *> parseExpr
  true@(tspan :< _) <-  indented *> reserved "then" *> indented *> parseExpr
  false@(fspan :< _) <- indented *> reserved "else" *> indented *> parseExpr
  pure $ inj (Case cond
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
  opSpecs <- use parserTypeOperators
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
  , parseTableType
  , parseRecordType
  , parens parseType
  ]

parseTableType :: Parser SourceType
parseTableType = withSource $ do
  ref <- Ref <$> (P.try hashSign' *> reference)
  pure $ inj $ TypeTable $ InRef ref

parseRecordType :: Parser SourceType
parseRecordType = withSource $ map (inj . TypeRecord) parseFields

parseFields :: Parser (Map Text SourceType)
parseFields = braces $ do
  fields <- P.sepBy1 ((,) <$> reference <* colon <*> parseType) comma
  pure $ Map.fromList fields

parsePolyType :: Parser SourcePolyType
parsePolyType = do
  (vars, preds) <- P.option ([], []) $ P.try $ do
    vs <- reserved "forall" *> some (indented *> identifier) <* indented <* dot
    ps <- many (P.try parseConstraint)
    pure (vs, ps)
  ty <- parseType
  pure $ ForAll vars preds ty

parseConstraint :: Parser SourceConstraint
parseConstraint = P.choice
  [ P.try parseClassConstraint
  , P.try parseFieldConstraint
  ] P.<?> "constraint"

parseClassConstraint :: Parser SourceConstraint
parseClassConstraint =
  (IsIn <$> (indented *> tyname) <*> parseType) <* indented <* rfatArrow

parseFieldConstraint :: Parser SourceConstraint
parseFieldConstraint = do
  v <- indented *> withSource (TypeVar <$> identifier)
  fields <- parseFields
  indented *> rfatArrow
  pure (HasFields fields v)
