{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns      #-}

module Lib.Compiler.Parser
  ( parseExpr
  , parse
  ) where

import           Lib.Prelude

import           Data.Foldable             (foldl')
import           Data.Functor              (($>))
import           Data.Functor.Foldable
import           Data.List                 (groupBy)
import           Data.Text                 (Text, pack)

import qualified Text.Megaparsec           as P
import           Text.Megaparsec.Expr

import           Lib.Compiler.AST
import           Lib.Compiler.Parser.Lexer
import           Lib.Compiler.Parser.State
import           Lib.Types

--

mkOpTable :: [OpSpec] -> [[Operator Parser Expr]]
mkOpTable = (map.map) binary . groupBy f . sortBy g
  where
  g (opFixity -> Infix _ x) (opFixity -> Infix _ y) = compare y x
  f (opFixity -> Infix _ x) (opFixity -> Infix _ y) = x == y
  binary :: OpSpec -> Operator Parser Expr
  binary (OpSpec (Infix assoc _) name) =
    let
      p = symbol name $> (\l r -> mkApp (mkApp (Fix $ Var name) l) r)
    in
      case assoc of
        AssocL -> InfixL p
        AssocN -> InfixN p
        AssocR -> InfixR p

parseExpr :: Parser Expr
parseExpr = do
  opSpecs <- gets parserOperators
  makeExprParser terms (mkOpTable opSpecs)
  where
    terms = P.choice
      [ parseIfThenElse
      , parseLet
      , parseAbs
      , parseApp
      , parseExpr'
      ]

parseExpr' :: Parser Expr
parseExpr' = P.choice
  [ P.try parseAccessor
  , P.try parseExpr''
  ]

parseExpr'' :: Parser Expr
parseExpr'' = P.choice
  [ parseVar
  , parseColOfTblRef
  , parseTblRef
  , parseColRef
  , parseLit
  , (parens parseExpr)
  ]

parseApp :: Parser Expr
parseApp = do
  start <- parseExpr'
  args <- some parseExpr'
  pure $ foldl' mkApp start args

parseAccessor :: Parser Expr
parseAccessor = do
  e <- parseExpr''
  refs <- some $ dot *> (Ref <$> identifier)
  pure $ foldl' mkAccessor e refs

parseLet :: Parser Expr
parseLet = do
  _ <- reserved "let"
  name <- identifier
  args <- many parseBinder
  equals
  e <- parseExpr
  semicolon
  body <- parseExpr
  pure $ Fix $ Let name (foldr mkAbs e args) body

parseAbs :: Parser Expr
parseAbs = do
  backslash
  binders <- some parseBinder
  rArrow
  body <- parseExpr
  pure $ foldr mkAbs body binders

parseBinder :: Parser Binder
parseBinder = VarBinder <$> identifier

parseVar :: Parser Expr
parseVar = Fix . Var <$> identifier

parseLit :: Parser Expr
parseLit = Fix . Literal <$> (parseString <|> parseNumber <|> parseBool)
  where
    parseString = StringLit <$> stringLit
    parseNumber = NumberLit <$> numberLit
    parseBool =
          P.try (reserved "True"  $> BoolLit True)
      <|> P.try (reserved "False" $> BoolLit False)
      P.<?> "True or False"

parseIfThenElse :: Parser Expr
parseIfThenElse = do
  cond <- reserved "if"   *> parseExpr
  true <- reserved "then" *> parseExpr
  false <- reserved "else" *> parseExpr
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

parse :: Text -> Either Text Expr
parse e = flip evalState initialParseState $
  P.runParserT (spaceConsumer *> parseExpr <* P.eof) "" e >>= \case
    Left msg -> pure $ Left $ pack $ show msg
    Right x  -> pure $ Right x
