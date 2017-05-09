{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |

module Lib.Compiler.Parse.Common where

import           Lib.Prelude

import           Control.Comonad.Cofree
import           Control.Lens              hiding ((:<))

import           Data.List                 (groupBy)

import qualified Text.Megaparsec           as P
import           Text.Megaparsec.Expr

import           Lib.Compiler.AST.Common
import           Lib.Compiler.AST.Position
import           Lib.Compiler.Parse.Lexer
import           Lib.Compiler.Parse.State

--------------------------------------------------------------------------------

mkOpTable
  :: forall f. Functor f
  => ((Span, Text) -> WithSpan f)
  -> (WithSpan f -> WithSpan f -> WithSpan f)
  -> OpTable -> [[Operator Parser (WithSpan f)]]
mkOpTable embedOp combine = (map.map) binary . groupBy f . sortBy g
  where
  g (_, Infix _ x) (_, Infix _ y) = compare y x

  f (_, Infix _ x) (_, Infix _ y) = x == y

  binary :: (Text, Fixity) -> Operator Parser (WithSpan f)
  binary (name, Infix assoc _) =
    let
      p = do
        (span, _) <- withSpan $ P.try (indented *> symbol name)
        pure $ \l r ->
          combine (combine (embedOp (span, name)) l) r
    in
      case assoc of
        AssocL -> InfixL p
        AssocN -> InfixN p
        AssocR -> InfixR p

withSpan :: Parser a -> Parser (Span, a)
withSpan p = do
  start <- getPosition
  x <- p
  end <- use parserLastTokenEnd
  pure (Span start end, x)

withSource :: Parser (f (WithSpan f)) -> Parser (WithSpan f)
withSource p = do
  (span, fa) <- withSpan p
  pure $ span :< fa

singletonSpan :: Parser Span
singletonSpan = do
  pos <- getPosition
  pure (Span pos pos)

