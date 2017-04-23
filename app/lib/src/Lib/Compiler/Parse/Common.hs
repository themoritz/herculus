{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
-- |

module Lib.Compiler.Parse.Common where

import           Lib.Prelude

import           Control.Comonad.Cofree

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
  -> [OpSpec] -> [[Operator Parser (WithSpan f)]]
mkOpTable embedOp combine = (map.map) binary . groupBy f . sortBy g
  where
  g (opFixity -> Infix _ x) (opFixity -> Infix _ y) = compare y x

  f (opFixity -> Infix _ x) (opFixity -> Infix _ y) = x == y

  binary :: OpSpec -> Operator Parser (WithSpan f)
  binary (OpSpec (Infix assoc _) name) =
    let
      p = do
        (span, _) <- withSpan $ P.try (symbol name)
        pure $ \l r ->
          combine (combine (embedOp (span, name)) l) r
    in
      case assoc of
        AssocL -> InfixL p
        AssocN -> InfixN p
        AssocR -> InfixR p

withSpan :: Parser a -> Parser (Span, a)
withSpan p = do
  start <- P.getPosition
  x <- p
  end <- gets parserLastTokenEnd
  pure (Span start end, x)

withSource :: Parser (f (WithSpan f)) -> Parser (WithSpan f)
withSource p = do
  (span, fa) <- withSpan p
  pure $ span :< fa

singletonSpan :: Parser Span
singletonSpan = do
  pos <- P.getPosition
  pure (Span pos pos)

