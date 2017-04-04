{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |

module Lib.Compiler.Type where

import           Lib.Prelude               hiding (empty)

import           Control.Comonad.Cofree

import           Data.Functor.Foldable

import           Lib.Compiler.AST.Common
import           Lib.Compiler.AST.Position
import           Lib.Model.Column
import           Lib.Types

data KindF a
  = KindStar
  | KindFun a a
  deriving (Functor, Show)

type Kind = Fix KindF

data TypeF a
  = TypeVar Text
  | TypeConstructor Text
  | TypeApp a a
  | RecordCons (Ref Column) a a
  | RecordNil
  deriving (Functor, Show)

type Type = Fix TypeF
type SourceType = WithSource TypeF

mkSourceTypeConstructor :: (SourceSpan, Text) -> SourceType
mkSourceTypeConstructor (span, t) = span :< TypeConstructor t

mkSourceTypeApp :: SourceType -> SourceType -> SourceType
mkSourceTypeApp f@(fspan :< _) arg@(argspan :< _) =
  sourceUnion fspan argspan :< TypeApp f arg

mkRecordCons :: Text -> SourceType -> SourceType -> SourceType
mkRecordCons f t@(tspan :< _) r@(rspan :< _) =
  sourceUnion tspan rspan :< RecordCons (Ref f) t r

-- Type variables and predicates
data PolyType t
  = ForAll [Text] [Predicate t] t
  deriving (Functor, Show)

-- | Class and type, which should be member of the class
data Predicate t
  = IsIn Text t
  deriving (Functor, Show)
