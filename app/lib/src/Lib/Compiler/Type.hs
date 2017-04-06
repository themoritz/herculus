{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
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
  = KindType
  | KindFun a a
  | KindRecord a
  | KindVar Int
  deriving (Functor, Foldable, Traversable, Show)

type Kind = Fix KindF

kindType :: Kind
kindType = Fix KindType

kindFun :: Kind -> Kind -> Kind
kindFun f arg = Fix (KindFun f arg)

kindRecord :: Kind -> Kind
kindRecord = Fix . KindRecord

kindVar :: Int -> Kind
kindVar = Fix . KindVar

-- Replace remaining kind variables with `Type`
tidyKind :: Kind -> Kind
tidyKind = cata $ \case
  KindVar _ -> kindType
  other -> Fix other

--------------------------------------------------------------------------------

data TypeF a
  = TypeVar Text
  | TypeConstructor Text
  | TypeApp a a
  | RecordCons (Ref Column) a a
  | RecordNil
  deriving (Functor, Foldable, Traversable, Show)

type Type = Fix TypeF
type SourceType = WithSpan TypeF

typeConstructor :: Text -> Type
typeConstructor = Fix . TypeConstructor

typeApp :: Type -> Type -> Type
typeApp a b = Fix (TypeApp a b)

spanTypeConstructor :: (Span, Text) -> SourceType
spanTypeConstructor (span, t) = span :< TypeConstructor t

spanTypeApp :: SourceType -> SourceType -> SourceType
spanTypeApp f@(fspan :< _) arg@(argspan :< _) =
  spanUnion fspan argspan :< TypeApp f arg

spanRecordCons :: Text -> SourceType -> SourceType -> SourceType
spanRecordCons f t@(tspan :< _) r@(rspan :< _) =
  spanUnion tspan rspan :< RecordCons (Ref f) t r

-- Type variables and predicates
data PolyType t
  = ForAll [Text] [Predicate t] t
  deriving (Functor, Show)

-- | Class and type, which should be member of the class
data Predicate t
  = IsIn Text t
  deriving (Functor, Show)
