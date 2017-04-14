{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- |

module Lib.Compiler.Type where

import           Lib.Prelude               hiding (empty)

import           Control.Comonad.Cofree

import           Data.Functor.Foldable
import qualified Data.Map                  as Map
import qualified Data.Set                  as Set

import           Lib.Compiler.AST.Common
import           Lib.Compiler.AST.Position
import           Lib.Model.Column
import           Lib.Types

data KindF a
  = KindType
  | KindFun a a
  | KindRecord a
  | KindUnknown Int
  deriving (Functor, Foldable, Traversable, Show)

type Kind = Fix KindF

kindType :: Kind
kindType = Fix KindType

kindFun :: Kind -> Kind -> Kind
kindFun f arg = Fix (KindFun f arg)

kindRecord :: Kind -> Kind
kindRecord = Fix . KindRecord

kindUnknown :: Int -> Kind
kindUnknown = Fix . KindUnknown

-- Replace remaining unknown kind variables with `Type`
tidyKind :: Kind -> Kind
tidyKind = cata $ \case
  KindUnknown _ -> kindType
  other -> Fix other

--------------------------------------------------------------------------------

data TypeF a
  = TypeVar Text
  | TypeConstructor Text
  | TypeApp a a
  | RecordCons Text a a
  | RecordNil
  deriving (Functor, Foldable, Traversable, Show)

type Type = Fix TypeF
type SourceType = WithSpan TypeF

typeVar :: Text -> Type
typeVar = Fix . TypeVar

typeConstructor :: Text -> Type
typeConstructor = Fix . TypeConstructor

typeApp :: Type -> Type -> Type
typeApp a b = Fix (TypeApp a b)

recordCons :: Text -> Type -> Type -> Type
recordCons f t rest = Fix (RecordCons f t rest)

recordNil :: Type
recordNil = Fix RecordNil

spanTypeConstructor :: (Span, Text) -> SourceType
spanTypeConstructor (span, t) = span :< TypeConstructor t

spanTypeApp :: SourceType -> SourceType -> SourceType
spanTypeApp f@(fspan :< _) arg@(argspan :< _) =
  spanUnion fspan argspan :< TypeApp f arg

spanRecordCons :: Text -> SourceType -> SourceType -> SourceType
spanRecordCons f t@(tspan :< _) r@(rspan :< _) =
  spanUnion tspan rspan :< RecordCons f t r

--------------------------------------------------------------------------------

instance {-# OVERLAPS #-} Eq Type where
  a == b = toOrdType a == toOrdType b

instance {-# OVERLAPS #-} Ord Type where
  compare a b = compare (toOrdType a) (toOrdType b)

data OrdType
  = OTVar Text
  | OTConstructor Text
  | OTApp OrdType OrdType
  | OTRecord (Map Text OrdType)
  deriving (Eq, Ord)

toOrdType :: Type -> OrdType
toOrdType (Fix t) = case t of
  TypeVar x -> OTVar x
  TypeConstructor x -> OTConstructor x
  TypeApp (Fix (TypeConstructor "Record")) r -> OTRecord $ Map.fromList $ go r
  _ -> error "toOrdType: encounteres RecordCons or RecordNil"
  where
  go (Fix t') = case t' of
    RecordCons field ty rest -> (field, toOrdType ty) : go rest
    RecordNil                -> []
    _ -> error "toOrdType: did not encounter RecordCons or RecordNil"

--------------------------------------------------------------------------------

-- Type variables and predicates
data PolyTypeF t
  = ForAll [Text] [ConstraintF t] t
  deriving (Functor, Foldable, Traversable, Show)

type PolyType = PolyTypeF Type
type SourcePolyType = PolyTypeF SourceType

-- | Class and type, which should be member of the class
data ConstraintF t
  = IsIn Text t
  deriving (Eq, Ord, Functor, Foldable, Traversable, Show)

type Constraint = ConstraintF Type
type SourceConstraint = ConstraintF SourceType

type Instance = ([Constraint], Type)

-- Superclasses, signature of member functions, instances
type Class = ([Text], Map Text PolyType, [Instance])

--------------------------------------------------------------------------------

class HasFreeTypeVars t where
  getFtvs :: t -> Set Text

instance HasFreeTypeVars Type where
  getFtvs = cata $ \case
    TypeVar v        -> Set.singleton v
    TypeApp f arg    -> f `Set.union` arg
    RecordCons _ a b -> a `Set.union` b
    _ -> Set.empty

instance HasFreeTypeVars a => HasFreeTypeVars [a] where
  getFtvs = foldr Set.union Set.empty . map getFtvs

instance HasFreeTypeVars t => HasFreeTypeVars (PolyTypeF t) where
  getFtvs (ForAll as cs t) =
    (getFtvs t `Set.union` getFtvs cs) `Set.difference` Set.fromList as

instance HasFreeTypeVars t => HasFreeTypeVars (ConstraintF t) where
  getFtvs (IsIn _ t) = getFtvs t

instance HasFreeTypeVars v => HasFreeTypeVars (Map k v) where
  getFtvs = getFtvs . Map.elems
