{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveTraversable    #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- |

module Lib.Compiler.Type where

import           Lib.Prelude               hiding (empty)

import           Control.Comonad.Cofree
import           Control.Lens              hiding ((:<))

import           Data.Functor.Foldable
import           Data.List                 (nub)
import qualified Data.Map                  as Map
import qualified Data.Set                  as Set

import           Lib.Model.Table
import           Lib.Types

import           Lib.Compiler.AST.Common
import           Lib.Compiler.AST.Position

data KindF a
  = KindType
  | KindFun a a
  | KindUnknown Int
  | KindTable
  | KindRecord
  deriving (Functor, Foldable, Traversable, Show)

type Kind = Fix KindF

kindType :: Kind
kindType = Fix KindType

kindFun :: Kind -> Kind -> Kind
kindFun f arg = Fix (KindFun f arg)

kindUnknown :: Int -> Kind
kindUnknown = Fix . KindUnknown

kindTable :: Kind
kindTable = Fix KindTable

kindRecord :: Kind
kindRecord = Fix KindRecord

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
  | TypeTable (RefOrId Table)
  | TypeRecord (Map Text a)
  deriving (Functor, Foldable, Traversable, Show)

type Type = Fix TypeF
type SourceType = WithSpan TypeF

typeVar :: Text -> Type
typeVar = Fix . TypeVar

typeConstructor :: Text -> Type
typeConstructor = Fix . TypeConstructor

typeApp :: Type -> Type -> Type
typeApp a b = Fix (TypeApp a b)

typeTable :: RefOrId Table -> Type
typeTable = Fix . TypeTable

typeRecord :: Map Text Type -> Type
typeRecord = Fix . TypeRecord

spanTypeConstructor :: (Span, Text) -> SourceType
spanTypeConstructor (span, t) = span :< TypeConstructor t

spanTypeApp :: SourceType -> SourceType -> SourceType
spanTypeApp f@(fspan :< _) arg@(argspan :< _) =
  spanUnion fspan argspan :< TypeApp f arg

--------------------------------------------------------------------------------

instance {-# OVERLAPS #-} Eq Kind where
  a == b = toOrdKind a == toOrdKind b

data OrdKind
  = OKType
  | OKFun OrdKind OrdKind
  | OKUnknown Int
  | OKTable
  | OKRecord
  deriving (Eq, Ord)

toOrdKind :: Kind -> OrdKind
toOrdKind = cata $ \case
  KindType -> OKType
  KindFun a b -> OKFun a b
  KindUnknown i -> OKUnknown i
  KindTable -> OKTable
  KindRecord -> OKRecord

instance {-# OVERLAPS #-} Eq Type where
  a == b = toOrdType a == toOrdType b

instance {-# OVERLAPS #-} Ord Type where
  compare a b = compare (toOrdType a) (toOrdType b)

data OrdType
  = OTVar Text
  | OTConstructor Text
  | OTApp OrdType OrdType
  | OTRecord (Map Text OrdType)
  | OTTable (RefOrId Table)
  deriving (Eq, Ord)

toOrdType :: Type -> OrdType
toOrdType = cata $ \case
  TypeVar x -> OTVar x
  TypeConstructor x -> OTConstructor x
  TypeApp f arg -> OTApp f arg
  TypeRecord m -> OTRecord m
  TypeTable t -> OTTable t

--------------------------------------------------------------------------------

-- Type variables and predicates
data PolyTypeF t
  = ForAll [Text] [ConstraintF t] t
  deriving (Eq, Functor, Foldable, Traversable, Show)

type PolyType = PolyTypeF Type
type SourcePolyType = PolyTypeF SourceType

normalizePoly :: PolyType -> PolyType
normalizePoly (ForAll as cs t) = ForAll as' cs' t'
  where
  -- Order is important therefore we can't use `getFtvs`
  getVarsT = cata $ \case
    TypeVar v -> [v]
    TypeConstructor _ -> []
    TypeApp f arg -> f <> arg
    TypeRecord m -> mconcat $ Map.elems m
    TypeTable _ -> []
  getVarsC (IsIn _ ty) = getVarsT ty
  getVarsC (HasFields m ty) = mconcat (Map.elems $ map getVarsT m) <> getVarsT ty
  getVarsCS = nub . join . map getVarsC
  vs = [ v | v <- nub (getVarsCS (sort cs) <> getVarsT t), v `elem` as ]
  as' = map snd zipped
  zipped = zip vs (map show [0 :: Int ..])
  sub = map typeVar $ Map.fromList zipped
  cs' = applyTypeSubst sub (sort cs)
  t' = applyTypeSubst sub t

quantify :: [Text] -> [Constraint] -> Type -> PolyType
quantify as cs t = ForAll as' cs' t
  where
  cs' = untilFixed (map fst . filter needed . allWithRest) cs
  needed (c, rest) = case c of
    IsIn _ t'      -> checkType rest t'
    HasFields _ t' -> checkType rest t'
  checkType :: [Constraint] -> Type -> Bool
  checkType rest = cata $ \case
    TypeVar v -> v `elem` (fieldFtvss rest <> getFtvs t)
    TypeApp f _ -> f
  fieldFtvs = \case
    IsIn _ _ -> Set.empty
    HasFields f _ -> getFtvs f
  fieldFtvss = Set.unions . map fieldFtvs
  as' = [a | a <- as, a `elem` (getFtvs cs' <> getFtvs t)]

  untilFixed :: Eq a => (a -> a) -> a -> a
  untilFixed f a = let a' = f a in if a' == a then a' else untilFixed f a'

  allWithRest :: [a] -> [(a, [a])]
  allWithRest = go []
    where go _     []     = []
          go front (x:xs) = (x, front <> xs) : go (front <> [x]) xs

data ConstraintF t
  -- | Class and type that should be member of the class
  = IsIn Text t
  | HasFields (Map Text t) t
  deriving (Eq, Ord, Functor, Foldable, Traversable, Show)

type Constraint = ConstraintF Type
type SourceConstraint = ConstraintF SourceType

type ConstraintToSolve = (Span, Constraint)

--------------------------------------------------------------------------------

class HasFreeTypeVars t where
  getFtvs :: t -> Set Text

instance HasFreeTypeVars Type where
  getFtvs = cata $ \case
    TypeVar v     -> Set.singleton v
    TypeApp f arg -> f `Set.union` arg
    TypeRecord m  -> Set.unions $ Map.elems m
    _             -> Set.empty

instance HasFreeTypeVars a => HasFreeTypeVars [a] where
  getFtvs = foldr Set.union Set.empty . map getFtvs

instance HasFreeTypeVars t => HasFreeTypeVars (PolyTypeF t) where
  getFtvs (ForAll as cs t) =
    (getFtvs t `Set.union` getFtvs cs) `Set.difference` Set.fromList as

instance HasFreeTypeVars t => HasFreeTypeVars (ConstraintF t) where
  getFtvs (IsIn _ t)      = getFtvs t
  getFtvs (HasFields m t) = getFtvs (Map.elems m) `Set.union` getFtvs t

instance HasFreeTypeVars v => HasFreeTypeVars (Map k v) where
  getFtvs = getFtvs . Map.elems

--------------------------------------------------------------------------------

type KindSubst = Map Int Kind

kindSubstAfter :: KindSubst -> KindSubst -> KindSubst
kindSubstAfter s1 s2 = map (applyKindSubst s1) s2 `Map.union` s1

applyKindSubst :: KindSubst -> Kind -> Kind
applyKindSubst sub = cata $ \case
  KindUnknown i -> Map.findWithDefault (kindUnknown i) i sub
  other         -> Fix other

type TypeSubst = Map Text Type

typeSubstAfter :: TypeSubst -> TypeSubst -> TypeSubst
typeSubstAfter s1 s2 = map (applyTypeSubst s1) s2 `Map.union` s1

typeSubstMerge :: TypeSubst -> TypeSubst -> Maybe TypeSubst
typeSubstMerge s1 s2 = if agree then pure $ s2 `Map.union` s1 else Nothing
  where agree = all (\x -> Map.lookup x s1 == Map.lookup x s2) $
                Map.keys $ s2 `Map.intersection` s1

class TypeSubstitutable t where
  applyTypeSubst :: Map Text Type -> t -> t

instance TypeSubstitutable Type where
  applyTypeSubst s = cata $ \case
    TypeVar x -> Map.findWithDefault (typeVar x) x s
    other     -> Fix other

instance TypeSubstitutable a => TypeSubstitutable [a] where
  applyTypeSubst s = map (applyTypeSubst s)

instance TypeSubstitutable a => TypeSubstitutable (ConstraintF a) where
  applyTypeSubst s = \case
    IsIn c t ->
      IsIn c (applyTypeSubst s t)
    HasFields m t ->
      HasFields (applyTypeSubst s m) (applyTypeSubst s t)

instance TypeSubstitutable a => TypeSubstitutable (PolyTypeF a) where
  applyTypeSubst s = map (applyTypeSubst s)

instance TypeSubstitutable v => TypeSubstitutable (Map k v) where
  applyTypeSubst s = map (applyTypeSubst s)

instance TypeSubstitutable ConstraintToSolve where
  applyTypeSubst s (span, c) = (span, applyTypeSubst s c)

--------------------------------------------------------------------------------

data Instance = Instance
  { _instanceConstraints :: [Constraint]
  , _instanceHeadClass   :: Text
  , _instanceHeadType    :: Type
  }

makeLenses ''Instance

data Class = Class
  { _classSupers    :: [Text]
  , _classParam     :: Text
  , _classKind      :: Kind
  , _classMethods   :: Map Text PolyType
  , _classInstances :: [Instance]
  }

makeLenses ''Class

data TyconInfo = TyconInfo
  { tyconKind         :: Kind
  , tyconParams       :: [Text]
  , tyconValueConstrs :: [(Text, [Type])]
  }
