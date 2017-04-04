{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections      #-}

module Lib.Compiler.Types where

import           Lib.Prelude                  hiding (show)

import           Data.Aeson
import           Data.Functor.Foldable
import           Data.List                    (intercalate, unwords)
import qualified Data.Map                     as Map
import qualified Data.Set                     as Set
import           Data.Text                    (Text, unpack)
import qualified Data.UnionFind.IntMap        as UF

import           GHC.Show                     (Show (..))

import {-# SOURCE #-} Lib.Model.Column
import           Lib.Model.Dependencies.Types
import           Lib.Model.Table
import           Lib.Types


type TypeError = Text

data Kind
  = KindStar
  | KindFun Kind Kind
  deriving (Eq, Ord)

data TypeVar = TypeVar Int Kind
  deriving (Eq, Ord)

data TypeConst = TypeConst Text Kind
  deriving (Eq, Ord)

newtype ClassName = ClassName Text
  deriving (Eq, Ord)

data Predicate a = IsIn ClassName a
  deriving (Eq, Ord)

data Point = Point (UF.Point (MonoType Point))
data Type = Type { unType :: MonoType Type }
  deriving (Eq, Ord)

data MonoType a
  = TyVar TypeVar
  | TyConst TypeConst
  | TyApp a a
  | TyRecord a
  | TyRecordCons (Ref Column) a a
  | TyRecordNil

deriving instance Ord (MonoType Type)

instance Eq (MonoType Type) where
  TyVar a == TyVar b = a == b
  TyConst a == TyConst b = a == b
  TyApp c1 a1 == TyApp c2 a2 = c1 == c2 && a1 == a2
  TyRecord (Type s) == TyRecord (Type t) = rowMap s == rowMap t
    where rowMap :: MonoType Type -> Map (Ref Column) (MonoType Type)
          rowMap = Map.fromList . go
          go (TyVar _) = []
          go (TyRecordCons n (Type t') (Type rest)) = (n,t') : go rest
          go TyRecordNil = []
          go _ = error "eq MonoType: go: should not happen"
  TyRecordCons{} == TyRecordCons{} = error "eq MonoType: should not happen"
  TyRecordNil == TyRecordNil = True
  _ == _ = False

data PolyType a = ForAll [TypeVar] [Predicate a] a

instance Ord a => Eq (PolyType a) where
  ForAll varsa predsa a == ForAll varsb predsb b =
    Set.fromList varsa == Set.fromList varsb &&
    Set.fromList predsa == Set.fromList predsb &&
    a == b

instance Show Type where
  show (Type t) = case t of
    TyVar a -> show a
    TyConst c -> show c
    TyApp (Type (TyApp (Type (TyConst (TypeConst "(->)" _))) a)) b -> "(" <> show a <> " -> " <> show b <> ")"
    TyApp f arg -> "(" <> show f <> " " <> show arg <> ")"
    TyRecord r -> "{" <> show r <> "}"
    TyRecordCons name t' r -> show name <> " : " <> show t' <> ", " <> show r
    TyRecordNil -> "-"

instance Show Kind where
  show KindStar          = "*"
  show (KindFun arg res) = "(" <> show arg <> " -> " <> show res <> ")"

instance Show TypeVar where
  show (TypeVar a _) = show a -- <> " : " <> show k

instance Show TypeConst where
  show (TypeConst n _) = unpack n -- <> " : " <> show k

instance Show ClassName where
  show (ClassName name) = unpack name

instance Show a => Show (Predicate a) where
  show (IsIn cls t') = show cls <> " " <> show t'

instance Show (PolyType Type) where
  show (ForAll as preds t) =
    "forall " <> unwords (map show as) <> ". " <>
    "(" <> intercalate ", " (map show preds) <> ") " <>
    "=> " <> show t

--

data Lit
  = LNumber Number
  | LBool Bool
  | LString Text
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Lit
instance FromJSON Lit

data PExpr
  = PLam Name PExpr
  | PApp PExpr PExpr
  | PLet Name PExpr PExpr
  -- | PFix Expr
  | PIf PExpr PExpr PExpr
  | PVar Name
  | PLit Lit
  | PPrjRecord PExpr (Ref Column)
  --
  | PColumnRef (Ref Column)
  | PColumnOfTableRef (Ref Table) (Ref Column)
  | PTableRef (Ref Table)
  deriving (Eq, Show)

data TExprF a
  = TLam Name a
  | TApp a a
  | TLet Name a a
  -- | Fix Expr
  | TIf a a a
  | TVar Name
  | TLit Lit
  | TPrjRecord a (Ref Column)
  -- For type classes:
  | TWithPredicates [Predicate Point] a
  | TTypeClassDict (Predicate Point)
  --
  | TColumnRef (Id Column)
  | TWholeColumnRef (Id Table) (Id Column)
  | TTableRef (Id Table)
  deriving (Functor, Foldable, Traversable)

type TExpr = Fix TExprF

tLam :: Name -> TExpr -> TExpr
tLam n e = Fix (TLam n e)

tApp :: TExpr -> TExpr -> TExpr
tApp f arg = Fix (TApp f arg)

tVar :: Name -> TExpr
tVar = Fix . TVar

tWithPredicates :: [Predicate Point] -> TExpr -> TExpr
tWithPredicates p e = Fix (TWithPredicates p e)

tTypeClassDict :: Predicate Point -> TExpr
tTypeClassDict = Fix . TTypeClassDict

-- Core language
data CExpr
  = CLam Name CExpr
  | CApp CExpr CExpr
  | CLet Name CExpr CExpr
  | CIf CExpr CExpr CExpr -- desugar
  | CVar Name
  | CLit Lit
  | CPrjRecord CExpr (Ref Column) -- desugar
  --
  | CColumnRef (Id Column)
  | CWholeColumnRef (Id Table) (Id Column)
  | CTableRef (Id Table)
  deriving (Show, Eq, Generic)

instance ToJSON CExpr
instance FromJSON CExpr

toCoreExpr :: MonadError TypeError m => TExpr -> m CExpr
toCoreExpr = cataM $ \case
  TLam x e            -> pure $ CLam x e
  TApp f arg          -> pure $ CApp f arg
  TLet x e body       -> pure $ CLet x e body
  TIf c t e           -> pure $ CIf c t e
  TVar x              -> pure $ CVar x
  TLit l              -> pure $ CLit l
  TPrjRecord e r      -> pure $ CPrjRecord e r
  TWithPredicates _ _ -> throwError "TWithPredicates should have been eliminated before converting to core"
  TTypeClassDict _    -> throwError "TTypeClassDict should have been eliminated before converting to core"
  TColumnRef c        -> pure $ CColumnRef c
  TWholeColumnRef t c -> pure $ CWholeColumnRef t c
  TTableRef t         -> pure $ CTableRef t

--

collectCodeDependencies :: CExpr -> CodeDependencies
collectCodeDependencies = go
  where go e' = case e' of
          CLam _ body         -> go body
          CApp f e            -> go f <> go e
          CLet _ e body       -> go e <> go body
          CIf c t e           -> go c <> go t <> go e
          CVar _              -> mempty
          CLit _              -> mempty
          CPrjRecord e _      -> go e
          CColumnRef c        -> singleColumnRef c
          CWholeColumnRef t c -> singleWholeColumnRef t c
          CTableRef t         -> singleTableRef t
