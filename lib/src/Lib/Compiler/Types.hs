{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TupleSections  #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE StandaloneDeriving  #-}

module Lib.Compiler.Types where

import           Control.DeepSeq
import Control.Monad.Except

import           Data.Aeson
import Data.List (intercalate)
import           Data.Monoid
import Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text                    (Text, unpack)

import           GHC.Generics

import {-# SOURCE #-} Lib.Model.Column
import           Lib.Model.Dependencies.Types
import           Lib.Model.Table
import           Lib.Types
import qualified Data.UnionFind.IntMap as UF


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
data Type = Type (MonoType Type)
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
  TyApp c1 a1 == TyApp c2 a2 = c1 == c2 && a1 == a2
  TyRecord (Type s) == TyRecord (Type t) = rowMap s == rowMap t
    where rowMap :: MonoType Type -> Map (Ref Column) (MonoType Type)
          rowMap = Map.fromList . go
          go (TyVar _) = []
          go (TyRecordCons n (Type t') (Type rest)) = (n,t') : go rest
          go TyRecordNil = []
  TyRecordCons _ _ _ == TyRecordCons _ _ _ = error "eq SimpleType: should not happen"
  TyRecordNil == TyRecordNil = True
  _ == _ = False

-- "forall a. ..."
data PolyType a = ForAll [TypeVar] [Predicate a] a

instance Show Type where
  show (Type t) = case t of
    TyVar a -> show a
    TyConst c -> show c
    TyApp (Type (TyApp (Type (TyConst (TypeConst "(->)" _))) a)) b -> "(" <> show a <> " -> " <> show b <> ")"
    TyApp f arg -> "(" <> show f <> " " <> show arg <> ")"
    TyRecord r -> "{" <> show r <> "}"
    TyRecordCons name t r -> show name <> " : " <> show t <> ", " <> show r
    TyRecordNil -> "-"

instance Show Kind where
  show KindStar = "*"
  show (KindFun arg res) = "(" <> show arg <> " -> " <> show res <> ")"

instance Show TypeVar where
  show (TypeVar a k) = show a -- <> " : " <> show k

instance Show TypeConst where
  show (TypeConst n k) = unpack n -- <> " : " <> show k

instance Show ClassName where
  show (ClassName name) = unpack name

instance Show a => Show (Predicate a) where
  show (IsIn cls t') = show cls <> " " <> show t'

instance Show (PolyType Type) where
  show (ForAll as preds t) =
    "forall " <> intercalate " " (map show as) <> ". " <>
    "(" <> intercalate ", " (map show preds) <> ") " <>
    "=> " <> show t

--

data Lit
  = LNumber Number
  | LBool Bool
  | LString Text
  deriving (Show, Eq, Ord, Generic, NFData)

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

data TExpr
  = TLam Name TExpr
  | TApp TExpr TExpr
  | TLet Name TExpr TExpr
  --T | Fix Expr
  | TIf TExpr TExpr TExpr
  | TVar Name
  | TLit Lit
  | TPrjRecord TExpr (Ref Column)
  | TTypeClassDict (Predicate Point)
  --
  | TColumnRef (Id Column)
  | TWholeColumnRef (Id Column)
  | TTableRef (Id Table) [Id Column]

-- Core language
data CExpr
  = CLam Name CExpr
  | CApp CExpr CExpr
  | CLet Name CExpr CExpr
  | CIf CExpr CExpr CExpr
  | CVar Name
  | CLit Lit
  | CPrjRecord CExpr (Ref Column)
  --
  | CColumnRef (Id Column)
  | CWholeColumnRef (Id Column)
  | CTableRef (Id Table) [Id Column]
  deriving (Show, Eq, Generic, NFData)

instance ToJSON CExpr
instance FromJSON CExpr

toCoreExpr :: MonadError TypeError m => TExpr -> m CExpr
toCoreExpr = \case
  TLam x e -> CLam x <$> toCoreExpr e
  TApp f arg -> CApp <$> toCoreExpr f <*> toCoreExpr arg
  TLet x e body -> CLet x <$> toCoreExpr e <*> toCoreExpr body
  TIf c t e -> CIf <$> toCoreExpr c <*> toCoreExpr t <*> toCoreExpr e
  TVar x -> pure $ CVar x
  TLit l -> pure $ CLit l
  TPrjRecord e r -> CPrjRecord <$> toCoreExpr e <*> pure r
  TTypeClassDict _ -> throwError "TTypeClassDict should have been eliminated before converting to core"
  TColumnRef c -> pure $ CColumnRef c
  TWholeColumnRef c -> pure $ CWholeColumnRef c
  TTableRef t cs -> pure $ CTableRef t cs

--

collectDependencies :: CExpr -> [(Id Column, DependencyType)]
collectDependencies = go
  where go e' = case e' of
          CLam _ body       -> go body
          CApp f e          -> go f <> go e
          CLet _ e body     -> go e <> go body
          CIf c t e         -> go c <> go t <> go e
          CVar _            -> []
          CLit _            -> []
          CPrjRecord e _    -> go e
          CColumnRef c      -> [(c, OneToOne)]
          CWholeColumnRef c -> [(c, OneToAll)]
          CTableRef _ cs    -> map (,OneToAll) cs
