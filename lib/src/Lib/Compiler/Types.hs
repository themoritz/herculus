{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TupleSections  #-}
{-# LANGUAGE FlexibleInstances  #-}

module Lib.Compiler.Types where

import           Control.DeepSeq

import           Data.Aeson
import Data.List (intercalate)
import           Data.Monoid
import           Data.Text                    (Text, unpack)

import           GHC.Generics

import {-# SOURCE #-} Lib.Model.Column
import           Lib.Model.Dependencies.Types
import           Lib.Model.Table
import           Lib.Types
import qualified Data.UnionFind.IntMap as UF


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

data Point = Point (UF.Point (MonoType Point))
data Type = Type (MonoType Type)

data MonoType a
  = TyVar TypeVar
  | TyConst TypeConst
  | TyApp a a
  | TyRecord a
  | TyRecordCons (Ref Column) a a
  | TyRecordNil

instance Eq a => Eq (MonoType a) where
  TyVar a == TyVar b = a == b
  TyApp c1 a1 == TyApp c2 a2 = c1 == c2 && a1 == a2
  TyRecord s == TyRecord t = False --rowMap s == rowMap t
    -- where rowMap :: a -> Map (Ref Column) a
    --       rowMap = Map.fromList . go
    --       go TyRecordNil = []
    --       go (TyVar _) = []
    --       go (TyRecordCons n t' rest) = (n,t') : go rest
  TyRecordCons _ _ _ == TyRecordCons _ _ _ = error "eq SimpleType: should not happen"
  TyRecordNil == TyRecordNil = True
  _ == _ = False

-- "forall a. ..."
data PolyType a = ForAll [TypeVar] [Predicate a] a

type Class a = [Predicate a]

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

instance Show (PolyType Type) where
  show (ForAll as preds t) =
      "forall " <> intercalate " " (map show as) <> ". " <>
      "(" <> intercalate ", " (map showPred preds) <> ") " <>
      "=> " <> show t
    where showPred (IsIn cls t') = show cls <> " " <> show t'

--

data Lit
  = LNumber Number
  | LBool Bool
  | LString Text
  deriving (Show, Eq, Ord, Generic, NFData)

instance ToJSON Lit
instance FromJSON Lit

data Binop
  = Add
  | Sub
  | Mul
  | Div
  | Equal
  | NotEqual
  | LessEq
  | GreaterEq
  | Less
  | Greater
  | And
  | Or
  deriving (Eq, Ord, Show, Generic, NFData)

instance ToJSON Binop
instance FromJSON Binop

data PExpr
  = PLam Name PExpr
  | PApp PExpr PExpr
  | PLet Name PExpr PExpr
  -- | PFix Expr
  | PIf PExpr PExpr PExpr
  | PVar Name
  | PLit Lit
  | PBinop Binop PExpr PExpr
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
  | TBinop Binop TExpr TExpr
  | TPrjRecord TExpr (Ref Column)
  -- Temporary:
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
