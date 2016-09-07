{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE TupleSections  #-}

module Lib.Compiler.Types where

import           Control.DeepSeq

import           Data.Aeson
import           Data.Monoid
import           Data.Text                    (Text)

import           GHC.Generics

import {-# SOURCE #-} Lib.Model.Column
import           Lib.Model.Dependencies.Types
import           Lib.Model.Table
import           Lib.Types

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
  deriving (Eq, Show, Generic)

instance ToJSON PExpr
instance FromJSON PExpr

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
  --
  | TColumnRef (Id Column)
  | TWholeColumnRef (Id Column)
  | TTableRef (Id Table) [Id Column]
  deriving (Eq, Show, Generic, NFData)

instance ToJSON TExpr
instance FromJSON TExpr

--

collectDependencies :: TExpr -> [(Id Column, DependencyType)]
collectDependencies = go
  where go e' = case e' of
          TLam _ body       -> go body
          TApp f e          -> go f <> go e
          TLet _ e body     -> go e <> go body
          TIf c t e         -> go c <> go t <> go e
          TVar _            -> []
          TLit _            -> []
          TBinop _ l r      -> go l <> go r
          TPrjRecord e _    -> go e
          TColumnRef c      -> [(c, OneToOne)]
          TWholeColumnRef c -> [(c, OneToAll)]
          TTableRef _ cs    -> map (,OneToAll) cs
