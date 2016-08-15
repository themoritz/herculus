{-# LANGUAGE OverloadedStrings #-}

module Lib.Compiler.Typechecker.Types where

import           Data.List           (intercalate)
import           Data.Map            (Map)
import           Data.Monoid         ((<>))
import           Data.Text           (Text, unpack)

import           Lib.Compiler.Parser
import           Lib.Types

import           Lib.Model
import           Lib.Model.Column
import           Lib.Model.Types

--

newtype TVar = TV Text
  deriving (Eq, Ord)

instance Show TVar where
  show (TV a) = unpack a

data TNullary
  = TBool
  | TNumber
  | TString
  deriving (Show, Eq, Ord)

data TUnary
  = TList
  | TMaybe
  deriving (Show, Eq, Ord)

data Type
  = TVar TVar
  | TNullary TNullary
  | TUnary TUnary Type
  | TArr Type Type
  | TRecord Type
  | TRow (Ref Column) Type Type
  | TNoRow
  deriving (Eq, Ord)

instance Show Type where
  show (TVar a) = show a
  show (TNullary c) = show c
  show (TUnary t t1) = show t <> " " <> show t1
  show (TArr a b) = "(" <> show a <> " -> " <> show b <> ")"
  show (TRecord r) = "{" <> show r <> "}"
  show (TRow name t r) = show name <> " : " <> show t <> ", " <> show r
  show (TNoRow) = "-"

data Scheme = Forall [TVar] Type

instance Show Scheme where
  show (Forall as t) = "forall " <> intercalate " " (map show as) <> ". " <> show t

typeOfDataType :: DataType -> Type
typeOfDataType DataString = TNullary TString
typeOfDataType DataNumber = TNullary TNumber
typeOfDataType DataBool = TNullary TBool

--

data TypedExpr = Expr Id ::: Type

--

class Monad m => MonadTypecheck m where
  resolveColumnRef :: Id Table -> Ref Column -> m (Maybe (Entity Column))
  resolveColumnOfTableRef :: Ref Table -> Ref Column -> m (Maybe (Id Table, Entity Column))
  resolveTableRef :: Ref Table -> m (Maybe (Id Table, Map (Ref Column) Type))
