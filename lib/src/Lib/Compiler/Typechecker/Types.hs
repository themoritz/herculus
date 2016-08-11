{-# LANGUAGE OverloadedStrings #-}

module Lib.Compiler.Typechecker.Types where

import           Data.List           (intercalate)
import           Data.Monoid         ((<>))
import           Data.Text (Text, pack, unpack)
import Data.Map (Map)
import qualified Data.Map as Map

import           Lib.Compiler.Parser
import           Lib.Types

import           Lib.Model.Column
import           Lib.Model.Types
import           Lib.Model

--

newtype TVar = TV Text
  deriving (Eq, Ord)

instance Show TVar where
  show (TV a) = unpack a

newtype TRecord = TR (Map (Ref Column) (Id Column, Type))
  deriving (Eq, Ord)

instance Show TRecord where
  show (TR m) = "{" <> intercalate ", " (map showEntry $ Map.toList m) <> "}"
    where showEntry (c, t) = show c <> " : " <> show t

data Type
  = TVar TVar
  | TBase Text
  | TArr Type Type
  | TList Type
  | TRecord TRecord
  deriving (Eq, Ord)

instance Show Type where
  show (TVar a) = show a
  show (TBase c) = unpack c
  show (TArr a b) = "(" <> show a <> " -> " <> show b <> ")"
  show (TList a) = "List " <> show a
  show (TRecord r) = show r

data Scheme = Forall [TVar] Type

instance Show Scheme where
  show (Forall as t) = "forall " <> intercalate " " (map show as) <> ". " <> show t

typeBool :: Type
typeBool = TBase "Bool"

typeNumber :: Type
typeNumber = TBase "Number"

typeString :: Type
typeString = TBase "String"

typeOfDataType :: DataType -> Type
typeOfDataType DataString = typeString
typeOfDataType DataNumber = typeNumber
typeOfDataType DataBool = typeBool

--

data TypedExpr = Expr Id ::: Type

--

class Monad m => MonadTypecheck m where
  resolveColumnRef :: Id Table -> Ref Column -> m (Maybe (Entity Column))
  resolveColumnOfTableRef :: Ref Table -> Ref Column -> m (Maybe (Id Table, Entity Column))
  resolveTableRef :: Ref Table -> m (Maybe (Id Table, TRecord))
