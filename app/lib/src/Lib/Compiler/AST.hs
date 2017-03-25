{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}
-- |

module Lib.Compiler.AST where

import           Lib.Prelude

import           Data.Functor.Foldable

import           Lib.Compiler.Type
import           Lib.Model.Column
import           Lib.Model.Table
import           Lib.Types

data Module a = Module (Maybe Text) [Declaration a]

data Declaration a
  -- | Name, type arguments, constructors (name, arguments)
  = DataDecl Text [Text] [(Text, [Type])]
  -- | Name, binder, expression
  | ValueDecl Text Binder a
  deriving (Eq, Ord, Show, Functor)

data ExprF a
  = Literal (LiteralF a)
  | Abs Binder a
  | App a a
  | Var Text
  | Constructor Text
  | Case a [(Binder, a)]
  | Let Text a a
  | Accessor a (Ref Column)
  | TableRef (Ref Table)
  | ColumnRef (Ref Column)
  | ColumnOfTableRef (Ref Table) (Ref Column)
  deriving (Eq, Ord, Show, Functor)

type Expr = Fix ExprF

mkAbs :: Binder -> Expr -> Expr
mkAbs b body = Fix (Abs b body)

mkAccessor :: Expr -> Ref Column -> Expr
mkAccessor e ref = Fix (Accessor e ref)

mkApp :: Expr -> Expr -> Expr
mkApp f arg = Fix (App f arg)

data Binder
  = VarBinder Text
  | ConstructorBinder Text [Binder]
  deriving (Eq, Ord, Show)

data LiteralF a
  = NumberLit Double
  | StringLit Text
  | BoolLit Bool
  | RecordLit [(Text, a)]
  deriving (Eq, Ord, Show, Functor)
