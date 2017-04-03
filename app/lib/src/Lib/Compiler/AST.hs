{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}
-- |

module Lib.Compiler.AST where

import           Lib.Prelude

import           Control.Comonad.Cofree

import           Data.Functor.Foldable

import           Lib.Compiler.AST.Common
import           Lib.Compiler.AST.Position
import           Lib.Compiler.Type
import           Lib.Model.Column
import           Lib.Model.Table
import           Lib.Types

data AstF a
  = InjDecl (DeclarationF a)
  | InjExpr (ExprF a)
  | InjBinder (BinderF a)
  | InjType (TypeF a)
  deriving (Functor)

type Ast = Fix AstF
type SourceAst = WithSource AstF

unsafeType :: AstF a -> TypeF a
unsafeType (InjType t) = t

--------------------------------------------------------------------------------

data DeclarationF a
  -- | Name, type arguments, constructors (name, arguments)
  = DataDecl Text [Text] [(Text, [a])]
  -- | Name, type
  | TypeDecl Text (PolyType a)
  -- | Name, binder, expredssion
  | ValueDecl Text [a] a
  deriving (Functor)

type Declaration = Fix DeclarationF
type SourceDeclaration = WithSource DeclarationF

--------------------------------------------------------------------------------

data ExprF a
  = Literal (LiteralF a)
  -- | Binder, expression
  | Abs a a
  | App a a
  | Var Text
  | Constructor Text
  -- | Scrutinee, list of alternatives (binder, expression)
  | Case a [(a, a)]
  | Let Text a a
  | Accessor a (Ref Column)
  | TableRef (Ref Table)
  | ColumnRef (Ref Column)
  | ColumnOfTableRef (Ref Table) (Ref Column)
  deriving (Functor)

type Expr = Fix ExprF
type SourceExpr = WithSource ExprF

mkSourceAbs :: SourceAst -> SourceAst -> SourceAst
mkSourceAbs b@(bspan :< _) body@(bodyspan :< _) =
  sourceUnion bspan bodyspan :< InjExpr (Abs b body)

mkSourceAccessor :: SourceAst -> (SourceSpan, Ref Column) -> SourceAst
mkSourceAccessor e@(espan :< _) (span, ref) =
  sourceUnion espan span :< InjExpr (Accessor e ref)

mkSourceApp :: SourceAst -> SourceAst -> SourceAst
mkSourceApp f@(fspan :< _) arg@(argspan :< _) =
  sourceUnion fspan argspan :< InjExpr (App f arg)

mkSourceVar :: (SourceSpan, Text) -> SourceAst
mkSourceVar (span, v) = span :< InjExpr (Var v)

--------------------------------------------------------------------------------

data BinderF a
  = VarBinder Text
  | ConstructorBinder Text [a]
  deriving (Eq, Ord, Show, Functor)

type Binder = Fix BinderF
type SourceBinder = WithSource BinderF

--------------------------------------------------------------------------------

data LiteralF a
  = NumberLit Double
  | IntegerLit Integer
  | StringLit Text
  | BoolLit Bool
  | RecordLit [(Text, a)]
  deriving (Eq, Ord, Show, Functor)
