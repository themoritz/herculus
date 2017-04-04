{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeSynonymInstances  #-}
-- |

module Lib.Compiler.AST where

import           Lib.Prelude
import qualified Prelude as P (show)

import           Control.Comonad.Cofree

import           Data.Functor.Foldable

import           Lib.Compiler.AST.Common
import           Lib.Compiler.AST.Position
import           Lib.Compiler.Type
import           Lib.Model.Column
import           Lib.Model.Table
import           Lib.Types

type AstF = DeclarationF :+: ExprF :+: BinderF :+: TypeF

instance Show a => Show (AstF a) where
  show = ast show show show show

ast
  :: (DeclarationF a -> b)
  -> (ExprF a -> b)
  -> (BinderF a -> b)
  -> (TypeF a -> b)
  -> (AstF a -> b)
ast d e b t = coproduct d $ coproduct e $ coproduct b t

type Ast = Fix AstF
type SourceAst = WithSource AstF

--------------------------------------------------------------------------------

data DeclarationF a
  -- | Name, type arguments, constructors (name, arguments)
  = DataDecl Text [Text] [(Text, [a])]
  -- | Name, type
  | TypeDecl Text (PolyType a)
  -- | Name, binder, expredssion
  | ValueDecl Text [a] a
  deriving (Functor, Show)

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
  deriving (Functor, Show)

type Expr = Fix ExprF
type SourceExpr = WithSource ExprF

type ExprBinderF = ExprF :+: BinderF

mkSourceAbs
  :: ExprF :<: f
  => WithSource f -> WithSource f -> WithSource f
mkSourceAbs b@(bspan :< _) body@(bodyspan :< _) =
  sourceUnion bspan bodyspan :< inj (Abs b body)

mkSourceAccessor
  :: ExprF :<: f
  => WithSource f -> (SourceSpan, Ref Column) -> WithSource f
mkSourceAccessor e@(espan :< _) (span, ref) =
  sourceUnion espan span :< inj (Accessor e ref)

mkSourceApp
  :: ExprF :<: f
  => WithSource f -> WithSource f -> WithSource f
mkSourceApp f@(fspan :< _) arg@(argspan :< _) =
  sourceUnion fspan argspan :< inj (App f arg)

mkSourceVar
  :: ExprF :<: f
  => (SourceSpan, Text) -> WithSource f
mkSourceVar (span, v) = span :< inj (Var v)

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
