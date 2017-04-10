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
type SourceAst = WithSpan AstF

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
type SourceDeclaration = WithSpan DeclarationF

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
  | Let [(Text, a)] a
  | Accessor a (Ref Column)
  | TableRef (Ref Table)
  | ColumnRef (Ref Column)
  | ColumnOfTableRef (Ref Table) (Ref Column)
  deriving (Functor, Show)

type Expr = Fix ExprF
type SourceExpr = WithSpan ExprF

type ExprBinderF = ExprF :+: BinderF

spanAbs
  :: ExprF :<: f
  => WithSpan f -> WithSpan f -> WithSpan f
spanAbs b@(bspan :< _) body@(bodyspan :< _) =
  spanUnion bspan bodyspan :< inj (Abs b body)

spanAccessor
  :: ExprF :<: f
  => WithSpan f -> (Span, Ref Column) -> WithSpan f
spanAccessor e@(espan :< _) (span, ref) =
  spanUnion espan span :< inj (Accessor e ref)

spanApp
  :: ExprF :<: f
  => WithSpan f -> WithSpan f -> WithSpan f
spanApp f@(fspan :< _) arg@(argspan :< _) =
  spanUnion fspan argspan :< inj (App f arg)

spanVar
  :: ExprF :<: f
  => (Span, Text) -> WithSpan f
spanVar (span, v) = span :< inj (Var v)

--------------------------------------------------------------------------------

data BinderF a
  = VarBinder Text
  | ConstructorBinder Text [a]
  deriving (Eq, Ord, Show, Functor)

type Binder = Fix BinderF
type SourceBinder = WithSpan BinderF

--------------------------------------------------------------------------------

data LiteralF a
  = NumberLit Double
  | IntegerLit Integer
  | StringLit Text
  | RecordLit [(Text, a)]
  deriving (Eq, Ord, Show, Functor)
