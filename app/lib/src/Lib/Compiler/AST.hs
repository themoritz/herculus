{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable         #-}
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
import           Lib.Compiler.Parser.State
import           Lib.Compiler.Type
import           Lib.Model.Column
import           Lib.Model.Table
import           Lib.Types

-- List of declarations
type Module = [SourceAst]

-- List of declarations, expression
type Formula = ([SourceAst], SourceAst)

--------------------------------------------------------------------------------

type AstF =
  DeclarationF :+: ExprF :+: BinderF :+: TypeF :+: RefTextF

instance Show a => Show (AstF a) where
  show = ast show show show show show

ast
  :: (DeclarationF a -> b)
  -> (ExprF a -> b)
  -> (BinderF a -> b)
  -> (TypeF a -> b)
  -> (RefTextF a -> b)
  -> (AstF a -> b)
ast d e b t r = coproduct d $ coproduct e $ coproduct b $ coproduct t r

type Ast = Fix AstF
type SourceAst = WithSpan AstF

--------------------------------------------------------------------------------

type IntermedF = ExprF :+: BinderF :+: TypeF :+: ClassF :+: RefIdF
type Intermed = Fix IntermedF

intermed
  :: (ExprF a -> b)
  -> (BinderF a -> b)
  -> (TypeF a -> b)
  -> (ClassF a -> b)
  -> (RefIdF a -> b)
  -> (IntermedF a -> b)
intermed e b t c r = coproduct e $ coproduct b $ coproduct t $ coproduct c r

type CompiledF = ExprF :+: BinderF :+: RefIdF
type Compiled = Fix CompiledF

compiled
  :: (ExprF a -> b)
  -> (BinderF a -> b)
  -> (RefIdF a -> b)
  -> (CompiledF a -> b)
compiled e b r = coproduct e $ coproduct b r

--------------------------------------------------------------------------------

data DeclarationF a
  -- | Name, type arguments, constructors (name, arguments)
  = DataDecl Text [Text] [(Text, [a])]
  -- | (Classname, param), superclasses (name, param), method signatures
  -- (type declaration)
  | ClassDecl (Text, Text) [(Text, Text)] [a]
  -- | Head, constraints, methods (value declaration)
  | InstanceDecl (ConstraintF a) [ConstraintF a] [a]
  -- | Type signature: Name, type
  | TypeDecl Text (PolyTypeF a)
  -- | Name, binders, expression
  | ValueDecl Text [a] a
  -- | Variable, alias, fixity
  | FixityDecl Text Text Fixity
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
  | Accessor a Text
  deriving (Functor, Foldable, Traversable, Show)

type Expr = Fix ExprF
type SourceExpr = WithSpan ExprF

type ExprBinderF = ExprF :+: BinderF

literal :: ExprF :<: f => LiteralF (Fix f) -> Fix f
literal = Fix . inj . Literal

abs :: ExprF :<: f => Fix f -> Fix f -> Fix f
abs b body = Fix (inj $ Abs b body)

app :: ExprF :<: f => Fix f -> Fix f -> Fix f
app f arg = Fix (inj (App f arg))

var :: ExprF :<: f => Text -> Fix f
var = Fix . inj . Var

constructor :: ExprF :<: f => Text -> Fix f
constructor = Fix . inj . Constructor

case' :: ExprF :<: f => Fix f -> [(Fix f, Fix f)] -> Fix f
case' scrut alts = Fix (inj (Case scrut alts))

let' :: ExprF :<: f => [(Text, Fix f)] -> Fix f -> Fix f
let' defs body = Fix (inj (Let defs body))

accessor :: ExprF :<: f => Fix f -> Text -> Fix f
accessor e field = Fix (inj (Accessor e field))

spanAbs
  :: ExprF :<: f
  => WithSpan f -> WithSpan f -> WithSpan f
spanAbs b@(bspan :< _) body@(bodyspan :< _) =
  spanUnion bspan bodyspan :< inj (Abs b body)

spanAccessor
  :: ExprF :<: f
  => WithSpan f -> (Span, Text) -> WithSpan f
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
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

type Binder = Fix BinderF
type SourceBinder = WithSpan BinderF

varBinder :: (BinderF :<: f) => Text -> Fix f
varBinder = Fix . inj . VarBinder

--------------------------------------------------------------------------------

data ClassF a
  = Constrained [ConstraintF a] a
  | TypeClassDict Span (ConstraintF a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

typeClassDict :: ClassF :<: f => Span -> ConstraintF (Fix f) -> Fix f
typeClassDict span c = Fix $ inj $ TypeClassDict span c

--------------------------------------------------------------------------------

data LiteralF a
  = NumberLit Double
  | IntegerLit Integer
  | StringLit Text
  | RecordLit (Map Text a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

--------------------------------------------------------------------------------

data RefF t c a
  = TableRef t
  | ColumnRef c
  | ColumnOfTableRef t c
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

type RefTextF = RefF (Ref Table) (Ref Column)
type RefIdF = RefF (Id Table) (Id Column)
