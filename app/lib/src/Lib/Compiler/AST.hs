{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE PatternSynonyms       #-}
-- |

module Lib.Compiler.AST where

import           Lib.Prelude
import qualified Prelude as P (show)

import           Control.Comonad.Cofree

import           Data.Functor.Foldable

import           Lib.Compiler.AST.Common
import           Lib.Compiler.AST.Position
import           Lib.Compiler.Parse.State
import           Lib.Compiler.Type
import {-# SOURCE #-} Lib.Model.Column
import           Lib.Model.Table
import           Lib.Types

-- List of declarations
type Module = [SourceAst]

-- List of declarations, expression
type Formula = ([SourceAst], SourceAst)

--------------------------------------------------------------------------------

type AstF =
  DeclarationF :+: ExprF :+: BinderF :+: TypeF :+: RefTextF

pattern ExprPat :: ExprF a -> AstF a
pattern ExprPat e = InjR (InjL e)

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

type IntermedF = ExprF :+: BinderF :+: TypeF :+: PlaceholderF :+: RefIdF
type Intermed = Fix IntermedF

intermed
  :: (ExprF a -> b)
  -> (BinderF a -> b)
  -> (TypeF a -> b)
  -> (PlaceholderF a -> b)
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
  -- | DocString, Name, type arguments, constructors (DocString, name,
  -- arguments)
  = DataDecl Text a [a] [(Text, a, [a])]
  -- | DocString, (Classname, param), superclasses (name, param), method
  -- signatures (type declaration)
  | ClassDecl Text (a, a) [(a, a)] [a]
  -- | Head, constraints, methods (value declaration)
  | InstanceDecl (a, a) [(a, a)] [a]
  -- | Type signature: DocString, name, type
  | TypeDecl Text a (PolyTypeF a)
  -- | Name, binders, expression
  | ValueDecl a [a] a
  -- | DocString, Alias, operator, fixity
  | FixityDecl Text a a Fixity
  -- | General declaration name
  | DeclName Text
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
  | Access a a
  | Deref a
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

access :: ExprF :<: f => Fix f -> Fix f -> Fix f
access e field = Fix (inj (Access e field))

deref :: ExprF :<: f => Fix f -> Fix f
deref e = Fix (inj (Deref e))

spanAbs
  :: ExprF :<: f
  => WithSpan f -> WithSpan f -> WithSpan f
spanAbs b@(bspan :< _) body@(bodyspan :< _) =
  spanUnion bspan bodyspan :< inj (Abs b body)

spanAccess
  :: ExprF :<: f
  => WithSpan f -> WithSpan f -> WithSpan f
spanAccess e@(espan :< _) field@(fieldspan :< _) =
  spanUnion espan fieldspan :< inj (Access e field)

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
  | WildcardBinder
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

type Binder = Fix BinderF
type SourceBinder = WithSpan BinderF

varBinder :: (BinderF :<: f) => Text -> Fix f
varBinder = Fix . inj . VarBinder

--------------------------------------------------------------------------------

data PlaceholderF a
  -- | Class, type. Translate to dictionary that's in scope for the class and
  -- type combination. Constrained functions will be applied to these
  -- placeholders.
  = DictionaryPlaceholder Span Text a
  -- | Class, type, method name. Translate to dictionary selection in dictionary
  -- that's in scope.
  | MethodPlaceholder Span Text a Text
  -- | Function name, type. Recursively defined functions. Once the function has
  -- been generalized, convert this to application of `DictionaryPlaceholder`s.
  | RecursiveCallPlaceholder Span Text
  -- | Type. Similar role as method placeholder.
  | AccessPlaceholder Span a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

dictionaryPlaceholder
  :: PlaceholderF :<: f => Span -> Text -> Fix f -> Fix f
dictionaryPlaceholder span c t = Fix $ inj $ DictionaryPlaceholder span c t

methodPlaceholder
  :: PlaceholderF :<: f => Span -> Text -> Fix f -> Text -> Fix f
methodPlaceholder span c t m = Fix $ inj $ MethodPlaceholder span c t m

recursiveCallPlaceholder
  :: PlaceholderF :<: f => Span -> Text -> Fix f
recursiveCallPlaceholder span name =
  Fix $ inj $ RecursiveCallPlaceholder span name

accessPlaceholder
  :: PlaceholderF :<: f => Span -> Fix f -> Fix f
accessPlaceholder span t =
  Fix $ inj $ AccessPlaceholder span t

constrToPlaceholder
  :: (Functor g, Functor f, PlaceholderF :<: f, TypeF :<: g, TypeF :<: f)
  => Span -> ConstraintF (Fix g) -> Fix f
constrToPlaceholder span (map unsafePrjFix -> c)= case c of
  IsIn cls t -> dictionaryPlaceholder span cls (injFix (t :: Type))
  HasFields _ t -> accessPlaceholder span (injFix t)

--------------------------------------------------------------------------------

data LiteralF a
  = NumberLit Double
  | IntegerLit Integer
  | StringLit Text
  | RecordLit (Map Text a)
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

--------------------------------------------------------------------------------

data RefF i a
  = TableRef (i Table)
  | ColumnRef (i Column)
  | ColumnOfTableRef (i Table) (i Column)
  deriving (Functor, Foldable, Traversable)

deriving instance (Show (i Table), Show (i Column)) => Show (RefF i a)

type RefTextF = RefF Ref
type RefIdF = RefF Id

tableRef :: RefF i :<: f => i Table -> Fix f
tableRef = Fix . inj . TableRef

columnRef :: RefF i :<: f => i Column -> Fix f
columnRef = Fix . inj . ColumnRef

columnOfTableRef :: RefF i :<: f => i Table -> i Column -> Fix f
columnOfTableRef t c = Fix $ inj $ ColumnOfTableRef t c
