{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators     #-}
-- |

module Lib.Compiler.AST where

import           Lib.Prelude

import           Data.Functor.Foldable

import           Text.PrettyPrint.Leijen.Text

import           Lib.Compiler.Type
import           Lib.Model.Column
import           Lib.Model.Table
import           Lib.Types

data Module = Module [Declaration]

moduleDoc :: Module -> Doc
moduleDoc (Module ds) = vsep (map declarationDoc ds)

prettyModule :: Module -> Text
prettyModule = show . moduleDoc

data Formula = Formula [Declaration] Expr

data Declaration
  -- | Name, type arguments, constructors (name, arguments)
  = DataDecl Text [Text] [(Text, [Type])]
  -- | Name, type
  | TypeDecl Text PolyType
  -- | Name, binder, expression
  | ValueDecl Text [Binder] Expr

declarationDoc :: Declaration -> Doc
declarationDoc = \case
  DataDecl name args constrs ->
    textStrict "data" <+> textStrict name <+> hsep (map textStrict args) <$$>
    indent 2 (vsep (map goConstr (zip prefixes constrs))) <> line
    where
      prefixes = '=' : repeat '|'
      goConstr (p, (n, as)) = char p <+> textStrict n <+> hsep (map typeDoc as)
  TypeDecl name poly ->
    textStrict name <+> textStrict "::" <+> polyTypeDoc poly
  ValueDecl name binders expr ->
    textStrict name <+> hsep (map binderDoc binders) <+> equals <$$>
    indent 2 (exprDoc expr) <> line

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
  deriving (Functor)

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

binderDoc :: Binder -> Doc
binderDoc = \case
  VarBinder v            -> textStrict v
  ConstructorBinder c bs -> textStrict c <+> hsep (map binderDoc bs)

data LiteralF a
  = NumberLit Double
  | IntegerLit Integer
  | StringLit Text
  | BoolLit Bool
  | RecordLit [(Text, a)]
  deriving (Eq, Ord, Show, Functor)

literalDoc :: LiteralF Doc -> Doc
literalDoc = \case
  NumberLit n      -> double n
  IntegerLit i     -> integer i
  StringLit s      -> dquotes $ textStrict s
  BoolLit b        -> textStrict $ if b then "True" else "False"
  RecordLit fields -> braces $ hsep $ punctuate comma (map goField fields)
    where
      goField (k, v) = textStrict k <+> equals <+> v

exprDoc :: Expr -> Doc
exprDoc = cata $ \case
  Literal lit -> literalDoc lit
  Abs b body ->
    backslash <+> binderDoc b <+> textStrict "->" <$$>
    indent 2 body
  App f arg ->
    parens f <+> arg
  Var v ->
    textStrict v
  Constructor c ->
    textStrict c
  Case e cases ->
    textStrict "case" <+> e <+> textStrict "of" <$$>
    indent 2 (vsep (map goCase cases))
    where
      goCase (b, e') = binderDoc b <+> textStrict "->" <+> e'
  Let v body rest ->
    textStrict "let" <$$>
    indent 2 (textStrict v <+> equals <$$> indent 2 body) <$$>
    textStrict "in" <$$>
    indent 2 rest
  Accessor e (Ref field) ->
    parens e <> dot <> textStrict field
  TableRef (Ref t) ->
    char '#' <> textStrict t
  ColumnRef (Ref c) ->
    char '$' <> textStrict c
  ColumnOfTableRef (Ref t) (Ref c) ->
    char '#' <> textStrict t <> dot <> textStrict c

prettyExpr :: Expr -> Text
prettyExpr = show . exprDoc
