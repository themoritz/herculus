{-# LANGUAGE NoImplicitPrelude #-}
-- |

module Lib.Compiler.Pretty where

import           Lib.Prelude                  hiding (empty)

import           Control.Comonad.Cofree

import           Data.Functor.Foldable

import           Text.PrettyPrint.Leijen.Text

import           Lib.Compiler.AST
import           Lib.Compiler.AST.Common
import           Lib.Compiler.Type
import           Lib.Types

prettyAst :: Ast -> Text
prettyAst = show . histo astDoc

astDoc :: AstF (Cofree AstF Doc) -> Doc
astDoc = \case
  InjDecl d   -> liftAlg declarationDoc d
  InjExpr e   -> liftAlg exprDoc e
  InjBinder b -> liftAlg binderDoc b
  InjType t   -> typeDoc (map (mapCofree unsafeType) t)

predicateDoc :: Predicate Doc -> Doc
predicateDoc (IsIn cls ty) = textStrict cls <+> ty

polyTypeDoc :: PolyType Doc -> Doc
polyTypeDoc (ForAll vars preds ty) =
  goVars <+> goPreds <+> ty
  where
  goVars = if null vars
    then empty
    else textStrict "forall" <+>
         hsep (map textStrict vars) <> dot
  goPreds = foldr goPred empty preds
  goPred (IsIn cls t) rest =
    textStrict cls <+> parens t <+> textStrict "=>" <+> rest

typeDoc :: TypeF (Cofree TypeF Doc) -> Doc
typeDoc = \case
  TypeVar v -> textStrict v
  TypeConstructor c -> textStrict c
  TypeApp (_ :< TypeApp (arr :< TypeConstructor "->") (a :< _)) (b :< _) ->
    parens (a <+> arr <+> b)
  TypeApp (f :< _) (arg :< _) -> parens (f <+> arg)
  RecordCons (Ref ref) (t :< _) (rest :< _) ->
    textStrict ref <+> textStrict "::" <+> t <> comma <+> rest
  RecordNil -> empty

declarationDoc :: DeclarationF Doc -> Doc
declarationDoc = \case
  DataDecl name args constrs ->
    textStrict "data" <+> textStrict name <+> hsep (map textStrict args) <$$>
    indent 2 (vsep (map goConstr (zip prefixes constrs))) <> line
    where
      prefixes = '=' : repeat '|'
      goConstr (p, (n, as)) = char p <+> textStrict n <+> hsep as
  TypeDecl name poly ->
    textStrict name <+> textStrict "::" <+> polyTypeDoc poly
  ValueDecl name binders expr ->
    textStrict name <+> hsep binders <+> equals <$$>
    indent 2 expr <> line

binderDoc :: BinderF Doc -> Doc
binderDoc = \case
  VarBinder v            -> textStrict v
  ConstructorBinder c bs -> textStrict c <+> hsep bs

literalDoc :: LiteralF Doc -> Doc
literalDoc = \case
  NumberLit n      -> double n
  IntegerLit i     -> integer i
  StringLit s      -> dquotes $ textStrict s
  BoolLit b        -> textStrict $ if b then "True" else "False"
  RecordLit fields -> braces $ hsep $ punctuate comma (map goField fields)
    where
      goField (k, v) = textStrict k <+> equals <+> v

exprDoc :: ExprF Doc -> Doc
exprDoc = \case
  Literal lit -> literalDoc lit
  Abs b body ->
    backslash <+> b <+> textStrict "->" <$$>
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
      goCase (b, e') = b <+> textStrict "->" <+> e'
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
