{-# LANGUAGE NoImplicitPrelude #-}
-- |

module Lib.Compiler.Pretty where

import           Lib.Prelude                  hiding (empty)

import           Control.Comonad.Cofree

import           Data.Functor.Foldable
import qualified Data.Map                     as Map

import           Text.PrettyPrint.Leijen.Text

import           Lib.Compiler.AST
import           Lib.Compiler.AST.Common
import           Lib.Compiler.Parser.State
import           Lib.Compiler.Type
import           Lib.Types

prettyKind :: Kind -> Text
prettyKind = show . cata kindDoc
  where
  kindDoc = \case
    KindType      -> textStrict "Type"
    KindFun f arg -> parens $ f <+> textStrict "->" <+> arg
    KindRecord t  -> char '#' <+> t
    KindUnknown v -> int v

prettyType :: Type -> Text
prettyType = show . histo typeDoc

prettyPolyType :: PolyTypeF Type -> Text
prettyPolyType = show . polyTypeDoc . map (histo typeDoc)

--------------------------------------------------------------------------------

prettyAst :: Ast -> Text
prettyAst = show . histo astDoc

astDoc :: AstF (Cofree AstF Doc) -> Doc
astDoc = ast
  (liftAlg declarationDoc)
  (liftAlg exprDoc)
  (liftAlg binderDoc)
  (typeDoc . map (hoistCofree unsafePrj))
  (liftAlg refDoc)

constraintDoc :: ConstraintF Doc -> Doc
constraintDoc (IsIn cls ty) = textStrict cls <+> ty

polyTypeDoc :: PolyTypeF Doc -> Doc
polyTypeDoc (ForAll vars cs ty) =
  goVars <+> constraintsDoc cs <+> ty
  where
  goVars = if null vars
    then empty
    else textStrict "forall" <+>
         hsep (map textStrict vars) <> dot

constraintsDoc :: [ConstraintF Doc] -> Doc
constraintsDoc = foldr go empty where
  go (IsIn cls t) rest = textStrict cls <+> parens t <+> textStrict "=>" <+> rest

typeDoc :: TypeF (Cofree TypeF Doc) -> Doc
typeDoc = \case
  TypeVar v -> textStrict v
  TypeConstructor c -> textStrict c
  TypeApp (_ :< TypeApp (arr :< TypeConstructor "->") (a :< _)) (b :< _) ->
    parens (a <+> arr <+> b)
  TypeApp (f :< _) (arg :< _) -> parens (f <+> arg)
  RecordCons field (t :< _) (rest :< _) ->
    textStrict field <+> textStrict "::" <+> t <> comma <+> rest
  RecordNil -> empty

declarationDoc :: DeclarationF Doc -> Doc
declarationDoc = \case
  DataDecl name args constrs ->
    textStrict "data" <+> textStrict name <+> hsep (map textStrict args) <$$>
    indent 2 (vsep (map goConstr (zip prefixes constrs)))
    where
      prefixes = '=' : repeat '|'
      goConstr (p, (n, as)) = char p <+> textStrict n <+> hsep as
  ClassDecl (name, param) supers sigs ->
    textStrict "class" <+> goSupers <+> textStrict name <+> textStrict param <+> textStrict "where" <$$>
    indent 2 (vsep sigs)
    where
    goSupers = foldr goSuper empty supers
    goSuper (n, p) rest = textStrict n <+> textStrict p <+> textStrict "=>" <+> rest
  InstanceDecl (cls, t) cs vals ->
    textStrict "instance" <+> constraintsDoc cs <+>
      textStrict cls <+> t <+> textStrict "where" <$$>
    indent 2 (vsep vals)
  TypeDecl name poly ->
    textStrict name <+> textStrict "::" <+> polyTypeDoc poly
  ValueDecl name binders expr ->
    textStrict name <+> hsep binders <+> equals <$$>
    indent 2 expr
  FixityDecl x alias (Infix assoc fixity) ->
    assocDoc <+> int fixity <+> textStrict x <+> textStrict "as" <+> textStrict alias
    where
    assocDoc = case assoc of
      AssocL -> "infixl"
      AssocR -> "infixr"
      AssocN -> "infix"

binderDoc :: BinderF Doc -> Doc
binderDoc = \case
  VarBinder v            -> textStrict v
  ConstructorBinder c bs -> textStrict c <+> hsep bs

literalDoc :: LiteralF Doc -> Doc
literalDoc = \case
  NumberLit n      -> double n
  IntegerLit i     -> integer i
  StringLit s      -> dquotes $ textStrict s
  RecordLit fields -> braces $ hsep $ punctuate comma (map goField (Map.toList fields))
    where
      goField (k, v) = textStrict k <> equals <+> v

exprDoc :: ExprF Doc -> Doc
exprDoc = \case
  Literal lit -> literalDoc lit
  Abs b body ->
    backslash <+> b <+> textStrict "->" <$$>
    indent 2 body
  App f arg ->
    parens (f <+> arg)
  Var v ->
    textStrict v
  Constructor c ->
    textStrict c
  Case e cases ->
    textStrict "case" <+> e <+> textStrict "of" <$$>
    indent 2 (vsep (map goCase cases))
    where
      goCase (b, e') = b <+> textStrict "->" <+> e'
  Let bindings rest ->
    textStrict "let" <$$>
    indent 2 (vsep $ map goBinding bindings) <$$>
    textStrict "in" <$$>
    indent 2 rest
    where
      goBinding (v, body) =
        textStrict v <+> equals <$$> indent 2 body
  Accessor e field ->
    parens e <> dot <> textStrict field

refDoc :: RefTextF Doc -> Doc
refDoc = \case
  TableRef (Ref t) ->
    char '#' <> textStrict t
  ColumnRef (Ref c) ->
    char '$' <> textStrict c
  ColumnOfTableRef (Ref t) (Ref c) ->
    char '#' <> textStrict t <> dot <> textStrict c
