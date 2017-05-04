{-# LANGUAGE NoImplicitPrelude #-}
-- |

module Lib.Compiler.Pretty
  ( prettyKind
  , prettyType
  , prettyPolyType
  , prettyConstraint
  , prettyConstraints
  , prettyAst
  , prettyIntermed
  , prettyBinder
  , recordDoc
  ) where

import           Lib.Prelude                  hiding (empty)

import           Control.Comonad.Cofree

import           Data.Functor.Foldable
import qualified Data.Map                     as Map

import           Text.PrettyPrint.Leijen.Text

import           Lib.Compiler.AST
import           Lib.Compiler.AST.Common
import           Lib.Compiler.Parse.State
import           Lib.Compiler.Type
import           Lib.Types

prettyKind :: Kind -> Text
prettyKind = show . cata kindDoc
  where
  kindDoc = \case
    KindType      -> textStrict "Type"
    KindFun f arg -> parens $ f <+> textStrict "->" <+> arg
    KindUnknown v -> int v
    KindTable     -> textStrict "Table"

prettyType :: Type -> Text
prettyType = show . histo typeDoc

prettyPolyType :: PolyTypeF Type -> Text
prettyPolyType = show . polyTypeDoc . map (histo typeDoc)

prettyConstraint :: Constraint -> Text
prettyConstraint = show . constraintDoc . map (histo typeDoc)

prettyConstraints :: [Constraint] -> Text
prettyConstraints = show . constraintsDoc . map (map (histo typeDoc))

prettyBinder :: Binder -> Text
prettyBinder = show . cata binderDoc

--------------------------------------------------------------------------------

prettyAst :: Ast -> Text
prettyAst = show . histo astDoc

astDoc :: AstF (Cofree AstF Doc) -> Doc
astDoc = ast
  (liftAlg declarationDoc)
  (liftAlg exprDoc)
  (liftAlg binderDoc)
  (typeDoc . map (hoistCofree unsafePrj))
  (liftAlg refTextDoc)

prettyIntermed :: Intermed -> Text
prettyIntermed = show . histo intermedDoc

intermedDoc :: IntermedF (Cofree IntermedF Doc) -> Doc
intermedDoc = intermed
  (liftAlg exprDoc)
  (liftAlg binderDoc)
  (typeDoc . map (hoistCofree unsafePrj))
  (liftAlg placeholderDoc)
  (liftAlg refIdDoc)

--------------------------------------------------------------------------------

constraintDoc :: ConstraintF Doc -> Doc
constraintDoc = \case
  IsIn cls ty ->
    textStrict cls <+> parens ty
  HasFields m t ->
    t <+> recordDoc (map goField (Map.toList m))
    where
      goField (k, v) = textStrict k <> char ':' <+> v

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
  go c rest = constraintDoc c <+> textStrict "=>" <+> rest

typeDoc :: TypeF (Cofree TypeF Doc) -> Doc
typeDoc = \case
  TypeVar v -> textStrict v
  TypeConstructor c -> textStrict c
  TypeApp (_ :< TypeApp (arr :< TypeConstructor "->") (a :< _)) (b :< _) ->
    parens (a <+> arr <+> b)
  TypeApp (f :< _) (arg :< _) -> parens (f <+> arg)
  TypeTable t -> textStrict "#" <> textStrict (show t)
  TypeRecord m -> recordDoc (map goField (Map.toList m))
    where
      goField (k, v :< _) = textStrict k <+> char ':' <+> v

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
  InstanceDecl c cs vals ->
    textStrict "instance" <+> constraintsDoc cs <+>
      constraintDoc c <+> textStrict "where" <$$>
    indent 2 (vsep vals)
  TypeDecl name poly ->
    textStrict name <+> colon <+> polyTypeDoc poly
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
  WildcardBinder         -> textStrict "_"
  ConstructorBinder c bs -> textStrict c <+> hsep (map parens bs)

literalDoc :: LiteralF Doc -> Doc
literalDoc = \case
  NumberLit n      -> double n
  IntegerLit i     -> integer i
  StringLit s      -> dquotes $ textStrict s
  RecordLit fields -> recordDoc (map goField (Map.toList fields))
    where
      goField (k, v) = textStrict k <> char ':' <+> v

recordDoc :: [Doc] -> Doc
recordDoc = encloseSep (textStrict "{ ") (textStrict " }") (textStrict ", ")

exprDoc :: ExprF Doc -> Doc
exprDoc = \case
  Literal lit -> literalDoc lit
  Abs b body ->
    backslash <> b <+> textStrict "->" <$$>
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
  Access e field ->
    parens e <> dot <> field
  Deref e ->
    textStrict "*" <> parens e

placeholderDoc :: PlaceholderF Doc -> Doc
placeholderDoc = \case
  DictionaryPlaceholder _ c ->
    textStrict "<" <> constraintDoc c <> textStrict ">"
  MethodPlaceholder _ c name ->
    textStrict "<" <> constraintDoc c <+> dot <> textStrict name <> textStrict ">"
  RecursiveCallPlaceholder _ t ->
    textStrict "<" <> textStrict t <> textStrict ">"
  AccessPlaceholder _ t ->
    textStrict "<get " <> t <> textStrict " >"

refTextDoc :: RefTextF Doc -> Doc
refTextDoc = \case
  TableRef (Ref t) ->
    char '#' <> textStrict t
  ColumnRef (Ref c) ->
    char '$' <> textStrict c
  ColumnOfTableRef (Ref t) (Ref c) ->
    char '#' <> textStrict t <> dot <> textStrict c

refIdDoc :: RefIdF Doc -> Doc
refIdDoc = \case
  TableRef (Id t) ->
    char '#' <> textStrict (show t)
  ColumnRef (Id c) ->
    char '$' <> textStrict (show c)
  ColumnOfTableRef (Id t) (Id c) ->
    char '#' <> textStrict (show t) <> dot <> textStrict (show c)
