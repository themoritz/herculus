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
prettyKind = show . fst . cata kindDoc
  where
  kindDoc :: KindF BDoc -> BDoc
  kindDoc = \case
    KindType ->
      (textStrict "Type", 0)
    KindFun (f, nF) (arg, nArg) ->
      (pF <+> textStrict "->" <+> pArg, 2)
      where
        pF = if nF >= 2 then parens f else f
        pArg = if nArg > 2 then parens arg else arg
    KindUnknown v ->
      (int v, 0)
    KindTable ->
      (textStrict "Table", 0)

prettyType :: Type -> Text
prettyType = show . fst . histo typeDoc

prettyPolyType :: PolyTypeF Type -> Text
prettyPolyType = show . polyTypeDoc . map (fst . histo typeDoc)

prettyConstraint :: Constraint -> Text
prettyConstraint = show . constraintDoc . map (fst . histo typeDoc)

prettyConstraints :: [Constraint] -> Text
prettyConstraints = show . constraintsDoc . map (map (fst . histo typeDoc))

prettyBinder :: Binder -> Text
prettyBinder = show . cata binderDoc

--------------------------------------------------------------------------------

-- Second part is the "bracketing need". Currently only used for types and
-- kinds.
--
-- 0: atoms, never bracket
-- 1: App, left-associative
-- 2: ->, right-associative
type BDoc = (Doc, Int)

liftPretty
  :: Functor f => (f Doc -> Doc) -> f (Cofree g BDoc) -> BDoc
liftPretty = liftAlg . imapAlg (,0) fst

prettyAst :: Ast -> Text
prettyAst = show . fst . histo astDoc

astDoc :: AstF (Cofree AstF BDoc) -> BDoc
astDoc = ast
  (liftPretty declarationDoc)
  (liftPretty exprDoc)
  (liftPretty binderDoc)
  (typeDoc . map (hoistCofree unsafePrj))
  (liftPretty refTextDoc)

prettyIntermed :: Intermed -> Text
prettyIntermed = show . fst . histo intermedDoc

intermedDoc :: IntermedF (Cofree IntermedF BDoc) -> BDoc
intermedDoc = intermed
  (liftPretty exprDoc)
  (liftPretty binderDoc)
  (typeDoc . map (hoistCofree unsafePrj))
  (liftPretty placeholderDoc)
  (liftPretty refIdDoc)

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

typeDoc :: TypeF (Cofree TypeF BDoc) -> BDoc
typeDoc = \case
  TypeVar v ->
    (textStrict v, 0)
  TypeConstructor c ->
    (textStrict c, 0)
  TypeApp (_ :< TypeApp ((arr, _) :< TypeConstructor "->") ((a, nA) :< _)) ((b, nB) :< _) ->
    (pA <+> arr <+> pB, 2)
    where
      pA = if nA >= 2 then parens a else a
      pB = if nB > 2 then parens b else b
  TypeApp ((f, nF) :< _) ((arg, nArg) :< _) ->
    (pF <+> pArg, 1)
    where
      pF = if nF > 1 then parens f else f
      pArg = if nArg >= 1 then parens arg else arg
  TypeTable t ->
    (textStrict "#" <> textStrict (show t), 0)
  TypeRecord m ->
    (recordDoc (map goField (Map.toList m)), 0)
    where
      goField (k, (v, _) :< _) = textStrict k <+> char ':' <+> v

declarationDoc :: DeclarationF Doc -> Doc
declarationDoc = \case
  DataDecl name args constrs ->
    textStrict "data" <+> name <+> hsep args <$$>
    indent 2 (vsep (map goConstr (zip prefixes constrs)))
    where
      prefixes = '=' : repeat '|'
      goConstr (p, (n, as)) = char p <+> n <+> hsep as
  ClassDecl (name, param) supers sigs ->
    textStrict "class" <+> goSupers <+> name <+> param <+> textStrict "where" <$$>
    indent 2 (vsep sigs)
    where
    goSupers = foldr goSuper empty supers
    goSuper (n, p) rest = n <+> p <+> textStrict "=>" <+> rest
  InstanceDecl (hCls, hTy) cs vals ->
    textStrict "instance" <+> goConstrs <+>
      hCls <+> hTy <+> textStrict "where" <$$>
    indent 2 (vsep vals)
    where
    goConstrs = foldr goConst empty cs
    goConst (cls, t) rest = cls <+> t <+> textStrict "=>" <+> rest
  TypeDecl name poly ->
    name <+> colon <+> polyTypeDoc poly
  ValueDecl name binders expr ->
    name <+> hsep binders <+> equals <$$>
    indent 2 expr
  FixityDecl x alias (Infix assoc fixity) ->
    assocDoc <+> int fixity <+> x <+> textStrict "as" <+> alias
    where
    assocDoc = case assoc of
      AssocL -> "infixl"
      AssocR -> "infixr"
      AssocN -> "infix"
  DeclName t -> textStrict t

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
