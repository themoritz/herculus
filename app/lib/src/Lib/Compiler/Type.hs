{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |

module Lib.Compiler.Type where

import           Lib.Prelude                  hiding (empty)

import           Control.Comonad.Cofree

import           Data.Functor.Foldable

import           Text.PrettyPrint.Leijen.Text

import           Lib.Model.Column
import           Lib.Types

data KindF a
  = KindStar
  | KindFun a a
  deriving (Functor)

type Kind = Fix KindF

-- | Class and type, which should be member of the class
data Predicate = IsIn Text Type

data TypeF a
  = TypeVar Text
  | TypeConstructor Text
  | TypeApp a a
  | RecordCons (Ref Column) a a
  | RecordNil
  deriving (Functor)

type Type = Fix TypeF

mkTypeConstructor :: Text -> Type
mkTypeConstructor = Fix . TypeConstructor

mkTypeApp :: Type -> Type -> Type
mkTypeApp f arg = Fix (TypeApp f arg)

mkRecordCons :: Text -> Type -> Type -> Type
mkRecordCons f t r = Fix (RecordCons (Ref f) t r)

-- Type variables and predicates
data PolyType = ForAll [Text] [Predicate] Type

predicateDoc :: Predicate -> Doc
predicateDoc (IsIn cls ty) = textStrict cls <+> typeDoc ty

typeDoc :: Type -> Doc
typeDoc = histo $ \case
  TypeVar v -> textStrict v
  TypeConstructor c -> textStrict c
  TypeApp (_ :< TypeApp (arr :< TypeConstructor "->") (a :< _)) (b :< _) ->
    parens (a <+> arr <+> b)
  TypeApp (f :< _) (arg :< _) -> parens (f <+> arg)
  RecordCons (Ref ref) (t :< _) (rest :< _) ->
    textStrict ref <+> textStrict "::" <+> t <> comma <+> rest
  RecordNil -> empty

prettyType :: Type -> Text
prettyType = show . typeDoc

polyTypeDoc :: PolyType -> Doc
polyTypeDoc (ForAll vars preds ty) =
  goVars <+> goPreds <+> typeDoc ty
  where
  goVars = if null vars
    then empty
    else textStrict "forall" <+>
         hsep (map textStrict vars) <> dot
  goPreds = foldr goPred empty preds
  goPred (IsIn cls t) rest =
    textStrict cls <+> typeDoc t <+> textStrict "=>" <+> rest

prettyPolyType :: PolyType -> Text
prettyPolyType = show . polyTypeDoc
