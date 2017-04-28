{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |

module Lib.Compiler.Core where

import           Lib.Prelude

import           Data.Aeson
import           Data.Functor.Foldable
import qualified Data.Map                     as Map

import           Text.PrettyPrint.Leijen.Text

import {-# SOURCE #-} Lib.Model.Column
import           Lib.Model.Dependencies.Types
import           Lib.Model.Table
import           Lib.Types

import qualified Lib.Compiler.AST             as A
import           Lib.Compiler.Pretty          (record)

data Literal
  = NumberLit Double
  | IntegerLit Integer
  | StringLit Text
  | RecordLit (Map Text Expr)
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data Reference
  = TableRef (Id Table)
  | ColumnRef (Id Column)
  | ColumnOfTableRef (Id Table) (Id Column)
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data Binder
  = VarBinder Text
  | WildcardBinder
  | ConstructorBinder Text [Binder]
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

data Expr
  = Literal Literal
  | Var Text
  | Constructor Text
  | Reference Reference
  | Abs Binder Expr
  | App Expr Expr
  | Let [(Text, Expr)] Expr
  | Case Expr [(Binder, Expr)]
  | Accessor Expr Text
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

--------------------------------------------------------------------------------

literalDoc :: Literal -> Doc
literalDoc = \case
  NumberLit n      -> double n
  IntegerLit i     -> integer i
  StringLit s      -> dquotes $ textStrict s
  RecordLit fields -> record (map goField (Map.toList $ map exprDoc fields))
    where
      goField (k, v) = textStrict k <> char ':' <+> v

refDoc :: Reference -> Doc
refDoc = \case
  TableRef (Id t) ->
    char '#' <> textStrict (show t)
  ColumnRef (Id c) ->
    char '$' <> textStrict (show c)
  ColumnOfTableRef (Id t) (Id c) ->
    char '#' <> textStrict (show t) <> dot <> textStrict (show c)

binderDoc :: Binder -> Doc
binderDoc = \case
  VarBinder v            -> textStrict v
  WildcardBinder         -> textStrict "_"
  ConstructorBinder c bs -> textStrict c <+> hsep (map (parens . binderDoc) bs)

exprDoc :: Expr -> Doc
exprDoc = \case
  Literal lit -> literalDoc lit
  Var v ->
    textStrict v
  Constructor c ->
    textStrict c
  Reference r -> refDoc r
  Abs b body ->
    backslash <> binderDoc b <+> textStrict "->" <$$>
    indent 2 (exprDoc body)
  App f arg ->
    parens (exprDoc f <+> exprDoc arg)
  Let bindings rest ->
    textStrict "let" <$$>
    indent 2 (vsep $ map goBinding bindings) <$$>
    textStrict "in" <$$>
    indent 2 (exprDoc rest)
    where
      goBinding (v, body) =
        textStrict v <+> equals <$$> indent 2 (exprDoc body)
  Case e cases ->
    textStrict "case" <+> exprDoc e <+> textStrict "of" <$$>
    indent 2 (vsep (map goCase cases))
    where
      goCase (b, e') = binderDoc b <+> textStrict "->" <+> exprDoc e'
  Accessor e field ->
    parens (exprDoc e) <> dot <> textStrict field

prettyCore :: Expr -> Text
prettyCore = show . exprDoc

--------------------------------------------------------------------------------

toCore :: A.Compiled -> Expr
toCore (Fix com) = A.compiled goExpr undefined goRef com
  where
  goExpr :: A.ExprF A.Compiled -> Expr
  goExpr = \case
    A.Literal l ->
      Literal $ goLiteral l
    A.Abs b e ->
      Abs (binderToCore b) (toCore e)
    A.App f arg ->
      App (toCore f) (toCore arg)
    A.Var x -> Var x
    A.Constructor c -> Constructor c
    A.Case scrut alts ->
      Case (toCore scrut) (map (binderToCore *** toCore) alts)
    A.Let defs body ->
      Let (map (id *** toCore) defs) (toCore body)
    A.Accessor e field ->
      Accessor (toCore e) field
  goLiteral :: A.LiteralF A.Compiled -> Literal
  goLiteral = \case
    A.NumberLit n -> NumberLit n
    A.IntegerLit i -> IntegerLit i
    A.StringLit s -> StringLit s
    A.RecordLit fs -> RecordLit (map toCore fs)
  goRef :: A.RefIdF A.Compiled -> Expr
  goRef = Reference . \case
    A.TableRef t -> TableRef t
    A.ColumnRef c -> ColumnRef c
    A.ColumnOfTableRef t c -> ColumnOfTableRef t c

binderToCore :: A.Compiled -> Binder
binderToCore (Fix (unsafePrj -> b )) = case b of
  A.VarBinder x               -> VarBinder x
  A.WildcardBinder            -> WildcardBinder
  A.ConstructorBinder name bs -> ConstructorBinder name (map binderToCore bs)

collectCodeDependencies :: Expr -> CodeDependencies
collectCodeDependencies = go
  where
  go = \case
    Literal l              -> case l of
      RecordLit m -> mconcat . Map.elems $ map go m
      _           -> mempty
    Reference r            -> case r of
      ColumnRef c          -> singleColumnRef c
      TableRef t           -> singleTableRef t
      ColumnOfTableRef t c -> singleWholeColumnRef t c
    Var _                  -> mempty
    Constructor _          -> mempty
    Abs _ e                -> go e
    App f arg              -> go f <> go arg
    Let defs e             -> go e <> mconcat (map (go . snd) defs)
    Case e alts            -> go e <> mconcat (map (go . snd) alts)
    Accessor e _           -> go e
