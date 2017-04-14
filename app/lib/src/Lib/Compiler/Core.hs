{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns      #-}
-- |

module Lib.Compiler.Core where

import           Lib.Prelude

import           Control.Arrow         ((***))

import           Data.Functor.Foldable

import qualified Lib.Compiler.AST      as A
import           Lib.Model.Column
import           Lib.Model.Table
import           Lib.Types

data Literal
  = NumberLit Double
  | IntegerLit Integer
  | StringLit Text
  | RecordLit (Map Text Expr)
  deriving (Show)

data Reference
  = TableRef (Id Table)
  | ColumnRef (Id Column)
  | ColumnOfTableRef (Id Table) (Id Column)
  deriving (Show)

data Binder
  = VarBinder Text
  | ConstructorBinder Text [Binder]
  deriving (Show)

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
  deriving (Show)

toCore :: A.Compiled -> Expr
toCore (Fix com) = A.compiled goExpr undefined goRef com
  where
  goExpr :: A.ExprF A.Compiled -> Expr
  goExpr = \case
    A.Literal l ->
      Literal $ goLiteral l
    A.Abs b e ->
      Abs (goBinder b) (toCore e)
    A.App f arg ->
      App (toCore f) (toCore arg)
    A.Var x -> Var x
    A.Constructor c -> Constructor c
    A.Case scrut alts ->
      Case (toCore scrut) (map (goBinder *** toCore) alts)
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
  goBinder :: A.Compiled -> Binder
  goBinder (Fix (unsafePrj -> b )) = case b of
    A.VarBinder x               -> VarBinder x
    A.ConstructorBinder name bs -> ConstructorBinder name (map goBinder bs)
  goRef :: A.RefIdF A.Compiled -> Expr
  goRef = Reference . \case
    A.TableRef t -> TableRef t
    A.ColumnRef c -> ColumnRef c
    A.ColumnOfTableRef t c -> ColumnOfTableRef t c
