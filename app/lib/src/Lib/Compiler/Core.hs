{-# LANGUAGE NoImplicitPrelude #-}
-- |

module Lib.Compiler.Core where

import           Lib.Prelude

data Literal
  = NumberLit Double
  | IntLit Integer
  | StringLit Text
  deriving (Show)

data Binder
  = VarBinder Text
  | ConstructorBinder Text [Binder]
  deriving (Show)

data Expr
  = Literal Literal
  | Var Text
  | Constructor Text
  | Abs Text Expr
  | App Expr Expr
  | Let [(Text, Expr)] Expr
  | Case Expr [(Binder, Expr)]
  deriving (Show)
