{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms   #-}
-- |

module Lib.Compiler.Env where

import           Lib.Prelude

import           Data.Functor.Foldable
import qualified Data.Map              as Map

import           Lib.Compiler.Type

kindUnary :: Kind
kindUnary = kindFun kindType kindType

kindBinary :: Kind
kindBinary = kindFun kindType kindUnary

primKindEnv :: Map Text Kind
primKindEnv = Map.fromList
  [ ( "String"
    , kindType
    )
  , ( "Number"
    , kindType
    )
  , ( "Int"
    , kindType
    )
  , ( "->"
    , kindBinary
    )
  , ( "Array"
    , kindUnary
    )
  ]

--------------------------------------------------------------------------------

tyRecord :: Type
tyRecord = typeConstructor "Record"

tyString :: Type
tyString = typeConstructor "String"

tyNumber :: Type
tyNumber = typeConstructor "Number"

tyInt :: Type
tyInt = typeConstructor "Int"

tyFunction :: Type
tyFunction = typeConstructor "->"

infixr 2 ->:
(->:) :: Type -> Type -> Type
(->:) a b = typeApp (typeApp tyFunction a) b

pattern Arrow :: Type -> Type -> Type
pattern Arrow a b =
  Fix (TypeApp (Fix (TypeApp (Fix (TypeConstructor "->")) a)) b)

primTypeEnv :: Map Text (PolyType Type)
primTypeEnv = Map.fromList
  [ ( "+"
    , ForAll [] [] $ tyNumber ->: tyNumber ->: tyNumber
    )
  ]

--------------------------------------------------------------------------------

