{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms   #-}
-- |

module Lib.Compiler.Env where

import           Lib.Prelude

import           Data.Functor.Foldable
import qualified Data.Map                as Map

import           Lib.Compiler.Eval.Types
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
  , ( "Integer"
    , kindType
    )
  , ( "->"
    , kindBinary
    )
  , ( "Array"
    , kindUnary
    )
  , ( "Record"
    , kindFun (kindRecord kindType) kindType
    )
  ]

--------------------------------------------------------------------------------

tyRecord :: Type
tyRecord = typeConstructor "Record"

tyString :: Type
tyString = typeConstructor "String"

tyNumber :: Type
tyNumber = typeConstructor "Number"

tyInteger :: Type
tyInteger = typeConstructor "Integer"

tyFunction :: Type
tyFunction = typeConstructor "->"

infixr 2 -->
(-->) :: Type -> Type -> Type
(-->) a b = typeApp (typeApp tyFunction a) b

pattern Arrow :: Type -> Type -> Type
pattern Arrow a b =
  Fix (TypeApp (Fix (TypeApp (Fix (TypeConstructor "->")) a)) b)

primTypeEnv :: Map Text PolyType
primTypeEnv = Map.fromList
  [ ( "+"
    , ForAll [] [] $ tyNumber --> tyNumber --> tyNumber
    )
  ]

--------------------------------------------------------------------------------

primTermEnv :: TermEnv
primTermEnv = Map.fromList
  [ ( "+"
    , RPrimFun $ \(RValue (VNumber a)) -> pure $
      RPrimFun $ \(RValue (VNumber b)) -> pure $
      RValue $ VNumber $ a + b
    )
  ]
