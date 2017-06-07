{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE TypeApplications #-}
-- |

module Lib.Compiler.Env where

import           Lib.Prelude

import           Data.Functor.Foldable
import qualified Data.Map                as Map
import           Data.Time.Calendar      (toGregorian)
import           Data.Time.Clock         (utctDay)

import           Lib.Model.Column
import           Lib.Types

import           Lib.Compiler.Eval.Monad
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
  , ( "DateTime"
    , kindType
    )
  , ( "->"
    , kindBinary
    )
  , ( "Array"
    , kindUnary
    )
  , ( "Record"
    , kindFun kindRecord kindType
    )
  , ( "Row"
    , kindFun kindTable kindType
    )
  ]

primTycons :: Map Text TyconInfo
primTycons = map (\k -> TyconInfo k [] []) primKindEnv

--------------------------------------------------------------------------------

tyBoolean :: Type
tyBoolean = typeConstructor "Boolean"

tyOrdering :: Type
tyOrdering = typeConstructor "Ordering"

tyDateTime :: Type
tyDateTime = typeConstructor "DateTime"

tyRecord :: Type -> Type
tyRecord = typeApp (typeConstructor "Record")

pattern Record :: TypeF (Fix TypeF) -> TypeF (Fix TypeF)
pattern Record t =
  TypeApp (Fix (TypeConstructor "Record")) (Fix t)

tyString :: Type
tyString = typeConstructor "String"

tyNumber :: Type
tyNumber = typeConstructor "Number"

tyInteger :: Type
tyInteger = typeConstructor "Integer"

tyFunction :: Type
tyFunction = typeConstructor "->"

tyList :: Type
tyList = typeConstructor "List"

tyMaybe :: Type
tyMaybe = typeConstructor "Maybe"

tyRow :: Type -> Type
tyRow = typeApp (typeConstructor "Row")

pattern Row :: TypeF (Fix TypeF) -> TypeF (Fix TypeF)
pattern Row t =
  TypeApp (Fix (TypeConstructor "Row")) (Fix t)

infixr 2 -->
(-->) :: Type -> Type -> Type
(-->) a b = typeApp (typeApp tyFunction a) b

pattern Arrow :: Type -> Type -> Type
pattern Arrow a b =
  Fix (TypeApp (Fix (TypeApp (Fix (TypeConstructor "->")) a)) b)

typeOfDataType :: DataType -> Type
typeOfDataType = \case
  DataAlgebraic name args ->
    foldl typeApp (typeConstructor name) $ map typeOfDataType args
  DataTable t ->
    typeTable $ InId t
  DataRecord fields ->
    typeRecord $ map typeOfDataType $ Map.fromList fields

primTypeEnv :: Map Text PolyType
primTypeEnv = map fst (primEnv @Identity)

primTermEnv :: Monad m => TermEnv m
primTermEnv = map snd primEnv

primEnv :: Monad m => Map Text (PolyType, Result m)
primEnv = Map.fromList
  [
  -- Num Number
    ( "prim_addNumber"
    , ( ForAll [] [] $ tyNumber --> tyNumber --> tyNumber
      , RPrimFun $ \(RNumber a) -> pure $
        RPrimFun $ \(RNumber b) -> pure $
        RNumber $ a + b
      )
    )
  , ( "prim_mulNumber"
    , ( ForAll [] [] $ tyNumber --> tyNumber --> tyNumber
      , RPrimFun $ \(RNumber a) -> pure $
        RPrimFun $ \(RNumber b) -> pure $
        RNumber $ a * b
      )
    )
  , ( "prim_subNumber"
    , ( ForAll [] [] $ tyNumber --> tyNumber --> tyNumber
      , RPrimFun $ \(RNumber a) -> pure $
        RPrimFun $ \(RNumber b) -> pure $
        RNumber $ a - b
      )
    )
  , ( "prim_divNumber"
    , ( ForAll [] [] $ tyNumber --> tyNumber --> tyNumber
      , RPrimFun $ \(RNumber a) -> pure $
        RPrimFun $ \(RNumber b) -> if b == 0
          then evalError "Division by zero"
          else pure $ RNumber $ a / b
      )
    )
  -- Num Integer
  , ( "prim_addInteger"
    , ( ForAll [] [] $ tyInteger --> tyInteger --> tyInteger
      , RPrimFun $ \(RInteger a) -> pure $
        RPrimFun $ \(RInteger b) -> pure $
        RInteger $ a + b
      )
    )
  , ( "prim_mulInteger"
    , ( ForAll [] [] $ tyInteger --> tyInteger --> tyInteger
      , RPrimFun $ \(RInteger a) -> pure $
        RPrimFun $ \(RInteger b) -> pure $
        RInteger $ a * b
      )
    )
  , ( "prim_subInteger"
    , ( ForAll [] [] $ tyInteger --> tyInteger --> tyInteger
      , RPrimFun $ \(RInteger a) -> pure $
        RPrimFun $ \(RInteger b) -> pure $
        RInteger $ a - b
      )
    )
  , ( "prim_divInteger"
    , ( ForAll [] [] $ tyInteger --> tyInteger --> tyInteger
      , RPrimFun $ \(RInteger a) -> pure $
        RPrimFun $ \(RInteger b) -> if b == 0
          then evalError "Division by zero"
          else pure $ RInteger $ a `div` b
      )
    )
  -- Semigroup String
  , ( "prim_appendString"
    , ( ForAll [] [] $ tyString --> tyString --> tyString
      , RPrimFun $ \(RString a) -> pure $
        RPrimFun $ \(RString b) -> pure $
        RString $ a <> b
      )
    )
  -- Print
  , ( "prim_printNumber"
    , ( ForAll [] [] $ tyNumber --> tyString
      , RPrimFun $ \(RNumber n) -> pure $ RString $ show n
      )
    )
  , ( "prim_printInteger"
    , ( ForAll [] [] $ tyInteger --> tyString
      , RPrimFun $ \(RInteger n) -> pure $ RString $ show n
      )
    )
  -- Eq
  , ( "prim_eqNumber"
    , ( ForAll [] [] $ tyNumber --> tyNumber --> tyBoolean
      , RPrimFun $ \(RNumber a) -> pure $
        RPrimFun $ \(RNumber b) -> pure $
        dataBool $ a == b
      )
    )
  , ( "prim_eqInteger"
    , ( ForAll [] [] $ tyInteger --> tyInteger --> tyBoolean
      , RPrimFun $ \(RInteger a) -> pure $
        RPrimFun $ \(RInteger b) -> pure $
        dataBool $ a == b
      )
    )
  , ( "prim_eqDateTime"
    , ( ForAll [] [] $ tyDateTime --> tyDateTime --> tyBoolean
      , RPrimFun $ \(RDateTime a) -> pure $
        RPrimFun $ \(RDateTime b) -> pure $
        dataBool $ a == b
      )
    )
  , ( "prim_eqString"
    , ( ForAll [] [] $ tyString --> tyString --> tyBoolean
      , RPrimFun $ \(RString a) -> pure $
        RPrimFun $ \(RString b) -> pure $
        dataBool $ a == b
      )
    )
  , ( "prim_eqRow"
    , ( ForAll ["t"] [] $ tyRow (typeVar "t") --> tyRow (typeVar "t") --> tyBoolean
      , RPrimFun $ \(RRowRef a) -> pure $
        RPrimFun $ \(RRowRef b) -> pure $
        dataBool $ a == b
      )
    )
  -- Ord
  , ( "prim_compareNumber"
    , ( ForAll [] [] $ tyNumber --> tyNumber --> tyOrdering
      , RPrimFun $ \(RNumber a) -> pure $
        RPrimFun $ \(RNumber b) -> pure $
        dataOrdering $ compare a b
      )
    )
  , ( "prim_compareInteger"
    , ( ForAll [] [] $ tyInteger --> tyInteger --> tyOrdering
      , RPrimFun $ \(RInteger a) -> pure $
        RPrimFun $ \(RInteger b) -> pure $
        dataOrdering $ compare a b
      )
    )
  , ( "prim_compareDateTime"
    , ( ForAll [] [] $ tyDateTime --> tyDateTime --> tyOrdering
      , RPrimFun $ \(RDateTime a) -> pure $
        RPrimFun $ \(RDateTime b) -> pure $
        dataOrdering $ compare a b
      )
    )
  -- Date functions
  , ( "prim_formatNumber"
    , ( ForAll [] [] $ tyString --> tyNumber --> tyString
      , RPrimFun $ \(RString f) -> pure $
        RPrimFun $ \(RNumber n) -> pure $
        RString $ formatNumber f n
      )
    )
  , ( "prim_formatDateTime"
    , ( ForAll [] [] $ tyString --> tyDateTime --> tyString
      , RPrimFun $ \(RString f) -> pure $
        RPrimFun $ \(RDateTime d) -> pure $
        RString $ formatTime f d
      )
    )
  , ( "prim_year"
    , ( ForAll [] [] $ tyDateTime --> tyInteger
      , RPrimFun $ \(RDateTime (Time d)) -> pure $
        RInteger $ let (y, _, _) = toGregorian $ utctDay d in y
      )
    )
  , ( "prim_month"
    , ( ForAll [] [] $ tyDateTime --> tyInteger
      , RPrimFun $ \(RDateTime (Time d)) -> pure $
        RInteger $ let (_, m, _) = toGregorian $ utctDay d in fromIntegral m
      )
    )
  , ( "prim_day"
    , ( ForAll [] [] $ tyDateTime --> tyInteger
      , RPrimFun $ \(RDateTime (Time d)) -> pure $
        RInteger $ let (_, _, d') = toGregorian $ utctDay d in fromIntegral d'
      )
    )
  -- Misc
  , ( "prim_roundTo"
    , ( ForAll [] [] $ tyInteger --> tyNumber --> tyNumber
      , RPrimFun $ \(RInteger p) -> pure $
        RPrimFun $ \(RNumber (Number n)) -> pure $
        RNumber $ Number $
          (fromInteger $ round $ n * (10^p)) / (10.0^^p)
      )
    )
  , ( "prim_toNumber"
    , ( ForAll [] [] $ tyInteger --> tyNumber
      , RPrimFun $ \(RInteger i) -> pure $
        RNumber $ Number $ fromInteger i
      )
    )
  ]

--------------------------------------------------------------------------------

dataBool :: Bool -> Result m
dataBool True  = RData "True" []
dataBool False = RData "False" []

dataOrdering :: Ordering -> Result m
dataOrdering EQ = RData "EQ" []
dataOrdering LT = RData "LT" []
dataOrdering GT = RData "GT" []
