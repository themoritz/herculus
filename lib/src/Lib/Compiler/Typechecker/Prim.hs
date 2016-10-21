{-# LANGUAGE FlexibleContexts            #-}

module Lib.Compiler.Typechecker.Prim where

import           Control.Monad.State
import           Control.Lens

import qualified Data.Map                       as Map
import           Data.Text                      (Text)

import           Lib.Compiler.Types
import           Lib.Compiler.Typechecker.Types
import           Lib.Model.Column
import           Lib.Model.Table
import           Lib.Types

mkMonoTypeConst :: Text -> Kind -> (MonoType a)
mkMonoTypeConst n k = TyConst $ TypeConst n k

nullaryType :: Kind
nullaryType = KindStar

unaryType :: Kind
unaryType = KindFun KindStar nullaryType

binaryType :: Kind
binaryType = KindFun KindStar unaryType

tyFun :: (MonoType a)
tyFun = mkMonoTypeConst "(->)" binaryType

tyList :: (MonoType a)
tyList = mkMonoTypeConst "List" unaryType

tyMaybe :: (MonoType a)
tyMaybe = mkMonoTypeConst "Maybe" unaryType

tyNumber :: (MonoType a)
tyNumber = mkMonoTypeConst "Number" nullaryType

tyString :: (MonoType a)
tyString = mkMonoTypeConst "String" nullaryType

tyTime :: (MonoType a)
tyTime = mkMonoTypeConst "Time" nullaryType

tyBool :: (MonoType a)
tyBool = mkMonoTypeConst "Bool" nullaryType

tyArr :: Type -> Type -> Type
tyArr a b = Type (TyApp (Type (TyApp (Type tyFun) a)) b)

tyApp :: MonoType Type -> Type -> Type
tyApp f arg = Type $ TyApp (Type f) arg

arr :: MonadState InferState m => Point -> Point -> m Point
arr a b = do
  fn <- mkPoint tyFun
  a' <- mkPoint $ TyApp fn a
  mkPoint $ TyApp a' b

typeVar :: Int -> TypeVar
typeVar i = TypeVar i KindStar

tyVar :: Int -> Type
tyVar = Type . TyVar . typeVar

typeOfDataType :: Applicative m => (Id Table -> m Type) -> DataType -> m Type
typeOfDataType f = \case
  DataBool       -> pure $ Type tyBool
  DataString     -> pure $ Type tyString
  DataNumber     -> pure $ Type tyNumber
  DataTime       -> pure $ Type tyTime
  (DataRecord t) -> Type . TyRecord <$> f t
  (DataList t)   -> Type . TyApp (Type tyList) <$> typeOfDataType f t
  (DataMaybe t)  -> Type . TyApp (Type tyMaybe) <$> typeOfDataType f t

loadPrelude :: MonadState InferState m => m ()
loadPrelude = do
  forM_ primPrelude $ \(n, poly) -> do
    polyPoint <- polyToPoint poly
    inferContext . contextTypes %= Map.insert n polyPoint
  forM_ primPreludeDicts $ \(p, n) ->
    inferContext . contextInstanceDicts %= Map.insert p n

primPreludeDicts :: [(Predicate Type, Name)]
primPreludeDicts =
  [ ( IsIn (ClassName "Show") $ Type tyNumber
    , "$ShowNumber"
    )
  , ( IsIn (ClassName "Show") $ Type tyBool
    , "$ShowBool"
    )
  , ( IsIn (ClassName "Eq") $ Type tyNumber
    , "$EqNumber"
    )
  , ( IsIn (ClassName "Eq") $ Type tyBool
    , "$EqBool"
    )
  , ( IsIn (ClassName "Eq") $ Type tyTime
    , "$EqTime"
    )
  , ( IsIn (ClassName "Eq") $ Type tyString
    , "$EqString"
    )
  , ( IsIn (ClassName "Ord") $ Type tyNumber
    , "$OrdNumber"
    )
  , ( IsIn (ClassName "Ord") $ Type tyTime
    , "$OrdTime"
    )
  ]

primPrelude :: [(Name, PolyType Type)]
primPrelude =
  [ ( "sum"
    , ForAll [] [] $
        Type (TyApp (Type tyList) (Type tyNumber)) `tyArr` Type tyNumber
    )
  , ( "length"
    , ForAll [typeVar 1] [] $
        tyApp tyList (tyVar 1) `tyArr` Type tyNumber
    )
  , ( "not"
    , ForAll [] [] $
        Type tyBool `tyArr` Type tyBool
    )
  , ( "*"
    , ForAll [] [] $
        Type tyNumber `tyArr` (Type tyNumber `tyArr` Type tyNumber)
    )
  , ( "+"
    , ForAll [] [] $
        Type tyNumber `tyArr` (Type tyNumber `tyArr` Type tyNumber)
    )
  , ( "-"
    , ForAll [] [] $
        Type tyNumber `tyArr` (Type tyNumber `tyArr` Type tyNumber)
    )
  , ( "show"
    , ForAll [typeVar 1] [IsIn (ClassName "Show") (tyVar 1)] $
        tyVar 1 `tyArr` Type tyString
    )
  , ( "=="
    , ForAll [typeVar 1] [IsIn (ClassName "Eq") (tyVar 1)] $
        tyVar 1 `tyArr` (tyVar 1 `tyArr` Type tyBool)
    )
  , ( "!="
    , ForAll [typeVar 1] [IsIn (ClassName "Eq") (tyVar 1)] $
        tyVar 1 `tyArr` (tyVar 1 `tyArr` Type tyBool)
    )
  , ( "<="
    , ForAll [typeVar 1] [IsIn (ClassName "Ord") (tyVar 1)] $
        tyVar 1 `tyArr` (tyVar 1 `tyArr` Type tyBool)
    )
  , ( ">="
    , ForAll [typeVar 1] [IsIn (ClassName "Ord") (tyVar 1)] $
        tyVar 1 `tyArr` (tyVar 1 `tyArr` Type tyBool)
    )
  , ( "<"
    , ForAll [typeVar 1] [IsIn (ClassName "Ord") (tyVar 1)] $
        tyVar 1 `tyArr` (tyVar 1 `tyArr` Type tyBool)
    )
  , ( ">"
    , ForAll [typeVar 1] [IsIn (ClassName "Ord") (tyVar 1)] $
        tyVar 1 `tyArr` (tyVar 1 `tyArr` Type tyBool)
    )
  , ( "formatNumber"
    , ForAll [] [] $
        Type tyString `tyArr` (Type tyNumber `tyArr` Type tyString)
    )
  , ( "formatTime"
    , ForAll [] [] $
        Type tyString `tyArr` (Type tyTime `tyArr` Type tyString)
    )
  , ( "map"
    , ForAll [typeVar 1, typeVar 2] [] $
        (tyVar 1 `tyArr` tyVar 2) `tyArr` (tyApp tyList (tyVar 1) `tyArr` tyApp tyList (tyVar 2))
    )
  , ( "filter"
    , ForAll [typeVar 1] [] $
        (tyVar 1 `tyArr` Type tyBool) `tyArr` (tyApp tyList (tyVar 1) `tyArr` tyApp tyList (tyVar 1))
    )
  , ( "find"
    , ForAll [typeVar 1] [] $
        (tyVar 1 `tyArr` Type tyBool) `tyArr` (tyApp tyList (tyVar 1) `tyArr` tyApp tyMaybe (tyVar 1))
    )
  , ( "&&"
    , ForAll [] [] $
        Type tyBool `tyArr` (Type tyBool `tyArr` Type tyBool)
    )
  , ( "||"
    , ForAll [] [] $
        Type tyBool `tyArr` (Type tyBool `tyArr` Type tyBool)
    )
  ]
