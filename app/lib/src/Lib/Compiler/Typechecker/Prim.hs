{-# LANGUAGE FlexibleContexts #-}

module Lib.Compiler.Typechecker.Prim where

import           Control.Lens
import           Control.Monad.State

import qualified Data.Map                       as Map
import           Data.Text                      (Text)

import           Lib.Compiler.Typechecker.Types
import           Lib.Compiler.Types
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

tyApp :: Type -> Type -> Type
tyApp f arg = Type $ TyApp f arg

arr :: MonadState InferState m => Point -> Point -> m Point
arr a b = do
  fn <- mkPoint tyFun
  a' <- mkPoint $ TyApp fn a
  mkPoint $ TyApp a' b

mkList :: MonadState InferState m => Point -> m Point
mkList a = do
  l <- mkPoint tyList
  mkPoint $ TyApp l a

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
  (DataRowRef t) -> Type . TyRecord <$> f t
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
  [ ( IsIn (ClassName "Functor") $ Type tyList
    , "$FunctorList"
    )
  , ( IsIn (ClassName "Functor") $ Type tyMaybe
    , "$FunctorMaybe"
    )
  , ( IsIn (ClassName "Semigroup") $ Type tyString
    , "$SemigroupString"
    )
  -- TODO: Does not work atm, since in 'lookupInstanceDict', the types would
  -- need to be unified...
  , ( IsIn (ClassName "Semigroup") $ tyApp (Type tyList) (tyVar 1)
    , "$SemigroupList"
    )
  , ( IsIn (ClassName "Show") $ Type tyNumber
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
        tyApp (Type tyList) (tyVar 1) `tyArr` Type tyNumber
    )
  , ( "const"
    , ForAll [typeVar 1, typeVar 2] [] $
        tyVar 1 `tyArr` (tyVar 2 `tyArr` tyVar 1)
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
  , ( "/"
    , ForAll [] [] $
        Type tyNumber `tyArr` (Type tyNumber `tyArr` Type tyNumber)
    )
  , ( "<>"
    , ForAll [typeVar 1] [IsIn (ClassName "Semigroup") (tyVar 1)] $
        tyVar 1 `tyArr` (tyVar 1 `tyArr` tyVar 1)
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
    , ForAll [typeVar 1] [IsIn (ClassName "Eq") (tyVar 1), IsIn (ClassName "Ord") (tyVar 1)] $
        tyVar 1 `tyArr` (tyVar 1 `tyArr` Type tyBool)
    )
  , ( ">="
    , ForAll [typeVar 1] [IsIn (ClassName "Eq") (tyVar 1), IsIn (ClassName "Ord") (tyVar 1)] $
        tyVar 1 `tyArr` (tyVar 1 `tyArr` Type tyBool)
    )
  , ( "<"
    , ForAll [typeVar 1] [IsIn (ClassName "Eq") (tyVar 1), IsIn (ClassName "Ord") (tyVar 1)] $
        tyVar 1 `tyArr` (tyVar 1 `tyArr` Type tyBool)
    )
  , ( ">"
    , ForAll [typeVar 1] [IsIn (ClassName "Eq") (tyVar 1), IsIn (ClassName "Ord") (tyVar 1)] $
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
    , ForAll [typeVar 1, typeVar 2, typeVar 3] [IsIn (ClassName "Functor") (tyVar 3)] $
        (tyVar 1 `tyArr` tyVar 2) `tyArr` (tyApp (tyVar 3) (tyVar 1) `tyArr` tyApp (tyVar 3) (tyVar 2))
    )
  , ( "filter"
    , ForAll [typeVar 1] [] $
        (tyVar 1 `tyArr` Type tyBool) `tyArr` (tyApp (Type tyList) (tyVar 1) `tyArr` tyApp (Type tyList) (tyVar 1))
    )
  , ( "find"
    , ForAll [typeVar 1] [] $
        (tyVar 1 `tyArr` Type tyBool) `tyArr` (tyApp (Type tyList) (tyVar 1) `tyArr` tyApp (Type tyMaybe) (tyVar 1))
    )
  , ( "&&"
    , ForAll [] [] $
        Type tyBool `tyArr` (Type tyBool `tyArr` Type tyBool)
    )
  , ( "||"
    , ForAll [] [] $
        Type tyBool `tyArr` (Type tyBool `tyArr` Type tyBool)
    )
  , ( "fromMaybe"
    , ForAll [typeVar 1] [] $
        tyVar 1 `tyArr` (tyApp (Type tyMaybe) (tyVar 1) `tyArr` tyVar 1)
    )
  , ( "maybe"
    , ForAll [typeVar 1, typeVar 2] [] $
        tyVar 2 `tyArr` ((tyVar 1 `tyArr` tyVar 2) `tyArr` (tyApp (Type tyMaybe) (tyVar 1) `tyArr` tyVar 2))
    )
  ]
