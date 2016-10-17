{-# LANGUAGE FlexibleContexts            #-}

module Lib.Compiler.Typechecker.Prim where

import           Control.Monad.State

import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import           Data.Text                      (Text)

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

arr :: MonadState InferState m => Point -> Point -> m Point
arr a b = do
  fn <- mkPoint tyFun
  a' <- mkPoint $ TyApp fn a
  mkPoint $ TyApp a' b

-- typeOfDataType :: Applicative m => (Id Table -> m (MonoType a)) -> DataType -> m (MonoType a)
-- typeOfDataType f = \case
--   DataBool       -> pure tyBool
--   DataString     -> pure tyString
--   DataNumber     -> pure tyNumber
--   DataTime       -> pure tyTime
--   (DataRecord t) -> TyRecord <$> f t
--   (DataList t)   -> TyApp tyList <$> typeOfDataType f t
--   (DataMaybe t)  -> TyApp tyMaybe <$> typeOfDataType f t

-- primContext :: Map Name PolyType
-- primContext = Map.fromList
--   [ ( "zero"
--     , Forall [] $ TyNullary TyNumber
--     )
--   , ( "double"
--     , Forall [] $ TyNullary TyNumber `TyArr` TyNullary TyNumber
--     )
--   , ( "sum"
--     , Forall [] $ TyUnary TyList (TyNullary TyNumber) `TyArr` TyNullary TyNumber
--     )
--   , ( "length"
--     , Forall [] $ TyUnary TyList (TyVar (TV "_a")) `TyArr` TyNullary TyNumber
--     )
--   , ( "not"
--     , Forall [] $ TyNullary TyBool `TyArr` TyNullary TyBool
--     )
--   , ( "show"
--     , Forall [] $ TyNullary TyNumber `TyArr` TyNullary TyString
--     )
--   , ( "formatNumber"
--     , Forall [] $ TyNullary TyString `TyArr` (TyNullary TyNumber `TyArr` TyNullary TyString)
--     )
--   , ( "formatTime"
--     , Forall [] $ TyNullary TyString `TyArr` (TyNullary TyTime `TyArr` TyNullary TyString)
--     )
--   , ( "map"
--     , Forall [TV "_a", TV "_b"] $
--         (TyVar (TV "_a") `TyArr` TyVar (TV "_b")) `TyArr`
--         (TyUnary TyList (TyVar (TV "_a")) `TyArr` TyUnary TyList (TyVar (TV "_b")))
--     )
--   , ( "filter"
--     , Forall [TV "_a"] $
--         (TyVar (TV "_a") `TyArr` TyNullary TyBool) `TyArr`
--         (TyUnary TyList (TyVar (TV "_a")) `TyArr` TyUnary TyList (TyVar (TV "_a")))
--     )
--   , ( "find"
--     , Forall [TV "_a"] $
--         (TyVar (TV "_a") `TyArr` TyNullary TyBool) `TyArr`
--         (TyUnary TyList (TyVar (TV "_a")) `TyArr` TyUnary TyMaybe (TyVar (TV "_a")))
--     )
--   ]

-- primTypeClasses :: Map ClassName (Map Name PolyType)
-- primTypeClasses = Map.fromList
--   [ ( ClassName "Eq"
--     , Map.fromList
--       [ ( "=="
--         , ForAll [TypeVar "a"] [] $ TyFun (TyVar (TypeVar "a")) (TyFun (TyVar (TypeVar "a")) tyBool)
--         )
--       ]
--     )
--   ]

-- primTypeClassDicts :: Map ClassName (Map MonoType TypeClassDict)
-- primTypeClassDicts = Map.fromList
--   [ ( ClassName "Eq"
--     , Map.fromList
--       [ ( tyNumber
--         , TypeClassDict "eqNumber"
--         )
--       , ( tyString
--         , TypeClassDict "eqString"
--         )
--       ]
--     )
--   ]
