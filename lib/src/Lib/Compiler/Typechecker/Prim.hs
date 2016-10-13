module Lib.Compiler.Typechecker.Prim where

import           Lib.Compiler.Typechecker.Types

tyList :: TypeConstructor
tyList = TypeConstructor "List"

tyMaybe :: TypeConstructor
tyMaybe = TypeConstructor "Maybe"

tyNumber :: SimpleType
tyNumber = TyApp (TypeConstructor "Number") Nothing

tyString :: SimpleType
tyString = TyApp (TypeConstructor "String") Nothing

tyTime :: SimpleType
tyTime = TyApp (TypeConstructor "Time") Nothing

tyBool :: SimpleType
tyBool = TyApp (TypeConstructor "Bool") Nothing

typeOfDataType :: Applicative m => (Id Table -> m SimpleType) -> DataType -> m SimpleType
typeOfDataType f = \case
  DataBool       -> pure tyBool
  DataString     -> pure tyString
  DataNumber     -> pure tyNumber
  DataTime       -> pure tyTime
  (DataRecord t) -> TyRecord <$> f t
  (DataList t)   -> TyApp tyList . Just <$> typeOfDataType f t
  (DataMaybe t)  -> TyApp tyMaybe . Just <$> typeOfDataType f t

primTypeClasses :: Map ClassName (Map Name PolymorphicType)
primTypeClasses = Map.fromList
  [ ( ClassName "Eq"
    , Map.fromList
      [ ( "=="
        , ForAll [TypeVar "a"] $ Constraints [] $ TyFun (TyVar (TypeVar "a")) (TyFun (TyVar (TypeVar "a")) tyBool)
        )
      ]
    )
  ]

primTypeClassDicts :: Map ClassName (Map SimpleType TypeClassDict)
primTypeClassDicts = Map.fromList
  [ ( ClassName "Eq"
    , Map.fromList
      [ ( tyNumber
        , TypeClassDict "eqNumber"
        )
      , ( tyString
        , TypeClassDict "eqString"
        )
      ]
    )
  ]
