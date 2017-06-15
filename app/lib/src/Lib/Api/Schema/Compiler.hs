{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
-- |

module Lib.Api.Schema.Compiler where

import           Lib.Prelude

import           Control.Lens

import           Data.Aeson
import           Data.Functor.Foldable
import qualified Data.HashMap.Strict      as HashMap
import qualified Data.Map                 as Map

import           Lib.Model.Table
import           Lib.Types

import qualified Lib.Compiler             as C
import qualified Lib.Compiler.Check       as C
import qualified Lib.Compiler.Check.Types as C
import qualified Lib.Compiler.Core        as C
import qualified Lib.Compiler.Parse.State as C
import qualified Lib.Compiler.Type        as C

data Kind
  = KindType
  | KindTable
  | KindRecord
  | KindFun Kind Kind
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

toSchemaKind :: C.Kind -> Kind
toSchemaKind = cata $ \case
  C.KindType -> KindType
  C.KindTable -> KindTable
  C.KindRecord -> KindRecord
  C.KindFun f arg -> KindFun f arg
  C.KindUnknown _ -> KindType

fromSchemaKind :: Kind -> C.Kind
fromSchemaKind = \case
  KindType -> C.kindType
  KindTable -> C.kindTable
  KindRecord -> C.kindRecord
  KindFun f arg -> C.kindFun (fromSchemaKind f) (fromSchemaKind arg)

data Type
  = TypeVar Text
  | TypeConstructor Text
  | TypeApp Type Type
  | TypeTable (Id Table)
  | TypeRecord [(Text, Type)]
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

toSchemaType :: C.Type -> Type
toSchemaType = cata $ \case
  C.TypeVar v -> TypeVar v
  C.TypeConstructor c -> TypeConstructor c
  C.TypeApp f arg -> TypeApp f arg
  C.TypeTable (InId i) -> TypeTable i
  C.TypeRecord r -> TypeRecord $ Map.toList r

fromSchemaType :: Type -> C.Type
fromSchemaType = \case
  TypeVar v -> C.typeVar v
  TypeConstructor c -> C.typeConstructor c
  TypeApp f arg -> C.typeApp (fromSchemaType f) (fromSchemaType arg)
  TypeTable t -> C.typeTable (InId t)
  TypeRecord r -> C.typeRecord $ map fromSchemaType $ Map.fromList r

data Constraint
  = IsIn Text Type
  | HasFields [(Text, Type)] Type
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

toSchemaConstraint :: C.Constraint -> Constraint
toSchemaConstraint = \case
  C.IsIn c t ->
    IsIn c (toSchemaType t)
  C.HasFields fs t ->
    HasFields (Map.toList $ map toSchemaType fs) (toSchemaType t)

fromSchemaConstraint :: Constraint -> C.Constraint
fromSchemaConstraint = \case
  IsIn c t ->
    C.IsIn c (fromSchemaType t)
  HasFields fs t ->
    C.HasFields (map fromSchemaType $ Map.fromList fs) (fromSchemaType t)

data PolyType = ForAll [Text] [Constraint] Type
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

toSchemaPolyType :: C.PolyType -> PolyType
toSchemaPolyType (C.ForAll as cs t) =
  ForAll as (map toSchemaConstraint cs) (toSchemaType t)

fromSchemaPolyType :: PolyType -> C.PolyType
fromSchemaPolyType (ForAll as cs t) =
  C.ForAll as (map fromSchemaConstraint cs) (fromSchemaType t)

data TyconInfo = TyconInfo
  { _tyconKind         :: Kind
  , _tyconParams       :: [Text]
  , _tyconValueConstrs :: [(Text, [Type])]
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

toSchemaTyconInfo :: C.TyconInfo -> TyconInfo
toSchemaTyconInfo C.TyconInfo {..} = TyconInfo
  (toSchemaKind tyconKind)
  tyconParams
  (map (id *** map toSchemaType) tyconValueConstrs)

fromSchemaTyconInfo :: TyconInfo -> C.TyconInfo
fromSchemaTyconInfo TyconInfo {..} = C.TyconInfo
  (fromSchemaKind _tyconKind)
  _tyconParams
  (map (id *** map fromSchemaType) _tyconValueConstrs)

makeLenses ''TyconInfo

preludeTycons :: Map Text TyconInfo
preludeTycons = map toSchemaTyconInfo C.preludeTycons

data Instance = Instance
  { _instanceConstraints :: [Constraint]
  , _instanceHeadClass   :: Text
  , _instanceHeadType    :: Type
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

toSchemaInstance :: C.Instance -> Instance
toSchemaInstance (C.Instance cs c t) =
  Instance (map toSchemaConstraint cs) c (toSchemaType t)

fromSchemaInstance :: Instance -> C.Instance
fromSchemaInstance (Instance cs c t) =
  C.Instance (map fromSchemaConstraint cs) c (fromSchemaType t)

data Class = Class
  { _classSupers    :: [Text]
  , _classParam     :: Text
  , _classKind      :: Kind
  , _classMethods   :: [(Text, PolyType)]
  , _classInstances :: [Instance]
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

toSchemaClass :: C.Class -> Class
toSchemaClass (C.Class supers p k methods insts) = Class
  supers
  p
  (toSchemaKind k)
  (Map.toList $ map toSchemaPolyType methods)
  (map toSchemaInstance insts)

fromSchemaClass :: Class -> C.Class
fromSchemaClass (Class supers p k methods insts) = C.Class
  supers
  p
  (fromSchemaKind k)
  (map fromSchemaPolyType $ Map.fromList methods)
  (map fromSchemaInstance insts)

data Module = Module
  { _moduleKinds     :: [(Text, Kind)]
  , _moduleTypes     :: [(Text, PolyType)]
  , _moduleOperators :: [(Text, (Text, C.Fixity))]
  , _moduleClasses   :: [(Text, Class)]
  , _moduleTerms     :: [(C.Ident, C.Expr)]
  , _moduleTycons    :: [(Text, TyconInfo)]
  } deriving (Show, Eq, Generic, ToJSON, FromJSON)

checkResultToModule :: C.CheckResult -> Module
checkResultToModule (C.CheckResult (C.CheckEnv ks ts ops _ cls) terms tycons) =
  Module (Map.toList $ map toSchemaKind ks)
         (Map.toList $ map (toSchemaPolyType . C.etPoly) ts)
         (Map.toList ops)
         (Map.toList $ map toSchemaClass cls)
         (HashMap.toList terms)
         (Map.toList $ map toSchemaTyconInfo tycons)

moduleToCheckResult :: Module -> C.CheckResult
moduleToCheckResult (Module ks ts ops cls terms tycons) = C.CheckResult
  (C.CheckEnv
     (map fromSchemaKind $ Map.fromList ks)
     (map (C.defaultEnvType . fromSchemaPolyType) $ Map.fromList ts)
     (Map.fromList ops)
     Map.empty
     (map fromSchemaClass $ Map.fromList cls)
  )
  (HashMap.fromList terms)
  (map fromSchemaTyconInfo $ Map.fromList tycons)

