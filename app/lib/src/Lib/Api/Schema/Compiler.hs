{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
-- |

module Lib.Api.Schema.Compiler where

import           Lib.Prelude

import           Control.Lens

import           Data.Aeson
import           Data.Functor.Foldable
import qualified Data.Map              as Map

import           Lib.Model.Table
import           Lib.Types

import qualified Lib.Compiler          as C
import qualified Lib.Compiler.Type     as C

data Kind
  = KindType
  | KindTable
  | KindRecord
  | KindFun Kind Kind
  deriving (Generic, ToJSON, FromJSON)

toSchemaKind :: C.Kind -> Kind
toSchemaKind = cata $ \case
  C.KindType -> KindType
  C.KindTable -> KindTable
  C.KindRecord -> KindRecord
  C.KindFun f arg -> KindFun f arg
  C.KindUnknown _ -> KindType

data Type
  = TypeVar Text
  | TypeConstructor Text
  | TypeApp Type Type
  | TypeTable (Id Table)
  | TypeRecord [(Text, Type)]
  deriving (Generic, ToJSON, FromJSON)

toSchemaType :: C.Type -> Type
toSchemaType = cata $ \case
  C.TypeVar v -> TypeVar v
  C.TypeConstructor c -> TypeConstructor c
  C.TypeApp f arg -> TypeApp f arg
  C.TypeTable (InId i) -> TypeTable i
  C.TypeRecord r -> TypeRecord $ Map.toList r

data TyconInfo = TyconInfo
  { _tyconKind         :: Kind
  , _tyconParams       :: [Text]
  , _tyconValueConstrs :: [(Text, [Type])]
  } deriving (Generic, ToJSON, FromJSON)

toSchemaTyconInfo :: C.TyconInfo -> TyconInfo
toSchemaTyconInfo C.TyconInfo {..} = TyconInfo
  (toSchemaKind tyconKind)
  tyconParams
  (map (id *** map toSchemaType) tyconValueConstrs)

makeLenses ''TyconInfo

preludeTycons :: Map Text TyconInfo
preludeTycons = map toSchemaTyconInfo C.preludeTycons
