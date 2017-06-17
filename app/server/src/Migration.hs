{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications  #-}
-- |

module Migration
  ( migrate
  ) where

import           Lib.Prelude

import           Control.Lens            hiding (( # ))

import           Data.Aeson
import           Data.Aeson.Bson
import           Data.Bson.Lens

import           Database.MongoDB        ((=:))
import qualified Database.MongoDB        as Mongo

import           Lib.Api.Schema.Compiler (Module)
import           Lib.Model.Class
import           Lib.Model.Column
import           Lib.Model.Common
import           Lib.Model.Project
import           Lib.Model.Table
import           Lib.Types

import           Monads

import           Migration.Tasks

currentVersion :: Int
currentVersion = 3

storeCollection :: Text
storeCollection = "store"

getStoreValue
  :: (MonadIO m, MonadHexlEnv m, Mongo.Val a)
  => Text -> m (Maybe a)
getStoreValue k = do
  res <- runMongo $ Mongo.findOne $
    Mongo.select [ "key" =: k ] storeCollection
  pure $ do
    doc <- res
    Mongo.lookup "value" doc

setStoreValue
  :: (MonadIO m, MonadHexlEnv m, Mongo.Val a)
  => Text -> a -> m ()
setStoreValue k v =
  runMongo $ Mongo.upsert
    (Mongo.select [ "key" =: k ] storeCollection)
    [ "key" =: k
    , "value" =: v
    ]

success :: (MonadHexlEnv m, MonadIO m) => Int -> m ()
success i = do
  putStrLn ("Successfully migrated to version " <> show i :: Text)
  setStoreValue "schemaVersion" i

--------------------------------------------------------------------------------

data DataType1
  = DataBool
  | DataString
  | DataNumber
  | DataInteger
  | DataTime
  | DataRowRef (Id Table)
  | DataList DataType1
  | DataMaybe DataType1
  deriving (Generic, ToJSON, FromJSON)

instance ToBSON DataType1
instance FromBSON DataType1

convDataType :: DataType1 -> DataType
convDataType = \case
  DataBool      -> DataAlgebraic "Boolean" []
  DataString    -> DataAlgebraic "String" []
  DataNumber    -> DataAlgebraic "Number" []
  DataInteger   -> DataAlgebraic "Integer" []
  DataTime      -> DataAlgebraic "DateTime" []
  DataRowRef t  -> DataAlgebraic "Row" [DataTable t]
  DataList sub  -> DataAlgebraic "List" [convDataType sub]
  DataMaybe sub -> DataAlgebraic "Maybe" [convDataType sub]

migration
  :: (MonadHexl m, MonadHexlEnv m, MonadIO m)
  => Int -> m ()
migration = \case

  0 -> do
    recompileColumns
    success 1

  1 -> do
    let
      colCollection = collectionName (Proxy @Column)
      qry = [ "kind.tag" =: ("ColumnData" :: Text) ]
    cols <- runMongo $
      Mongo.find (Mongo.select qry colCollection) >>= Mongo.rest

    let
      dataTypeLens = key "kind" . key "contents" . key "_dataColType"
      goDataType dt = let Success v = fromValue dt in toValue (convDataType v)
      goColumn = over dataTypeLens goDataType
    runMongo $ traverse_ (Mongo.save colCollection . goColumn) cols

    recompileColumns
    success 2

  2 -> do
    let
      prjCollection = collectionName (Proxy @Project)
      goProj p = p <>
        [ "moduleSource" =: ("" :: Text)
        , "module" =: (CompileResultNone :: CompileResult Module)
        ]
    runMongo $ do
      projs <- Mongo.find (Mongo.select [] prjCollection) >>= Mongo.rest
      traverse_ (Mongo.save prjCollection . goProj) projs

    success 3

  i -> throwError $ ErrBug $
    "No migration found for version `" <> show i <> "`."

--------------------------------------------------------------------------------

migrate
  :: (MonadHexl m, MonadHexlEnv m, MonadIO m)
  => m ()
migrate = do
  version <- fromMaybe 0 <$> getStoreValue "schemaVersion"
  traverse_ migration [version .. currentVersion - 1]
