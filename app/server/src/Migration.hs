{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications  #-}
-- |

module Migration
  ( migrate
  ) where

import           Lib.Prelude

import           Control.Lens

import           Database.MongoDB  ((=:))
import qualified Database.MongoDB  as Mongo

import           Lib.Model
import           Lib.Model.Class
import           Lib.Model.Column
import           Lib.Model.Project
import           Lib.Model.Table
import           Lib.Types

import           Engine
import           Engine.Compile
import           Engine.Monad
import           Engine.Propagate
import           Monads

currentVersion :: Int
currentVersion = 1

storeCollection :: Text
storeCollection = "store"

getStoreValue
  :: (MonadIO m, MonadHexlEnv m, Mongo.Val a)
  => Text -> m (Maybe a)
getStoreValue key = do
  res <- runMongo $ Mongo.findOne $
    Mongo.select [ "key" =: key ] storeCollection
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

migration
  :: (MonadHexl m, MonadHexlEnv m, MonadIO m)
  => Int -> m ()
migration = \case

  0 -> do
    -- Delete old compile results
    let
      qry =
        [ "kind.tag" =: ("ColumnData" :: Text)
        ]
      upd =
        [ "$set" =:
          [ "kind.contents._dataColCompileResult" =:
              [ "tag" =: ("CompileResultNone" :: Text)
              ]
          ]
        ]
    runMongo $ Mongo.modify
      (Mongo.select qry (collectionName $ Proxy @Column))
      upd
    let
      qry' =
        [ "kind.tag" =: ("ColumnReport" :: Text)
        ]
      upd' =
        [ "$set" =:
          [ "kind.contents._reportColCompiledTemplate" =:
              [ "tag" =: ("CompileResultNone" :: Text)
              ]
          ]
        ]
    runMongo $ Mongo.modify
      (Mongo.select qry' (collectionName $ Proxy @Column))
      upd'

    -- Recompile all columns
    ps <- listAll
    for_ ps $ \(Entity projectId project) -> do
      tables <- listByQuery [ "projectId" =: toObjectId projectId ]
      cols <- for tables $ \(Entity tableId _) -> do
        cols <- listByQuery [ "tableId" =: toObjectId (tableId :: Id Table) ]
        for cols $ \(Entity columnId col) -> case col ^. columnKind of
          ColumnData dataCol -> case dataCol ^. dataColIsDerived of
            Derived    -> pure [columnId]
            NotDerived -> pure []
          ColumnReport _ -> pure [columnId]
      (_, st) <- runEngineT projectId (project ^. projectDependencyGraph) $ do
        traverse_ scheduleCompileColumn (join $ join cols)
        getCompileTargets >>= traverse_ compileColumn
        propagate
      commitEngineState projectId st

    -- Update version
    putStrLn ("Successfully migrated to version 1" :: Text)
    setStoreValue "schemaVersion" (1 :: Int)

  i -> throwError $ ErrBug $
    "No migration found for version `" <> show i <> "`."

migrate
  :: (MonadHexl m, MonadHexlEnv m, MonadIO m)
  => m ()
migrate = do
  version <- fromMaybe 0 <$> getStoreValue "schemaVersion"
  traverse_ migration [version .. currentVersion - 1]
