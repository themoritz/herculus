{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications  #-}
-- |

module Migration.Tasks where

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

recompileColumns :: (MonadHexl m, MonadHexlEnv m, MonadIO m) => m ()
recompileColumns = do
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
