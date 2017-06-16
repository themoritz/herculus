{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications  #-}
-- |

module Migration.Tasks where

import           Lib.Prelude

import           Control.Lens           hiding (( # ))

import           Database.MongoDB       ((=:))
import qualified Database.MongoDB       as Mongo

import           Lib.Model
import           Lib.Model.Class
import           Lib.Model.Column
import           Lib.Model.Dependencies
import           Lib.Model.Project
import           Lib.Model.Table
import           Lib.Types

import           Engine
import           Engine.Compile
import           Engine.Monad
import           Engine.Propagate
import           Monads

-- | Also rebuilds every project's dependency graph.
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

  -- Recompile everything
  ps <- listAll
  for_ ps $ \(Entity projectId project) -> do
    tables <- listByQuery [ "projectId" =: toObjectId projectId ]
    cols <- for tables $ \(Entity tableId _) -> do
      listByQuery [ "tableId" =: toObjectId (tableId :: Id Table) ]
    liftIO $ putStrLn $
      "Compiling project " <> project ^. projectName <>
      " (" <> show projectId <> ") ..."
    -- The empty dependency graph will be rebuilt completely in this process
    p <- getById' projectId
    let p' = p # projectDependencyGraph .~ emptyDependencyGraph
    (_, st) <- runEngineT (Entity projectId p') $ do
      for_ (join cols) $ \(Entity columnId col) -> do
        let compile = scheduleCompileColumn columnId
        case col ^. columnKind of
          ColumnData dataCol -> case dataCol ^. dataColIsDerived of
            Derived -> do
              let refDeps = getTypeDependencies (dataCol ^. dataColType)
              _ <- graphSetTypeDependencies columnId refDeps
              compile
            NotDerived ->
              pure ()
          ColumnReport _ ->
            compile
      getCompileTargets >>= traverse_ compileColumn
      propagate
    commitEngineState projectId st
