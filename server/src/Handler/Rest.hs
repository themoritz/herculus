{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Handler.Rest where

import           Control.Lens
import           Control.Monad

import           Data.Aeson.Bson
import           Data.Map         (Map)
import qualified Data.Map         as Map
import           Data.Maybe
import           Data.Text        (Text)

import           Servant

import           Database.MongoDB ((=:))
import qualified Database.MongoDB as Mongo

import           Lib
import           Lib.Api.Rest
import           Monads

handle :: MonadHexl m => ServerT Routes m
handle =
       handleProject
  :<|> handleTable
  :<|> handleColumn
  :<|> handleRecord
  :<|> handleCell

--

handleProject :: MonadHexl m => ServerT ProjectRoutes m
handleProject =
       handleProjectCreate
  :<|> handleProjectList

handleProjectCreate :: MonadHexl m => Text -> m (Id Project)
handleProjectCreate name = do
  Mongo.ObjId i <- runMongo $ Mongo.insert "projects"
    [ "name" =: name
    ]
  pure $ fromObjectId i

handleProjectList :: MonadHexl m => m [Project]
handleProjectList = do
  projects <- runMongo $ Mongo.find (Mongo.select [] "projects") >>= Mongo.rest
  let go project = do
        i <- Mongo.lookup "_id" project
        n <- Mongo.lookup "name" project
        pure $ Project (fromObjectId i) n
  pure $ mapMaybe go projects

--

handleTable :: MonadHexl m => ServerT TableRoutes m
handleTable =
       handleTableCreate
  :<|> handleTableList
  :<|> handleTableData

handleTableCreate :: MonadHexl m => TableCreate -> m (Id Table)
handleTableCreate (TableCreate projectId name)= do
  Mongo.ObjId i <- runMongo $ Mongo.insert "tables"
    [ "name" =: name
    , "projectId" =: toObjectId projectId
    ]
  pure $ fromObjectId i

handleTableList :: MonadHexl m => Id Project -> m [Table]
handleTableList projectId = do
  let query = [ "projectId" =: toObjectId projectId ]
  tables <- runMongo $ Mongo.find (Mongo.select query "tables") >>= Mongo.rest
  let go table = do
        i <- Mongo.lookup "_id" table
        n <- Mongo.lookup "name" table
        pure $ Table (fromObjectId i) projectId n
  pure $ mapMaybe go tables

handleTableData :: MonadHexl m => Id Table -> m [(Id Record, [(Id Column, Text)])]
handleTableData tableId = do
  cells <- runMongo $ Mongo.find
    (Mongo.select ["aspects.tableId" =: toObjectId tableId] "cells") >>= Mongo.rest
  let go cell m = let mrc = do
                        aspects <- Mongo.lookup "aspects" cell
                        r <- Mongo.lookup "recordId" aspects
                        c <- Mongo.lookup "columnId" aspects
                        v <- Mongo.lookup "value" cell
                        pure (fromObjectId r, fromObjectId c, v)
        in case mrc of
             Nothing -> m
             Just (r, c, v) -> m & at r . non Map.empty . at c .~ v
      maps = foldr go Map.empty cells
  pure $ Map.toList $ fmap Map.toList maps

--

handleColumn :: MonadHexl m => ServerT ColumnRoutes m
handleColumn =
       handleColumnCreate
  :<|> handleColumnList

handleColumnCreate :: MonadHexl m => ColumnCreate -> m (Id Column)
handleColumnCreate (ColumnCreate tableId name ct)= do
  Mongo.ObjId i <- runMongo $ Mongo.insert "columns"
      [ "name" =: name
      , "tableId" =: toObjectId tableId
      , "columnType" =: toValue ct
      ]
  pure $ fromObjectId i

handleColumnList :: MonadHexl m => Id Table -> m [(Id Column, Text, ColumnType)]
handleColumnList tableId = do
  columns <- runMongo $ Mongo.find
    (Mongo.select ["tableId" =: toObjectId tableId] "columns") >>= Mongo.rest
  let go column = do
        i <- Mongo.lookup "_id" column
        n <- Mongo.lookup "name" column
        ct <- Mongo.lookup "columnType" column >>= decodeValue
        pure (fromObjectId i, n, ct)
  pure $ mapMaybe go columns

--

handleRecord :: MonadHexl m => ServerT RecordRoutes m
handleRecord =
       handleRecordCreate

handleRecordCreate :: MonadHexl m => RecordCreate -> m (Id Record)
handleRecordCreate (RecordCreate tableId) = do
  recordId@(Mongo.ObjId i) <- runMongo $ Mongo.insert "records"
    [ "tableId" =: toObjectId tableId
    ]
  pure $ fromObjectId i
  -- let go (colId, val) =
  --       [ "value" =: val
  --       , "aspects" =:
  --         [ "columnId" =: toObjectId colId -- "concept"
  --         , "recordId" =: recordId
  --         , "tableId" =: toObjectId tableId
  --         ]
  --       ]
  -- void $ runMongo $ Mongo.insertMany "cells" $ map go entries

--

handleCell :: MonadHexl m => ServerT CellRoutes m
handleCell =
       handleCellSet

handleCellSet :: MonadHexl m => CellSet -> m ()
handleCellSet (CellSet tableId columnId recordId value) = do
  let query =
        [ "aspects.columnId" =: toObjectId columnId
        , "aspects.recordId" =: toObjectId recordId
        , "aspects.tableId"  =: toObjectId tableId
        ]
  void $ runMongo $ Mongo.upsert (Mongo.select query "cells") ["value" =: value ]
