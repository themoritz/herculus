{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Handler.Rest where

import           Control.Monad

import           Data.Aeson.Bson
import           Data.Maybe
import           Data.Monoid
import           Data.Text        (Text)

import           Servant

import           Database.MongoDB ((=:))
import qualified Database.MongoDB as Mongo

import           Lib
import           Lib.Api.Rest
import           Monads
import           Dependencies
import           Propagate
import           Lib.Expression
import           Eval

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
handleTableCreate (TableCreate projId name) = do
  Mongo.ObjId i <- runMongo $ Mongo.insert "tables"
    [ "name" =: name
    , "projectId" =: toObjectId projId
    ]
  pure $ fromObjectId i

handleTableList :: MonadHexl m => Id Project -> m [Table]
handleTableList projId = do
  let query = [ "projectId" =: toObjectId projId ]
  tables <- runMongo $ Mongo.find (Mongo.select query "tables") >>= Mongo.rest
  let go table = do
        i <- Mongo.lookup "_id" table
        n <- Mongo.lookup "name" table
        pure $ Table (fromObjectId i) projId n
  pure $ mapMaybe go tables

handleTableData :: MonadHexl m => Id Table -> m [(Id Column, Id Record, Value)]
handleTableData tblId = do
  cells <- runMongo $ Mongo.find
    (Mongo.select ["aspects.tableId" =: toObjectId tblId] "cells") >>= Mongo.rest
  let go cell = do
        aspects <- Mongo.lookup "aspects" cell
        r <- Mongo.lookup "recordId" aspects
        c <- Mongo.lookup "columnId" aspects
        v <- Mongo.lookup "value" cell
        pure (fromObjectId c, fromObjectId r, v)
  pure $ mapMaybe go cells

--

handleColumn :: MonadHexl m => ServerT ColumnRoutes m
handleColumn =
       handleColumnCreate
  :<|> handleColumnSetName
  :<|> handleColumnSetType
  :<|> handleColumnList

handleColumnCreate :: MonadHexl m => Id Table -> m (Id Column)
handleColumnCreate tblId = do
  Mongo.ObjId i <- runMongo $ Mongo.insert "columns"
      [ "name" =: ("" :: Text)
      , "tableId" =: toObjectId tblId
      , "columnType" =: toValue (ColumnInput DataString)
      ]
  pure $ fromObjectId i

handleColumnSetName :: MonadHexl m => Id Column -> Text -> m ()
handleColumnSetName colId name = do
  let query = Mongo.select [ "_id" =: toObjectId colId ] "columns"
  void $ runMongo $ Mongo.fetch query >>=
    Mongo.save "columns" . Mongo.merge [ "name" =: name ]

handleColumnSetType :: MonadHexl m => Id Column -> ColumnType -> m ()
handleColumnSetType colId cType = do
  let query = Mongo.select [ "_id" =: toObjectId colId ] "columns"
  runMongo $ Mongo.fetch query >>=
    Mongo.save "columns" . Mongo.merge [ "columnType" =: toValue cType ]
  -- Get old dependencies
  mDep <- runMongo $ Mongo.findOne (Mongo.select [] "dependencies")
  let graph = fromMaybe emptyDependencyGraph $ mDep >>=
                                               Mongo.lookup "graph" >>=
                                               decodeValue
  graph' <- case cType of
    ColumnInput _ -> pure $ setDependencies colId [] graph
    ColumnDerived formula -> case parseExpression formula of
      Left _ -> pure $ setDependencies colId [] graph
      Right expr -> do
        deps <- collectDependencies expr
        let graph' = setDependencies colId deps graph
            topoResult = getDependentTopological colId graph'
        case topoResult of
          Nothing -> pure $ setDependencies colId [] graph
          Just order -> do
            -- TODO: Propagate
            pure graph'
  runMongo $ Mongo.save "dependencies" $
          Mongo.merge [ "graph" =: toValue graph' ] $
          fromMaybe [] mDep

handleColumnList :: MonadHexl m => Id Table -> m [Column]
handleColumnList tblId = do
  columns <- runMongo $ Mongo.find
    (Mongo.select ["tableId" =: toObjectId tblId] "columns") >>= Mongo.rest
  let go column = do
        i <- Mongo.lookup "_id" column
        n <- Mongo.lookup "name" column
        ct <- Mongo.lookup "columnType" column >>= decodeValue
        pure $ Column (fromObjectId i) n ct
  pure $ mapMaybe go columns

--

handleRecord :: MonadHexl m => ServerT RecordRoutes m
handleRecord =
       handleRecordCreate
  :<|> handleRecordList

handleRecordCreate :: MonadHexl m => Id Table -> m (Id Record)
handleRecordCreate tblId = do
  (Mongo.ObjId i) <- runMongo $ Mongo.insert "records"
    [ "tableId" =: toObjectId tblId
    ]
  pure $ fromObjectId i

handleRecordList :: MonadHexl m => Id Table -> m [Record]
handleRecordList tblId = do
  records <- runMongo $ Mongo.find
    (Mongo.select ["tableId" =: toObjectId tblId] "records") >>= Mongo.rest
  let go record = (Record . fromObjectId) <$> Mongo.lookup "_id" record
  pure $ mapMaybe go records

--

handleCell :: MonadHexl m => ServerT CellRoutes m
handleCell =
       handleCellSet

handleCellSet :: MonadHexl m => CellSet -> m ()
handleCellSet (CellSet tblId colId recId value) = do
  let query =
        [ "aspects" =:
          [ "columnId" =: toObjectId colId -- "concept"
          , "recordId" =: toObjectId recId
          , "tableId"  =: toObjectId tblId
          ]
        ]
  void $ runMongo $ Mongo.upsert (Mongo.select query "cells") $
    [ "value" =: value ] <> query
  mDep <- runMongo $ Mongo.findOne (Mongo.select [] "dependencies")
  let graph = fromMaybe emptyDependencyGraph $ mDep >>=
                                               Mongo.lookup "graph" >>=
                                               decodeValue
      mOrder = getDependentTopological colId graph
  case mOrder of
    Just order -> runPropagate recId order
    Nothing -> pure ()
