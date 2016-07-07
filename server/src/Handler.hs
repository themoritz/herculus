{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Handler where

import           Control.Monad    (void)
import           Control.Lens

import           Data.Maybe       (mapMaybe)
import qualified Data.Map as Map

import           Database.MongoDB ((=:))
import qualified Database.MongoDB as Mongo

import           Data.Aeson.Bson
import           Lib
import           Monads

handleClientMessage :: MonadHexl m => WsUpMessage -> m ()
handleClientMessage wsUp = case wsUp of

  WsUpGreet msg ->
    sendWS $ WsDownGreet msg
  WsUpStore x ->
    void $ runMongo $ Mongo.insert "messages"
      [ "text" =: x
      ]
  WsUpList -> do
    docs <- runMongo $ Mongo.find (Mongo.select [] "messages") >>= Mongo.rest
    sendWS $ WsDownList $ mapMaybe (Mongo.lookup "text") docs

  WsUpCreateProject name ->
    void $ runMongo $ Mongo.insert "projects"
      [ "name" =: name
      ]
  WsUpCreateTable projectId name ->
    void $ runMongo $ Mongo.insert "tables"
      [ "name" =: name
      , "projectId" =: toObjectId projectId
      ]
  WsUpCreateColumn tableId name ct ->
    void $ runMongo $ Mongo.insert "columns"
      [ "name" =: name
      , "tableId" =: toObjectId tableId
      , "columnType" =: toValue ct
      ]
  WsUpAddRecord tableId entries -> do
    recordId <- runMongo $ Mongo.insert "records"
      [ "tableId" =: toObjectId tableId
      ]
    let go (colId, val) =
          [ "value" =: val
          , "aspects" =:
            [ "columnId" =: toObjectId colId -- "concept"
            , "recordId" =: recordId
            , "tableId" =: toObjectId tableId
            ]
          ]
    void $ runMongo $ Mongo.insertMany "cells" $ map go entries
  WsUpSetCell tableId columnId recordId value -> do
    let query =
          [ "aspects.columnId" =: toObjectId columnId
          , "aspects.recordId" =: toObjectId recordId
          , "aspects.tableId"  =: toObjectId tableId
          ]
    void $ runMongo $ Mongo.upsert (Mongo.select query "cells") ["value" =: value ]

  WsUpListProjects -> do
    projects <- runMongo $ Mongo.find (Mongo.select [] "projects") >>= Mongo.rest
    let go project = do
          i <- Mongo.lookup "_id" project
          n <- Mongo.lookup "name" project
          pure (fromObjectId i, n)
    sendWS $ WsDownProjects $ mapMaybe go projects
  WsUpListTables prjId -> do
    let query = [ "projectId" =: toObjectId prjId ]
    tables <- runMongo $ Mongo.find (Mongo.select query "tables") >>= Mongo.rest
    let go table = do
          i <- Mongo.lookup "_id" table
          n <- Mongo.lookup "name" table
          pure (fromObjectId i, n)
    sendWS $ WsDownTables prjId $ mapMaybe go tables
  WsUpGetColumns tableId -> do
    columns <- runMongo $ Mongo.find
      (Mongo.select ["tableId" =: toObjectId tableId] "columns") >>= Mongo.rest
    let go column = do
          i <- Mongo.lookup "_id" column
          n <- Mongo.lookup "name" column
          ct <- Mongo.lookup "columnType" column >>= decodeValue
          pure (fromObjectId i, n, ct)
    sendWS $ WsDownColumns tableId $ mapMaybe go columns
  WsUpGetTable tableId -> do
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
    sendWS $ WsDownTable tableId $ Map.toList $ fmap Map.toList maps
