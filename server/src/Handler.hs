{-# LANGUAGE OverloadedStrings #-}

module Handler where

import Control.Monad (void)
import Data.Maybe (mapMaybe)

import           Database.MongoDB     ((=:))
import qualified Database.MongoDB     as Mongo

import Lib
import Monads
import Data.Aeson.Bson

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
  WsUpAddRecord tableId entries ->
    undefined

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
