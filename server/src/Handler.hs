{-# LANGUAGE OverloadedStrings #-}

module Handler where

import Control.Monad (void)
import Data.Maybe (mapMaybe)

import           Database.MongoDB     ((=:))
import qualified Database.MongoDB     as Mongo

import Lib
import Monads

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
  WsUpCreateTable prjId name ->
    void $ runMongo $ Mongo.insert "tables"
      [ "name" =: name
      , "projectId" =: uuId prjId
      ]
  WsUpCreateColumn tblId name columnType ->
    void $ runMongo $ Mongo.insert "columns"
      [ "name" =: name
      , "tableId" =: uuId tblId
      , "columnType"
      ]
