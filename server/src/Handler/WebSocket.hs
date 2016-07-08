{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.WebSocket where

import           Control.Monad    (void)

import           Data.Maybe       (mapMaybe)

import           Database.MongoDB ((=:))
import qualified Database.MongoDB as Mongo

import           Lib.Api.WebSocket
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
