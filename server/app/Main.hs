{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Monad                  (forever, void)

import           Data.Aeson
import           Data.Foldable
import           Data.Maybe                     (mapMaybe)
import           Data.Proxy
import           Data.Text
import qualified Data.Text.IO                   as Text

import           Database.MongoDB               ((=:))
import qualified Database.MongoDB               as Mongo

import           Servant
import           Servant.API

import           Network.Wai.Handler.Warp       as Warp
import           Network.Wai.Handler.WebSockets

import           Network.WebSockets

import           Lib
import           Server

type Api = "command" :> Get '[JSON] Text

api :: Proxy Api
api = Proxy

rest :: Server Api
rest = pure "Hello"

wsApp :: Mongo.Pipe -> ServerApp
wsApp pipe pending = if requestPath (pendingRequest pending) == "/websocket"
  then do
    let run = Mongo.access pipe Mongo.master "test"
    connection <- acceptRequest pending
    forever $ do
      message <- receiveData connection
      case eitherDecode message of
        Left err  -> putStrLn err
        Right wsUp -> do
          response <- case wsUp of
            WsUpGreet msg ->
              pure $ Just $ WsDownGreet msg
            WsUpStore x -> do
              run $ Mongo.insert "messages" ["text" =: x]
              pure Nothing
            WsUpList -> do
              docs <- run $ Mongo.find (Mongo.select [] "messages") >>= Mongo.rest
              pure $ Just $ WsDownList $ mapMaybe (Mongo.lookup "text") docs
          for_ response $ sendTextData connection . encode

  else rejectRequest pending "Wrong path"

main :: IO ()
main = do
  pipe <- Mongo.connect $ Mongo.host "127.0.0.1"
  Warp.run 3000 $ websocketsOr defaultConnectionOptions (wsApp pipe) $ serve api rest
