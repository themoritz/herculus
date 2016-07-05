{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Monad                  (forever)

import           Data.Aeson
import           Data.Proxy
import           Data.Text

import qualified Database.MongoDB               as Mongo

import           Servant

import           Network.Wai.Handler.Warp       as Warp
import           Network.Wai.Handler.WebSockets

import           Network.WebSockets

import           Handler
import           Monads

type Api = "command" :> Get '[JSON] Text

api :: Proxy Api
api = Proxy

rest :: Server Api
rest = pure "Hello"

wsApp :: Mongo.Pipe -> ServerApp
wsApp pipe pending = if requestPath (pendingRequest pending) == "/websocket"
  then do
    connection <- acceptRequest pending
    forever $ do
      message <- receiveData connection
      case eitherDecode message of
        Left err  -> putStrLn err
        Right wsUp -> do
          let env = HexlEnv pipe "test" connection
          runHexl env $ handleClientMessage wsUp

  else rejectRequest pending "Wrong path"

main :: IO ()
main = do
  pipe <- Mongo.connect $ Mongo.host "127.0.0.1"
  Warp.run 3000 $ websocketsOr defaultConnectionOptions (wsApp pipe) $ serve api rest
