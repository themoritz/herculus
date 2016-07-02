{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Monad                  (forever)

import           Data.Aeson

import           Data.Proxy
import           Data.Text
import qualified Data.Text.IO                   as Text

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

wsApp :: ServerApp
wsApp pending = if requestPath (pendingRequest pending) == "/websocket"
  then do
    connection <- acceptRequest pending
    forever $ do
        message <- receiveData connection
        case eitherDecode message of
            Left err  -> putStrLn err
            Right (WsUpGreet msg) -> do
                let response = WsDownGreet msg
                sendTextData connection $ encode response
  else rejectRequest pending "Wrong path"

main :: IO ()
main = do
  Text.putStrLn foo
  Warp.run 3000 $ websocketsOr defaultConnectionOptions wsApp $ serve api rest
