{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Concurrent.STM
import           Control.Monad.IO.Class
import           Control.Monad.Except

import           Data.Aeson
import           Data.Proxy

import qualified Database.MongoDB               as Mongo

import           Servant

import           Network.Wai.Handler.Warp       as Warp
import           Network.Wai.Handler.WebSockets

import           Network.WebSockets

import           Handler.Rest
import           Handler.WebSocket
import           Lib.Api.Rest
import           Monads

type AllRoutes =
       Routes
  :<|> Raw

routes :: Proxy AllRoutes
routes = Proxy

rest :: HexlEnv -> Server AllRoutes
rest env =
         enter hexlToEither handle
    :<|> serveDirectory "../client/.stack-work/install/x86_64-linux/lts-6.0/ghcjs-0.2.0_ghc-7.10.3/bin/client.jsexe/"
  where
    hexlToEither :: HexlT IO :~> ExceptT ServantErr IO
    hexlToEither = Nat $ \hexlAction -> do
      result <- liftIO $ runHexl env hexlAction
      pure result

wsApp :: HexlEnv -> ServerApp
wsApp env pending = if requestPath (pendingRequest pending) == "/websocket"
  then do
    connection <- acceptRequest pending
    forever $ do
      message <- receiveData connection
      case eitherDecode message of
        Left err  -> putStrLn err
        Right wsUp -> runHexl env $ handleClientMessage wsUp

  else rejectRequest pending "Wrong path"

main :: IO ()
main = do
  pipe <- Mongo.connect $ Mongo.host "127.0.0.1"
  connections <- atomically $ newTVar []
  let env = HexlEnv pipe "test" connections
      webSocketApp = wsApp env
      restApp = serve routes $ rest env
  Warp.run 3000 $ websocketsOr defaultConnectionOptions webSocketApp restApp
