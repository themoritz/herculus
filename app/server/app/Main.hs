{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Lib.Prelude                    hiding (handle)

import           Control.Concurrent.STM

import           Data.Aeson

import           Database.MongoDB               ((=:))
import qualified Database.MongoDB               as Mongo
import           Network.Wai.Handler.Warp       as Warp
import           Network.Wai.Handler.WebSockets
import           Network.WebSockets
import           Servant                        ((:<|>) (..), (:>), Get,
                                                 PlainText, Raw, Server, serve,
                                                 serveDirectoryFileServer, hoistServer)

import           ConnectionManager
import           Handler.Rest
import           Handler.WebSocket
import           HexlNat                        (hexlToServant)
import           Lib.Api.Rest                   (Routes)
import           Lib.Model.Cell
import           Lib.Model.Class
import           Lib.Model.Column
import           Lib.Model.Row
import           Migration
import           Monads
import           Options                        (Options (..), getOptions)

type AllRoutes =
       "api" :> "status" :> Get '[PlainText] Text
  :<|> "api" :> Routes
  :<|> Raw

routes :: Proxy AllRoutes
routes = Proxy

rest :: HexlEnv -> FilePath -> Server AllRoutes
rest env path =
         pure ("Everything is ok." :: Text)
    :<|> hoistServer (Proxy :: Proxy Routes) (hexlToServant env) handle
    :<|> serveDirectoryFileServer path

wsApp :: HexlEnv -> ServerApp
wsApp env pending =
    if requestPath (pendingRequest pending) == "/websocket"
        then handleRequest
        else rejectRequest pending "Wrong path"
  where
    handleRequest = do
      connection <- acceptRequest pending
      let connections = envConnections env
      connectionId <- atomicallyConnectionMgr connections $
        addConnection connection
      -- To keep connection alive in some browsers
      withPingThread connection 30 (pure ()) $ do
        let disconnect = atomicallyConnectionMgr connections $
              removeConnection connectionId
        flip finally disconnect $ forever $
          eitherDecode <$> receiveData connection >>= \case
            Left msg -> putStrLn msg
            Right wsUp -> runHexlT env (handleClientMessage connectionId wsUp)
              >>= either print pure

main :: IO ()
main = do
  Options{..} <- getOptions
  pipe <- Mongo.connect $ Mongo.host optMongoHost
  -- Ensure Mongo indices
  Mongo.access pipe Mongo.master optMongoCollection $ do
    let collColumn = collectionName (Proxy :: Proxy Column)
        collCell   = collectionName (Proxy :: Proxy Cell)
        collRecord = collectionName (Proxy :: Proxy Row)
        asc = 1 :: Int
    Mongo.ensureIndex $ Mongo.index collColumn [ "tableId" =: asc ]
    Mongo.ensureIndex $ Mongo.index collRecord [ "tableId" =: asc ]
    Mongo.ensureIndex $ Mongo.index collCell   [ "tableId" =: asc ]
    Mongo.ensureIndex $ Mongo.index collCell   [ "columnId" =: asc ]
    Mongo.ensureIndex $ Mongo.index collCell   [ "rowId" =: asc ]
  --
  connections <- atomically $ newTVar mkConnectionManager
  let env = HexlEnv pipe optMongoCollection connections optTexInputs
      webSocketApp = wsApp env
      restApp = serve routes $ rest env optAssetDir
  --
  runHexlT env migrate >>= \case
    Left err -> putStrLn $ "Error during migration: " <> prettyAppError err
    Right () -> do
      putStrLn ("Listening on port " <> show optPort <> " ..." :: Text)
      Warp.run optPort $
        websocketsOr defaultConnectionOptions webSocketApp restApp
