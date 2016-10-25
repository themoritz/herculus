{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Concurrent.STM
import           Control.Exception              (finally)
import           Control.Monad.Except
import           Data.Aeson
import           Data.Monoid                    ((<>))
import           Data.Proxy
import           Database.MongoDB               ((=:))
import qualified Database.MongoDB               as Mongo
import           Network.Wai.Handler.Warp       as Warp
import           Network.Wai.Handler.WebSockets
import           Network.WebSockets
import           Servant                        ((:<|>) (..), Context (..), Raw,
                                                 Server, serveDirectory,
                                                 serveWithContext)

import           Auth                           (AuthMiddleware, authHandler)
import           ConnectionManager
import           Handler.Rest
import           Handler.WebSocket
import           HexlNat                        (hexlToServant)
import           Lib.Api.Rest                   (Routes)
import           Lib.Model.Cell
import           Lib.Model.Class
import           Lib.Model.Column
import           Lib.Model.Record
import           Monads
import           Options                        (Options (..), getOptions)

type AllRoutes =
       Routes
  :<|> Raw

routes :: Proxy AllRoutes
routes = Proxy

rest :: HexlEnv -> FilePath -> Server AllRoutes
rest env path =
         (hexlToServant env) handle
    :<|> serveDirectory path

wsApp :: HexlEnv -> ServerApp
wsApp env pending =
    if requestPath (pendingRequest pending) == "/websocket"
        then handleRequest
        else rejectRequest pending "Wrong path"
  where
    handleRequest = do
      connection <- acceptRequest pending
      let connections = envConnections env
      connectionId <- atomically $ do
        mgr <- readTVar connections
        let (i, mgr') = addConnection connection mgr
        writeTVar connections mgr'
        pure i
      -- To keep connection alive in some browsers
      forkPingThread connection 30
      let disconnect = atomically $ modifyTVar connections $
            removeConnection connectionId
      flip finally disconnect $ forever $
            eitherDecode <$> receiveData connection >>= \case
              Left msg -> putStrLn msg
              Right wsUp -> runHexl env (handleClientMessage wsUp)
                >>= either print pure

-- middleware
handlerContext :: HexlEnv -> Context (AuthMiddleware ': '[])
handlerContext env = authHandler env :. EmptyContext

main :: IO ()
main = do
  Options{..} <- getOptions
  pipe <- Mongo.connect $ Mongo.host optMongoHost
  -- Ensure Mongo indices
  Mongo.access pipe Mongo.master optMongoCollection $ do
    let collColumn = collectionName (Proxy :: Proxy Column)
        collCell   = collectionName (Proxy :: Proxy Cell)
        collRecord = collectionName (Proxy :: Proxy Record)
        asc = 1 :: Int
    Mongo.ensureIndex $ Mongo.index collColumn [ "tableId" =: asc ]
    Mongo.ensureIndex $ Mongo.index collRecord [ "tableId" =: asc ]
    Mongo.ensureIndex $ Mongo.index collCell   [ "tableId" =: asc ]
    Mongo.ensureIndex $ Mongo.index collCell   [ "columnId" =: asc ]
    Mongo.ensureIndex $ Mongo.index collCell   [ "recordId" =: asc ]
  --
  connections <- atomically $ newTVar newConnectionManager
  let env = HexlEnv pipe optMongoCollection connections
      webSocketApp = wsApp env
      restApp = serveWithContext routes (handlerContext env) $ rest env optAssetDir
  putStrLn $ "Listening on port " <> show optPort <> " ..."
  Warp.run optPort $ websocketsOr defaultConnectionOptions webSocketApp restApp
