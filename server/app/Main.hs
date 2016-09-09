{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Control.Concurrent.STM
import           Control.Exception              (finally)
import           Control.Monad.Except

import           Data.Aeson
import           Data.ByteString.Lazy           (fromStrict)
import           Data.Monoid                    ((<>))
import           Data.Proxy
import           Data.Text.Encoding             (encodeUtf8)

import qualified Database.MongoDB               as Mongo

import           Servant

import           Network.Wai.Handler.Warp       as Warp
import           Network.Wai.Handler.WebSockets

import           Network.WebSockets

import           ConnectionManager
import           Handler.Rest
import           Handler.WebSocket
import           Lib.Api.Rest
import           Monads

import           Options                        (Options (..), getOptions)

type AllRoutes =
       Routes
  :<|> Raw

routes :: Proxy AllRoutes
routes = Proxy

rest :: HexlEnv -> Server AllRoutes
rest env =
         enter hexlToEither handle
    :<|> serveDirectory "../assets/public/"
  where
    hexlToEither :: HexlT IO :~> ExceptT ServantErr IO
    hexlToEither = Nat $ \hexlAction -> do
      result <- liftIO $ runHexl env hexlAction
      case result of
        Left err -> throwError (appErrToServantErr err)
        Right a  -> pure a

appErrToServantErr :: AppError -> ServantErr
appErrToServantErr err = case err of
    (ErrUser msg) -> err400 { errBody = toBS msg }
    (ErrBug msg)  -> err500 { errBody = toBS msg }
  where toBS = fromStrict . encodeUtf8

wsApp :: HexlEnv -> ServerApp
wsApp env pending = if requestPath (pendingRequest pending) == "/websocket"
  then do
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
    flip finally disconnect $ forever $ do
        message <- receiveData connection
        case eitherDecode message of
          Left err  -> putStrLn err
          Right wsUp -> do
            res <- runHexl env $ handleClientMessage wsUp
            case res of
              Left err -> print err
              Right a  -> pure a

  else rejectRequest pending "Wrong path"

main :: IO ()
main = do
  Options{..} <- getOptions
  pipe <- Mongo.connect $ Mongo.host optMongoHost
  connections <- atomically $ newTVar newConnectionManager
  let env = HexlEnv pipe "test" connections
      webSocketApp = wsApp env
      restApp = serve routes $ rest env
  putStrLn $ "Listening on port " <> show optPort <> " ..."
  Warp.run optPort $ websocketsOr defaultConnectionOptions webSocketApp restApp
