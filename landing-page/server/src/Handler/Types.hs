{-# LANGUAGE FlexibleContexts #-}
-- |

module Handler.Types
  ( HandlerT
  , runInHandlerEnv
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.Default           (def)
import qualified Database.MongoDB       as Mongo
import           Diener                 (DienerT, LogEnv (LogEnv), withLogger)
import           Diener.Logger          (Settings (filePath))
import           Network.Socket         (HostName)

import           Types                  (AppError (..), Env (..))

type HandlerT = DienerT AppError Env

runInHandlerEnv :: HostName -> Mongo.Database -> (LogEnv Env -> IO a) -> IO a
runInHandlerEnv host dbName action =
  withLogger def { filePath = "subscribers.log" } $ \logFn -> liftIO $ do
    pipe <- Mongo.connect $ Mongo.host host
    let runDb = Mongo.access pipe Mongo.master dbName
        env = LogEnv logFn (Env runDb)
    action env
