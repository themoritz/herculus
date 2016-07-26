{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
-- {-# LANGUAGE StandaloneDeriving #-}

module Monads
  ( AppError (..)
  , MonadDB (..)
  , MonadHexl (..)
  , HexlEnv (..)
  , HexlT
  , runHexl
  ) where

import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Control.Monad.Except
import           Control.Monad.Logger
import           Control.Monad.Trans.Control
-- import Control.Monad.Base

import           Data.Aeson
import           Data.Text
import qualified Data.ByteString.Char8 as B8

import qualified Database.MongoDB            as Mongo

import           System.Log.FastLogger

import           Network.WebSockets

import           Lib.Api.WebSocket
import           ConnectionManager

data AppError
  = ErrUser Text
  | ErrBug Text
  deriving (Show)

-- DB layer: Typed queries

class (Monad m, MonadLogger m, MonadError AppError m) => MonadDB m where

-- Hexl layer: Business logic

class (Monad m, MonadLogger m, MonadError AppError m) => MonadHexl m where
  sendWS :: WsDownMessage -> m ()

data HexlEnv = HexlEnv
  { envPipe        :: Mongo.Pipe
  , envDatabase    :: Text
  , envConnections :: TVar ConnectionManager
  }

newtype HexlT m a = HexlT
  { unHexlT :: ExceptT AppError (ReaderT HexlEnv m) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadReader HexlEnv
             , MonadError AppError
             )

instance MonadIO m => MonadLogger (HexlT m) where
  monadLoggerLog loc src lvl msg = do
    liftIO $ B8.putStrLn $ fromLogStr $ toLogStr msg

--

instance (MonadBaseControl IO m, MonadIO m) => MonadDB (HexlT m) where

--

instance (MonadIO m, MonadDB (HexlT m)) => MonadHexl (HexlT m) where
  sendWS msg = do
    connectionsRef <- asks envConnections
    connections <- allConnections <$> (liftIO $ atomically $ readTVar connectionsRef)
    forM_ connections $ \connection ->
      liftIO $ sendTextData connection $ encode msg

--

runHexl :: HexlEnv -> HexlT m a -> m (Either AppError a)
runHexl env action = runReaderT (runExceptT (unHexlT action)) env

--

runMongo :: (Monad m, MonadIO m) => Mongo.Action IO a -> HexlT m a
runMongo action = do
  pipe <- asks envPipe
  database <- asks envDatabase
  liftIO $ Mongo.access pipe Mongo.master database action

-- deriving instance (MonadBase b m) => MonadBase b (HexlT m)

-- instance MonadTrans HexlT where
--   lift = HexlT . lift

-- instance MonadTransControl HexlT where
--   type StT HexlT a = StT (ReaderT HexlEnv) a
--   liftWith f = HexlT $ liftWith $ \run -> f (run . unHexlT)
--   restoreT = HexlT . restoreT

-- instance MonadBaseControl b m => MonadBaseControl b (HexlT m) where
--   type StM (HexlT m) a = ComposeSt HexlT m a
--   liftBaseWith = defaultLiftBaseWith
--   restoreM     = defaultRestoreM
