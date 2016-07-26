{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
-- {-# LANGUAGE StandaloneDeriving #-}

module Monads
  ( MonadHexl(..)
  , HexlEnv(..)
  , HexlT
  , runHexl
  ) where

import           Control.Concurrent.STM
import           Control.Monad.Reader
import           Control.Monad.Trans.Control
-- import Control.Monad.Base

import           Data.Aeson
import           Data.Text

import qualified Database.MongoDB            as Mongo

import           Network.WebSockets

import           Lib.Api.WebSocket
import           ConnectionManager

class Monad m => MonadHexl m where
  sendWS :: WsDownMessage -> m ()
  runMongo :: Mongo.Action IO a -> m a

data HexlEnv = HexlEnv
  { envPipe        :: Mongo.Pipe
  , envDatabase    :: Text
  , envConnections :: TVar ConnectionManager
  }

newtype HexlT m a = HexlT
  { unHexlT :: ReaderT HexlEnv m a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadReader HexlEnv
             )

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

instance (MonadBaseControl IO m, MonadIO m) => MonadHexl (HexlT m) where
  sendWS msg = do
    connectionsRef <- asks envConnections
    connections <- allConnections <$> (liftIO $ atomically $ readTVar connectionsRef)
    forM_ connections $ \connection ->
      liftIO $ sendTextData connection $ encode msg
  runMongo action = do
    pipe <- asks envPipe
    database <- asks envDatabase
    liftIO $ Mongo.access pipe Mongo.master database action

runHexl :: HexlEnv -> HexlT m a -> m a
runHexl env action = runReaderT (unHexlT action) env
