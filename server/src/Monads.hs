{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE StandaloneDeriving #-}

module Monads
  ( MonadHexl(..)
  , HexlEnv(..)
  , runHexl
  ) where

import           Control.Monad.Reader
import Control.Monad.Trans.Control
-- import Control.Monad.Base

import           Data.Aeson
import           Data.Text

import qualified Database.MongoDB     as Mongo

import           Network.WebSockets

import           Lib.Api.WebSocket

class Monad m => MonadHexl m where
  sendWS :: WsDownMessage -> m ()
  runMongo :: Mongo.Action IO a -> m a

data HexlEnv = HexlEnv
  { envPipe       :: Mongo.Pipe
  , envDatabase   :: Text
  , envConnection :: Connection
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
    connection <- asks envConnection
    liftIO $ sendTextData connection $ encode msg
  runMongo action = do
    pipe <- asks envPipe
    database <- asks envDatabase
    liftIO $ Mongo.access pipe Mongo.master database action

runHexl :: HexlEnv -> HexlT m a -> m a
runHexl env action = runReaderT (unHexlT action) env
