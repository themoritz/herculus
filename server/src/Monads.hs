{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE InstanceSigs           #-}
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

import           Data.Proxy
import           Data.Aeson
import           Data.Text
import           Database.MongoDB ((=:))
import qualified Data.ByteString.Char8 as B8

import qualified Database.MongoDB            as Mongo

import           System.Log.FastLogger

import           Network.WebSockets

import           Lib.Model
import           Lib.Model.Types
import           Lib.Model.Dependencies
import           Lib.Model.Class
import           Lib.Types
import           Lib.Api.WebSocket
import           ConnectionManager

data AppError
  = ErrUser Text
  | ErrBug Text
  deriving (Show)

-- DB layer: Typed queries

class (Monad m, MonadLogger m, MonadError AppError m) => MonadDB m where
  getById :: Model a => Id a -> m (Either Text a)
  -- Throws `ErrBug` in Left case
  getById' :: Model a => Id a -> m a
  getOneByQuery :: Model a => Mongo.Selector -> m (Either Text (Entity a))
  -- Throws `ErrBug` in Left case
  getOneByQuery' :: Model a => Mongo.Selector -> m (Entity a)
  listByQuery :: Model a => Mongo.Selector -> m [Entity a]
  listAll :: Model a => m [Entity a]
  create :: Model a => a -> m (Id a)
  update :: Model a => Id a -> (a -> a) -> m ()
  upsert :: Model a => Mongo.Selector -> a -> m ()

-- Hexl layer: Business logic

class (Monad m, MonadLogger m, MonadError AppError m, MonadDB m) => MonadHexl m where
  sendWS :: WsDownMessage -> m ()

  getDependencyGraph :: m DependencyGraph
  storeDependencyGraph :: DependencyGraph -> m ()

  upsertCell :: Cell -> m ()

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
  getById :: forall a. Model a => Id a -> HexlT m (Either Text a)
  getById i = do
    let query = [ "_id" =: toObjectId i ]
        collection = collectionName (Proxy :: Proxy a)
    res <- runMongo $ Mongo.findOne (Mongo.select query collection)
    case res of
      Nothing -> pure $ Left "Not found"
      Just doc -> pure $ parseDocument doc

  -- -- Throws `ErrBug` in Left case
  getById' :: Model a => Id a -> HexlT m a
  getById' i = getById i >>= \case
    Left msg -> throwError $ ErrBug msg
    Right x -> pure x

  getOneByQuery :: forall a. Model a => Mongo.Selector -> HexlT m (Either Text (Entity a))
  getOneByQuery query = do
    let collection = collectionName (Proxy :: Proxy a)
    res <- runMongo $ Mongo.findOne (Mongo.select query collection)
    case res of
      Nothing -> pure $ Left "Not found"
      Just doc -> pure $ parseDocument doc

  -- -- Throws `ErrBug` in Left case
  getOneByQuery' :: Model a => Mongo.Selector -> HexlT m (Entity a)
  getOneByQuery' query = getOneByQuery query >>= \case
    Left msg -> throwError $ ErrBug msg
    Right x -> pure x

  listByQuery :: forall a. Model a => Mongo.Selector -> HexlT m [Entity a]
  listByQuery query = do
    let collection = collectionName (Proxy :: Proxy a)
    res <- runMongo $ Mongo.find (Mongo.select query collection) >>= Mongo.rest
    case traverse parseDocument res of
      Left msg -> throwError $ ErrBug msg
      Right xs -> pure xs

  listAll :: Model a => HexlT m [Entity a]
  listAll = listByQuery []

  create :: forall a. Model a => a -> HexlT m (Id a)
  create x = do
    let collection = collectionName (Proxy :: Proxy a)
    Mongo.ObjId i <- runMongo $ Mongo.insert collection $ toDocument x
    pure $ fromObjectId i

  update :: forall a. Model a => Id a -> (a -> a) -> HexlT m ()
  update i f = do
    let collection = collectionName (Proxy :: Proxy a)
    x <- getById' i
    runMongo $ Mongo.save collection $ toDocument $ Entity i (f x)

  upsert :: forall a. Model a => Mongo.Selector -> a -> HexlT m ()
  upsert query new = do
    let collection = collectionName (Proxy :: Proxy a)
    runMongo $ Mongo.upsert (Mongo.select query collection) $ toDocument new

--

instance (MonadIO m, MonadDB (HexlT m)) => MonadHexl (HexlT m) where
  sendWS msg = do
    connectionsRef <- asks envConnections
    connections <- allConnections <$> (liftIO $ atomically $ readTVar connectionsRef)
    forM_ connections $ \connection ->
      liftIO $ sendTextData connection $ encode msg

  getDependencyGraph = getOneByQuery [] >>= \case
    Right e -> pure $ dependenciesGraph $ entityVal e
    Left "Not found" -> pure emptyDependencyGraph
    Left _ -> throwError $ ErrBug "Dependency graph corrupt"

  storeDependencyGraph graph = getOneByQuery [] >>= \case
    Right e -> update (entityId e) (const (Dependencies graph))
    Left "Not found" -> void $ create (Dependencies graph)
    Left _ -> throwError $ ErrBug "Dependency graph corrupt"

  upsertCell cell@(Cell _ (Aspects t c r)) = do
    let query =
            [ "aspects.columnId" =: toObjectId c
            , "aspects.recordId" =: toObjectId r
            , "aspects.tableId"  =: toObjectId t
            ]
    upsert query cell

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
