{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Monads
  ( AppError (..)
  , MonadDB (..)
  , MonadHexl (..)
  , HexlEnv (..)
  , HexlT
  , atomicallyConnectionMgr
  , runHexlT
  , askHexlEnv
  ) where

import           Control.Concurrent.STM      as STM
import           Control.Lens                (over)
import           Control.Lens.Prism          (_Left)
import           Control.Monad.Except
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Control

import           Data.Aeson
import qualified Data.ByteString.Char8       as B8
import qualified Data.ByteString.Lazy        as BL
import           Data.Foldable               (for_)
import           Data.Maybe                  (catMaybes)
import           Data.Monoid
import           Data.Proxy
import           Data.Text                   (Text, pack)
import           Data.Time.Clock             as Clock (getCurrentTime)
import           Data.Traversable            (for)

import           Database.MongoDB            ((=:))
import qualified Database.MongoDB            as Mongo

import           System.Log.FastLogger
import qualified Text.Pandoc                 as Pandoc
import qualified Text.Pandoc.PDF             as Pandoc

import           Network.WebSockets

import           ConnectionManager
import           Lib.Api.WebSocket
import           Lib.Model
import           Lib.Model.Class
import           Lib.Types

import qualified Latex

data AppError
  = ErrUser Text
  | ErrBug Text
    -- unauthorized: invalid request, e.g. missing authorization cookie
  | ErrUnauthorized Text
    -- forbidden: valid request but not allowed, e.g. session expired
  | ErrForbidden Text
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
  updateByQuery' :: Model a => Mongo.Selector -> (a -> a) -> m ()
  upsert :: Model a => Mongo.Selector -> a -> (a -> a) -> m (Maybe (Id a))
  upsertMany :: Model a => [Entity a] -> m ()
  delete :: Model a => Id a -> m ()
  deleteByQuery :: Model a => Proxy a -> Mongo.Selector -> m ()
  -- Other
  getCurrentTime :: m Time

-- Hexl layer: Business logic

class (Monad m, MonadLogger m, MonadError AppError m, MonadDB m) => MonadHexl m where
  sendWS :: [ConnectionId] -> WsDownMessage -> m ()
  withConnectionMgr :: State ConnectionManager a -> m a

  -- Pandoc stuff
  getDefaultTemplate :: String -> m (Either String String)
  runLatex :: Pandoc.WriterOptions -> String
           -> m (Either BL.ByteString BL.ByteString)
  makePDF :: Pandoc.WriterOptions -> Pandoc.Pandoc
          -> m (Either BL.ByteString BL.ByteString)

data HexlEnv = HexlEnv
  { envPipe        :: Mongo.Pipe
  , envDatabase    :: Text
  , envConnections :: STM.TVar ConnectionManager
  }

newtype HexlT m a = HexlT
  { unHexlT :: ExceptT AppError (ReaderT HexlEnv m) a
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadIO
             , MonadError AppError
             )

runHexlT :: HexlEnv -> HexlT m a -> m (Either AppError a)
runHexlT env action = runReaderT (runExceptT (unHexlT action)) env

askHexlEnv :: Monad m => HexlT m HexlEnv
askHexlEnv = HexlT $ lift $ ask

asksHexlEnv :: Monad m => (HexlEnv -> a) -> HexlT m a
asksHexlEnv f = f <$> askHexlEnv

instance MonadTrans HexlT where
  lift = HexlT . lift . lift

instance MonadReader r m => MonadReader r (HexlT m) where
  ask = lift ask
  local f action = do
    env <- askHexlEnv
    result <- lift $ local f (runHexlT env action)
    either throwError pure result

-- TODO: Filter for logLevel and show context
instance MonadIO m => MonadLogger (HexlT m) where
  monadLoggerLog _ _ _ msg =
    liftIO $ B8.putStrLn $ fromLogStr $ toLogStr msg

--

instance (MonadBaseControl IO m, MonadIO m) => MonadDB (HexlT m) where
  getById :: forall a. Model a => Id a -> HexlT m (Either Text a)
  getById i = do
    let query = [ "_id" =: toObjectId i ]
        collection = collectionName (Proxy :: Proxy a)
    res <- runMongo $ Mongo.findOne (Mongo.select query collection)
    case res of
      Nothing -> pure $ Left $
        "getById: Not found. Collection: " <> collection <>
        ", id: " <> pack (show i)
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
      Nothing -> pure $ Left $
        "getOneByQuery: Not found. Collection: " <> collection <>
        ", query: " <> pack (show query)
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

  updateByQuery' :: forall a. Model a => Mongo.Selector -> (a -> a) -> HexlT m ()
  updateByQuery' query f = do
    let collection = collectionName (Proxy :: Proxy a)
    getOneByQuery query >>= \case
      Left _ -> throwError $ ErrBug $
                  "updateByQuery: nothing found. Collection: " <>
                  collection <> ", query: " <>
                  pack (show query)
      Right (Entity i x) ->
        runMongo $ Mongo.save collection $ toDocument $ Entity i (f x)

  upsert :: Model a => Mongo.Selector -> a -> (a -> a) -> HexlT m (Maybe (Id a))
  upsert query new f =
    getOneByQuery query >>= \case
      Right (Entity i _) -> update i f *> pure Nothing
      Left _ -> Just <$> create (f new)

  upsertMany :: forall a. Model a => [Entity a] -> HexlT m ()
  upsertMany entities = do
    let collection = collectionName (Proxy :: Proxy a)
        toUpsert (Entity i a) =
          ( [ "_id" =: toObjectId i ]
          , toDocument a
          , [ Mongo.Upsert ]
          )
    void $ runMongo $ Mongo.updateMany collection (map toUpsert entities)

  delete :: forall a. Model a => Id a -> HexlT m ()
  delete i = deleteByQuery (Proxy :: Proxy a) [ "_id" =: toObjectId i ]

  deleteByQuery :: Model a => Proxy a -> Mongo.Selector -> HexlT m ()
  deleteByQuery proxy query = do
    let collection = collectionName proxy
    runMongo $ Mongo.delete (Mongo.select query collection)

  --

  getCurrentTime = Time <$> liftIO Clock.getCurrentTime

--

instance (MonadIO m, MonadDB (HexlT m)) => MonadHexl (HexlT m) where
  sendWS connectionIds msg = do
    mgr <- asksHexlEnv envConnections
    liftIO $ do
      connections <- fmap catMaybes $ atomicallyConnectionMgr mgr $
        for connectionIds getConnection
      for_ connections $ \connection ->
        sendTextData connection $ encode msg

  withConnectionMgr action = do
    ref <- asksHexlEnv envConnections
    liftIO $ atomicallyConnectionMgr ref action

  --

  getDefaultTemplate writer =
    over _Left show <$>
      liftIO (Pandoc.getDefaultTemplate Nothing writer)


  runLatex options source =
    liftIO $ Latex.makePDF options "pdflatex" source

  makePDF options pandoc =
    liftIO $ Pandoc.makePDF "pdflatex" Pandoc.writeLaTeX options pandoc

--

atomicallyConnectionMgr :: TVar ConnectionManager
                        -> State ConnectionManager a
                        -> IO a
atomicallyConnectionMgr ref action = atomically $ do
  manager <- readTVar ref
  let (a, manager') = runState action manager
  writeTVar ref manager'
  pure a

--

runMongo :: MonadIO m => Mongo.Action IO a -> HexlT m a
runMongo action = do
  pipe <- asksHexlEnv envPipe
  database <- asksHexlEnv envDatabase
  liftIO $ Mongo.access pipe Mongo.master database action
