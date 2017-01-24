{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Database.MongoDb where

import           Control.Exception.Lifted    (handleJust)
import           Control.Monad.Except        (throwError)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Bson                   as Bson
import           Data.Maybe                  (mapMaybe)
import           Data.Monoid                 ((<>))
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import qualified Database.MongoDB            as Mongo
import           Diener                      (asks)

import           Handler.Types               (HandlerT)
import           Types                       (AppError (..), Env (envRunDb))

import           Database.MongoDb.Class      (FromDocument (..),
                                              ToDocument (..))

type IOConstraint m = (MonadIO m, MonadBaseControl IO m)

class ReadOnly m where
  getAll :: FromDocument x => Mongo.Collection -> m [x]

class Insert m where
  insert_ :: ToDocument x => Mongo.Collection -> x -> m ()
  insert :: ToDocument x => Mongo.Collection -> x -> m Bson.Value

instance IOConstraint m => Insert (HandlerT m) where
  insert collection o = do
    runDb <- asks envRunDb
    handleWriteFailure $
      liftIO $ runDb $ Mongo.insert collection $ toDocument o
  insert_ collection o = do
    runDb <- asks envRunDb
    liftIO $ runDb $ Mongo.insert_ collection $ toDocument o

instance IOConstraint m => ReadOnly (HandlerT m) where
  getAll collection = do
    runDb <- asks envRunDb
    docs <- liftIO $ runDb $ Mongo.find (Mongo.select [] collection)
                         >>= Mongo.rest
    pure $ mapMaybe fromDocument docs

handleWriteFailure :: IOConstraint m => HandlerT m a -> HandlerT m a
handleWriteFailure = handleJust writeFailure (handleDbErr "write failure")
  where
    writeFailure err@Mongo.WriteFailure{} = Just err
    writeFailure _                        = Nothing

handleDbErr :: Monad m => Text -> Mongo.Failure -> HandlerT m a
handleDbErr desc err = throwError $ ErrDatabase $
  "Error while accessing database: " <> desc <> ": " <> (Text.pack . show) err
