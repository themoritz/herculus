{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Handler
  ( handlers
  ) where

import           Control.Monad.Except   (MonadError, throwError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Text              (Text)
import           Data.Time.Clock        (UTCTime, getCurrentTime)
import           Servant                ((:<|>) (..), ServerT)

import qualified Api
import qualified Database.MongoDb       as Db
import           Handler.Types          (HandlerT)

import           Types

handlers :: ServerT Api.Routes (HandlerT IO)
handlers = subscriberAdd
      :<|> subscriberAll

subscriberAdd :: (Db.Insert m, MonadIO m, MonadError AppError m)
              => Text -> m SubscriberAddResult
subscriberAdd str = do
    email <- maybe errInvalid pure $ mkEmail str
    now <- liftIO getCurrentTime
    Db.insert_ "subscribers" $ Subscriber now email Subscribed
    pure SAROK
  where
    errInvalid = throwError $ ErrUser "The provided email is invalid"

subscriberAll :: (Functor m, Db.ReadOnly m) => m [UTCTime]
subscriberAll = map subCreated <$> Db.getAll "subscribers"
