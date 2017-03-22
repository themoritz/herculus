{-# LANGUAGE TypeFamilies #-}
-- | server-side auth logic, including database queries

module Auth
  ( mkSession
  , lookUpSession
  , getUserInfo
  , prolongSession
  ) where

import           Control.Lens           ((&), (.~), (^.))
import           Control.Monad.Except   (throwError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.Functor           (($>))
import           Data.Text              (Text)
import qualified Data.Time.Clock        as Clock
import           Database.MongoDB       ((=:))
import           System.Entropy         (getEntropy)

import           Lib.Api.Schema.Auth
import           Lib.Model              (Entity (..))
import           Lib.Model.Auth         (Session (..), SessionKey, User,
                                         sessionExpDate, sessionUserId,
                                         userEmail, userName)
import           Lib.Types              (Id, Time (Time), addSeconds)
import           Lib.Util.Base64        (mkBase64Url)
import           Monads                 (AppError (..), MonadDB (..))

-- These two function might as well go in to the shared lib
-- in Lib.Model.Auth
-- However, getEntropy uses the entropy package that doesn't
-- build well with ghcjs (and shouldn't have to)

-- | Session length in seconds
sessionLength :: Clock.NominalDiffTime
sessionLength = 60 * 60 * 24 * 2 -- 2 days

-- | Initialize a session object
mkSession :: MonadIO m => Id User -> m Session
mkSession userId = liftIO $ do
  -- session expiry in seconds
  created <- addSeconds sessionLength . Time <$> Clock.getCurrentTime
  key <- mkBase64Url <$> getEntropy 32
  pure $ Session userId key created

-- | Prolong a session to current time + sessionLength
prolongSession :: MonadIO m => Session -> m Session
prolongSession session = do
  now <- Time <$> liftIO Clock.getCurrentTime
  pure $ session & sessionExpDate .~ addSeconds sessionLength now

lookUpSession :: (MonadIO m, MonadDB m)
              => SessionKey -> m (Either Text UserInfo)
lookUpSession sessionKey =
  getOneByQuery [ "sessionKey" =: sessionKey ] >>= \case
    Left _ -> pure $ Left "Session not found."
    Right (Entity sessionId session) -> do
      now <- getCurrentTime
      if now > session ^. sessionExpDate
        then pure $ Left "Session expired."
        else do
          session' <- prolongSession session
          update sessionId (const session')
          let userId = session' ^. sessionUserId
          user <- getById' userId
          pure $ Right $ UserInfo userId
                                  (user ^. userName)
                                  (user ^. userEmail)
                                  sessionKey

getUserInfo :: (MonadIO m, MonadDB m) => Maybe SessionKey -> m UserInfo
getUserInfo = \case
  Nothing -> throwError $ ErrUnauthorized "Missing session key"
  Just s -> lookUpSession s >>= \case
    Left e -> throwError $ ErrUnauthorized e
    Right ui -> pure ui
