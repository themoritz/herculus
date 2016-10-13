{-# LANGUAGE TypeFamilies #-}
-- | server-side auth logic, including database queries

module Auth
  ( authHandler
  , AuthMiddleware
  , mkSession
  , prolongSession
  ) where

import           Control.Lens                     ((^.))
import           Control.Monad.Except             (throwError)
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import qualified Data.ByteString.Base64           as Base64
import           Data.Functor                     (($>))
import qualified Data.List                        as List
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import qualified Data.Text.Encoding               as Text
import           Database.MongoDB                 ((=:))
import           Network.Wai                      (Request, requestHeaders)
import           Servant.Server.Experimental.Auth (AuthHandler, AuthServerData,
                                                   mkAuthHandler)
import           System.Entropy                   (getEntropy)


import           HexlNat                          (hexlToServant)
import           Lib.Api.Rest                     (SessionData, SessionProtect)
import           Lib.Model                        (Entity (..))
import           Lib.Model.Auth                   (SessionKey, User,
                                                   sessionExpDate,
                                                   sessionUserId)
import           Lib.Model.Auth                   (Session (..))
import           Lib.Types                        (Id, Time, addSeconds)
import           Monads                           (AppError (..), HexlEnv,
                                                   MonadDB (..))

type AuthMiddleware = AuthHandler Request SessionData
type instance AuthServerData SessionProtect = SessionData

--

-- these two function might as well go in to the shared lib
-- in Lib.Model.Auth
-- However, getEntropy uses the entropy package that doesn't
-- build well with ghcjs (and shouldn't have to)

mkSession :: MonadIO m => Id User -> Time -> m Session
mkSession userId created = do
  key <- liftIO $ Text.decodeUtf8 . Base64.encode <$> getEntropy 32
  -- session expiry in seconds
  pure $ Session userId key (addSeconds 600 created)

prolongSession :: Session -> Session
prolongSession session@Session{ _sessionExpDate = expiry } =
  session { _sessionExpDate = addSeconds 600 expiry }

--

-- handling auth middleware

lookUpSession :: MonadDB m => SessionKey -> m (Either Text (Id User))
lookUpSession sessionKey =
    getOneByQuery [ "sessionKey" =: sessionKey]
        >>= either (pure . Left) getUserId
  where
    getUserId (Entity sessionId session) = do
      now <- getCurrentTime
      if now > session ^. sessionExpDate
        then delete sessionId $> Left "session expired"
        else update sessionId prolongSession $> Right (session ^. sessionUserId)

authHandler :: HexlEnv -> AuthMiddleware
authHandler env = mkAuthHandler $ hexlToServant env $ \request ->
    case List.lookup "servant-auth-cookie" (requestHeaders request) of
      Nothing -> throwError $ ErrUnauthorized "Missing header 'servant-auth-cookie'"
      Just bs -> case Text.decodeUtf8' bs of
        Left  err -> throwError $ ErrForbidden $ Text.pack $ show err
        Right key -> lookUpSession key
                       >>= either (throwError . ErrForbidden) pure
