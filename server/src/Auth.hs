{-# LANGUAGE TypeFamilies #-}
-- | server-side auth logic, including database queries

module Auth
  ( authHandler
  , AuthMiddleware
  , mkSession
  , prolongSession
  ) where

import           Control.Lens                     ((%~), (&), (^.))
import           Control.Monad.Except             (throwError)
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Data.Functor                     (($>))
import qualified Data.List                        as List
import           Data.Text                        (Text)
import qualified Data.Time.Clock                  as Clock (getCurrentTime)
import           Database.MongoDB                 ((=:))
import           Network.Wai                      (Request, requestHeaders)
import           Servant.Server.Experimental.Auth (AuthHandler, AuthServerData,
                                                   mkAuthHandler)
import           System.Entropy                   (getEntropy)

import           HexlNat                          (hexlToServant)
import           Lib.Api.Rest                     (SessionData, SessionProtect)
import           Lib.Base64                       (mkBase64, mkBase64')
import           Lib.Model                        (Entity (..))
import           Lib.Model.Auth                   (Session (..), SessionKey,
                                                   User, sessionExpDate,
                                                   sessionUserId)
import           Lib.Types                        (Id, Time (Time), addSeconds)
import           Monads                           (AppError (..), HexlEnv,
                                                   MonadDB (..))

type AuthMiddleware = AuthHandler Request SessionData
type instance AuthServerData SessionProtect = SessionData

--

-- these two function might as well go in to the shared lib
-- in Lib.Model.Auth
-- However, getEntropy uses the entropy package that doesn't
-- build well with ghcjs (and shouldn't have to)

mkSession :: MonadIO m => Id User -> m Session
mkSession userId = liftIO $ do
  -- session expiry in seconds
  created <- addSeconds 600 . Time <$> Clock.getCurrentTime
  key <- mkBase64 <$> getEntropy 32
  pure $ Session userId key created

prolongSession :: Session -> Session
prolongSession session = session & sessionExpDate %~ addSeconds 600

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
      Just bs -> case mkBase64' bs of
        Left  err -> throwError $ ErrForbidden err
        Right key -> lookUpSession key
                       >>= either (throwError . ErrForbidden) pure
