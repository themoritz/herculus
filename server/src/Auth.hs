-- | server-side auth logic, including database queries

module Auth
  ( authHandler
  , AuthMiddleware
  ) where

import           Control.Lens                     ((^.))
import           Control.Monad.Except             (throwError)
import           Data.Functor                     (($>))
import qualified Data.List                        as List
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import qualified Data.Text.Encoding               as Text
import           Database.MongoDB                 ((=:))
import           Network.Wai                      (Request, requestHeaders)
import           Servant.Server.Experimental.Auth (AuthHandler, mkAuthHandler)


import           HexlNat                          (hexlToServant)
import           Lib.Model                        (Entity (..))
import           Lib.Model.Auth                   (SessionKey, User,
                                                   prolongSession,
                                                   sessionExpDate,
                                                   sessionUserId)
import           Lib.Types                        (Id)
import           Monads                           (AppError (..), HexlEnv,
                                                   MonadDB (..))

type AuthMiddleware = AuthHandler Request (Id User)

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
