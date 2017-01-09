{-# LANGUAGE FlexibleContexts #-}
-- |

module Store.Types where

import           Control.DeepSeq                (NFData)

import           Data.Text                      (Text)
import qualified Data.Text.Encoding             as Text

import           React.Flux.Addons.Servant      (ApiRequestConfig (..),
                                                 HandleResponse,
                                                 RequestTimeout (NoTimeout))
import           React.Flux.Addons.Servant.Auth (AuthClientData,
                                                 AuthenticateReq,
                                                 mkAuthenticateReq)

import           Lib.Api.Rest                   as Api
import           Lib.Model.Auth                 (SessionKey)
import           Lib.Util.Base64                (unBase64Url)

import qualified Config
import qualified Store.Message                  as Message

type instance AuthClientData Api.SessionProtect = SessionKey

mkAuthHeader :: AuthClientData Api.SessionProtect -> (Text, Text)
mkAuthHeader sessionKey =
  (Api.sessionParamStr, Text.decodeUtf8 $ unBase64Url sessionKey)

session :: SessionKey -> AuthenticateReq Api.SessionProtect
session key = mkAuthenticateReq key mkAuthHeader

class Monad m => MonadStore m where
  apiCall :: NFData a => (HandleResponse a -> IO ()) -> m a
  showMessage :: Message.Action -> m ()
  haltMessage :: Text -> m a


api :: ApiRequestConfig Routes
api = ApiRequestConfig Config.apiUrl NoTimeout
