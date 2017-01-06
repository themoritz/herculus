{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
-- |

module Action where

import           Control.DeepSeq                (NFData)
import           Data.Text                      (Text)
import qualified Data.Text.Encoding             as Text
import           Data.Typeable                  (Typeable)
import           GHC.Generics                   (Generic)
import           React.Flux.Addons.Servant.Auth (AuthClientData,
                                                 AuthenticateReq,
                                                 mkAuthenticateReq)

import           Lib.Api.Rest                   as Api
import           Lib.Api.WebSocket              (WsDownMessage, WsUpMessage)
import           Lib.Model.Auth                 (ChangePwdData, LoginData,
                                                 SessionKey, SignupData)
import           Lib.Model.Project              (ProjectClient)
import           Lib.Types                      (Id)
import           Lib.Util.Base64                (unBase64Url)
import qualified Project

import qualified Action.Message                 as Message

type instance AuthClientData Api.SessionProtect = SessionKey

mkAuthHeader :: AuthClientData Api.SessionProtect -> (Text, Text)
mkAuthHeader sessionKey =
  (Api.sessionParamStr, Text.decodeUtf8 $ unBase64Url sessionKey)

session :: SessionKey -> AuthenticateReq Api.SessionProtect
session key = mkAuthenticateReq key mkAuthHeader

data Action
  -- Global
  = MessageAction Message.Action
  | GlobalInit Text -- WebSocket URL
  | ApplyWebSocketMsg WsDownMessage
  | GlobalSendWebSocket WsUpMessage
  -- Session
  | ToSignupForm
  | ToLoginForm
  | Signup SignupData
  | Login LoginData
  | Logout
  | ToggleUserSettingsDialog
  | ToChangePasswordForm
  | ChangePassword ChangePwdData

  | SetProjectOverview SessionKey
  -- Project overview
  | ProjectsCreate Text -- project name
  | ProjectsLoadProject (Id ProjectClient)
  | ProjectDelete (Id ProjectClient)
  | ProjectAction Project.Action
  deriving (Typeable, Generic, NFData, Show)
