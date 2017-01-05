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
import           React.Flux.Addons.Servant      (ApiRequestConfig (..),
                                                 RequestTimeout (NoTimeout))
import           React.Flux.Addons.Servant.Auth (AuthClientData,
                                                 AuthenticateReq,
                                                 mkAuthenticateReq)

import qualified Config
import           Lib.Api.Rest                   as Api
import           Lib.Api.WebSocket              (WsDownMessage, WsUpMessage)
import           Lib.Model.Auth                 (LoginData, SessionKey,
                                                 SignupData)
import           Lib.Model.Cell                 (Value)
import           Lib.Model.Column
import           Lib.Model.Project              (ProjectClient)
import           Lib.Model.Row                  (Row)
import           Lib.Model.Table                (Table)
import           Lib.Types                      (Id)
import           Lib.Util.Base64                (unBase64Url)

import qualified Action.Message                 as Message

type instance AuthClientData Api.SessionProtect = SessionKey

mkAuthHeader :: AuthClientData Api.SessionProtect -> (Text, Text)
mkAuthHeader sessionKey =
  (Api.sessionParamStr, Text.decodeUtf8 $ unBase64Url sessionKey)

session :: SessionKey -> AuthenticateReq Api.SessionProtect
session key = mkAuthenticateReq key mkAuthHeader

api :: ApiRequestConfig Routes
api = ApiRequestConfig Config.apiUrl NoTimeout

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
  | SetProjectOverview SessionKey
  -- Project overview
  | ProjectsCreate Text -- project name
  | ProjectsLoadProject (Id ProjectClient)
  | ProjectSetName Text
  | ProjectDelete (Id ProjectClient)
  -- Tables
  | TablesCreate (Id ProjectClient) Text
  | TablesLoadTable (Id Table)
  -- Table
  | TableCreateDataCol
  | TableCreateReportCol
  | TableRenameColumn (Id Column) Text
  | TableUpdateDataCol (Id Column) DataType IsDerived Text
  | TableUpdateReportCol (Id Column) Text ReportFormat (Maybe ReportLanguage)
  | TableDeleteColumn (Id Column)
  | TableAddRow
  | TableDeleteRow (Id Row)
  | TableSetName (Id Table) Text
  | TableDelete (Id Table)
  -- Cell
  | CellSetValue (Id Column) (Id Row) Value
  deriving (Typeable, Generic, NFData)
