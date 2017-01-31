{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Lib.Api.Rest where

import           Control.DeepSeq               (NFData)

import           Data.Aeson                    (FromJSON, ToJSON)
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as LBS
import           Data.CaseInsensitive          (original)
import           Data.Text
import qualified Data.Text.Encoding            as Text

import           GHC.Generics
import           Network.HTTP.Types.Header     (HeaderName)
import           Servant.API                   ((:<|>), (:>), Capture, Delete,
                                                Get, JSON, PlainText, Post,
                                                QueryParam, ReqBody)
import           Servant.API.Experimental.Auth (AuthProtect)

import           Lib.Api.Rest.Report
import           Lib.Model
import           Lib.Model.Auth                (ChangePwdData,
                                                ChangePwdResponse,
                                                GetUserInfoResponse, LoginData,
                                                LoginResponse, SessionKey,
                                                SignupData, SignupResponse,
                                                UserInfo)
import           Lib.Model.Cell
import           Lib.Model.Column
import           Lib.Model.Project
import           Lib.Model.Row
import           Lib.Model.Table
import           Lib.Types

type Routes =
      AuthLogin
 :<|> AuthLogout
 :<|> AuthSignup
 :<|> AuthGetUserInfo -- given session key, retrieve user info
 :<|> AuthChangePassword
 :<|> AuthSendResetLink
 :<|> AuthResetPassword

 :<|> ProjectCreate
 :<|> ProjectList
 :<|> ProjectSetName
 :<|> ProjectDelete
 :<|> ProjectLoad
 :<|> ProjectRunCommand

 :<|> CellGetReportPDF
 :<|> CellGetReportHTML
 :<|> CellGetReportPlain

type SessionProtect = AuthProtect "cookie-auth"
type SessionData = UserInfo

sessionParam :: HeaderName
sessionParam = "servant-auth-cookie"

sessionParamStr :: Text
sessionParamStr = Text.decodeUtf8 sessionParamBStr

sessionParamBStr :: BS.ByteString
sessionParamBStr = original sessionParam

-- | All the critical commands that should be atomic, undoable, replayable etc.
-- within a project.
data Command
  = CmdTableCreate Text
  | CmdTableSetName (Id Table) Text
  | CmdTableDelete (Id Table)
  | CmdDataColCreate (Id Table)
  | CmdDataColUpdate (Id Column) DataType IsDerived Text
  | CmdReportColCreate (Id Table)
  | CmdReportColUpdate (Id Column) Text ReportFormat (Maybe ReportLanguage)
  | CmdColumnSetName (Id Column) Text
  | CmdColumnDelete (Id Column)
  | CmdRowCreate (Id Table)
  | CmdRowDelete (Id Row)
  | CmdCellSet (Id Column) (Id Row) Value
  deriving (Generic, Show)

instance NFData Command

instance ToJSON Command
instance FromJSON Command

type AuthLogin          =                   "auth"      :> "login"          :> ReqBody '[JSON] LoginData  :> Post '[JSON] LoginResponse
type AuthLogout         = SessionProtect :> "auth"      :> "logout"                                       :> Get '[JSON] ()
type AuthSignup         =                   "auth"      :> "signup"         :> ReqBody '[JSON] SignupData :> Post '[JSON] SignupResponse
type AuthGetUserInfo    =                   "auth"      :> "userInfo"       :> ReqBody '[JSON] SessionKey :> Post '[JSON] GetUserInfoResponse
type AuthChangePassword = SessionProtect :> "auth"      :> "changePassword" :> ReqBody '[JSON] ChangePwdData :> Post '[JSON] ChangePwdResponse
type AuthSendResetLink  =                   "auth"      :> "sendResetLink"  :> ReqBody '[JSON] Text       :> Post '[JSON] ()
type AuthResetPassword  =                   "auth"      :> "resetPassword"  :> Capture "sessionKey" SessionKey :> Get '[PlainText] Text

type ProjectCreate      = SessionProtect :> "project"   :> "create"         :> ReqBody '[JSON] Text                                              :> Post '[JSON] (Entity ProjectClient)
type ProjectList        = SessionProtect :> "project"   :> "list"                                                                                :> Get '[JSON] [Entity ProjectClient]
type ProjectSetName     = SessionProtect :> "project"   :> "setName"        :> Capture "projectId" (Id ProjectClient) :> ReqBody '[JSON] Text    :> Post '[JSON] ()
type ProjectDelete      = SessionProtect :> "project"   :> "delete"         :> Capture "projectId" (Id ProjectClient)                            :> Delete '[JSON] ()
type ProjectLoad        = SessionProtect :> "project"   :> "load"           :> Capture "projectId" (Id ProjectClient)                            :> Get '[JSON] (ProjectClient, [Entity Table], [Entity Column], [Entity Row], [Entity Cell])
type ProjectRunCommand  = SessionProtect :> "project"   :> "runCommand"     :> Capture "projectId" (Id ProjectClient) :> ReqBody '[JSON] Command :> Post '[JSON] ()

type CellGetReportPDF   = SessionProtect :> "cell"      :> "getReportPDF"   :> QueryParam "sessionKey" SessionKey :> Capture "columnId" (Id Column) :> Capture "rowId" (Id Row) :> Get '[PDF] LBS.ByteString
type CellGetReportHTML  = SessionProtect :> "cell"      :> "getReportHTML"  :> QueryParam "sessionKey" SessionKey :> Capture "columnId" (Id Column) :> Capture "rowId" (Id Row) :> Get '[HTML] Text
type CellGetReportPlain = SessionProtect :> "cell"      :> "getReportPlain" :> QueryParam "sessionKey" SessionKey :> Capture "columnId" (Id Column) :> Capture "rowId" (Id Row) :> Get '[PlainText] Text
