{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Lib.Api.Rest where

import           Control.DeepSeq      (NFData)

import           Data.Aeson           (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy as LBS
import           Data.Text

import           GHC.Generics
import           Servant.API          ((:<|>), (:>), Capture, Delete, Get,
                                       Header, JSON, PlainText, Post,
                                       QueryParam, ReqBody)

import           Lib.Api.Rest.Report
import           Lib.Model
import           Lib.Model.Auth       (ChangePwdData, ChangePwdResponse,
                                       GetUserInfoResponse, LoginData,
                                       LoginResponse, SessionKey, SignupData,
                                       SignupResponse)
import           Lib.Model.Cell
import           Lib.Model.Column
import           Lib.Model.Project
import           Lib.Model.Row
import           Lib.Model.Table
import           Lib.Types

type AuthHeader = Header "Authorization" SessionKey
type AuthParam = QueryParam "authToken" SessionKey

type Routes =
      "auth"                     :> AuthRoutes
 :<|> "project"    :> AuthHeader :> ProjectRoutes
 :<|> "reportCell" :> AuthParam  :> ReportCellRoutes

type Routes' =
      "auth"                     :> AuthRoutes'
 :<|> "project"    :> AuthHeader :> ProjectRoutes

type AuthRoutes =
      AuthLogin
 :<|> AuthLogout
 :<|> AuthSignup
 :<|> AuthGetUserInfo
 :<|> AuthChangePassword
 :<|> AuthSendResetLink
 :<|> AuthResetPassword

type AuthRoutes' =
      AuthLogin
 :<|> AuthLogout
 :<|> AuthSignup
 :<|> AuthGetUserInfo
 :<|> AuthChangePassword
 :<|> AuthSendResetLink

type ProjectRoutes =
      ProjectCreate
 :<|> ProjectList
 :<|> ProjectSetName
 :<|> ProjectDelete
 :<|> ProjectLoad
 :<|> ProjectRunCommand

type ReportCellRoutes =
      ReportCellGetPdf
 :<|> ReportCellGetHtml
 :<|> ReportCellGetPlain

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

type AuthLogin          = "login"          :> ReqBody '[JSON] LoginData                     :> Post '[JSON] LoginResponse
type AuthLogout         = "logout"         :> AuthHeader                                    :> Get '[JSON] ()
type AuthSignup         = "signup"         :> ReqBody '[JSON] SignupData                    :> Post '[JSON] SignupResponse
type AuthGetUserInfo    = "userInfo"       :> AuthHeader                                    :> Get '[JSON] GetUserInfoResponse
type AuthChangePassword = "changePassword" :> AuthHeader :> ReqBody '[JSON] ChangePwdData   :> Post '[JSON] ChangePwdResponse
type AuthSendResetLink  = "sendResetLink"  :> ReqBody '[JSON] Text                          :> Post '[JSON] ()
type AuthResetPassword  = "resetPassword"  :> Capture "sessionKey" SessionKey               :> Get '[PlainText] Text

type ProjectCreate      = "create"         :> ReqBody '[JSON] Text                                              :> Post '[JSON] (Entity ProjectClient)
type ProjectList        = "list"                                                                                :> Get '[JSON] [Entity ProjectClient]
type ProjectSetName     = "setName"        :> Capture "projectId" (Id ProjectClient) :> ReqBody '[JSON] Text    :> Post '[JSON] ()
type ProjectDelete      = "delete"         :> Capture "projectId" (Id ProjectClient)                            :> Delete '[JSON] ()
type ProjectLoad        = "load"           :> Capture "projectId" (Id ProjectClient)                            :> Get '[JSON] (ProjectClient, [Entity Table], [Entity Column], [Entity Row], [Entity Cell])
type ProjectRunCommand  = "runCommand"     :> Capture "projectId" (Id ProjectClient) :> ReqBody '[JSON] Command :> Post '[JSON] ()

type ReportCellGetPdf   = "pdf"   :> Capture "columnId" (Id Column) :> Capture "rowId" (Id Row) :> Get '[PDF] LBS.ByteString
type ReportCellGetHtml  = "html"  :> Capture "columnId" (Id Column) :> Capture "rowId" (Id Row) :> Get '[HTML] Text
type ReportCellGetPlain = "plain" :> Capture "columnId" (Id Column) :> Capture "rowId" (Id Row) :> Get '[PlainText] Text
