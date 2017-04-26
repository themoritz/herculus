{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Lib.Api.Rest where

import qualified Data.ByteString.Lazy   as LBS
import           Data.Text

import           Servant.API            ((:<|>), (:>), Capture, Delete, Get,
                                         Header, JSON, PlainText, Post,
                                         QueryParam, ReqBody)

import           Lib.Api.Rest.Report
import           Lib.Api.Schema.Auth
import           Lib.Api.Schema.Project
import           Lib.Compiler.Error
import qualified Lib.Model.Auth         as M (SessionKey)
import qualified Lib.Model.Column       as M
import qualified Lib.Model.Project      as M
import qualified Lib.Model.Row          as M
import qualified Lib.Model.Table        as M
import           Lib.Types

type AuthHeader = Header "Authorization" M.SessionKey
type AuthParam = QueryParam "authToken" M.SessionKey

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
 :<|> ProjectColSetWidth
 :<|> ProjectReorderCols
 :<|> ProjectLoad
 :<|> ProjectRunCommands
 :<|> ProjectLintDataCol
 :<|> ProjectLintReportCol

type ReportCellRoutes =
      ReportCellGetPdf
 :<|> ReportCellGetHtml
 :<|> ReportCellGetPlain

type AuthLogin           = "login"          :> ReqBody '[JSON] LoginData                   :> Post '[JSON] LoginResponse
type AuthLogout          = "logout"         :> AuthHeader                                  :> Get '[JSON] ()
type AuthSignup          = "signup"         :> ReqBody '[JSON] SignupData                  :> Post '[JSON] SignupResponse
type AuthGetUserInfo     = "userInfo"       :> AuthHeader                                  :> Get '[JSON] GetUserInfoResponse
type AuthChangePassword  = "changePassword" :> AuthHeader :> ReqBody '[JSON] ChangePwdData :> Post '[JSON] ChangePwdResponse
type AuthSendResetLink   = "sendResetLink"  :> ReqBody '[JSON] Text                        :> Post '[JSON] ()
type AuthResetPassword   = "resetPassword"  :> Capture "sessionKey" M.SessionKey           :> Get '[PlainText] Text

type ProjectCreate       = "create"      :> ReqBody '[JSON] Text                                              :> Post '[JSON] Project
type ProjectList         = "list"                                                                             :> Get '[JSON] [Project]
type ProjectSetName      = "setName"     :> Capture "projectId" (Id M.Project) :> ReqBody '[JSON] Text        :> Post '[JSON] ()
type ProjectDelete       = "delete"      :> Capture "projectId" (Id M.Project)                                :> Delete '[JSON] ()
type ProjectColSetWidth  = "colSetWidth" :> Capture "columnId" (Id M.Column)   :> ReqBody '[JSON] Int         :> Post '[JSON] ()
type ProjectReorderCols  = "reorderCols" :> Capture "tableId" (Id M.Table)   :> ReqBody '[JSON] [Id M.Column] :> Post '[JSON] ()
type ProjectLoad         = "load"        :> Capture "projectId" (Id M.Project)                                :> Get '[JSON] ProjectData
type ProjectRunCommands  = "runCommands" :> Capture "projectId" (Id M.Project) :> ReqBody '[JSON] [Command]   :> Post '[JSON] ()
type ProjectLintDataCol  = "lintDataCol" :> Capture "columnId" (Id M.Column) :> ReqBody '[JSON] (M.DataType, Text) :> Post '[JSON] [Error]
type ProjectLintReportCol = "lintReportCol" :> Capture "columnId" (Id M.Column) :> ReqBody '[JSON] Text       :> Post '[JSON] [Error]

type ReportCellGetPdf    = "pdf"   :> Capture "columnId" (Id M.Column) :> Capture "rowId" (Id M.Row) :> Get '[PDF] LBS.ByteString
type ReportCellGetHtml   = "html"  :> Capture "columnId" (Id M.Column) :> Capture "rowId" (Id M.Row) :> Get '[HTML] Text
type ReportCellGetPlain  = "plain" :> Capture "columnId" (Id M.Column) :> Capture "rowId" (Id M.Row) :> Get '[PlainText] Text
