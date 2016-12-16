{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Lib.Api.Rest where

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as LBS
import           Data.CaseInsensitive          (original)
import           Data.Text
import qualified Data.Text.Encoding            as Text
import           Network.HTTP.Types.Header     (HeaderName)
import           Servant.API                   ((:<|>), (:>), Capture, Delete,
                                                Get, JSON, PlainText, Post,
                                                QueryParam, ReqBody)
import           Servant.API.Experimental.Auth (AuthProtect)

import           Lib.Api.Rest.Report
import           Lib.Model
import           Lib.Model.Auth                (GetUserInfoResponse, LoginData,
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

 :<|> ProjectCreate
 :<|> ProjectList
 :<|> ProjectSetName
 :<|> ProjectDelete
 :<|> ProjectLoad

 :<|> TableCreate
 :<|> TableData
 :<|> TableGetWhole
 :<|> TableSetName
 :<|> TableDelete

 :<|> ColumnCreate
 :<|> ColumnDelete
 :<|> ColumnList
 :<|> ColumnSetName
 :<|> DataColUpdate
 :<|> ReportColUpdate

 :<|> RowCreate
 :<|> RowDelete
 :<|> RowData
 :<|> RowList
 :<|> RowListWithData

 :<|> CellSet
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

type AuthLogin          =                   "auth"      :> "login"          :> ReqBody '[JSON] LoginData  :> Post '[JSON] LoginResponse
type AuthLogout         = SessionProtect :> "auth"      :> "logout"                                       :> Get '[JSON] ()
type AuthSignup         =                   "auth"      :> "signup"         :> ReqBody '[JSON] SignupData :> Post '[JSON] SignupResponse
type AuthGetUserInfo    =                   "auth"      :> "userInfo"       :> ReqBody '[JSON] SessionKey :> Post '[JSON] GetUserInfoResponse

type ProjectCreate      = SessionProtect :> "project"   :> "create"         :> ReqBody '[JSON] Text                                     :> Post '[JSON] (Entity ProjectClient)
type ProjectList        = SessionProtect :> "project"   :> "list"                                                                       :> Get '[JSON] [Entity ProjectClient]
type ProjectSetName     = SessionProtect :> "project"   :> "setName"        :> Capture "projectId" (Id Project) :> ReqBody '[JSON] Text :> Post '[JSON] ()
type ProjectDelete      = SessionProtect :> "project"   :> "delete"         :> Capture "projectId" (Id Project)                         :> Delete '[JSON] ()
type ProjectLoad        = SessionProtect :> "table"     :> "list"           :> Capture "projectId" (Id Project)                         :> Get '[JSON] (ProjectClient, [Entity Table])

type TableCreate        = SessionProtect :> "table"     :> "create"         :> ReqBody '[JSON] Table                                :> Post '[JSON] (Id Table)
type TableData          = SessionProtect :> "table"     :> "data"           :> Capture "tableId" (Id Table)                         :> Get '[JSON] [(Id Column, Id Row, CellContent)]
type TableGetWhole      = SessionProtect :> "table"     :> "getWhole"       :> Capture "tableId" (Id Table)                         :> Get '[JSON] ([Entity Column], [Entity Row], [(Id Column, Id Row, CellContent)])
type TableSetName       = SessionProtect :> "table"     :> "setName"        :> Capture "tableId" (Id Table) :> ReqBody '[JSON] Text :> Post '[JSON] ()
type TableDelete        = SessionProtect :> "table"     :> "delete"         :> Capture "tableId" (Id Table)                         :> Delete '[JSON] ()

type ColumnCreate       = SessionProtect :> "column"    :> "create"         :> ReqBody '[JSON] Column                                                                       :> Post '[JSON] (Entity Column, [Entity Cell])
type ColumnDelete       = SessionProtect :> "column"    :> "delete"         :> Capture "columnId" (Id Column)                                                               :> Get '[JSON] ()
type ColumnList         = SessionProtect :> "column"    :> "list"           :> Capture "tableId" (Id Table)                                                                 :> Get '[JSON] [Entity Column]
type ColumnSetName      = SessionProtect :> "column"    :> "setName"        :> Capture "columnId" (Id Column) :> ReqBody '[JSON] Text                                       :> Post '[JSON] ()
type DataColUpdate      = SessionProtect :> "dataCol"   :> "update"         :> Capture "columnId" (Id Column) :> ReqBody '[JSON] (DataType, IsDerived, Text)                :> Post '[JSON] ()
type ReportColUpdate    = SessionProtect :> "reportCol" :> "update"         :> Capture "columnId" (Id Column) :> ReqBody '[JSON] (Text, ReportFormat, Maybe ReportLanguage) :> Post '[JSON] ()

type RowCreate          = SessionProtect :> "row"       :> "create"         :> ReqBody '[JSON] (Id Table)   :> Post '[JSON] (Entity Row, [Entity Cell])
type RowDelete          = SessionProtect :> "row"       :> "delete"         :> Capture "rowId" (Id Row)     :> Get '[JSON] ()
type RowData            = SessionProtect :> "row"       :> "data"           :> Capture "rowId" (Id Row)     :> Get '[JSON] [(Entity Column, CellContent)]
type RowList            = SessionProtect :> "row"       :> "list"           :> Capture "tableId" (Id Table) :> Get '[JSON] [Entity Row]
type RowListWithData    = SessionProtect :> "row"       :> "listWithData"   :> Capture "tableId" (Id Table) :> Get '[JSON] [(Id Row, [(Entity Column, CellContent)])]

type CellSet            = SessionProtect :> "cell"      :> "set"            :> Capture "columnId" (Id Column) :> Capture "rowId" (Id Row) :> ReqBody '[JSON] Value              :> Post '[JSON] ()
type CellGetReportPDF   = SessionProtect :> "cell"      :> "getReportPDF"   :> QueryParam "sessionKey" SessionKey :> Capture "columnId" (Id Column) :> Capture "rowId" (Id Row) :> Get '[PDF] LBS.ByteString
type CellGetReportHTML  = SessionProtect :> "cell"      :> "getReportHTML"  :> QueryParam "sessionKey" SessionKey :> Capture "columnId" (Id Column) :> Capture "rowId" (Id Row) :> Get '[HTML] Text
type CellGetReportPlain = SessionProtect :> "cell"      :> "getReportPlain" :> QueryParam "sessionKey" SessionKey :> Capture "columnId" (Id Column) :> Capture "rowId" (Id Row) :> Get '[PlainText] Text
