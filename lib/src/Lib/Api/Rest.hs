{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Lib.Api.Rest where

import qualified Data.ByteString.Lazy          as BL
import           Data.CaseInsensitive          (original)
import           Data.Text
import qualified Data.Text.Encoding            as Text
import           Network.HTTP.Types.Header     (HeaderName)
import           Servant.API                   ((:<|>), (:>), Capture, Delete,
                                                Get, JSON, PlainText, Post,
                                                ReqBody)
import           Servant.API.Experimental.Auth (AuthProtect)

import           Lib.Api.Rest.Report
import           Lib.Model
import           Lib.Model.Auth                (LoginData, LoginResponse,
                                                SignupData, SignupResponse,
                                                User)
import           Lib.Model.Cell
import           Lib.Model.Column
import           Lib.Model.Project
import           Lib.Model.Record
import           Lib.Model.Table
import           Lib.Types

type Routes =
      AuthLogin
 :<|> AuthLogout
 :<|> AuthSignup

 :<|> ProjectCreate
 :<|> ProjectList
 :<|> ProjectSetName
 :<|> ProjectDelete

 :<|> TableCreate
 :<|> TableList
 :<|> TableListGlobal
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

 :<|> RecordCreate
 :<|> RecordDelete
 :<|> RecordData
 :<|> RecordList
 :<|> RecordListWithData

 :<|> CellSet
 :<|> CellGetReportPDF
 :<|> CellGetReportHTML
 :<|> CellGetReportPlain

type SessionProtect = AuthProtect "cookie-auth"
type SessionData = Id User

sessionHeader :: HeaderName
sessionHeader = "servant-auth-cookie"

sessionHeaderStr :: Text
sessionHeaderStr = Text.decodeUtf8 $ original sessionHeader

type AuthLogin            = "auth"          :> "login"      :> ReqBody '[JSON] LoginData        :> Post '[JSON] LoginResponse
type AuthLogout           = SessionProtect  :> "auth"       :> "logout"                         :> Get '[JSON] ()
type AuthSignup           = "auth"          :> "signup"     :> ReqBody '[JSON] SignupData       :> Post '[JSON] SignupResponse

type ProjectCreate        = SessionProtect  :> "project"    :> "create"   :> ReqBody '[JSON] Project          :> Post '[JSON] (Id Project)
type ProjectList          = SessionProtect  :> "project"    :> "list"     :> Get '[JSON] [Entity Project]
type ProjectSetName       = SessionProtect  :> "project"    :> "setName"  :> Capture "projectId" (Id Project) :> ReqBody '[JSON] Text :> Post '[JSON] ()
type ProjectDelete        = SessionProtect  :> "project"    :> "delete"   :> Capture "projectId" (Id Project) :> Delete '[JSON] ()

type TableCreate          = SessionProtect  :> "table"      :> "create"     :> ReqBody '[JSON] Table            :> Post '[JSON] (Id Table)
type TableList            = SessionProtect  :> "table"      :> "list"       :> Capture "projectId" (Id Project) :> Get '[JSON] [Entity Table]
type TableListGlobal      = SessionProtect  :> "table"      :> "listGlobal" :> Get '[JSON] [Entity Table]
type TableData            = SessionProtect  :> "table"      :> "data"       :> Capture "tableId" (Id Table)     :> Get '[JSON] [(Id Column, Id Record, CellContent)]
type TableGetWhole        = SessionProtect  :> "table"      :> "getWhole"   :> Capture "tableId" (Id Table)     :> Get '[JSON] ([Entity Column], [Entity Record], [(Id Column, Id Record, CellContent)])
type TableSetName         = SessionProtect  :> "setName"    :> Capture "tableId" (Id Table)     :> ReqBody '[JSON] Text :> Post '[JSON] ()
type TableDelete          = SessionProtect  :> "table"      :> "delete"     :> Capture "tableId" (Id Table)     :> Delete '[JSON] ()

type ColumnCreate         = SessionProtect  :> "column"     :> "create"     :> ReqBody '[JSON] Column           :> Post '[JSON] (Entity Column, [Entity Cell])
type ColumnDelete         = SessionProtect  :> "column"     :> "delete"     :> Capture "columnId" (Id Column)   :> Get '[JSON] ()
type ColumnList           = SessionProtect  :> "column"     :> "list"       :> Capture "tableId" (Id Table)     :> Get '[JSON] [Entity Column]
type ColumnSetName        = SessionProtect  :> "column"     :> "setName"    :> Capture "columnId" (Id Column)   :> ReqBody '[JSON] Text                                       :> Post '[JSON] ()
type DataColUpdate        = SessionProtect  :> "dataCol"    :> "update"     :> Capture "columnId" (Id Column)   :> ReqBody '[JSON] (DataType, IsDerived, Text)                :> Post '[JSON] ()
type ReportColUpdate      = SessionProtect  :> "reportCol"  :> "update"     :> Capture "columnId" (Id Column)   :> ReqBody '[JSON] (Text, ReportFormat, Maybe ReportLanguage) :> Post '[JSON] ()

type RecordCreate         = SessionProtect  :> "record"     :> "create"     :> ReqBody '[JSON] (Id Table)       :> Post '[JSON] (Entity Record, [Entity Cell])
type RecordDelete         = SessionProtect  :> "record"     :> "delete"     :> Capture "recordId" (Id Record)   :> Get '[JSON] ()
type RecordData           = SessionProtect  :> "record"     :> "data"       :> Capture "recordId" (Id Record)   :> Get '[JSON] [(Entity Column, CellContent)]
type RecordList           = SessionProtect  :> "record"     :> "list"       :> Capture "tableId" (Id Table)     :> Get '[JSON] [Entity Record]
type RecordListWithData   = SessionProtect  :> "record"     :> "listWithData" :> Capture "tableId" (Id Table)     :> Get '[JSON] [(Id Record, [(Entity Column, CellContent)])]

type CellSet              = SessionProtect  :> "cell"       :> "set"            :> Capture "columnId" (Id Column)   :> Capture "recordId" (Id Record) :> ReqBody '[JSON] Value  :> Post '[JSON] ()
type CellGetReportPDF     = SessionProtect  :> "cell"       :> "getReportPDF"   :> Capture "columnId" (Id Column)   :> Capture "recordId" (Id Record)                           :> Get '[PDF] BL.ByteString
type CellGetReportHTML    = SessionProtect  :> "cell"       :> "getReportHTML"  :> Capture "columnId" (Id Column)   :> Capture "recordId" (Id Record)                           :> Get '[HTML] Text
type CellGetReportPlain   = SessionProtect  :> "cell"       :> "getReportPlain" :> Capture "columnId" (Id Column)   :> Capture "recordId" (Id Record)                           :> Get '[PlainText] Text
