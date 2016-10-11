{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Lib.Api.Rest where

import qualified Data.ByteString.Lazy as BL
import           Data.Text

import           Servant.API          ((:<|>), (:>), Capture, Delete, Get, JSON,
                                       PlainText, Post, ReqBody)

import           Lib.Api.Rest.Report
import           Lib.Model
import           Lib.Model.Cell
import           Lib.Model.Column
import           Lib.Model.Project
import           Lib.Model.Record

import           Lib.Model.Auth       (LoginData, LoginResponse)
import           Lib.Model.Table
import           Lib.Types

type Routes =
      AuthLogin
 -- :<|> AuthLogout

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

type AuthLogin            = "auth"      :> "login"          :> ReqBody '[JSON] LoginData        :> Post '[JSON] LoginResponse

type ProjectCreate        = "project"   :> "create"         :> ReqBody '[JSON] Project          :> Post '[JSON] (Id Project)
type ProjectList          = "project"   :> "list"           :> Get '[JSON] [Entity Project]
type ProjectSetName       = "project"   :> "setName"        :> Capture "projectId" (Id Project) :> ReqBody '[JSON] Text :> Post '[JSON] ()
type ProjectDelete        = "project"   :> "delete"         :> Capture "projectId" (Id Project) :> Delete '[JSON] ()

type TableCreate          = "table"     :> "create"         :> ReqBody '[JSON] Table            :> Post '[JSON] (Id Table)
type TableList            = "table"     :> "list"           :> Capture "projectId" (Id Project) :> Get '[JSON] [Entity Table]
type TableListGlobal      = "table"     :> "listGlobal"     :> Get '[JSON] [Entity Table]
type TableData            = "table"     :> "data"           :> Capture "tableId" (Id Table)     :> Get '[JSON] [(Id Column, Id Record, CellContent)]
type TableGetWhole        = "table"     :> "getWhole"       :> Capture "tableId" (Id Table)     :> Get '[JSON] ([Entity Column], [Entity Record], [(Id Column, Id Record, CellContent)])
type TableSetName         = "table"     :> "setName"        :> Capture "tableId" (Id Table)     :> ReqBody '[JSON] Text :> Post '[JSON] ()
type TableDelete          = "table"     :> "delete"         :> Capture "tableId" (Id Table)     :> Delete '[JSON] ()

type ColumnCreate         = "column"    :> "create"         :> ReqBody '[JSON] Column           :> Post '[JSON] (Entity Column, [Entity Cell])
type ColumnDelete         = "column"    :> "delete"         :> Capture "columnId" (Id Column)   :> Get '[JSON] ()
type ColumnList           = "column"    :> "list"           :> Capture "tableId" (Id Table)     :> Get '[JSON] [Entity Column]
type ColumnSetName        = "column"    :> "setName"        :> Capture "columnId" (Id Column)   :> ReqBody '[JSON] Text                                       :> Post '[JSON] ()
type DataColUpdate        = "dataCol"   :> "update"         :> Capture "columnId" (Id Column)   :> ReqBody '[JSON] (DataType, IsDerived, Text)                :> Post '[JSON] ()
type ReportColUpdate      = "reportCol" :> "update"         :> Capture "columnId" (Id Column)   :> ReqBody '[JSON] (Text, ReportFormat, Maybe ReportLanguage) :> Post '[JSON] ()

type RecordCreate         = "record"    :> "create"         :> ReqBody '[JSON] (Id Table)       :> Post '[JSON] (Entity Record, [Entity Cell])
type RecordDelete         = "record"    :> "delete"         :> Capture "recordId" (Id Record)   :> Get '[JSON] ()
type RecordData           = "record"    :> "data"           :> Capture "recordId" (Id Record)   :> Get '[JSON] [(Entity Column, CellContent)]
type RecordList           = "record"    :> "list"           :> Capture "tableId" (Id Table)     :> Get '[JSON] [Entity Record]
type RecordListWithData   = "record"    :> "listWithData"   :> Capture "tableId" (Id Table)     :> Get '[JSON] [(Id Record, [(Entity Column, CellContent)])]

type CellSet              = "cell"      :> "set"            :> Capture "columnId" (Id Column)   :> Capture "recordId" (Id Record) :> ReqBody '[JSON] Value  :> Post '[JSON] ()
type CellGetReportPDF     = "cell"      :> "getReportPDF"   :> Capture "columnId" (Id Column)   :> Capture "recordId" (Id Record)                           :> Get '[PDF] BL.ByteString
type CellGetReportHTML    = "cell"      :> "getReportHTML"  :> Capture "columnId" (Id Column)   :> Capture "recordId" (Id Record)                           :> Get '[HTML] Text
type CellGetReportPlain   = "cell"      :> "getReportPlain" :> Capture "columnId" (Id Column)   :> Capture "recordId" (Id Record)                           :> Get '[PlainText] Text
