{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Lib.Api.Rest where

import           Data.Text

import           Servant.API

import           Lib.Model
import           Lib.Model.Types
import           Lib.Model.Column
import           Lib.Model.Cell
import           Lib.Types

type Routes =
      ProjectCreate
 :<|> ProjectList

 :<|> TableCreate
 :<|> TableList
 :<|> TableListGlobal
 :<|> TableData
 :<|> TableGetWhole

 :<|> ColumnCreate
 :<|> ColumnDelete
 :<|> ColumnSetName
 :<|> ColumnSetDataType
 :<|> ColumnSetInput
 :<|> ColumnList

 :<|> RecordCreate
 :<|> RecordDelete
 :<|> RecordData
 :<|> RecordList
 :<|> RecordListWithData

 :<|> CellSet

type ProjectCreate = "project" :> "create" :> ReqBody '[JSON] Project :> Post '[JSON] (Id Project)
type ProjectList   = "project" :> "list"   :> Get '[JSON] [Entity Project]

type TableCreate     = "table" :> "create" :> ReqBody '[JSON] Table :> Post '[JSON] (Id Table)
type TableList       = "table" :> "list" :> Capture "projectId" (Id Project) :> Get '[JSON] [Entity Table]
type TableListGlobal = "table" :> "listGlobal" :> Get '[JSON] [Entity Table]
type TableData       = "table" :> "data" :> Capture "tableId" (Id Table) :> Get '[JSON] [(Id Column, Id Record, CellContent)]
type TableGetWhole   = "table" :> "getWhole" :> Capture "tableId" (Id Table) :> Get '[JSON] ([Entity Column], [Entity Record], [(Id Column, Id Record, CellContent)])

type ColumnCreate      = "column" :> "create"  :> ReqBody '[JSON] (Id Table) :> Post '[JSON] (Id Column, [Entity Cell])
type ColumnDelete      = "column" :> "delete"  :> Capture "columnId" (Id Column) :> Get '[JSON] ()
type ColumnSetName     = "column" :> "setName" :> Capture "columnId" (Id Column) :> ReqBody '[JSON] Text :> Post '[JSON] ()
type ColumnSetDataType = "column" :> "setDataType" :> Capture "columnId" (Id Column) :> ReqBody '[JSON] DataType :> Post '[JSON] ()
type ColumnSetInput    = "column" :> "setInput" :> Capture "columnId" (Id Column) :> ReqBody '[JSON] (InputType, Text) :> Post '[JSON] ()
type ColumnList        = "column" :> "list" :> Capture "tableId" (Id Table) :> Get '[JSON] [Entity Column]

type RecordCreate       = "record" :> "create" :> ReqBody '[JSON] (Id Table) :> Post '[JSON] (Id Record, [Entity Cell])
type RecordDelete       = "record" :> "delete" :> Capture "recordId" (Id Record) :> Get '[JSON] ()
type RecordData         = "record" :> "data" :> Capture "recordId" (Id Record) :> Get '[JSON] [(Entity Column, CellContent)]
type RecordList         = "record" :> "list" :> Capture "tableId" (Id Table) :> Get '[JSON] [Entity Record]
type RecordListWithData = "record" :> "listWithData" :> Capture "tableId" (Id Table) :> Get '[JSON] [(Id Record, [(Text, CellContent)])]

type CellSet = "cell" :> "set" :> Capture "columnId" (Id Column) :> Capture "recordId" (Id Record) :> ReqBody '[JSON] Value :> Post '[JSON] ()
