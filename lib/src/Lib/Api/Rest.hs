{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Lib.Api.Rest where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Text

import           GHC.Generics

import           Servant.API

import           Lib

type Routes =
      "project" :> ProjectRoutes
 :<|> "table" :> TableRoutes
 :<|> "column" :> ColumnRoutes
 :<|> "record" :> RecordRoutes
 :<|> "cell" :> CellRoutes

type ProjectRoutes =
      "create" :> ReqBody '[JSON] Text :> Post '[JSON] (Id Project)
 :<|> "list" :> Get '[JSON] [Project]

data TableCreate = TableCreate
  { tableCreateProjectId :: Id Project
  , tableCreateName :: Text
  } deriving (Generic)

instance ToJSON TableCreate
instance FromJSON TableCreate

type TableRoutes =
      "create" :> ReqBody '[JSON] TableCreate :> Post '[JSON] (Id Table)
 :<|> "list" :> Capture "projectId" (Id Project) :> Get '[JSON] [Table]
 :<|> "data" :> Capture "tableId" (Id Table) :> Get '[JSON] [(Id Column, Id Record, Value)]

data ColumnCreate = ColumnCreate
  { columnCreateTableId    :: Id Table
  , columnCreateName       :: Text
  , columnCreateColumnType :: ColumnType
  } deriving (Generic)

instance ToJSON ColumnCreate
instance FromJSON ColumnCreate

type ColumnRoutes =
      "create" :> ReqBody '[JSON] (Id Table) :> Post '[JSON] (Id Column)
 :<|> "setName" :> Capture "columnId" (Id Column) :> ReqBody '[JSON] Text :> Post '[JSON] ()
 :<|> "setType" :> Capture "columnId" (Id Column) :> ReqBody '[JSON] ColumnType :> Post '[JSON] ()
 :<|> "list" :> Capture "tableId" (Id Table) :> Get '[JSON] [Column]

type RecordRoutes =
      "create" :> ReqBody '[JSON] (Id Table) :> Post '[JSON] (Id Record)
 :<|> "list" :> Capture "tableId" (Id Table) :> Get '[JSON] [Record]

data CellSet = CellSet
  { cellSetTableId  :: Id Table
  , cellSetColumnId :: Id Column
  , cellSetRecordId :: Id Record
  , cellSetValue    :: Value
  } deriving (Generic)

instance ToJSON CellSet
instance FromJSON CellSet

type CellRoutes =
      "set" :> ReqBody '[JSON] CellSet :> Post '[JSON] ()
