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
 :<|> "list" :> Get '[JSON] [(Id Project, Text)]

type TableRoutes =
      "create" :> ReqBody '[JSON] Text :> Post '[JSON] (Id Table)
 :<|> "list" :> Capture "projectId" (Id Project) :> Get '[JSON] [(Id Table, Text)]
 :<|> "data" :> Capture "tableId" (Id Table) :> Get '[JSON] [(Id Record, [(Id Column, Text)])]

data ColumnCreate = ColumnCreate
  { columnCreateTableId    :: Id Table
  , columnCreateName       :: Text
  , columnCreateColumnType :: ColumnType
  } deriving (Generic)

instance ToJSON ColumnCreate
instance FromJSON ColumnCreate

type ColumnRoutes =
      "create" :> ReqBody '[JSON] ColumnCreate :> Post '[JSON] (Id Column)
 :<|> "list" :> Capture "tableId" (Id Table) :> Get '[JSON] [(Id Column, Text, ColumnType)]

data RecordCreate = RecordCreate
  { recordCreateTableId :: Id Table
  } deriving (Generic)

instance ToJSON RecordCreate
instance FromJSON RecordCreate

type RecordRoutes =
      "create" :> ReqBody '[JSON] RecordCreate :> Post '[JSON] (Id Record)

data CellSet = CellSet
  { cellSetTableId  :: Id Table
  , cellSetColumnId :: Id Column
  , cellSetRecordId :: Id Record
  , cellSetValue    :: Value
  } deriving (Generic)

instance ToJSON CellSet
instance FromJSON CellSet

type CellRoutes =
      "create" :> ReqBody '[JSON] CellSet :> Post '[JSON] (Id Cell)
