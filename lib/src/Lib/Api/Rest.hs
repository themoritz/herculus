{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Lib.Api.Rest where

import           Data.Aeson      (FromJSON, ToJSON)
import           Data.Text

import           GHC.Generics

import           Servant.API

import           Lib.Model
import           Lib.Model.Types
import           Lib.Types

type Routes =
      "project" :> ProjectRoutes
 :<|> "table" :> TableRoutes
 :<|> "column" :> ColumnRoutes
 :<|> "record" :> RecordRoutes
 :<|> "cell" :> CellRoutes

type ProjectRoutes =
      "create" :> ReqBody '[JSON] Project :> Post '[JSON] (Id Project)
 :<|> "list" :> Get '[JSON] [Project]

type TableRoutes =
      "create" :> ReqBody '[JSON] Table :> Post '[JSON] (Id Table)
 :<|> "list" :> Capture "projectId" (Id Project) :> Get '[JSON] [Entity Table]
 :<|> "data" :> Capture "tableId" (Id Table) :> Get '[JSON] [(Id Column, Id Record, Value)]

type ColumnRoutes =
      "create" :> ReqBody '[JSON] (Id Table) :> Post '[JSON] (Id Column)
 :<|> "setName" :> Capture "columnId" (Id Column) :> ReqBody '[JSON] Text :> Post '[JSON] ()
 :<|> "setType" :> Capture "columnId" (Id Column) :> ReqBody '[JSON] ColumnType :> Post '[JSON] ()
 :<|> "list" :> Capture "tableId" (Id Table) :> Get '[JSON] [Entity Column]

type RecordRoutes =
      "create" :> ReqBody '[JSON] (Id Table) :> Post '[JSON] (Id Record)
 :<|> "list" :> Capture "tableId" (Id Table) :> Get '[JSON] [Entity Record]

type CellRoutes =
      "set" :> ReqBody '[JSON] Cell :> Post '[JSON] ()
