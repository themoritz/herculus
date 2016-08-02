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
import           Lib.Model.Column
import           Lib.Model.Cell
import           Lib.Types

type Routes =
      "project" :> ProjectRoutes
 :<|> "table" :> TableRoutes
 :<|> "column" :> ColumnRoutes
 :<|> "record" :> RecordRoutes
 :<|> "cell" :> CellRoutes

type ProjectRoutes =
      "create" :> ReqBody '[JSON] Project :> Post '[JSON] (Id Project)
 :<|> "list" :> Get '[JSON] [Entity Project]

type TableRoutes =
      "create" :> ReqBody '[JSON] Table :> Post '[JSON] (Id Table)
 :<|> "list" :> Capture "projectId" (Id Project) :> Get '[JSON] [Entity Table]
 :<|> "data" :> Capture "tableId" (Id Table)
             :> Get '[JSON] [(Id Column, Id Record, CellResult)]

type ColumnRoutes =
        "create"  :> ReqBody '[JSON] (Id Table) :> Post '[JSON] (Id Column)
  :<|>  "setName" :> Capture "columnId" (Id Column)
                  :> ReqBody '[JSON] Text :> Post '[JSON] ()
  :<|>  "setType" :> Capture "columnId" (Id Column)
                  :> ReqBody '[JSON] DataType :> Post '[JSON] ()
  :<|>  "setInputType" :> Capture "columnId" (Id Column)
                  :> ReqBody '[JSON] ColumnType :> Post '[JSON] ()
  :<|>  "setSourceCode" :> Capture "columnId" (Id Column)
                  :> ReqBody '[JSON] Text :> Post '[JSON] CompiledCode
  :<|>  "list" :> Capture "tableId" (Id Table) :> Get '[JSON] [Entity Column]

type RecordRoutes =
      "create" :> ReqBody '[JSON] (Id Table) :> Post '[JSON] (Id Record)
 :<|> "list" :> Capture "tableId" (Id Table) :> Get '[JSON] [Entity Record]

type CellRoutes =
      "set" :> Capture "columnId" (Id Column) :> Capture "recordId" (Id Record)
            :> ReqBody '[JSON] Text :> Post '[JSON] CellResult
