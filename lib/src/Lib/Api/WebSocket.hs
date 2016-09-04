{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Lib.Api.WebSocket where

import           Control.DeepSeq

import           Data.Aeson       (FromJSON, ToJSON)

import           GHC.Generics

import           Lib.Model
import           Lib.Model.Cell
import           Lib.Model.Column
import           Lib.Model.Record
import           Lib.Model.Table
import           Lib.Types

data WsUpMessage
  = WsUpDummy
  deriving (Generic, Show, NFData)

instance ToJSON WsUpMessage
instance FromJSON WsUpMessage

data WsDownMessage
  = WsDownCellsChanged [Cell]
  | WsDownColumnsChanged [Entity Column]
  | WsDownRecordCreated (Id Table) (Id Record) [(Entity Column, CellContent)]
  | WsDownRecordDeleted (Id Table) (Id Record)
  deriving (Generic)

instance ToJSON WsDownMessage
instance FromJSON WsDownMessage
