{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib.Api.WebSocket where

import Control.DeepSeq

import Data.Aeson (ToJSON, FromJSON)

import GHC.Generics

import Lib.Types
import Lib.Model
import Lib.Model.Types
import Lib.Model.Column
import Lib.Model.Cell

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
