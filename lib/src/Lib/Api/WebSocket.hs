{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Lib.Api.WebSocket where

import           Control.DeepSeq

import           Data.Aeson       (FromJSON, ToJSON)

import           GHC.Generics

import           Lib.Model
import           Lib.Model.Cell
import           Lib.Model.Column
import           Lib.Model.Row
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
  | WsDownRowCreated (Id Table) (Id Row) [(Entity Column, CellContent)]
  | WsDownRowDeleted (Id Table) (Id Row)
  deriving (Generic)

instance ToJSON WsDownMessage
instance FromJSON WsDownMessage
