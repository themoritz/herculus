{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib.Api.WebSocket where

import Control.DeepSeq

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)

import GHC.Generics

import Lib.Types
import Lib.Model
import Lib.Model.Types
import Lib.Model.Column
import Lib.Model.Cell

data WsUpMessage
  -- Play
  = WsUpGreet Text
  | WsUpStore Text
  | WsUpList
  deriving (Generic, Show, NFData)

instance ToJSON WsUpMessage
instance FromJSON WsUpMessage

data WsDownMessage
  -- Play
  = WsDownGreet Text
  | WsDownList [Text]
  -- Updates
  | WsDownCellsChanged [(Id Column, Id Record, CellContent)]
  | WsDownColumnsChanged [Entity Column]
  deriving (Generic)

instance ToJSON WsDownMessage
instance FromJSON WsDownMessage
