{-# LANGUAGE DeriveGeneric #-}

module Lib.Api.WebSocket where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)

import GHC.Generics

import Lib.Types
import Lib.Model.Types
import Lib.Model.Column

data WsUpMessage
  -- Play
  = WsUpGreet Text
  | WsUpStore Text
  | WsUpList
  deriving (Generic, Show)

instance ToJSON WsUpMessage
instance FromJSON WsUpMessage

data WsDownMessage
  -- Play
  = WsDownGreet Text
  | WsDownList [Text]
  -- Cell updates
  | WsDownCellsChanged [(Id Column, Id Record, CellResult)]
  deriving (Generic, Show)

instance ToJSON WsDownMessage
instance FromJSON WsDownMessage
