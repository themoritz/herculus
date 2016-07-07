{-# LANGUAGE DeriveGeneric #-}

module Lib.Api.WebSocket where

import Data.Aeson
import Data.Text (Text)

import GHC.Generics

import Lib

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
  deriving (Generic, Show)

instance ToJSON WsDownMessage
instance FromJSON WsDownMessage
