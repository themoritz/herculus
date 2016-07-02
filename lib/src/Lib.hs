{-# LANGUAGE DeriveGeneric #-}

module Lib where

import Data.Aeson
import Data.Text

import GHC.Generics

data WsUpMessage
  = WsUpGreet Text
  deriving (Generic, Show)

instance ToJSON WsUpMessage
instance FromJSON WsUpMessage

data WsDownMessage
  = WsDownGreet Text
  deriving (Generic, Show)

instance ToJSON WsDownMessage
instance FromJSON WsDownMessage
