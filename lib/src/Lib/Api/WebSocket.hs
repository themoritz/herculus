{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Lib.Api.WebSocket where

import           Control.DeepSeq

import           Data.Aeson       (FromJSON, ToJSON)

import           GHC.Generics

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

type Diff a = [(Id a, ChangeOp, a)]

data WsDownMessage
  = WsDownProjectDiff (Diff Cell)
                      (Diff Column)
                      (Diff Row)
                      (Diff Table)
  deriving (Generic, NFData, Show)

instance ToJSON WsDownMessage
instance FromJSON WsDownMessage
