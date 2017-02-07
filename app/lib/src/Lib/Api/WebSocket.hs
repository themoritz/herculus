{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Lib.Api.WebSocket where

import           Data.Aeson          (FromJSON, ToJSON)
import           Data.Text           (Text)


import           GHC.Generics

import           Lib.Api.Schema.Auth (GetUserInfoResponse)
import           Lib.Model.Auth      (SessionKey)
import           Lib.Model.Cell
import           Lib.Model.Column
import           Lib.Model.Project
import           Lib.Model.Row
import           Lib.Model.Table
import           Lib.Types

data WsUpMessage
  = WsUpAuthenticate SessionKey
  | WsUpLogout
  | WsUpSubscribe (Id Project)
  | WsUpUnsubscribe
  deriving (Generic, Show)

instance ToJSON WsUpMessage
instance FromJSON WsUpMessage

type Diff a = [(Id a, ChangeOp, a)]

data WsDownMessage
  = WsDownAuthResponse GetUserInfoResponse
  | WsDownSubscribeError Text
  | WsDownProjectDiff (Id Project)
                      (Diff Cell)
                      (Diff Column)
                      (Diff Row)
                      (Diff Table)
  deriving (Generic, Show)

instance ToJSON WsDownMessage
instance FromJSON WsDownMessage
