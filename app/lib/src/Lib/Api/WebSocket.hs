{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Lib.Api.WebSocket where

import           Lib.Prelude

import           Data.Aeson            (FromJSON, ToJSON)
import           Data.Text             (Text)

import           Lib.Api.Schema.Column
import           Lib.Model
import qualified Lib.Model.Auth        as M (SessionKey)
import qualified Lib.Model.Cell        as M
import qualified Lib.Model.Project     as M
import qualified Lib.Model.Row         as M
import qualified Lib.Model.Table       as M
import           Lib.Types

data WsUpMessage
  = WsUpSubscribe M.SessionKey (Id M.Project)
  deriving (Generic, ToJSON, FromJSON, Show)

type Diff a = [(ChangeOp, a)]

data WsDownMessage
  = WsDownSubscribeError Text
  | WsDownProjectDiff (Id M.Project)
                      (Diff (Entity M.Cell))
                      (Diff Column)
                      (Diff (Entity M.Row))
                      (Diff (Entity M.Table))
  deriving (Generic, ToJSON, FromJSON)
