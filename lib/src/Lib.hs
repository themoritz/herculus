{-# LANGUAGE DeriveGeneric #-}

module Lib where

import Data.Aeson
import Data.Text

import GHC.Generics

data Command
  = CommandNew Text
  | CommandDel Int
  deriving (Generic, Show)

instance ToJSON Command
instance FromJSON Command
