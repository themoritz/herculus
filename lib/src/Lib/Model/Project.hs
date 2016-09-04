{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib.Model.Project where

import Control.DeepSeq

import           Data.Aeson      (FromJSON, ToJSON)
import           Data.Text       (Text)

import           Data.Bson       ((=:))
import qualified Data.Bson       as Bson

import           GHC.Generics

import           Lib.Model.Class


data Project = Project
  { projectName :: Text
  } deriving (Generic, NFData)

instance Model Project where
  collectionName = const "projects"

instance ToJSON Project
instance FromJSON Project

instance ToDocument Project where
  toDocument (Project name) =
    [ "name" =: name
    ]

instance FromDocument Project where
  parseDocument doc = Project <$> Bson.lookup "name" doc
