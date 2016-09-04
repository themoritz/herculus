{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

module Lib.Model.Table where

import Control.DeepSeq

import           Data.Aeson      (FromJSON, ToJSON)
import           Data.Text       (Text)

import           Data.Bson       ((=:))
import qualified Data.Bson       as Bson

import           GHC.Generics

import           Lib.Model.Class
import           Lib.Model.Project
import           Lib.Types

data Table = Table
  { tableProjectId :: Id Project
  , tableName      :: Text
  } deriving (Generic, NFData)

instance Model Table where
  collectionName = const "tables"

instance ToJSON Table
instance FromJSON Table

instance ToDocument Table where
  toDocument (Table prj name) =
    [ "projectId" =: toObjectId prj
    , "name" =: name
    ]

instance FromDocument Table where
  parseDocument doc = Table <$> (fromObjectId <$> Bson.lookup "projectId" doc)
                            <*> Bson.lookup "name" doc
