{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Lib.Model.Table where

import           Lib.Prelude

import           Control.Lens      (Lens', lens)

import           Data.Aeson        (FromJSON, ToJSON)
import           Data.Text         (Text)

import           Data.Bson         ((=:))
import qualified Data.Bson         as Bson

import           Lib.Model.Class
import {-# SOURCE #-} Lib.Model.Project
import           Lib.Types

data Table = Table
  { _tableProjectId :: Id Project
  , _tableName      :: Text
  } deriving (Generic, Show)

tableProjectId :: Lens' Table (Id Project)
tableProjectId = lens _tableProjectId (\s a -> s { _tableProjectId = a })

tableName :: Lens' Table Text
tableName = lens _tableName (\s a -> s { _tableName = a })

instance Model Table where
  collectionName = const "tables"

instance ToJSON Table
instance FromJSON Table

instance ToDocument Table where
  toDocument (Table p name) =
    [ "projectId" =: toObjectId p
    , "name" =: name
    ]

instance FromDocument Table where
  parseDocument doc = Table <$> (fromObjectId <$> Bson.lookup "projectId" doc)
                            <*> Bson.lookup "name" doc
