{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib.Model.Types where

import           Data.Aeson      (FromJSON, ToJSON)
import           Data.Aeson.Bson
import           Data.Text       (Text, pack)
import           Data.Typeable

import           Data.Bson       (Val, (=:))
import qualified Data.Bson       as Bson

import           GHC.Generics

import           Lib.Model.Class
import           Lib.Types


data Project = Project
  { projectName :: Text
  } deriving (Generic)

instance ToJSON Project
instance FromJSON Project

instance ToDocument Project where
  toDocument (Project name) =
    [ "name" =: name
    ]

instance FromDocument Project where
  parseDocument doc = Project <$> Bson.lookup "name" doc

data Table = Table
  { tableProjectId :: Id Project
  , tableName      :: Text
  } deriving (Generic)

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

data Record = Record
  { recordTableId :: Id Table
  }
  deriving (Generic)

instance ToJSON Record
instance FromJSON Record

instance ToDocument Record where
  toDocument (Record tblId)=
    [ "tableId" =: toObjectId tblId
    ]

instance FromDocument Record where
  parseDocument doc = Record <$> Bson.lookup "tableId" doc
