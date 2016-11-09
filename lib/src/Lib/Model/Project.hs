{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib.Model.Project where

import           Control.DeepSeq

import           Control.Lens    (makeLenses)
import           Data.Aeson      (FromJSON, ToJSON)
import           Data.Text       (Text)

import           Data.Bson       ((=:))
import qualified Data.Bson       as Bson

import           GHC.Generics

import           Lib.Model.Auth  (User)
import           Lib.Model.Class
import           Lib.Types       (Id, fromObjectId, toObjectId)


data Project = Project
  { _projectName  :: Text
  , _projectOwner :: Id User -- user id of project creator/owner
  } deriving (Generic, NFData)

makeLenses ''Project

instance Model Project where
  collectionName = const "projects"

instance ToJSON Project
instance FromJSON Project

instance ToDocument Project where
  toDocument (Project name owner) =
    [ "name" =: name
    , "owner" =: toObjectId owner
    ]

instance FromDocument Project where
  parseDocument doc =
    Project <$> Bson.lookup "name" doc
            <*> (fromObjectId <$> Bson.lookup "owner" doc)
