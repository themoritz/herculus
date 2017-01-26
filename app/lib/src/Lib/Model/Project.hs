{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Lib.Model.Project where

import           Control.DeepSeq

import           Control.Lens           (Lens', lens)
import           Data.Aeson             (FromJSON, ToJSON)
import           Data.Serialize         (decode, encode)
import           Data.Text              (Text)

import           Data.Bson              ((=:))
import qualified Data.Bson              as Bson

import           GHC.Generics

import           Lib.Model.Auth         (User)
import           Lib.Model.Class
import           Lib.Model.Dependencies
import           Lib.Types              (Id, fromObjectId, toObjectId)

--------------------------------------------------------------------------------

data ProjectClient = ProjectClient
  { _projectClientName  :: Text
  , _projectClientOwner :: Id User
  } deriving (Generic, NFData, Show)

projectClientName :: Lens' ProjectClient Text
projectClientName = lens _projectClientName (\s a -> s { _projectClientName = a })

projectClientOwner :: Lens' ProjectClient (Id User)
projectClientOwner = lens _projectClientOwner (\s a -> s { _projectClientOwner = a })

instance ToJSON ProjectClient
instance FromJSON ProjectClient

--------------------------------------------------------------------------------

data Project = Project
  { _projectName            :: Text
  , _projectOwner           :: Id User
  , _projectDependencyGraph :: DependencyGraph
  }

projectName :: Lens' Project Text
projectName = lens _projectName (\s a -> s { _projectName = a })

projectOwner :: Lens' Project (Id User)
projectOwner = lens _projectOwner (\s a -> s { _projectOwner = a })

projectDependencyGraph :: Lens' Project DependencyGraph
projectDependencyGraph = lens _projectDependencyGraph (\s a -> s { _projectDependencyGraph = a })

--------------------------------------------------------------------------------

instance ClientModel Project ProjectClient where
  toClient (Project name owner _) = ProjectClient name owner
  fromClient (ProjectClient name owner) = Project name owner emptyDependencyGraph

instance Model Project where
  collectionName = const "projects"

instance ToDocument Project where
  toDocument (Project name owner graph) =
    [ "name"            =: name
    , "owner"           =: toObjectId owner
    , "dependencyGraph" =: Bson.Binary (encode graph)
    ]

instance FromDocument Project where
  parseDocument doc = do
    name <- Bson.lookup "name" doc
    owner <- Bson.lookup "owner" doc
    Bson.Binary graph <- Bson.lookup "dependencyGraph" doc
    case decode graph of
      Left err     -> fail err
      Right graph' -> pure $ Project name (fromObjectId owner) graph'
