{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module Lib.Model.Project where

import           Control.Lens           (makeLenses)

import           Data.Bson              ((=:))
import qualified Data.Bson              as Bson
import           Data.Serialize         (decode, encode)
import           Data.Text              (Text)

import           Lib.Model.Auth         (User)
import           Lib.Model.Class
import           Lib.Model.Dependencies
import           Lib.Types              (Id, fromObjectId, toObjectId)

--------------------------------------------------------------------------------

data Project = Project
  { _projectName            :: Text
  , _projectOwner           :: Id User
  , _projectDependencyGraph :: DependencyGraph
  }

makeLenses ''Project

--------------------------------------------------------------------------------

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
