{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Lib.Model.Project where

import           Lib.Prelude

import           Control.Lens            (makeLenses)

import           Data.Bson               ((=:))
import qualified Data.Bson               as Bson
import           Data.Serialize          (decode, encode)
import           Data.Text               (Text, pack)

import           Lib.Api.Schema.Compiler

import           Lib.Model.Auth          (User)
import           Lib.Model.Class
import           Lib.Model.Common
import           Lib.Model.Dependencies
import           Lib.Types               (Id, fromObjectId, toObjectId)

--------------------------------------------------------------------------------

data Project = Project
  { _projectName            :: Text
  , _projectOwner           :: Id User
  , _projectModuleSource    :: Text
  , _projectModule          :: CompileResult Module
  , _projectDependencyGraph :: DependencyGraph
  }

makeLenses ''Project

--------------------------------------------------------------------------------

instance Model Project where
  collectionName = const "projects"

instance ToDocument Project where
  toDocument (Project name owner moduSrc modu graph) =
    [ "name"            =: name
    , "owner"           =: toObjectId owner
    , "moduleSource"    =: moduSrc
    , "module"          =: modu
    , "dependencyGraph" =: Bson.Binary (encode graph)
    ]

instance FromDocument Project where
  parseDocument doc = do
    name <- Bson.lookup "name" doc
    owner <- Bson.lookup "owner" doc
    moduSrc <- Bson.lookup "moduleSource" doc
    modu <- Bson.lookup "module" doc
    Bson.Binary graph <- Bson.lookup "dependencyGraph" doc
    case decode graph of
      Left err ->
        throwError $ pack err
      Right graph' ->
        pure $ Project name (fromObjectId owner) moduSrc modu graph'
