{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Dependencies
  ( DependencyGraph
  , emptyDependencyGraph
  , setDependency
  , removeDependency
  , DependencyType (..)
  ) where

import           Control.Lens

import           Data.Aeson
import           Data.Aeson.Bson
import           Data.Map        (Map)
import qualified Data.Map        as Map

import           GHC.Generics

import           Lib
import           Lib.NamedMap

data DependencyType
  = OneToOne
  | OneToAll
  deriving (Generic, Eq, Ord)

instance ToJSON DependencyType
instance FromJSON DependencyType

instance ToValue DependencyType
instance FromValue DependencyType

type Connections = NamedMap (Id Column) DependencyType

data DependencyGraph = DependencyGraph
  { _dependsOnColumns  :: NamedMap (Id Column) Connections
  , _influencesColumns :: NamedMap (Id Column) Connections
  } deriving (Generic)

makeLenses ''DependencyGraph

connection :: Id Column -> Id Column
           -> Lens' (NamedMap (Id Column) Connections) (Maybe DependencyType)
connection c1 c2 = namedMap . at c1 . non emptyNamedMap . namedMap . at c2

instance ToJSON DependencyGraph
instance FromJSON DependencyGraph

instance ToValue DependencyGraph
instance FromValue DependencyGraph

emptyDependencyGraph :: DependencyGraph
emptyDependencyGraph = DependencyGraph (NamedMap Map.empty) (NamedMap Map.empty)

setDependency :: Id Column -> Id Column -> DependencyType
              -> DependencyGraph -> DependencyGraph
setDependency start end typ graph = graph
  & dependsOnColumns . connection start end .~ Just typ
  & influencesColumns . connection end start .~ Just typ

removeDependency :: Id Column -> Id Column
                 -> DependencyGraph -> DependencyGraph
removeDependency start end graph = graph
  & dependsOnColumns . connection start end .~ Nothing
  & influencesColumns . connection end start .~ Nothing
