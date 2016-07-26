{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Dependencies
  ( DependencyGraph
  , emptyDependencyGraph
  , setDependency
  , setDependencies
  , removeDependency
  , getDependentTopological
  , DependencyType (..)
  ) where

import           Control.Lens

import           Data.Aeson
import           Data.Aeson.Bson
import qualified Data.Map        as Map
import           Data.Monoid

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

setDependencies :: Id Column -> [(Id Column, DependencyType)]
                -> DependencyGraph -> DependencyGraph
setDependencies start edges graph =
  let connections = maybe [] (map fst . Map.toList . unNamedMap)
        (graph ^. dependsOnColumns . namedMap . at start)
      removeOld g = foldr (removeDependency start) g connections
      setNew g    = foldr (\(end, typ) -> setDependency start end typ) g edges
  in setNew . removeOld $ graph

getDependentTopological :: Id Column -> DependencyGraph -> Maybe [Id Column]
getDependentTopological root graph =
    let _:ordering = bfs [root]
    in if root `elem` ordering then Nothing else Just ordering
  where
    bfs :: [Id Column] -> [Id Column]
    bfs cols = cols <> bfs (concatMap getChildren cols)

    getChildren :: Id Column -> [Id Column]
    getChildren col = map fst . Map.toList $
      graph ^. influencesColumns . namedMap . at col . non emptyNamedMap . namedMap
