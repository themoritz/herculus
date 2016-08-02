{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Lib.Model.Dependencies
  ( DependencyGraph
  , Dependencies (..)
  , emptyDependencyGraph
  , setDependency
  , setDependencies
  , removeDependency
  , getDependentTopological
  , DependencyType (..)
  , ColumnOrder
  ) where

import           Control.Lens

import           Data.Aeson
import           Data.Aeson.Bson
import           Data.Bson       ((=:))
import qualified Data.Bson       as Bson
import qualified Data.Map        as Map
import           Data.Monoid
import           Data.Text       (pack)

import           GHC.Generics

import           Lib.Model.Class
import           Lib.Model.Column
import           Lib.Model.Dependencies.Types
import           Lib.NamedMap
import           Lib.Types

type Connections = NamedMap (Id Column) DependencyType

data DependencyGraph = DependencyGraph
  { _dependsOnColumns  :: NamedMap (Id Column) Connections
  , _influencesColumns :: NamedMap (Id Column) Connections
  } deriving (Show, Generic)

makeLenses ''DependencyGraph

connection :: Id Column -> Id Column
           -> Lens' (NamedMap (Id Column) Connections) (Maybe DependencyType)
connection c1 c2 = namedMap . at c1 . non emptyNamedMap . namedMap . at c2

instance ToJSON DependencyGraph
instance FromJSON DependencyGraph

instance ToBSON DependencyGraph
instance FromBSON DependencyGraph


data Dependencies = Dependencies
  { dependenciesGraph :: DependencyGraph
  }

instance ToDocument Dependencies where
  toDocument (Dependencies graph) =
    [ "graph" =: toValue graph
    ]

instance FromDocument Dependencies where
  parseDocument doc = do
    val <- Bson.lookup "graph" doc
    case fromValue val of
      Error msg -> Left $ pack msg
      Success g -> pure $ Dependencies g

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

type ColumnOrder = [(Id Column, [(Id Column, DependencyType)])]

getDependentTopological :: Id Column -> DependencyGraph -> Maybe ColumnOrder
getDependentTopological root graph =
    let ordering = bfs [root]
    in if root `elem` (tail ordering)
         then Nothing
         else Just $ map (\c -> (c, getChildrenWithType c)) ordering
  where
    bfs :: [Id Column] -> [Id Column]
    bfs [] = []
    bfs cols = cols <> bfs (concatMap getChildren cols)

    getChildren :: Id Column -> [Id Column]
    getChildren col = map fst . Map.toList $
      graph ^. influencesColumns . namedMap . at col . non emptyNamedMap . namedMap

    getChildrenWithType :: Id Column -> [(Id Column, DependencyType)]
    getChildrenWithType col = Map.toList $
      graph ^. influencesColumns . namedMap . at col . non emptyNamedMap . namedMap
