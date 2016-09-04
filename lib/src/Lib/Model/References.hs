{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE OverloadedStrings   #-}

module Lib.Model.References
  ( ReferenceGraph
  , References (..)
  , emptyReferenceGraph
  , setReference
  , removeReference
  , getReferringColumns
  ) where

import Control.Lens

import           Data.Aeson
import           Data.Aeson.Bson
import           Data.Bson       ((=:))
import qualified Data.Bson       as Bson
import           Data.Text       (pack)
import Data.Set (Set)
import qualified Data.Set as Set

import GHC.Generics

import           Lib.Model.Class
import           Lib.Model.Column
import           Lib.Model.Table
import           Lib.NamedMap
import           Lib.Types

data ReferenceGraph = ReferenceGraph
  { _referencesOfTable :: NamedMap (Id Table) (Set (Id Column))
  , _referencingColumns :: NamedMap (Id Column) (Id Table)
  } deriving (Show, Generic)

makeLenses ''ReferenceGraph

instance ToJSON ReferenceGraph
instance FromJSON ReferenceGraph

instance ToBSON ReferenceGraph
instance FromBSON ReferenceGraph

data References = References
  { referenceGraph :: ReferenceGraph
  }

instance Model References where collectionName = const "references"

instance ToDocument References where
  toDocument (References graph) =
    [ "graph" =: toValue graph
    ]

instance FromDocument References where
  parseDocument doc = do
    val <- Bson.lookup "graph" doc
    case fromValue val of
      Error msg -> Left $ pack msg
      Success g -> pure $ References g

emptyReferenceGraph :: ReferenceGraph
emptyReferenceGraph = ReferenceGraph emptyNamedMap emptyNamedMap

setReference :: Id Table -> Id Column -> ReferenceGraph -> ReferenceGraph
setReference t c = addReference . removeReference c
  where
    addReference graph =
      graph & referencesOfTable . namedMap . at t . non Set.empty . at c .~ Just ()
            & referencingColumns . namedMap . at c .~ Just t

removeReference :: Id Column -> ReferenceGraph -> ReferenceGraph
removeReference c graph =
  case graph ^. referencingColumns . namedMap . at c of
    Nothing -> graph
    Just t ->
      graph & referencesOfTable . namedMap . at t . non Set.empty . at c .~ Nothing
            & referencingColumns . namedMap . at c .~ Nothing

getReferringColumns :: Id Table -> ReferenceGraph -> [Id Column]
getReferringColumns t graph =
  Set.toList $ graph ^. referencesOfTable . namedMap . at t . non Set.empty
