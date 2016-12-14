{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

module Lib.Model.Dependencies
  ( DependencyGraph
  , emptyDependencyGraph
  , setColumnDependency
  , setColumnDependencies
  , removeColumnDependency
  , getColumnChildren
  , getColumnChildrenOnly
  , setTableDependency
  , setTableDependencies
  , removeTableDependency
  , getTableChildren
  , getTableChildrenOnly
  , getDependentTopological
  , ColumnOrder
  ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State
import           Control.Monad.Trans.Maybe

import           Data.Align
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Maybe                   (fromMaybe)
import           Data.Serialize
import           Data.These

import           GHC.Generics

import           Lib.Model.Column
import           Lib.Model.Dependencies.Types
import           Lib.Model.Table
import           Lib.Types

data DependencyGraph = DependencyGraph
  { _dependencyColumns :: Map (Id Column) (Id Table, Map (Id Column) ColumnDependency)
  , _dependencyTables  :: Map (Id Table) (Map (Id Column) TableDependency)
  } deriving (Show, Generic)

makeLenses ''DependencyGraph

instance Serialize DependencyGraph

columnDependency :: (Id Column, Id Table) -> Id Column
                 -> Lens' DependencyGraph (Maybe ColumnDependency)
columnDependency (c1, t1) c2 =
  dependencyColumns . at c1 . non (t1, Map.empty) . _2 . at c2

tableDependency :: Id Table -> Id Column
                -> Lens' DependencyGraph (Maybe TableDependency)
tableDependency t1 c2 =
  dependencyTables . at t1 . non Map.empty . at c2

emptyDependencyGraph :: DependencyGraph
emptyDependencyGraph = DependencyGraph Map.empty Map.empty

--

setColumnDependency :: (Id Column, Id Table) -> Id Column -> ColumnDependency
                    -> DependencyGraph -> DependencyGraph
setColumnDependency start end typ = columnDependency start end .~ Just typ

removeColumnDependency :: (Id Column, Id Table) -> Id Column
                       -> DependencyGraph -> DependencyGraph
removeColumnDependency start end = columnDependency start end .~ Nothing

setColumnDependencies :: (Id Column, Id Table) -> [(Id Column, ColumnDependency)]
                      -> DependencyGraph -> DependencyGraph
setColumnDependencies (c, t) edges =
  dependencyColumns . at c . non (t, Map.empty) . _2 .~ Map.fromList edges

getColumnChildren :: Id Column -> DependencyGraph -> [(Id Column, ColumnDependency)]
getColumnChildren c graph = case graph ^. dependencyColumns . at c of
  Nothing        -> []
  Just (_, deps) -> Map.toList deps

getColumnChildrenOnly :: ColumnDependency -> Id Column -> DependencyGraph -> [Id Column]
getColumnChildrenOnly typ c graph =
  map fst $ filter (\(_, typ') -> typ == typ') $ getColumnChildren c graph

--

setTableDependency :: Id Table -> Id Column -> TableDependency
                   -> DependencyGraph -> DependencyGraph
setTableDependency start end typ = tableDependency start end .~ Just typ

removeTableDependency :: Id Table -> Id Column
                      -> DependencyGraph -> DependencyGraph
removeTableDependency start end = tableDependency start end .~ Nothing

setTableDependencies :: Id Table -> [(Id Column, TableDependency)]
                     -> DependencyGraph -> DependencyGraph
setTableDependencies t edges =
  dependencyTables . at t . non Map.empty .~ Map.fromList edges

getTableChildren :: Id Table -> DependencyGraph -> [(Id Column, TableDependency)]
getTableChildren t graph = case graph ^. dependencyTables . at t of
  Nothing   -> []
  Just deps -> Map.toList deps

getTableChildrenOnly :: TableDependency -> Id Table -> DependencyGraph -> [Id Column]
getTableChildrenOnly typ t graph =
  map fst $ filter (\(_, typ') -> typ == typ') $ getTableChildren t graph

--

type ColumnOrder = [(Id Column, [(Id Column, AddTargetMode)])]

getDependentTopological :: [Id Column] -> DependencyGraph -> Maybe ColumnOrder
getDependentTopological roots graph =
    tail <$> evalState (runMaybeT $ topSort nullObjectId) Map.empty
  where
    topSort :: Id Column -> MaybeT (State (Map (Id Column) Bool)) ColumnOrder
    topSort x = gets (Map.lookup x) >>= \case
      Just False -> empty -- Found circle
      Just True -> pure []
      Nothing -> do
        modify $ Map.insert x False
        childOrders <- mapM topSort (map fst $ collectNext x)
        modify $ Map.insert x True
        pure $ (x, collectNext x) : join (reverse childOrders)

    collectNext :: Id Column -> [(Id Column, AddTargetMode)]
    collectNext c = Map.toList $ case graph' ^. dependencyColumns . at c of
      Nothing -> Map.empty
      Just (t, direct) ->
        let indirect = fromMaybe nil (graph' ^. dependencyTables . at t)
        in alignWith alignDeps direct indirect

    alignDeps (This ColDepRef) = AddOne
    alignDeps _                = AddAll

    -- Modified graph that contains edges from a fake node to all the root
    -- columns.
    graph' = setColumnDependencies (nullObjectId, nullObjectId)
                                   (map (, ColDepRef) roots)
                                   graph
