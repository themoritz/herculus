{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

module Lib.Model.Dependencies
  ( DependencyGraph
  , emptyDependencyGraph
  , getAllColumnDependants
  , getDirectColumnDependants
  , getTableDependants
  , getTableDependantsOnly
  , setCodeDependencies
  , setTypeDependencies
  , getDependantsTopological
  , ColumnOrder
  ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State
import           Control.Monad.Trans.Maybe

import           Data.Align
import           Data.Foldable                (for_)
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Maybe                   (fromMaybe)
import           Data.Serialize
import qualified Data.Set                     as Set
import           Data.These

import           GHC.Generics

import {-# SOURCE #-}           Lib.Model.Column
import           Lib.Model.Dependencies.Types
import {-# SOURCE #-}           Lib.Model.Table
import           Lib.Types

--------------------------------------------------------------------------------

data DependencyGraph = DependencyGraph
  { _columnDependants :: Map (Id Column) (Id Table, Map (Id Column) ColumnDependant)
  , _tableDependants  :: Map (Id Table) (Map (Id Column) TableDependant)
  } deriving (Show, Generic)

makeLenses ''DependencyGraph

instance Serialize DependencyGraph

emptyDependencyGraph :: DependencyGraph
emptyDependencyGraph = DependencyGraph Map.empty Map.empty

--------------------------------------------------------------------------------

setColumnDependants :: (Id Column, Id Table) -> [(Id Column, ColumnDependant)]
                    -> DependencyGraph -> DependencyGraph
setColumnDependants (c, t) edges =
  columnDependants . at c . non (t, Map.empty) . _2 .~ Map.fromList edges

getDirectColumnDependants :: Id Column -> DependencyGraph -> [(Id Column, ColumnDependant)]
getDirectColumnDependants c graph = case graph ^. columnDependants . at c of
  Nothing        -> []
  Just (_, deps) -> Map.toList deps

getAllColumnDependants :: (Id Column, Id Table)
                       -- ^ Pair of column together with its table
                       -> DependencyGraph -> [(Id Column, AddTargetMode)]
getAllColumnDependants (c, t) graph =
    Map.toList $ alignWith alignDeps direct indirect
  where
    direct   = fromMaybe nil (graph ^? columnDependants . at c . _Just . _2)
    indirect = fromMaybe nil (graph ^. tableDependants . at t)

getAllColumnDependants' :: Id Column -> DependencyGraph -> [(Id Column, AddTargetMode)]
getAllColumnDependants' c graph = Map.toList $ case graph ^. columnDependants . at c of
  Nothing -> Map.empty
  Just (t, direct) ->
    let indirect = fromMaybe nil (graph ^. tableDependants . at t)
    in alignWith alignDeps direct indirect

alignDeps :: These ColumnDependant TableDependant -> AddTargetMode
alignDeps (This ColDepRef) = AddOne
alignDeps _                = AddAll

--------------------------------------------------------------------------------

getTableDependants :: Id Table -> DependencyGraph -> [(Id Column, TableDependant)]
getTableDependants t graph = case graph ^. tableDependants . at t of
  Nothing   -> []
  Just deps -> Map.toList deps

getTableDependantsOnly :: TableDependant -> Id Table -> DependencyGraph -> [Id Column]
getTableDependantsOnly typ t graph =
  map fst $ filter (\(_, typ') -> typ == typ') $ getTableDependants t graph

--------------------------------------------------------------------------------

setTypeDependencies :: Monad m
                    => (Id Column -> m (Id Table))
                    -> Id Column
                    -> TypeDependencies
                    -> DependencyGraph
                    -> m (Maybe DependencyGraph)
setTypeDependencies = undefined
-- TODO:

setCodeDependencies :: Monad m
                    => (Id Column -> m (Id Table))
                    -- ^ A monadic function that can get the tableId of a column
                    -> Id Column
                    -- ^ The column to set the dependencies for
                    -> CodeDependencies
                    -> DependencyGraph
                    -> m (Maybe DependencyGraph)
setCodeDependencies getTableId c codeDeps graph = do
  graph' <- flip execStateT graph $ do
    let colDepsSet = columnsOfCodeDeps codeDeps
        tblDepsSet = tablesOfCodeDeps codeDeps
    -- Remove edges
    columnDependants . itraversed . withIndex
                     . filtered (flip Set.notMember colDepsSet . fst)
                     . _2 . _2 . at c .= Nothing
    tableDependants  . itraversed . withIndex
                     . filtered (flip Set.notMember tblDepsSet . fst)
                     . _2 . at c .= Nothing
    -- TODO:
    -- Add edges
    -- for_ colDeps $ \(dep, typ) -> do
    --   mNode <- use (columnDependants . at dep)
    --   let addEdge tblId =
    --         columnDependants . at dep . non (tblId, Map.empty) . _2 . at c .= Just typ
    --   case mNode of
    --     Just (tblId, _) -> addEdge tblId
    --     Nothing         -> lift (getTableId dep) >>= addEdge
    -- for_ tblDeps $ \(dep, typ) ->
    --   tableDependants . at dep . non Map.empty . at c .= Just typ
  case getDependantsTopological [c] graph' of
    Nothing -> pure Nothing
    Just _  -> pure $ Just graph'

--------------------------------------------------------------------------------

type ColumnOrder = [(Id Column, [(Id Column, AddTargetMode)])]

getDependantsTopological :: [Id Column] -> DependencyGraph -> Maybe ColumnOrder
getDependantsTopological roots graph =
    tail <$> evalState (runMaybeT $ topSort nullObjectId) Map.empty
  where
    topSort :: Id Column -> MaybeT (State (Map (Id Column) Bool)) ColumnOrder
    topSort x = gets (Map.lookup x) >>= \case
      Just False -> empty -- Found circle
      Just True -> pure []
      Nothing -> do
        modify $ Map.insert x False
        childOrders <- mapM topSort (map fst $ getAllColumnDependants' x graph')
        modify $ Map.insert x True
        pure $ (x, getAllColumnDependants' x graph') : join (reverse childOrders)

    -- Modified graph that contains edges from a fake node to all the root
    -- columns.
    graph' = setColumnDependants (nullObjectId, nullObjectId)
                                 (map (, ColDepRef) roots)
                                 graph
