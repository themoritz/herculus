{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}

module Lib.Model.Dependencies
  ( DependencyGraph
  , emptyDependencyGraph
  , getAllColumnDependants
  , getTableDependants
  , getTableDependantsOnly
  , setCodeDependencies
  , setTypeDependencies
  , purgeColumn
  , purgeTable
  , getDependantsTopological
  , ColumnOrder
  ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.State
import           Control.Monad.Trans.Maybe

import           Data.Align
import           Data.Foldable                (for_)
import           Data.Functor                 (($>))
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

type ColumnDependendants =
  Map (Id Column) (Id Table, Map (Id Column) ColumnDependency)
type TableDependendants =
  Map (Id Table) (Map (Id Column) TableDependency)

data DependencyGraph = DependencyGraph
  { _columnDependants :: ColumnDependendants
  , _tableDependants  :: TableDependendants
  } deriving (Show, Generic)

columnDependants :: Lens' DependencyGraph ColumnDependendants
columnDependants = lens _columnDependants (\s a -> s { _columnDependants = a })

tableDependants :: Lens' DependencyGraph TableDependendants
tableDependants = lens _tableDependants (\s a -> s { _tableDependants = a })

instance Serialize DependencyGraph

emptyDependencyGraph :: DependencyGraph
emptyDependencyGraph = DependencyGraph Map.empty Map.empty

--------------------------------------------------------------------------------

setColumnDependants :: (Id Column, Id Table) -> [(Id Column, ColumnDependency)]
                    -> DependencyGraph -> DependencyGraph
setColumnDependants (c, t) edges =
  columnDependants . at c . non (t, Map.empty) . _2 .~ Map.fromList edges

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

alignDeps :: These ColumnDependency TableDependency -> AddTargetMode
alignDeps (This ColDepRef) = AddOne
alignDeps _                = AddAll

--------------------------------------------------------------------------------

getTableDependants :: Id Table
                   -> DependencyGraph -> [(Id Column, TableDependency)]
getTableDependants t graph = case graph ^. tableDependants . at t of
  Nothing   -> []
  Just deps -> Map.toList deps

getTableDependantsOnly :: Id Table -> (TableDependency -> Bool)
                       -> DependencyGraph -> [Id Column]
getTableDependantsOnly t p graph =
  map fst $ filter (p . snd) $ getTableDependants t graph

--------------------------------------------------------------------------------

purgeColumn :: Id Column -> DependencyGraph -> DependencyGraph
purgeColumn c graph = flip execState graph $ purgeColumn' c

purgeColumn' :: Id Column -> State DependencyGraph ()
purgeColumn' c = do
  -- Edges to c
  columnDependants . traversed . _2 . at c .= Nothing
  tableDependants  . traversed . at c .= Nothing
  -- c itself
  columnDependants . at c .= Nothing

-- | Also purges from the graph all columns that reference the given table
purgeTable :: Id Table -> DependencyGraph -> DependencyGraph
purgeTable t graph = flip execState graph $ do
  columns <- uses columnDependants $ Map.keys . Map.filter ((== t) . fst)
  mapM_ purgeColumn' columns
  tableDependants . at t .= Nothing

setTypeDependencies :: Id Column
                    -> TypeDependencies
                    -> DependencyGraph
                    -> Maybe DependencyGraph
setTypeDependencies c typeDeps graph =
    getDependantsTopological [c] graph' $> graph'
  where
    graph' = flip execState graph $ do
      let tblDepsSet = tablesOfTypeDeps typeDeps

      -- Remove edges:
      let removeRowRef x = case x of
            TblDepRowRef -> Nothing
            _            -> Just x
      tableDependants . itraversed . withIndex
                      . filtered (flip Set.notMember tblDepsSet . fst)
                      . _2 %= Map.update removeRowRef c
      -- Columns have to type deps so we don't touch them

      -- Add edges:
      for_ (Set.toList tblDepsSet) $ \dep ->
        tableDependants . at dep . non Map.empty . at c .= Just TblDepRowRef

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

    -- Remove edges:
    columnDependants . itraversed . withIndex
                     . filtered (flip Set.notMember colDepsSet . fst)
                     . _2 . _2 . at c .= Nothing
    -- We need to make sure we only affect actual code dependencies here
    let keepRowRef x = case x of
          TblDepRowRef -> Just x
          _            -> Nothing
    tableDependants  . itraversed . withIndex
                     . filtered (flip Set.notMember tblDepsSet . fst)
                     . _2 %= Map.update keepRowRef c

    -- Add edges:
    forCodeDeps_ codeDeps $ \case
      Left (dep, typ) -> do
        mNode <- use (columnDependants . at dep)
        let addEdge tblId =
              columnDependants . at dep . non (tblId, Map.empty) . _2 . at c .= Just typ
        case mNode of
          Just (tblId, _) -> addEdge tblId
          Nothing         -> lift (getTableId dep) >>= addEdge
      Right (dep, typ) -> do
        tableDependants . at dep . non Map.empty . at c .= Just typ

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
