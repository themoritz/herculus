module Views.Table where

import Control.Lens hiding (view)

import qualified Data.Map.Strict as Map

import React.Flux

import Lib.Model

import Store
import Views.Foreign
import Views.Column
import Views.Record
import Views.Cell

tableGrid_ :: State -> ReactElementM eh ()
tableGrid_ !st = view tableGrid st mempty

tableGrid :: ReactView State
tableGrid = defineView "tableGrid" $ \st -> do
  -- autoSizer_ $ \(AutoSizerRenderArgs width height) -> do
    let cells = st ^. stateCells
        cols = st ^. stateColumns
        recs = st ^. stateRecords

        numCols = Map.size cols
        numRecs = Map.size recs

        colByIndex = Map.fromList $ zip [0..] (Map.toList cols)
        recByIndex = Map.fromList $ zip [0..] (Map.toList recs)

        getRecord y = let Just r = Map.lookup y recByIndex in uncurry Entity r
        getColumn x = let Just c = Map.lookup x colByIndex in uncurry Entity c
        getCellProps x y = let Just res = do
                                 (c, col) <- Map.lookup x colByIndex
                                 (r, _)   <- Map.lookup y recByIndex
                                 content  <- Map.lookup (Coords c r) cells
                                 pure $ CellProps c r col content
                           in res

        renderer (GridRenderArgs x y _)
          | x == 0 && y == (numRecs + 1) =
              button_
                [ onClick $ \_ _ -> dispatch TableAddRecord
                ] "Add Record"
          | x == 0 && 0 < y && y <= numRecs =
              record_ $ getRecord (y - 1)
          | y == 0 && x == (numCols + 1) =
              button_
                [ onClick $ \_ _ -> dispatch TableAddColumn
                ] "New Column"
          | y == 0 && 0 < x && x <= numCols =
              column_ $ getColumn (x - 1)
          | 0 < x && x <= numCols && 0 < y && y <= numRecs =
              cell_ $ getCellProps (x - 1) (y - 1)
          | otherwise = div_ mempty

        props = GridProps
          (defineView "cellRenderer" renderer)
          900 -- width
          900 -- height
          300
          (numCols + 2)
          300
          (numRecs + 2)

    grid_ props

--

-- toCellGrid :: State -> Map Coords CellInfo
-- toCellGrid (State _ cells columns records) =
--   let indexedCols = map (\((i, c), p) -> (i, (c, p))) $ zip (Map.toList columns) [0..]
--       indexedRows = zip (Map.keys records) [0..]
--       colMap = Map.fromList indexedCols
--       rowMap = Map.fromList indexedRows
--       go m (Coords colId recId) val =
--         let may = do
--               (col, x) <- Map.lookup colId colMap
--               y <- Map.lookup recId rowMap
--               pure (Position x y, col)
--         in case may of
--           Just (pos, col) -> Map.insert (Coords colId recId) (CellInfo pos col val) m
--           Nothing         -> m
--   in Map.foldlWithKey' go Map.empty cells

-- toColumns :: State -> Map (Id Column) (Int, Column, Maybe (Id Table))
-- toColumns (State tblId _ columns _) =
--   let indexedCols = zip (Map.toList columns) [0..]
--   in Map.fromList $ map (\((colId, col), i) -> (colId, (i, col, tblId))) indexedCols

-- toRecords :: State -> Map (Id Record) (Int, Record)
-- toRecords (State _ _ _ records) =
--   let indexedRecs = zip (Map.toList records) [0..]
--   in Map.fromList $ map (\((recId, reco), i) -> (recId, (i, reco))) indexedRecs
