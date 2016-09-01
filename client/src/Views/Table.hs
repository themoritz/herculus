module Views.Table where

import           Control.Lens      hiding (view)

import qualified Data.Map.Strict   as Map
import           Data.Monoid       ((<>))

import           React.Flux

import           Lib.Model

import           Store
import           Views.Cell
import           Views.Column
import           Views.Combinators
import           Views.Foreign
import           Views.Record

tableGrid_ :: State -> ReactElementM eh ()
tableGrid_ !st = view tableGrid st mempty

tableGrid :: ReactView State
tableGrid = defineView "tableGrid" $ \st -> do
  let cells = st ^. stateCells
      cols = st ^. stateColumns
      recs = st ^. stateRecords

      numCols = Map.size cols
      numRecs = Map.size recs

      colByIndex = Map.fromList $ zip [0..] (Map.toList cols)
      recByIndex = Map.fromList $ zip [0..] (Map.toList recs)

      getRecord y = let Just r = Map.lookup y recByIndex in uncurry Entity r
      getColumn x = let Just c = Map.lookup x colByIndex in uncurry Entity c
      getCellProps x y = do
        (c, col) <- Map.lookup x colByIndex
        (r, _)   <- Map.lookup y recByIndex
        content  <- Map.lookup (Coords c r) cells
        pure $ CellProps c r col content

      renderer (GridRenderArgs x y _)
        | x == 0 && y == 0 = cldiv_ "origin" mempty
        | x == 0 && y == (numRecs + 1) = cldiv_ "record-new" $
            faButton_ "plus-circle" $ dispatch TableAddRecord
        | x == 0 && 0 < y && y <= numRecs = cldiv_ "record" $
            record_ $ getRecord (y - 1)
        | y == 0 && x == (numCols + 1) = cldiv_ "column-new" $
            faButton_ "plus-circle" $ dispatch TableAddColumn
        | y == 0 && 0 < x && x <= numCols =
            column_ $ getColumn (x - 1)
        | 0 < x && x <= numCols && 0 < y && y <= numRecs = cldiv_ "cell" $
            case getCellProps (x - 1) (y - 1) of
              Nothing -> mempty
              Just res -> cell_ res
        | otherwise = cldiv_ "empty" mempty

      props = GridProps
        { gridCellRenderer = defineView "cellRenderer" renderer
        , gridColumnWidths = [37] <> replicate numCols 250 <> [37]
        , gridColumnCount = numCols + 2
        , gridRowHeights = [54] <> replicate numRecs 37 <> [37]
        , gridRowCount = numRecs + 2
        }

  grid_ props
