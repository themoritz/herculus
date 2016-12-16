{-# LANGUAGE TemplateHaskell #-}

module Views.Table where

import           Control.Lens       hiding (view)

import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Map
import           Data.Maybe         (fromMaybe)
import           Data.Monoid        ((<>))

import           React.Flux

import           Lib.Model
import           Lib.Model.Auth     (SessionKey)
import           Lib.Model.Cell     (CellContent)
import           Lib.Model.Column
import           Lib.Model.Project  (Project)
import           Lib.Model.Record   (Record)
import           Lib.Model.Table
import           Lib.Types

import           Action             (Action (TableAddColumn, TableAddRecord))
import           Store              (Coords (..), dispatch)
import           Views.Cell
import           Views.Column
import           Views.Combinators
import           Views.Foreign
import           Views.Record
import           Views.ReportCell

data TableGridProps = TableGridProps
  { _cells      :: Map Coords CellContent
  , _colByIndex :: IntMap (Id Column, Column)
  , _recByIndex :: IntMap (Id Record, Record)
  , _tableId    :: Maybe (Id Table)
  , _projectId  :: Id Project
  , _sKey       :: SessionKey
  }

makeLenses ''TableGridProps

tableGrid_ :: TableGridProps -> ReactElementM eh ()
tableGrid_ !props = view tableGrid props mempty

tableGrid :: ReactView TableGridProps
tableGrid = defineView "tableGrid" $ \props -> do
  let numCols = IntMap.size $ props ^. colByIndex
      numRecs = IntMap.size $ props ^. recByIndex

      getRecord y =
        let Just r = IntMap.lookup y $ props ^. recByIndex
        in  uncurry Entity r

      getColumn x =
        let Just c = IntMap.lookup x $ props ^. colByIndex
        in  uncurry Entity c

      renderCell :: Int -> Int -> ReactElementM eh ()
      renderCell x y =
        case IntMap.lookup x $ props ^. colByIndex of
          Nothing -> mempty
          Just (c, col) -> case col ^. columnKind of
            ColumnReport rep ->
              case  IntMap.lookup y $ props ^. recByIndex of
                Just (r, _) -> reportCell_ $ ReportCellProps (props ^. sKey) c r rep
                Nothing     -> mempty
            ColumnData   dat -> do
              let mRC = do (r, _)  <- IntMap.lookup y $ props ^. recByIndex
                           content <- Map.lookup (Coords c r) $ props ^. cells
                           pure (r, content)
              case mRC of
                Just (r, content) -> dataCell_ $ DataCellProps c r dat content
                Nothing           -> mempty

      renderer (GridRenderArgs x y _)
        | x == 0 && y == 0 = cldiv_ "origin" mempty
        | x == 0 && y == (numRecs + 1) = cldiv_ "record-new" $
            faButton_ "plus-circle" $ dispatch TableAddRecord
        | x == 0 && 0 < y && y <= numRecs = cldiv_ "record" $
            record_ $ getRecord (y - 1)
        | y == 0 && x == (numCols + 1) = cldiv_ "column-new" $ do
            faButton_ "plus-circle" $ fromMaybe [] $
              dispatch . TableAddColumn . emptyDataCol <$> props ^. tableId
            faButton_ "bars" $ fromMaybe [] $
              dispatch . TableAddColumn . emptyReportCol <$> props ^. tableId
        | y == 0 && 0 < x && x <= numCols =
            column_ (props ^. projectId) (props ^. sKey) (getColumn (x - 1))
        | 0 < x && x <= numCols && 0 < y && y <= numRecs = cldiv_ "cell" $
            renderCell (x - 1) (y - 1)
        | otherwise = cldiv_ "empty" mempty

  grid_ GridProps
        { gridCellRenderer = defineView "cellRenderer" renderer
        , gridColumnWidths = [37] <> replicate numCols 230 <> [37]
        , gridColumnCount = numCols + 2
        , gridRowHeights = [54] <> replicate numRecs 37 <> [37]
        , gridRowCount = numRecs + 2
        }
