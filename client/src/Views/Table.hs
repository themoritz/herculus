{-# LANGUAGE TemplateHaskell #-}

module Views.Table where

import           Control.Lens       hiding (view)

import           Control.Monad      (when)
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import           Data.Map.Strict    (Map)
import qualified Data.Map.Strict    as Map
import           Data.Monoid        ((<>))

import           React.Flux

import           Lib.Model
import           Lib.Model.Auth     (SessionKey)
import           Lib.Model.Cell     (CellContent)
import           Lib.Model.Column
import           Lib.Model.Project  (ProjectClient)
import           Lib.Model.Row      (Row)
import           Lib.Model.Table
import           Lib.Types

import           Action             (Action (ProjectAction))
import qualified Project
import           Store              (dispatch)
import           Views.Cell
import           Views.Column
import           Views.Combinators
import           Views.Foreign
import           Views.ReportCell
import           Views.Row

data TableGridProps = TableGridProps
  { _cells            :: Map Project.Coords CellContent
  , _colByIndex       :: IntMap (Id Column, Column)
  , _recByIndex       :: IntMap (Id Row, Row)
  , _tableId          :: Maybe (Id Table)
  , _projectId        :: Id ProjectClient
  , _showNewColDialog :: Bool
  , _sKey             :: SessionKey
  , _tables           :: TableNames
  }

makeLenses ''TableGridProps

tableGrid_ :: TableGridProps -> ReactElementM eh ()
tableGrid_ !props = view tableGrid props mempty

tableGrid :: ReactView TableGridProps
tableGrid = defineView "tableGrid" $ \props -> do
  let numCols = IntMap.size $ props ^. colByIndex
      numRecs = IntMap.size $ props ^. recByIndex

      getRow y =
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
                           content <- Map.lookup (Project.Coords c r) $ props ^. cells
                           pure (r, content)
              case mRC of
                Just (r, content) -> dataCell_ $ DataCellProps c r dat content
                Nothing           -> mempty

      renderer (GridRenderArgs x y _)
        | x == 0 && y == 0 = cldiv_ "origin" mempty
        | x == 0 && y == (numRecs + 1) = cldiv_ "record-new" $
            faButton_ "plus-circle" $ dispatch $ ProjectAction Project.TableAddRow
        | x == 0 && 0 < y && y <= numRecs = cldiv_ "record" $
            row_ $ getRow (y - 1)
        | y == 0 && x == (numCols + 1) = cldiv_ "column-new" $ do
            faButton_ "plus-circle" $ dispatch $ ProjectAction Project.TableToggleNewColumnDialog
            when (props ^. showNewColDialog) $ cldiv_ "small-menu" $ do
              menuItem_ "columns" "New data column" $
                dispatch $ ProjectAction Project.TableCreateDataCol
              menuItem_ "file-text" "New report column" $
                dispatch $ ProjectAction Project.TableCreateReportCol
        | y == 0 && 0 < x && x <= numCols =
            column_ (props ^. projectId) (props ^. sKey) (props ^. tables) (getColumn (x - 1))
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
