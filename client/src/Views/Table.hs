module Views.Table where

import           Control.Lens      hiding (view)

import qualified Data.Map.Strict   as Map
import           Data.Maybe        (fromMaybe)
import           Data.Monoid       ((<>))

import           React.Flux

import           Lib.Model
import           Lib.Model.Column
import           Lib.Model.Table
import           Lib.Types

import           Store
import           Views.Cell
import           Views.Column
import           Views.Combinators
import           Views.Foreign
import           Views.Record
import           Views.ReportCell

tableGrid_ :: State -> ReactElementM eh ()
tableGrid_ !st = view tableGrid st mempty

tableGrid :: ReactView State
tableGrid = defineView "tableGrid" $ \st -> do
  let cells = st ^. stateCells
      cols = st ^. stateColumns
      recs = st ^. stateRecords
      tableId = st ^. stateTableId

      numCols = Map.size cols
      numRecs = Map.size recs

      colByIndex = Map.fromList $ zip [0..] (Map.toList cols)
      recByIndex = Map.fromList $ zip [0..] (Map.toList recs)

      getRecord y = let Just r = Map.lookup y recByIndex in uncurry Entity r
      getColumn x = let Just c = Map.lookup x colByIndex in uncurry Entity c

      renderCell :: Int -> Int -> ReactElementM eh ()
      renderCell x y =
        case Map.lookup x colByIndex of
          Nothing -> mempty
          Just (c, col) -> case col ^. columnKind of
            ColumnReport rep ->
              case  Map.lookup y recByIndex of
                Just (r, _) -> reportCell_ $ ReportCellProps c r rep
                Nothing     -> mempty
            ColumnData   dat -> do
              let mRC = do (r, _)  <- Map.lookup y recByIndex
                           content <- Map.lookup (Coords c r) cells
                           pure (r, content)
              case mRC of
                Just (r, content) -> dataCell_ $ DataCellProps c r dat content
                Nothing           -> mempty

      emptyDataCol :: Id Table -> Column
      emptyDataCol tableId = Column
        { _columnTableId = tableId
        , _columnName    = ""
        , _columnKind    = ColumnData DataCol
          { _dataColType          = DataNumber
          , _dataColIsDerived     = NotDerived
          , _dataColSourceCode    = ""
          , _dataColCompileResult = CompileResultNone
          }
        }

      emptyReportCol :: Id Table -> Column
      emptyReportCol tableId = Column
        { _columnTableId = tableId
        , _columnName = ""
        , _columnKind = ColumnReport ReportCol
          { _reportColTemplate         = ""
          , _reportColCompiledTemplate = CompileResultNone
          , _reportColLanguage         = Just ReportLanguageMarkdown
          , _reportColFormat           = ReportFormatPDF
          }
        }

      renderer (GridRenderArgs x y _)
        | x == 0 && y == 0 = cldiv_ "origin" mempty
        | x == 0 && y == (numRecs + 1) = cldiv_ "record-new" $
            faButton_ "plus-circle" $ dispatch TableAddRecord
        | x == 0 && 0 < y && y <= numRecs = cldiv_ "record" $
            record_ $ getRecord (y - 1)
        | y == 0 && x == (numCols + 1) = cldiv_ "column-new" $ do
            faButton_ "plus-circle" $ fromMaybe [] $
              dispatch . TableAddColumn . emptyDataCol <$> tableId
            faButton_ "bars" $ fromMaybe [] $
              dispatch . TableAddColumn . emptyReportCol <$> tableId
        | y == 0 && 0 < x && x <= numCols =
            column_ (st ^. stateSessionKey) (getColumn (x - 1))
        | 0 < x && x <= numCols && 0 < y && y <= numRecs = cldiv_ "cell" $
            renderCell (x - 1) (y - 1)
        | otherwise = cldiv_ "empty" mempty

      props = GridProps
        { gridCellRenderer = defineView "cellRenderer" renderer
        , gridColumnWidths = [37] <> replicate numCols 230 <> [37]
        , gridColumnCount = numCols + 2
        , gridRowHeights = [54] <> replicate numRecs 37 <> [37]
        , gridRowCount = numRecs + 2
        }

  if not . null $ st ^. stateTables
  then grid_ props
  else cldiv_ "" mempty
