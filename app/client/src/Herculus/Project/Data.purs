module Herculus.Project.Data where

import Herculus.Prelude
import Data.Map as Map
import Data.Generic (gCompare, gEq)
import Data.Lens (Lens', _1, _2, _Just, lens, use, (.=))
import Data.Lens.At (at)
import Data.Map (Map)
import Data.Maybe.First (First(..))
import Herculus.Utils (nonEmpty)
import Lib.Api.Schema.Column (Column, columnId, columnTableId)
import Lib.Custom (ColumnTag, Id)
import Lib.Model (Entity(..))
import Lib.Model.Cell (Cell, CellContent, cellColumnId, cellContent, cellRowId, cellTableId)
import Lib.Model.Row (Row, rowTableId)
import Lib.Model.Table (Table)
import Lib.Types (ChangeOp(..))

type Diff a = Array (Tuple ChangeOp a)

data Coords = Coords (Id ColumnTag) (Id Row)
derive instance genericCoords :: Generic Coords
instance eqCoords :: Eq Coords where eq = gEq
instance ordCoords :: Ord Coords where compare = gCompare

type RowCache = Map (Id Table) (Map (Id Row) (Map (Id ColumnTag) (Tuple Column CellContent)))

--------------------------------------------------------------------------------

type TableDesc =
  { _descTable :: Table
  , _descColumns :: Map (Id ColumnTag) Column
  , _descRows :: Map (Id Row) Row
  }

mkTableDesc :: Table -> TableDesc
mkTableDesc table =
  { _descTable: table
  , _descColumns: Map.empty
  , _descRows: Map.empty
  }

descTable :: Lens' TableDesc Table
descTable = lens _._descTable _{ _descTable = _ }

descColumns :: Lens' TableDesc (Map (Id ColumnTag) Column)
descColumns = lens _._descColumns _{ _descColumns = _ }

descRows :: Lens' TableDesc (Map (Id Row) Row)
descRows = lens _._descRows _{ _descRows = _ }

--------------------------------------------------------------------------------

type ProjectData =
  { _pdCells :: Map Coords CellContent
  , _pdRowCache :: RowCache
  , _pdTables :: Map (Id Table) TableDesc
  }

mkProjectData :: ProjectData
mkProjectData =
  { _pdCells: Map.empty
  , _pdRowCache: Map.empty
  , _pdTables: Map.empty
  }

pdCells :: Lens' ProjectData (Map Coords CellContent)
pdCells = lens _._pdCells _{ _pdCells = _ }

pdRowCache :: Lens' ProjectData RowCache
pdRowCache = lens _._pdRowCache _{ _pdRowCache = _ }

pdTables :: Lens' ProjectData (Map (Id Table) TableDesc)
pdTables = lens _._pdTables _{ _pdTables = _ }

--------------------------------------------------------------------------------

prepare
  :: forall m
   . MonadState ProjectData m
  => Array (Entity Table)
  -> Array Column
  -> Array (Entity Row)
  -> Array (Entity Cell)
  -> m Unit
prepare tables columns rows cells = do
  -- For every table, initialize the table description
  for_ tables $ \(Entity t) ->
    pdTables <<< at t.entityId .= Just (mkTableDesc t.entityVal)
  -- Initialize the cell by coords query
  for_ cells $ \(Entity c) -> do
    let cell = c.entityVal
        columnId = cell ^. cellColumnId
        rowId = cell ^. cellRowId
    pdCells <<< at (Coords columnId rowId) .= Just (cell ^. cellContent)
  for_ rows $ \(Entity r) -> do
    let row = r.entityVal
        rowId = r.entityId
        tableId = row ^. rowTableId
    -- Add the row to the table description
    pdTables <<< at tableId <<< _Just <<< descRows <<< at rowId .= Just row
    -- For every column with the same tableId, set the content of the
    -- row cache.
    for_ columns $ \column -> do
      let colId = column ^. columnId
      when (tableId == column ^. columnTableId) $
        use (pdCells <<< at (Coords colId rowId)) >>= case _ of
          Nothing -> pure unit
          Just content ->
            pdRowCache <<< at tableId <<< nonEmpty
                       <<< at rowId <<< nonEmpty
                       <<< at colId .= Just (Tuple column content)
    -- Add every column to the description of its corresponding table
  for_ columns $ \column -> do
    let tableId = column ^. columnTableId
    pdTables <<< at tableId <<< _Just
             <<< descColumns <<< at (column ^. columnId) .= Just column

applyDiff
  :: forall m
   . MonadState ProjectData m => MonadWriter (First (Id Table)) m
  => Maybe (Id Table)
  -> Diff (Entity Table)
  -> Diff Column
  -> Diff (Entity Row)
  -> Diff (Entity Cell)
  -> m Unit
applyDiff currentTable tableDiff columnDiff rowDiff cellDiff = do
  -- Tables
  for_ tableDiff $ \(Tuple op (Entity t)) -> do
    let tableId = t.entityId
        table = t.entityVal
        tableLens = pdTables <<< at tableId
        rowCacheLens = pdRowCache <<< at tableId
    case op of
      Create -> do
        tableLens .= Just (mkTableDesc table)
        rowCacheLens .= Just Map.empty
        -- Go to new table if currently viewing none
        case currentTable of
          Just _  -> pure unit
          Nothing -> tell $ First $ Just tableId
      Update -> tableLens <<< _Just <<< descTable .= table
      Delete -> do
        tableLens .= Nothing
        rowCacheLens .= Nothing
        -- Select another table if available
        tables <- use pdTables
        let mNextTableId = Map.lookupLT tableId tables
                       <|> Map.lookupGT tableId tables
        case mNextTableId of
          Nothing ->
            tell $ First Nothing
          Just next ->
            tell $ First $ Just next.key
  -- Cells
  for_ cellDiff $ \(Tuple op (Entity c)) -> do
    let cell = c.entityVal
        cellId = c.entityId
        columnId = cell ^. cellColumnId
        rowId = cell ^. cellRowId
        tableId = cell ^. cellTableId
        coords = Coords columnId rowId
        content = cell ^. cellContent
        cellLens = pdCells <<< at coords
    case op of
      Create -> cellLens .= Just content
      Update -> do
        cellLens .= Just content
        -- Update of the row cache when cells are created / deleted is
        -- handled by the rows / columns
        pdRowCache <<< at tableId <<< _Just
                   <<< at rowId <<< _Just
                   <<< at columnId <<< _Just
                   <<< _2 .= content
      Delete -> cellLens .= Nothing
  -- Rows
  for_ rowDiff $ \(Tuple op (Entity r)) -> do
    let row = r.entityVal
        rowId = r.entityId
        tableId = row ^. rowTableId
        tablesLens = pdTables <<< at tableId <<< _Just <<< descRows <<< at rowId
        rowCacheLens = pdRowCache <<< at tableId <<< nonEmpty <<< at rowId
    case op of
      Create -> do
        tablesLens .= Just row
        columns <- use (pdTables <<< at tableId <<< _Just <<< descColumns)
        let
          arr :: Array (Tuple (Id ColumnTag) Column)
          arr = Map.toUnfoldable columns
        for_ arr $ \(Tuple columnId column) ->
          use (pdCells <<< at (Coords columnId rowId)) >>= case _ of
            Nothing -> pure unit
            Just content ->
              rowCacheLens <<< nonEmpty <<< at columnId .=
                Just (Tuple column content)
      Update -> tablesLens .= Just row
      Delete -> do
        tablesLens .= Nothing
        rowCacheLens .= Nothing
  -- Columns
  for_ columnDiff $ \(Tuple op column) -> do
    let colId = column ^. columnId
        tableId = column ^. columnTableId
        tablesLens = pdTables <<< at tableId <<< _Just
                              <<< descColumns <<< at colId
        rowCacheLens r = pdRowCache <<< at tableId <<< nonEmpty
                                    <<< at r <<< nonEmpty <<< at colId
    rows <- use (pdTables <<< at tableId <<< _Just <<< descRows)
    case op of
      Create -> do
        tablesLens .= Just column
        for_ (Map.keys rows) $ \rowId ->
          use (pdCells <<< at (Coords colId rowId)) >>= case _ of
            Nothing -> pure unit
            Just content ->
              rowCacheLens rowId .= Just (Tuple column content)
      Update -> do
        tablesLens .= Just column
        for_ (Map.keys rows) $ \rowId ->
          rowCacheLens rowId <<< _Just <<< _1 .= column
      Delete -> do
        tablesLens .= Nothing
        for_ (Map.keys rows) $ \rowId ->
          rowCacheLens rowId .= Nothing
