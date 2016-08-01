{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}

module Widgets.Table
  ( table
  ) where

import Control.Lens

import Data.Monoid
import           Data.Map        (Map)
import qualified Data.Map as Map
import Data.Text (Text)

import Reflex.Dom hiding (Value)

import Lib.Types
import Lib.Model
import Lib.Model.Types

import Api.Rest (loader, api)
import qualified Api.Rest as Api
import Misc

import Widgets.Column
import Widgets.Record
import Widgets.Cell
import Widgets.Canvas


data Coords = Coords
  { coordsCol :: Id Column
  , coordsRec :: Id Record
  } deriving (Eq, Ord)

data Position = Position
  { posX :: Int
  , posY :: Int
  } deriving (Eq, Ord)

data State = State
  { _stateTableId :: Maybe (Id Table)
  , _stateCells   :: Map Coords Value
  , _stateColumns :: Map (Id Column) Column
  , _stateRecords :: Map (Id Record) Record
  }

makeLenses ''State

emptyState :: State
emptyState = State Nothing Map.empty Map.empty Map.empty

data Action
  = SetColumns [Entity Column]
  | SetRecords [Entity Record]
  | SetCells [(Id Column, Id Record, Value)]
  | UpdateCells [(Id Column, Id Record, Value)]
  | AddColumn (Id Column)
  | AddRecord (Id Record)
  | UpdateColumn (Id Column) Column
  | SetTableId (Id Table)

update :: Action -> State -> State
update action st = case action of
    SetColumns cols -> st & stateColumns .~
      (Map.fromList $ map (\(Entity i c) -> (i, c)) cols)
    SetRecords recs -> st & stateRecords .~
      (Map.fromList $ map (\(Entity i r) -> (i, r)) recs)
    SetCells entries -> st & stateCells .~ fillEntries entries Map.empty
    UpdateCells entries -> st & stateCells %~ fillEntries entries
    AddColumn i -> case st ^. stateTableId of
      Nothing -> st
      Just t  -> st & stateColumns %~ Map.insert i (Column t "" DataString ColumnInput "")
    AddRecord i -> case st ^. stateTableId of
      Nothing -> st
      Just t  -> st & stateRecords %~ Map.insert i (Record t)
    UpdateColumn i c -> st & stateColumns %~ Map.insert i c
    SetTableId tblId -> st & stateTableId .~ Just tblId
  where
    fillEntries entries m = foldr (\(c, v) -> Map.insert c v) m $
        map (\(colId, recId, val) -> ((Coords colId recId), val)) entries

toCellGrid :: State -> Map (Position, Coords, Maybe (Id Table)) Value
toCellGrid (State tblId cells columns records) =
  let indexedCols = zip (Map.keys columns) [0..]
      indexedRows = zip (Map.keys records) [0..]
      colMap = Map.fromList indexedCols
      rowMap = Map.fromList indexedRows
      emptyGrid = Map.fromList [ ((Position x y, Coords col row, tblId), "")
                               | (col, x) <- indexedCols
                               , (row, y) <- indexedRows
                               ]
      set (Coords colId recId, val) m =
        case Position <$> Map.lookup colId colMap <*> Map.lookup recId rowMap of
          Just pos -> Map.insert (pos, Coords colId recId, tblId) val m
          Nothing  -> m
  in foldr set emptyGrid $ Map.toList cells

toColumns :: State -> Map (Int, Id Column, Maybe (Id Table)) Column
toColumns (State tblId _ columns _) =
  let indexedCols = zip (Map.toList columns) [0..]
  in Map.fromList $ map (\((colId, col), i) -> ((i, colId, tblId), col)) indexedCols

toRecords :: State -> Map (Int, Id Record, Maybe (Id Table)) Record
toRecords (State tblId _ _ records) =
  let indexedRecs = zip (Map.toList records) [0..]
  in Map.fromList $ map (\((recId, reco), i) -> ((i, recId, tblId), reco)) indexedRecs

table :: MonadWidget t m
      => Event t (Id Table) -> Event t [(Id Column, Id Record, Value)]-> m ()
table loadTable updateCells = el "div" $ mdo
  tableIdArg <- hold (Left "") (Right <$> loadTable)
  columnsRes <- loader (Api.columnList api tableIdArg) $
                       () <$ loadTable
  recordsRes <- loader (Api.recordList api tableIdArg) $
                       () <$ loadTable
  dataRes <- loader (Api.tableData api tableIdArg) $
                    () <$ loadTable
  state <- foldDyn update emptyState $ leftmost
    [ SetColumns  <$> columnsRes
    , SetRecords  <$> recordsRes
    , SetCells    <$> dataRes
    , UpdateCells <$> updateCells
    , (\((_, Coords c r, _), v) -> UpdateCells [(c, r, v)]) <$> cellChanged
    , AddColumn   <$> newColId
    , AddRecord   <$> newRecId
    , (\((_, i, _), c) -> UpdateColumn i c) <$> colChanged
    , SetTableId  <$> loadTable
    ]
  let colHeight = 100
      cellWidth = 200
      cellHeight = 50
      recWidth = 50
  columns <- mapDyn toColumns state
  records <- mapDyn toRecords state
  cells <- mapDyn toCellGrid state
  (addCol, colChanged, addRec, cellChanged) <- divClass "canvas" $ do
    -- Columns
    colChanged <- listWithKeyEvent columns $ \(i, columnId, mTblId) initial valE ->
      rectangle (Rectangle (i * cellWidth + recWidth) 0 cellWidth colHeight) $
        case mTblId of
          Nothing -> pure never
          Just tblId -> column tblId columnId $ ColumnConfig valE initial
    addColRect <- forDyn columns $ \cols ->
      Rectangle ((Map.size cols) * cellWidth + recWidth) 0 cellWidth colHeight
    addCol <- rectangleDyn addColRect $ button "+"
    -- Records
    _ <- listWithKeyNoHold records $ \(i, recordId, _) _ _ ->
      rectangle (Rectangle 0 (i * cellHeight + colHeight) recWidth cellHeight) $
        record recordId
    addRecRect <- forDyn records $ \recs ->
      Rectangle 0 ((Map.size recs) * cellHeight + colHeight) recWidth cellHeight
    addRec <- rectangleDyn addRecRect $ button "+"
    -- Cells
    cellChanged <- listWithKeyEvent cells $ \(Position x y, Coords colId recId, mTblId) valInit valE ->
      rectangle (Rectangle (x * cellWidth + recWidth)
                           (y * cellHeight + colHeight)
                           cellWidth
                           cellHeight) $
        case mTblId of
          Nothing -> pure never
          Just tblId -> cell tblId colId recId (CellConfig valE valInit)
    pure (addCol, colChanged, addRec, cellChanged)
  newColId <- loader (Api.columnCreate api tableIdArg) addCol
  newRecId <- loader (Api.recordCreate api tableIdArg) addRec
  pure ()
