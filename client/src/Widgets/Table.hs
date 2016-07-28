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
  , _stateColumns :: Map (Id Column) (Text, ColumnType)
  , _stateRecords :: Map (Id Record) ()
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
  | SetTableId (Id Table)

update :: Action -> State -> State
update action st = case action of
    SetColumns cols -> st & stateColumns .~
      (Map.fromList $ map (\(Entity i (Column _ n t)) -> (i, (n, t))) cols)
    SetRecords recs -> st & stateRecords .~
      (Map.fromList $ map (\(Entity i _) -> (i, ())) recs)
    SetCells entries -> st & stateCells .~ fillEntries entries Map.empty
    UpdateCells entries -> st & stateCells %~ fillEntries entries
    AddColumn i -> st & stateColumns %~ Map.insert i ("", ColumnInput DataString)
    AddRecord i -> st & stateRecords %~ Map.insert i ()
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

toColumns :: State -> Map (Int, Id Column) (Text, ColumnType)
toColumns (State _ _ columns _) =
  let indexedCols = zip (Map.toList columns) [0..]
  in Map.fromList $ map (\((colId, col), i) -> ((i, colId), col)) indexedCols

toRecords :: State -> Map (Int, Id Record) ()
toRecords (State _ _ _ records) =
  let indexedRecs = zip (Map.toList records) [0..]
  in Map.fromList $ map (\((recId, reco), i) -> ((i, recId), reco)) indexedRecs

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
    , UpdateCells <$> cellChanged
    , AddColumn   <$> newColId
    , AddRecord   <$> newRecId
    , SetTableId  <$> loadTable
    ]
  let colHeight = 100
      cellWidth = 200
      cellHeight = 50
      recWidth = 50
  columns <- mapDyn toColumns state
  records <- mapDyn toRecords state
  cells <- mapDyn toCellGrid state
  (addCol, addRec, cellChanged) <- divClass "canvas" $ do
    -- Columns
    _ <- listWithKeyNoHold columns $ \(i, columnId) initial valE ->
      rectangle (Rectangle (i * cellWidth + recWidth) 0 cellWidth colHeight) $
        column columnId $ ColumnConfig
          { _columnConfig_setType = snd <$> valE
          , _columnConfig_setName = fst <$> valE
          , _columnConfig_initialType = snd initial
          , _columnConfig_initialName = fst initial
          }
    addColRect <- forDyn columns $ \cols ->
      Rectangle ((Map.size cols) * cellWidth + recWidth) 0 cellWidth colHeight
    addCol <- rectangleDyn addColRect $ button "+"
    -- Records
    _ <- listWithKeyNoHold records $ \(i, recordId) _ _ ->
      rectangle (Rectangle 0 (i * cellHeight + colHeight) recWidth cellHeight) $
        record recordId
    addRecRect <- forDyn records $ \recs ->
      Rectangle 0 ((Map.size recs) * cellHeight + colHeight) recWidth cellHeight
    addRec <- rectangleDyn addRecRect $ button "+"
    -- Cells
    dynMap <- listWithKeyNoHold cells $ \(Position x y, Coords colId recId, mTblId) valInit valE ->
      rectangle (Rectangle (x * cellWidth + recWidth)
                           (y * cellHeight + colHeight)
                           cellWidth
                           cellHeight) $
        case mTblId of
          Nothing -> pure never
          Just tblId -> cell tblId colId recId (CellConfig valE valInit)
    dynEvent <- mapDyn (leftmost . map (\(k, e) -> (\ex -> (k, ex)) <$> e) . Map.toList) dynMap
    let cellChanged = ffor (switchPromptlyDyn dynEvent) $ \((_, Coords c r, _), v) -> [(c, r, v)]
    pure (addCol, addRec, cellChanged)
  newColId <- loader (Api.columnCreate api tableIdArg) addCol
  newRecId <- loader (Api.recordCreate api tableIdArg) addRec
  pure ()
