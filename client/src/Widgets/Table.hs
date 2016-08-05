{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}

module Widgets.Table
  ( table
  ) where

import Control.Lens
import Control.Monad (void)

import Data.Map (Map)
import qualified Data.Map as Map

import Reflex.Dom hiding (Value)

import Lib.Types
import Lib.Model
import Lib.Model.Types
import Lib.Model.Column
import Lib.Model.Cell

import Api.Rest (loader', loader, api)
import qualified Api.Rest as Api
import Misc

import Widgets.Column
import Widgets.Record
import Widgets.Cell
import Widgets.Canvas


data Coords = Coords (Id Column) (Id Record)
  deriving (Eq, Ord, Show)

data Position = Position Int Int
  deriving (Eq, Ord)

data CellInfo = CellInfo
  { ciPos :: Position
  , ciCol :: Column
  , ciContent :: CellContent
  } deriving (Eq)

data State = State
  { _stateTableId :: Maybe (Id Table)
  , _stateCells   :: Map Coords CellContent
  , _stateColumns :: Map (Id Column) Column
  , _stateRecords :: Map (Id Record) Record
  }

makeLenses ''State

emptyState :: State
emptyState = State Nothing Map.empty Map.empty Map.empty

data Action
  = SetColumns [Entity Column]
  | SetRecords [Entity Record]
  | SetCells [(Id Column, Id Record, CellContent)]
  | UpdateCells [(Id Column, Id Record, CellContent)]
  | UpdateColumns [Entity Column]
  | AddColumn (Id Column)
  | AddRecord (Id Record)
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
      Just t  -> st & stateColumns %~ Map.insert i
        (Column t "" DataString ColumnInput "" CompileResultNone)
    AddRecord i -> case st ^. stateTableId of
      Nothing -> st
      Just t  -> st & stateRecords %~ Map.insert i (Record t)
    UpdateColumns entries -> st & stateColumns %~ \cols ->
      foldl (\cols' (Entity i c) -> Map.insert i c cols') cols entries
    SetTableId tblId -> st & stateTableId .~ Just tblId
  where
    fillEntries entries m = foldr (\(c, v) -> Map.insert c v) m $
        map (\(colId, recId, val) -> ((Coords colId recId), val)) entries

toCellGrid :: State -> Map Coords CellInfo
toCellGrid (State _ cells columns records) =
  let indexedCols = map (\((i, c), p) -> (i, (c, p))) $ zip (Map.toList columns) [0..]
      indexedRows = zip (Map.keys records) [0..]
      colMap = Map.fromList indexedCols
      rowMap = Map.fromList indexedRows
      emptyGrid = Map.fromList [ (Coords c r, CellInfo (Position x y) col CellNothing)
                               | (c, (col, x)) <- indexedCols
                               , (r, y) <- indexedRows
                               ]
      go (Coords colId recId, val) m =
        let may = do
              (col, x) <- Map.lookup colId colMap
              y <- Map.lookup recId rowMap
              pure (Position x y, col)
        in case may of
          Just (pos, col) -> Map.insert (Coords colId recId) (CellInfo pos col val) m
          Nothing         -> m
  in foldr go emptyGrid $ Map.toList cells

toColumns :: State -> Map (Int, Id Column, Maybe (Id Table)) Column
toColumns (State tblId _ columns _) =
  let indexedCols = zip (Map.toList columns) [0..]
  in Map.fromList $ map (\((colId, col), i) -> ((i, colId, tblId), col)) indexedCols

toRecords :: State -> Map (Int, Id Record, Maybe (Id Table)) Record
toRecords (State tblId _ _ records) =
  let indexedRecs = zip (Map.toList records) [0..]
  in Map.fromList $ map (\((recId, reco), i) -> ((i, recId, tblId), reco)) indexedRecs

table :: MonadWidget t m
      => Event t (Id Table)
      -> Event t [(Id Column, Id Record, CellContent)]
      -> Event t [Entity Column]
      -> m ()
table loadTable updateCells updateColumns = el "div" $ divClass "canvas" $ mdo

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
    , (\((Coords c r), val) -> UpdateCells [(c, r, CellValue val)]) <$> cellChanged
    , UpdateCells <$> updateCells
    , UpdateColumns <$> updateColumns
    , AddColumn   <$> newColId
    , AddRecord   <$> newRecId
    , (\((_, i, _), c) -> UpdateColumns [Entity i c]) <$> colChanged
    , SetTableId  <$> loadTable
    ]

  let colHeight = 200
      cellWidth = 200
      cellHeight = 60
      recWidth = 50

  columns <- mapDyn toColumns state
  records <- mapDyn toRecords state
  cells <- mapDyn toCellGrid state

  -- Columns
  colChanged <- (>>= dynMapEvents) $ listWithKeyNoHold columns $ \(i, columnId, mTblId) initial valE ->
    rectangle (Rectangle (i * cellWidth + recWidth) 0 cellWidth colHeight) $
      case mTblId of
        Nothing -> pure never
        Just tblId -> column tblId columnId $ ColumnConfig valE initial

  addColRect <- forDyn columns $ \cols ->
    Rectangle ((Map.size cols) * cellWidth + recWidth) 0 cellWidth colHeight
  addCol <- rectangleDyn addColRect $ button "+"

  -- Records
  void $ listWithKeyNoHold records $ \(i, recordId, _) _ _ ->
    rectangle (Rectangle 0 (i * cellHeight + colHeight) recWidth cellHeight) $
      record recordId

  addRecRect <- forDyn records $ \recs ->
    Rectangle 0 ((Map.size recs) * cellHeight + colHeight) recWidth cellHeight
  addRec <- rectangleDyn addRecRect $ button "+"

  -- Cells
  cellChanged <- (>>= dynMapEvents) $ listWithKeyEq cells $ \(Coords colId recId) cellInfo -> do
    pos <- mapDyn ciPos cellInfo
    colD <- mapDyn ciCol cellInfo
    content <- mapDyn ciContent cellInfo
    (Position x y) <- sample $ current pos
    rectangle (Rectangle (x * cellWidth + recWidth)
                         (y * cellHeight + colHeight)
                         cellWidth
                         cellHeight) $
      switchEvent $ dynWidget colD $ \col ->
        cell colId recId (CellConfig content col)

  colArg <- hold (Left "") $ (\(Coords c _, _) -> Right c) <$> cellChanged
  recArg <- hold (Left "") $ (\(Coords _ r, _) -> Right r) <$> cellChanged
  valArg <- hold (Left "") $ (\(Coords _ _, v) -> Right v) <$> cellChanged
  loader' (Api.cellSet api colArg recArg valArg) (() <$ cellChanged)

  newColId <- loader (Api.columnCreate api tableIdArg) addCol
  newRecId <- loader (Api.recordCreate api tableIdArg) addRec

  pure ()
