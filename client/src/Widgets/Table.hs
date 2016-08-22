{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Widgets.Table
  ( table
  ) where

import Control.Lens

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List.NonEmpty (NonEmpty)
import Data.Foldable (foldl')

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
  { _stateTableId :: !(Maybe (Id Table))
  , _stateCells   :: !(Map Coords CellContent)
  , _stateColumns :: !(Map (Id Column) Column)
  , _stateRecords :: !(Map (Id Record) Record)
  }

makeLenses ''State

emptyState :: State
emptyState = State Nothing Map.empty Map.empty Map.empty

data Action
  -- Initial
  = SetColumns [Entity Column]
  | SetRecords [Entity Record]
  | SetCells [(Id Column, Id Record, CellContent)]
  --
  | UpdateCells [(Id Column, Id Record, CellContent)]
  | UpdateColumns [Entity Column]
  -- Column
  | AddColumn (Id Column)
  | DeleteColumn (Id Column)
  -- Record
  | AddRecord (Id Record)
  | DeleteRecord (Id Record)
  -- Misc
  | SetTableId (Id Table)

update :: NonEmpty Action -> State -> State
update actions state = foldl' go state actions
  where
    go st action = case action of
      SetColumns cols -> st & stateColumns .~
        (Map.fromList $ map (\(Entity i c) -> (i, c)) cols)
      SetRecords recs -> st & stateRecords .~
        (Map.fromList $ map (\(Entity i r) -> (i, r)) recs)
      SetCells entries -> st & stateCells .~ fillEntries entries Map.empty

      UpdateCells entries -> st & stateCells %~ fillEntries entries
      UpdateColumns entries -> st & stateColumns %~ \cols ->
        foldl' (\cols' (Entity i c) -> Map.insert i c cols') cols $
        filter (\(Entity _ c) -> st ^. stateTableId == Just (columnTableId c)) $
        entries

      AddColumn i -> case st ^. stateTableId of
        Nothing -> st
        Just t  -> st & stateColumns %~ Map.insert i
          (Column t "" DataString ColumnInput "" CompileResultNone)
      DeleteColumn i -> st & stateColumns %~ Map.delete i
                           & stateCells %~ Map.filterWithKey
                                             (\(Coords c _) _ -> c /= i)

      AddRecord i -> case st ^. stateTableId of
        Nothing -> st
        Just t  -> st & stateRecords %~ Map.insert i (Record t)
      DeleteRecord i -> st & stateRecords %~ Map.delete i
                           & stateCells %~ Map.filterWithKey
                                             (\(Coords _ r) _ -> r /= i)

      SetTableId tblId -> st & stateTableId .~ Just tblId
    fillEntries entries m = foldl' (\m' (c, v) -> Map.insert c v m') m $
        map (\(colId, recId, val) -> ((Coords colId recId), val)) entries

toCellGrid :: State -> Map Coords CellInfo
toCellGrid (State _ cells columns records) =
  let indexedCols = map (\((i, c), p) -> (i, (c, p))) $ zip (Map.toList columns) [0..]
      indexedRows = zip (Map.keys records) [0..]
      colMap = Map.fromList indexedCols
      rowMap = Map.fromList indexedRows
      go m (Coords colId recId) val =
        let may = do
              (col, x) <- Map.lookup colId colMap
              y <- Map.lookup recId rowMap
              pure (Position x y, col)
        in case may of
          Just (pos, col) -> Map.insert (Coords colId recId) (CellInfo pos col val) m
          Nothing         -> m
  in Map.foldlWithKey' go Map.empty cells

toColumns :: State -> Map (Id Column) (Int, Column, Maybe (Id Table))
toColumns (State tblId _ columns _) =
  let indexedCols = zip (Map.toList columns) [0..]
  in Map.fromList $ map (\((colId, col), i) -> (colId, (i, col, tblId))) indexedCols

toRecords :: State -> Map (Id Record) (Int, Record)
toRecords (State _ _ _ records) =
  let indexedRecs = zip (Map.toList records) [0..]
  in Map.fromList $ map (\((recId, reco), i) -> (recId, (i, reco))) indexedRecs

toCellUpdate :: Entity Cell -> (Id Column, Id Record, CellContent)
toCellUpdate (Entity _ (Cell content (Aspects _ c r))) = (c, r, content)

table :: MonadWidget t m
      => Event t (Id Table)
      -> Event t [(Id Column, Id Record, CellContent)]
      -> Event t [Entity Column]
      -> m ()
table loadTable updateCells updateColumns = el "div" $ divClass "canvas" $ mdo

  tableIdArg <- holdDyn (Left "") (Right <$> loadTable)

  columnsRes <- loader (Api.columnList api tableIdArg) $
                       () <$ loadTable
  recordsRes <- loader (Api.recordList api tableIdArg) $
                       () <$ loadTable
  dataRes <- loader (Api.tableData api tableIdArg) $
                    () <$ loadTable

  state <- foldDyn update emptyState $ mergeList
    [ SetColumns  <$> columnsRes
    , SetRecords  <$> recordsRes
    , SetCells    <$> dataRes
    , (\((Coords c r), val) -> UpdateCells [(c, r, CellValue val)]) <$> cellChanged
    , UpdateCells <$> updateCells
    , UpdateColumns <$> updateColumns
    , (\(i, c) -> UpdateColumns [Entity i c]) <$> colChanged
    , AddColumn . fst <$> addColResult
    , UpdateCells . map toCellUpdate . snd <$> addColResult
    , DeleteColumn . fst <$> deleteColumn
    , AddRecord . fst <$> addRecResult
    , UpdateCells . map toCellUpdate . snd <$> addRecResult
    , DeleteRecord . fst <$> deleteRecord
    , SetTableId  <$> loadTable
    ]

  let colHeight = 300
      cellWidth = 300
      cellHeight = 150
      recWidth = 50

  let columns = toColumns <$> state
      records = toRecords <$> state
      cells = toCellGrid <$> state

  -- Columns
  colEvents <- listWithKey columns $ \colId colInfo -> do
    let rect = ffor colInfo $ \(i, _, _) ->
          Rectangle (i * cellWidth + recWidth) 0 cellWidth colHeight
        columnD = (\(_, c, _) -> c) <$> colInfo
        tableD = (\(_, _, t) -> t) <$> colInfo
    mTblId <- sample $ current tableD
    rectangleDyn rect $ case mTblId of
      Nothing -> pure (never, never)
      Just tblId -> column tblId colId columnD
  let colChanged = dynMapEventsWith fst colEvents
      deleteColumn = dynMapEventsWith snd colEvents
  delColArg <- holdDyn (Left "") (Right . fst <$> deleteColumn)
  loader' (Api.columnDelete api delColArg) (() <$ deleteColumn)

  let addColRect = ffor columns $ \cols ->
        Rectangle ((Map.size cols) * cellWidth + recWidth) 0 cellWidth colHeight
  addCol <- rectangleDyn addColRect $ button "+"

  -- Records
  deleteRecord <- fmap dynMapEvents <$> listWithKey records $ \recId recInfo -> do
    let rect = ffor recInfo $ \(i, _) ->
          Rectangle 0 (i * cellHeight + colHeight) recWidth cellHeight
    rectangleDyn rect $ record recId
  delRecArg <- holdDyn (Left "") (Right . fst <$> deleteRecord)
  loader' (Api.recordDelete api delRecArg) (() <$ deleteRecord)

  let addRecRect = ffor records $ \recs ->
        Rectangle 0 ((Map.size recs) * cellHeight + colHeight) recWidth cellHeight
  addRec <- rectangleDyn addRecRect $ button "+"

  -- Cells
  cellChanged <- (fmap dynMapEvents) <$> listWithKeyEq cells $ \(Coords colId recId) cellInfo -> do
    let rect = ffor cellInfo $ \ci ->
          let (Position x y) = ciPos ci
          in Rectangle (x * cellWidth + recWidth)
                       (y * cellHeight + colHeight)
                       cellWidth
                       cellHeight
        colD = ciCol <$> cellInfo
        content = ciContent <$> cellInfo
    rectangleDyn rect $
      switchEvent $ dynWidget colD $ \col ->
        cell colId recId (CellConfig content col)

  colArg <- holdDyn (Left "") $ (\(Coords c _, _) -> Right c) <$> cellChanged
  recArg <- holdDyn (Left "") $ (\(Coords _ r, _) -> Right r) <$> cellChanged
  valArg <- holdDyn (Left "") $ (\(Coords _ _, v) -> Right v) <$> cellChanged
  loader' (Api.cellSet api colArg recArg valArg) (() <$ cellChanged)

  addColResult <- loader (Api.columnCreate api tableIdArg) addCol
  addRecResult <- loader (Api.recordCreate api tableIdArg) addRec

  pure ()
