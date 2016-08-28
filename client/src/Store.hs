{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Store where

import           Control.Lens
import Control.DeepSeq

import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Proxy
import           Data.Typeable (Typeable)
import           Data.Text (Text, pack)
import           Data.Foldable (foldl')

import           GHC.Generics

import           React.Flux
import           React.Flux.Addons.Servant

import           Lib.Model
import           Lib.Model.Types
import           Lib.Model.Column
import           Lib.Model.Cell
import           Lib.Types

import           Lib.Api.Rest
import           Lib.Api.WebSocket

import WebSocket

data Coords = Coords (Id Column) (Id Record)
  deriving (Eq, Ord, Show)

data CellInfo = CellInfo
  { ciCol     :: Column
  , ciContent :: CellContent
  } deriving (Eq)

data State = State
  { _stateError     :: Maybe Text
  , _stateWebSocket :: Maybe JSWebSocket
  , _stateProjects  :: [Entity Project]
  , _stateProjectId :: Maybe (Id Project)
  , _stateTables    :: [Entity Table]
  , _stateTableId   :: Maybe (Id Table)
  , _stateCells     :: Map Coords CellContent
  , _stateColumns   :: Map (Id Column) Column
  , _stateRecords   :: Map (Id Record) Record
  }

makeLenses ''State

store :: ReactStore State
store = mkStore $ State Nothing Nothing [] Nothing [] Nothing
  Map.empty Map.empty Map.empty

data Action
  -- Global
  = GlobalSetError Text
  | GlobalInit Text -- WebSocket URL
  | GlobalSendWebSocket WsUpMessage
  -- Projects
  | ProjectsSet [Entity Project]
  | ProjectsCreate Project
  | ProjectsAdd (Entity Project)
  | ProjectsLoadProject (Id Project)
  -- Tables
  | TablesSet [Entity Table]
  | TablesCreate Table
  | TablesAdd (Entity Table)
  | TablesLoadTable (Id Table)
  -- Table
  | TableSet ([Entity Column], [Entity Record], [(Id Column, Id Record, CellContent)])
  | TableUpdateCells [(Id Column, Id Record, CellContent)]
  | TableUpdateColumns [Entity Column]
  | TableAddColumn
  | TableAddColumnDone (Entity Column, [Entity Cell])
  | TableDeleteColumn (Id Column)
  | TableAddRecord
  | TableAddRecordDone (Entity Record, [Entity Cell])
  | TableDeleteRecord (Id Record)
  -- Column
  | ColumnRename (Id Column) Text
  | ColumnSetDt (Id Column) DataType
  | ColumnSetInput (Id Column) (InputType, Text)
  -- Cell
  | CellSetValue (Id Column) (Id Record) Value
  deriving (Typeable, Generic, NFData)

api :: ApiRequestConfig Routes
api = ApiRequestConfig "" NoTimeout

dispatch :: Action -> [SomeStoreAction]
dispatch a = [SomeStoreAction store a]

instance StoreData State where
  type StoreAction State = Action
  transform action st = case action of

      GlobalSetError msg -> pure $
        st & stateError .~ Just msg

      GlobalInit wsUrl -> do
        ws <- jsonWebSocketNew wsUrl $ \case
          WsDownCellsChanged cs -> pure $ dispatch $ TableUpdateCells cs
          WsDownColumnsChanged cs -> pure $ dispatch $ TableUpdateColumns cs
          WsDownGreet _ -> pure []
          WsDownList _ -> pure []
        request api (Proxy :: Proxy ProjectList) $ \case
          Left (_, e) -> pure $ dispatch $ GlobalSetError $ pack e
          Right ps -> pure $ dispatch $ ProjectsSet ps
        pure $ st & stateWebSocket .~ Just ws

      GlobalSendWebSocket msg -> do
        case st ^. stateWebSocket of
          Nothing -> pure ()
          Just ws -> jsonWebSocketSend msg ws
        pure st

      -- Projects

      ProjectsSet ps -> pure $
        st & stateProjects .~ ps

      ProjectsCreate p -> do
        request api (Proxy :: Proxy ProjectCreate) p $ \case
          Left (_, e) -> pure $ dispatch $ GlobalSetError $ pack e
          Right i -> pure $ dispatch $ ProjectsAdd (Entity i p)
        pure st

      ProjectsAdd p -> pure $
        st & stateProjects %~ (p:)

      ProjectsLoadProject i -> do
        request api (Proxy :: Proxy TableList) i $ \case
          Left (_, e) -> pure $ dispatch $ GlobalSetError $ pack e
          Right ts -> pure $ dispatch $ TablesSet ts
        pure $ st & stateProjectId .~ Just i

      -- Tables

      TablesSet ts ->  pure $
        st & stateTables .~ ts

      TablesCreate t -> do
        request api (Proxy :: Proxy TableCreate) t $ \case
          Left (_, e) -> pure $ dispatch $ GlobalSetError $ pack e
          Right i -> pure $ dispatch $ TablesAdd (Entity i t)
        pure st

      TablesAdd t -> pure $
        st & stateTables %~ (t:)

      TablesLoadTable i -> do
        request api (Proxy :: Proxy TableGetWhole) i $ \case
          Left (_, e) -> pure $ dispatch $ GlobalSetError $ pack e
          Right res -> pure $ dispatch $ TableSet res
        pure $ st & stateTableId .~ Just i

      -- Table

      TableSet (cols, recs, entries) -> pure $
        st & stateColumns .~ (Map.fromList $ map (\(Entity i c) -> (i, c)) cols)
           & stateRecords .~ (Map.fromList $ map (\(Entity i r) -> (i, r)) recs)
           & stateCells .~ fillEntries entries Map.empty

      TableUpdateCells entries -> pure $
        st & stateCells %~ fillEntries entries

      TableUpdateColumns entries -> pure $
        st & stateColumns %~ \cols ->
          foldl' (\cols' (Entity i c) -> Map.insert i c cols') cols $
          filter (\(Entity _ c) -> st ^. stateTableId == Just (columnTableId c)) $
          entries

      TableAddColumn -> do
        case st ^. stateTableId of
          Nothing -> pure ()
          Just t -> request api (Proxy :: Proxy ColumnCreate) t $ \case
            Left (_, e) -> pure $ dispatch $ GlobalSetError $ pack e
            Right res -> pure $ dispatch $ TableAddColumnDone res
        pure st

      TableAddColumnDone (Entity i c, cells) -> pure $
        st & stateColumns %~ Map.insert i c
           & stateCells %~ fillEntries (map toCellUpdate cells)

      TableDeleteColumn i -> do
        request api (Proxy :: Proxy ColumnDelete) i $ \case
          Left (_, e) -> pure $ dispatch $ GlobalSetError $ pack e
          Right () -> pure []
        pure $ st & stateColumns %~ Map.delete i
                  & stateCells %~ Map.filterWithKey
                             (\(Coords c _) _ -> c /= i)

      TableAddRecord -> do
        case st ^. stateTableId of
          Nothing -> pure ()
          Just t -> request api (Proxy :: Proxy RecordCreate) t $ \case
            Left (_, e) -> pure $ dispatch $ GlobalSetError $ pack e
            Right res -> pure $ dispatch $ TableAddRecordDone res
        pure st

      TableAddRecordDone (Entity i r, cells) -> pure $
        st & stateRecords %~ Map.insert i r
           & stateCells %~ fillEntries (map toCellUpdate cells)

      TableDeleteRecord i -> do
        request api (Proxy :: Proxy RecordDelete) i $ \case
          Left (_, e) -> pure $ dispatch $ GlobalSetError $ pack e
          Right () -> pure []
        pure $ st & stateRecords %~ Map.delete i
                  & stateCells %~ Map.filterWithKey
                             (\(Coords _ r) _ -> r /= i)

      -- Column

      ColumnRename i n -> do
        request api (Proxy :: Proxy ColumnSetName) i n $ \case
          Left (_, e) -> pure $ dispatch $ GlobalSetError $ pack e
          Right ()    -> pure []
        pure $ st & stateColumns . at i . _Just %~ \c -> c { columnName = n }

      ColumnSetDt i dt -> do
        request api (Proxy :: Proxy ColumnSetDataType) i dt $ \case
          Left (_, e) -> pure $ dispatch $ GlobalSetError $ pack e
          Right ()    -> pure $ []
        pure $ st & stateColumns . at i . _Just %~ \c -> c { columnDataType = dt }

      ColumnSetInput i payload@(inpTyp, src) -> do
        request api (Proxy :: Proxy ColumnSetInput) i payload $ \case
          Left (_, e) -> pure $ dispatch $ GlobalSetError $ pack e
          Right ()    -> pure $ []
        pure $ st & stateColumns . at i . _Just %~ \c -> c { columnInputType = inpTyp
                                                           , columnSourceCode = src
                                                           }

      -- Cell

      CellSetValue c r val -> do
        request api (Proxy :: Proxy CellSet) c r val $ \case
          Left (_, e) -> pure $ dispatch $ GlobalSetError $ pack e
          Right () -> pure []
        pure $ st & stateCells %~ fillEntries [(c, r, CellValue val)]

    where

      fillEntries entries m = foldl' (\m' (c, v) -> Map.insert c v m') m $
        map (\(colId, recId, val) -> ((Coords colId recId), val)) entries

toCellUpdate :: Entity Cell -> (Id Column, Id Record, CellContent)
toCellUpdate (Entity _ (Cell content (Aspects _ c r))) = (c, r, content)
