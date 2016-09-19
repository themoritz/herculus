{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Store where

import           Control.Arrow             (second)
import           Control.Concurrent        (forkIO)
import           Control.DeepSeq
import           Control.Lens

import           Data.Foldable             (foldl')
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Proxy
import           Data.Text                 (Text, pack)
import           Data.Typeable             (Typeable)

import           GHC.Generics

import           React.Flux
import           React.Flux.Addons.Servant

import           Lib.Model
import           Lib.Model.Cell
import           Lib.Model.Column
import           Lib.Model.Project
import           Lib.Model.Record
import           Lib.Model.Table
import           Lib.Types

import           Lib.Api.Rest              as Api
import           Lib.Api.WebSocket

import qualified Config
import           WebSocket

data Coords = Coords (Id Column) (Id Record)
  deriving (Eq, Ord, Show)

data CellInfo = CellInfo
  { ciCol     :: Column
  , ciContent :: CellContent
  } deriving (Eq)

data State = State
  { _stateError        :: Maybe Text
  , _stateWebSocket    :: Maybe JSWebSocket
  , _stateCacheRecords :: Map (Id Table) (Map (Id Record) (Map (Id Column) (Column, CellContent)))
  , _stateProjects     :: Map (Id Project) Project
  , _stateProjectId    :: Maybe (Id Project)
  , _stateTables       :: Map (Id Table) Table
  , _stateTableId      :: Maybe (Id Table)
  , _stateCells        :: Map Coords CellContent
  , _stateColumns      :: Map (Id Column) Column
  , _stateRecords      :: Map (Id Record) Record
  }

makeLenses ''State

store :: ReactStore State
store = mkStore $ State Nothing Nothing Map.empty Map.empty Nothing Map.empty Nothing
  Map.empty Map.empty Map.empty

data Action
  -- Global
  = GlobalSetError Text
  | GlobalInit Text -- WebSocket URL
  | GlobalSendWebSocket WsUpMessage
  -- Cache
  | CacheRecordAdd (Id Table) (Id Record) [(Entity Column, CellContent)]
  | CacheRecordDelete (Id Table) (Id Record)
  | CacheRecordsGet (Id Table)
  | CacheRecordsSet (Id Table) [(Id Record, [(Entity Column, CellContent)])]
  -- Projects
  | ProjectsSet [Entity Project]
  | ProjectsCreate Project
  | ProjectsAdd (Entity Project)
  | ProjectsLoadProject (Id Project)
  -- Project
  | ProjectSetName (Id Project) Text
  -- Tables
  | TablesSet [Entity Table]
  | TablesCreate Table
  | TablesAdd (Entity Table)
  | TablesLoadTable (Id Table)
  -- Table
  | TableSet ([Entity Column], [Entity Record], [(Id Column, Id Record, CellContent)])
  | TableUpdateCells [Cell]
  | TableUpdateColumns [Entity Column]
  | TableAddColumn Column
  | TableAddColumnDone (Entity Column, [Entity Cell])
  | TableDeleteColumn (Id Column)
  | TableAddRecord
  | TableAddRecordDone (Entity Record, [Entity Cell])
  | TableDeleteRecord (Id Record)
  | TableSetName (Id Table) Text
  -- Column
  | ColumnRename (Id Column) Text
  -- Data column
  | DataColUpdate (Id Column) (DataType, IsDerived, Text)
  -- Report column
  | ReportColUpdate (Id Column) (Text, ReportFormat, Maybe ReportLanguage)
  -- Cell
  | CellSetValue (Id Column) (Id Record) Value
  deriving (Typeable, Generic, NFData)

api :: ApiRequestConfig Routes
api = ApiRequestConfig Config.apiUrl NoTimeout

dispatch :: Action -> [SomeStoreAction]
dispatch a = [SomeStoreAction store a]

instance StoreData State where
  type StoreAction State = Action
  transform action st = case action of

      GlobalSetError msg -> pure $
        st & stateError .~ Just msg

      GlobalInit wsUrl -> do
        ws <- jsonWebSocketNew wsUrl $ \case
          WsDownCellsChanged cs       -> pure $ dispatch $ TableUpdateCells cs
          WsDownColumnsChanged cs     -> pure $ dispatch $ TableUpdateColumns cs
          WsDownRecordCreated t r dat -> pure $ dispatch $ CacheRecordAdd t r dat
          WsDownRecordDeleted t r     -> pure $ dispatch $ CacheRecordDelete t r
        request api (Proxy :: Proxy Api.ProjectList) $ \case
          Left (_, e) -> pure $ dispatch $ GlobalSetError $ pack e
          Right ps -> pure $ dispatch $ ProjectsSet ps
        pure $ st & stateWebSocket .~ Just ws

      GlobalSendWebSocket msg -> do
        case st ^. stateWebSocket of
          Nothing -> pure ()
          Just ws -> jsonWebSocketSend msg ws
        pure st

      -- Cache

      CacheRecordAdd t r record -> do
        let toCol (Entity c col, content) = (c, (col, content))
            recMap = Map.fromList . map toCol $ record
        pure $ st & stateCacheRecords . at t . _Just . at r .~ Just recMap

      CacheRecordDelete t r ->
        pure $ st & stateCacheRecords . at t . _Just . at r .~ Nothing

      CacheRecordsGet t -> case st ^. stateCacheRecords . at t of
        Just _  -> pure st
        Nothing -> do
          request api (Proxy :: Proxy Api.RecordListWithData) t $ \case
            Left (_, e) -> pure $ dispatch $ GlobalSetError $ pack e
            Right res -> pure $ dispatch $ CacheRecordsSet t res
          pure st

      CacheRecordsSet t recs -> do
        let toCol (Entity c col, content) = (c, (col, content))
            recMaps = map (second $ Map.fromList . map toCol) recs
        pure $ st & stateCacheRecords . at t .~ Just (Map.fromList recMaps)

      -- Projects

      ProjectsSet ps -> do
        _ <- forkIO $ case ps of
          [] -> pure ()
          Entity i _ : _ ->
            alterStore store $ ProjectsLoadProject i
        pure $ st & stateProjects .~ projectsMap
          where
            projectsMap = Map.fromList $ map entityToTuple ps

      ProjectsCreate p -> do
        request api (Proxy :: Proxy Api.ProjectCreate) p $ \case
          Left (_, e) -> pure $ dispatch $ GlobalSetError $ pack e
          Right i -> pure $ dispatch $ ProjectsAdd (Entity i p)
        pure st

      ProjectsAdd (Entity i p) -> pure $
        st & stateProjects . at i .~ Just p

      ProjectsLoadProject i -> do
        request api (Proxy :: Proxy Api.TableList) i $ \case
          Left (_, e) -> pure $ dispatch $ GlobalSetError $ pack e
          Right ts -> pure $ dispatch $ TablesSet ts
        pure $ st & stateProjectId .~ Just i

      -- Project

      ProjectSetName i name -> do
        request api (Proxy :: Proxy Api.ProjectSetName) i name $ \case
          Left (_, e) -> pure $ dispatch $ GlobalSetError $ pack e
          Right () -> pure []
        pure $ st & stateProjects . at i . _Just . projectName .~name

      -- Tables

      TablesSet ts -> do
        _ <- forkIO $ case ts of
          [] -> pure ()
          Entity i _ : _ ->
            alterStore store $ TablesLoadTable i
        pure $ st & stateTables .~ tablesMap
          where
            tablesMap = Map.fromList $ map entityToTuple ts

      TablesCreate t -> do
        request api (Proxy :: Proxy Api.TableCreate) t $ \case
          Left (_, e) -> pure $ dispatch $ GlobalSetError $ pack e
          Right i -> pure $ dispatch $ TablesAdd (Entity i t)
        pure st

      TablesAdd (Entity i t) -> pure $
        st & stateTables . at i .~ Just t

      TablesLoadTable i -> do
        request api (Proxy :: Proxy Api.TableGetWhole) i $ \case
          Left (_, e) -> pure $ dispatch $ GlobalSetError $ pack e
          Right res -> pure $ dispatch $ TableSet res
        pure $ st & stateTableId .~ Just i

      -- Table

      TableSet (cols, recs, entries) -> pure $
        st & stateColumns .~ Map.fromList (map entityToTuple cols)
           & stateRecords .~ Map.fromList (map entityToTuple recs)
           & stateCells   .~ fillEntries entries Map.empty

      TableUpdateCells cells -> pure $
        let toEntry (Cell content (Aspects _ c r)) = (c, r, content)
            setRecordInCache st'' (Cell content (Aspects t c r)) =
              st'' & stateCacheRecords . at t . _Just . at r . _Just . at c . _Just . _2 .~ content
            st' = foldl' setRecordInCache st cells
        in st' & stateCells %~ fillEntries (map toEntry cells)

      TableUpdateColumns entries -> pure $
        st & stateColumns %~ \cols ->
          foldl' (\cols' (Entity i c) -> Map.insert i c cols') cols $
          filter (\(Entity _ c) -> st ^. stateTableId == Just (c ^. columnTableId)) entries

      TableAddColumn col -> do
        request api (Proxy :: Proxy Api.ColumnCreate) col $ \case
          Left (_, e) -> pure $ dispatch $ GlobalSetError $ pack e
          Right res -> pure $ dispatch $ TableAddColumnDone res
        pure st

      TableAddColumnDone (Entity i c, cells) -> pure $
        st & stateColumns %~ Map.insert i c
           & stateCells %~ fillEntries (map toCellUpdate cells)

      TableDeleteColumn i -> do
        request api (Proxy :: Proxy Api.ColumnDelete) i $ \case
          Left (_, e) -> pure $ dispatch $ GlobalSetError $ pack e
          Right () -> pure []
        pure $ st & stateColumns %~ Map.delete i
                  & stateCells %~ Map.filterWithKey
                             (\(Coords c _) _ -> c /= i)

      TableAddRecord -> do
        case st ^. stateTableId of
          Nothing -> pure ()
          Just t -> request api (Proxy :: Proxy Api.RecordCreate) t $ \case
            Left (_, e) -> pure $ dispatch $ GlobalSetError $ pack e
            Right res -> pure $ dispatch $ TableAddRecordDone res
        pure st

      TableAddRecordDone (Entity i r, cells) -> pure $
        st & stateRecords %~ Map.insert i r
           & stateCells %~ fillEntries (map toCellUpdate cells)

      TableDeleteRecord i -> do
        request api (Proxy :: Proxy Api.RecordDelete) i $ \case
          Left (_, e) -> pure $ dispatch $ GlobalSetError $ pack e
          Right () -> pure []
        pure $ st & stateRecords %~ Map.delete i
                  & stateCells %~ Map.filterWithKey
                             (\(Coords _ r) _ -> r /= i)

      TableSetName i name -> do
        request api (Proxy :: Proxy Api.TableSetName) i name $ \case
          Left (_, e) -> pure $ dispatch $ GlobalSetError $ pack e
          Right () -> pure []
        pure $ st & stateTables . at i . _Just . tableName .~ name

      -- Column

      ColumnRename i n -> do
        request api (Proxy :: Proxy Api.ColumnSetName) i n $ \case
          Left (_, e) -> pure $ dispatch $ GlobalSetError $ pack e
          Right ()    -> pure []
        pure $ st & stateColumns . at i . _Just . columnName .~ n

      -- Data column

      DataColUpdate i payload@(dt, inpTyp, src) -> do
        request api (Proxy :: Proxy Api.DataColUpdate) i payload $ \case
          Left (_, e) -> pure $ dispatch $ GlobalSetError $ pack e
          Right ()    -> pure []
        pure $ st & stateColumns . at i . _Just . columnKind . _ColumnData
                 %~ (dataColType .~ dt)
                  . (dataColIsDerived .~ inpTyp)
                  . (dataColSourceCode .~ src)

      -- Report column

      ReportColUpdate i payload@(templ, format, lang) -> do
        request api (Proxy :: Proxy Api.ReportColUpdate) i payload $ \case
          Left (_, e) -> pure $ dispatch $ GlobalSetError $ pack e
          Right ()    -> pure []
        pure $ st & stateColumns . at i . _Just . columnKind . _ColumnReport
                 %~ (reportColLanguage .~ lang)
                  . (reportColFormat .~ format)
                  . (reportColTemplate .~ templ)

      -- Cell

      CellSetValue c r val -> do
        request api (Proxy :: Proxy Api.CellSet) c r val $ \case
          Left (_, e) -> pure $ dispatch $ GlobalSetError $ pack e
          Right () -> pure []
        pure $ st & stateCells %~ fillEntries [(c, r, CellValue val)]

    where

      fillEntries entries m = foldl' (\m' (c, v) -> Map.insert c v m') m $
        map (\(colId, recId, val) -> (Coords colId recId, val)) entries

toCellUpdate :: Entity Cell -> (Id Column, Id Record, CellContent)
toCellUpdate (Entity _ (Cell content (Aspects _ c r))) = (c, r, content)
