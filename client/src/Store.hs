{-# LANGUAGE TemplateHaskell #-}

module Store where

import           Control.Applicative       ((<|>))
import           Control.Concurrent        (forkIO)
import           Control.Lens
import           Control.Monad             (when)
import           Data.Foldable             (foldl')
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Proxy
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           React.Flux
import           React.Flux.Addons.Servant (HandleResponse, request)
import           WebSocket

import qualified Lib.Api.Rest              as Api
import           Lib.Api.WebSocket         (WsDownMessage (..))
import           Lib.Model
import           Lib.Model.Auth            (LoginResponse (..), SessionKey)
import           Lib.Model.Cell            (Aspects (..), Cell (..),
                                            CellContent (..))
import           Lib.Model.Column          (Column, columnTableId)
import           Lib.Model.Project
import           Lib.Model.Record
import           Lib.Model.Table
import           Lib.Types

import           Action                    (Action (..), TableCache, api,
                                            session)
import qualified Action.Column             as Column
import qualified Action.RecordCache        as RecordCache

data Coords = Coords (Id Column) (Id Record)
  deriving (Eq, Ord, Show)

data CellInfo = CellInfo
  { ciCol     :: Column
  , ciContent :: CellContent
  } deriving (Eq)

data State = State
  { _stateError        :: Maybe Text
  , _stateSessionKey   :: Maybe SessionKey

  , _stateWebSocket    :: Maybe JSWebSocket

  , _stateCacheRecords :: Map (Id Table) RecordCache.State

  , _stateProjects     :: Map (Id Project) Project
  , _stateProjectId    :: Maybe (Id Project)

  , _stateTables       :: Map (Id Table) Table
  , _stateTableId      :: Maybe (Id Table)

  , _stateCells        :: Map Coords CellContent
  , _stateRecords      :: Map (Id Record) Record

  -- for column config
  , _stateTableCache   :: TableCache
  , _stateColumns      :: Map (Id Column) Column.State
  }

makeLenses ''State

store :: ReactStore State
store = mkStore State
  { _stateError = Nothing
  , _stateSessionKey = Nothing
  , _stateWebSocket = Nothing
  , _stateCacheRecords = Map.empty
  , _stateProjects = Map.empty
  , _stateProjectId = Nothing
  , _stateTables = Map.empty
  , _stateTableId = Nothing
  , _stateCells = Map.empty
  , _stateRecords = Map.empty
  , _stateTableCache = Map.empty
  , _stateColumns = Map.empty
  }

instance StoreData State where
  type StoreAction State = Action
  transform action st = case action of

      GlobalSetError msg -> pure $
        st & stateError .~ Just msg

      GlobalInit wsUrl -> do
        ws <- jsonWebSocketNew wsUrl $ \case
          WsDownCellsChanged cs       -> pure $ dispatch $ TableUpdateCells cs
          WsDownColumnsChanged cs     -> pure $ dispatch $ TableUpdateColumns cs
          WsDownRecordCreated t r dat -> pure $ dispatch $ RecordCacheAction t $ RecordCache.Add r dat
          WsDownRecordDeleted t r     -> pure $ dispatch $ RecordCacheAction t $ RecordCache.Delete r
        request api (Proxy :: Proxy Api.ProjectList)
          (session $ st ^. stateSessionKey) $ mkCallback GlobalSetError $
            \projects -> [ProjectsSet projects]
        pure $ st & stateWebSocket .~ Just ws

      GlobalSendWebSocket msg -> do
        case st ^. stateWebSocket of
          Nothing -> pure ()
          Just ws -> jsonWebSocketSend msg ws
        pure st

      -- Session

      Login loginData -> do
        request api (Proxy :: Proxy Api.AuthLogin) loginData $ mkCallback GlobalSetError $
          \loginResponse -> case loginResponse of
                        LoginSuccess sessionKey -> [LoggedIn sessionKey]
                        LoginFailed txt -> [GlobalSetError txt]
        pure st

      LoggedIn sKey -> pure $
        st & stateSessionKey .~ Just sKey

      Logout -> do
        request api (Proxy :: Proxy Api.AuthLogout)
                    (session $ st ^. stateSessionKey) $ mkCallback GlobalSetError $
                    const [LoggedOut]
        pure st

      LoggedOut ->
        pure $ st & stateSessionKey .~ Nothing

      -- Cache

      -- TODO: %%~ update
      RecordCacheAction tableId a -> do
        let cache :: RecordCache.State
            Just cache = st ^. stateCacheRecords . at tableId
        cache' <- RecordCache.runAction (mkCallback GlobalSetError)
                                           (st ^. stateSessionKey)
                                           tableId
                                           a
                                           cache
        pure $ st & stateCacheRecords . at tableId .~ Just cache'

      -- Column

      ColumnAction columnId a -> do
        let Just col = st ^. stateColumns . at columnId
        col' <- Column.runAction (mkCallback GlobalSetError)
                         (st ^. stateSessionKey)
                         columnId
                         a
                         col
        pure $ st & stateColumns . at columnId .~ Just col'

      -- Projects

      ProjectsSet ps -> do
        _ <- forkIO $ case ps of
          [] -> pure ()
          Entity i _ : _ ->
            alterStore store $ ProjectsLoadProject i
        pure $ st & stateProjects .~ projectsMap
          where
            projectsMap = Map.fromList $ map entityToTuple ps

      ProjectsCreate project -> do
        request api
                (Proxy :: Proxy Api.ProjectCreate)
                (session $ st ^. stateSessionKey)
                project $ mkCallback GlobalSetError $
          \projectId -> [ProjectsAdd $ Entity projectId project]
        pure st

      ProjectsAdd (Entity i p) -> pure $
        st & stateProjects . at i .~ Just p

      ProjectsLoadProject i -> do
        request api (Proxy :: Proxy Api.TableList)
                    (session $ st ^. stateSessionKey) i $ mkCallback GlobalSetError $
                    \tables -> [TablesSet tables]
        pure $ st & stateProjectId .~ Just i

      -- Project

      ProjectSetName i name -> do
        request api (Proxy :: Proxy Api.ProjectSetName)
                    (session $ st ^. stateSessionKey)
                    i name $ mkCallback GlobalSetError $ const []
        pure $ st & stateProjects . at i . _Just . projectName .~ name

      ProjectDelete projectId -> do
        request api (Proxy :: Proxy Api.ProjectDelete)
                    (session $ st ^. stateSessionKey) projectId $ mkCallback GlobalSetError $
                    const []

        if st ^. stateProjectId == Just projectId
          then do
            let nextProject = Map.lookupLT projectId (st ^. stateProjects)
                        <|> Map.lookupGT projectId (st ^. stateProjects)
                st' = st & stateProjects %~ Map.delete projectId
                         & stateTables %~ Map.filter (\table -> (table ^. tableProjectId) /= projectId)
                         & stateColumns .~ Map.empty
                         & stateCells .~ Map.empty
                         & stateRecords .~ Map.empty
                         & stateTableId .~ Nothing
                         & stateProjectId .~ (fst <$> nextProject)
            case nextProject of
              Just (nextProjectId, _) -> React.Flux.transform (ProjectsLoadProject nextProjectId) st'
              Nothing -> pure st'
          else
            pure $ st & stateProjects %~ Map.delete projectId

      -- Tables

      TablesSet ts -> do
        _ <- forkIO $ case ts of
          [] -> pure ()
          Entity i _ : _ ->
            alterStore store $ TablesLoadTable i
        pure $ st & stateTables .~ tablesMap
          where
            tablesMap = Map.fromList $ map entityToTuple ts

      TablesCreate table -> do
        request api (Proxy :: Proxy Api.TableCreate)
                    (session $ st ^. stateSessionKey)
                    table $ mkCallback GlobalSetError $
                    \tableId -> [TablesAdd $ Entity tableId table]
        pure st

      TablesAdd (Entity i t) -> pure $
        st & stateTables . at i .~ Just t

      TablesLoadTable i -> do
        request api (Proxy :: Proxy Api.TableGetWhole)
                    (session $ st ^. stateSessionKey) i $ mkCallback GlobalSetError $
                    \table -> [TableSet table]
        pure $ st & stateTableId .~ Just i

      -- Table

      TableSet (cols, recs, entries) -> pure $
        st & stateColumns .~ (Column.mkState <$> Map.fromList (map entityToTuple cols))
           & stateRecords .~ Map.fromList (map entityToTuple recs)
           & stateCells   .~ fillEntries entries Map.empty

      TableUpdateCells cells -> pure $
        let toEntry (Cell content (Aspects _ c r)) = (c, r, content)
            setRecordInCache st'' (Cell content (Aspects t c r)) =
              st'' & stateCacheRecords . at t . _Just
                   . RecordCache.recordCache . at r . _Just . at c . _Just . _2 .~ content
            st' = foldl' setRecordInCache st cells
        in st' & stateCells %~ fillEntries (map toEntry cells)

      TableUpdateColumns entries -> pure $
        st & stateColumns %~ \cols ->
          foldl' (\cols' (Entity i c) -> Map.insert i (Column.mkState c) cols') cols $
          filter (\(Entity _ c) -> st ^. stateTableId == Just (c ^. columnTableId)) entries

      TableAddColumn col -> do
        request api (Proxy :: Proxy Api.ColumnCreate)
                    (session $ st ^. stateSessionKey) col $ mkCallback GlobalSetError $
                    \column -> [TableAddColumnDone column]
        pure st

      TableAddColumnDone (Entity i c, cells) -> pure $
        st & stateColumns %~ Map.insert i (Column.mkState c)
           & stateCells %~ fillEntries (map toCellUpdate cells)

      TableDeleteColumn i -> do
        request api (Proxy :: Proxy Api.ColumnDelete)
                    (session $ st ^. stateSessionKey) i $ mkCallback GlobalSetError $ const []
        pure $ st & stateColumns %~ Map.delete i
                  & stateCells %~ Map.filterWithKey
                             (\(Coords c _) _ -> c /= i)

      TableAddRecord -> do
        case st ^. stateTableId of
          Nothing -> pure ()
          Just t -> request api (Proxy :: Proxy Api.RecordCreate)
                            (session $ st ^. stateSessionKey) t $ mkCallback GlobalSetError $
                            \record -> [TableAddRecordDone record]
        pure st

      TableAddRecordDone (Entity i r, cells) -> pure $
        st & stateRecords %~ Map.insert i r
           & stateCells %~ fillEntries (map toCellUpdate cells)

      TableDeleteRecord i -> do
        request api (Proxy :: Proxy Api.RecordDelete)
                    (session $ st ^. stateSessionKey) i $ mkCallback GlobalSetError $ const []
        pure $ st & stateRecords %~ Map.delete i
                  & stateCells %~ Map.filterWithKey
                             (\(Coords _ r) _ -> r /= i)

      TableSetName i name -> do
        request api (Proxy :: Proxy Api.TableSetName)
                    (session $ st ^. stateSessionKey)
                    i name $ mkCallback GlobalSetError $ const []
        pure $ st & stateTables . at i . _Just . tableName .~ name

      TableDelete tableId -> do
        request api (Proxy :: Proxy Api.TableDelete)
                    (session $ st ^. stateSessionKey)
                    tableId $ mkCallback GlobalSetError $ const []

        if st ^. stateTableId == Just tableId
          then do
            let nextTable = Map.lookupLT tableId (st ^. stateTables)
                        <|> Map.lookupGT tableId (st ^. stateTables)

                st' = st & stateTables %~ Map.delete tableId
                        & stateColumns .~ Map.empty
                        & stateCells .~ Map.empty
                        & stateRecords .~ Map.empty
                        & stateTableId .~ (fst <$> nextTable)

            case nextTable of
              Just (nextTableId, _) -> React.Flux.transform (TablesLoadTable nextTableId) st'
              Nothing -> pure st'
          else
            pure $ st & stateTables %~ Map.delete tableId

      -- Column

      GetTableCache sKey -> do
        let toTableMap = Map.fromList . map (\(Entity tableId table) -> (tableId, table ^. tableName))
        when (Map.null $ st ^. stateTableCache) $
          request api (Proxy :: Proxy Api.TableListGlobal) (session sKey) $
                  mkCallback GlobalSetError $ \tables -> [SetTableCache $ toTableMap tables]
        pure st
      SetTableCache m ->
        pure $ st & stateTableCache .~ m

      -- Cell

      CellSetValue c r val -> do
        request api (Proxy :: Proxy Api.CellSet)
                    (session $ st ^. stateSessionKey) c r val $ mkCallback GlobalSetError $
                    const []
        pure $ st & stateCells %~ fillEntries [(c, r, CellValue val)]

    where
      fillEntries entries m = foldl' (\m' (c, v) -> Map.insert c v m') m $
        map (\(colId, recId, val) -> (Coords colId recId, val)) entries

toCellUpdate :: Entity Cell -> (Id Column, Id Record, CellContent)
toCellUpdate (Entity _ (Cell content (Aspects _ c r))) = (c, r, content)

dispatch :: Action -> [SomeStoreAction]
dispatch a = [SomeStoreAction store a]

mkCallback :: (Text -> Action)
           -> (a -> [Action])
           -> HandleResponse a
mkCallback cbFail cbSuccess = pure . \case
  Left  (_, e) -> dispatch $ cbFail $ Text.pack e
  Right x      -> SomeStoreAction store <$> cbSuccess x
