{-# LANGUAGE TemplateHaskell #-}

module Store where

import           Control.Applicative       ((<|>))
import           Control.Concurrent        (forkIO)
import           Control.Lens
import           Control.Monad             (when)
import           Data.Foldable             (foldl', for_)
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Monoid               ((<>))
import           Data.Proxy
import qualified Data.Text                 as Text
import           React.Flux
import           React.Flux.Addons.Servant (HandleResponse, request)
import           WebSocket

import qualified Lib.Api.Rest              as Api
import           Lib.Api.WebSocket         (WsDownMessage (..))
import           Lib.Model
import           Lib.Model.Auth            (LoginResponse (..), SessionKey,
                                            SignupResponse (..),
                                            UserInfo (UserInfo))
import           Lib.Model.Cell            (Aspects (..), Cell (..),
                                            CellContent (..))
import           Lib.Model.Column          (Column, columnTableId)
import           Lib.Model.Project
import           Lib.Model.Record
import           Lib.Model.Table
import           Lib.Types

import           Action                    (Action (..), TableCache, api,
                                            session)
import qualified Store.Column              as Column
import qualified Store.Message             as Message
import qualified Store.RecordCache         as RecordCache

data Coords = Coords (Id Column) (Id Record)
  deriving (Eq, Ord, Show)

data CellInfo = CellInfo
  { ciCol     :: Column
  , ciContent :: CellContent
  } deriving (Eq)

data State = State
  -- session-agnostic state
  { _stateWebSocket :: Maybe JSWebSocket
  , _stateMessage   :: Message.State
  -- session state
  , _stateSession   :: SessionState
  }

data SessionState
  = StateLoggedIn  LoggedInState
  | StateLoggedOut LoggedOutState

data LoggedInState = LoggedInState
  -- global
  -- TODO: get rid of Maybes
  { _stateUserInfo     :: UserInfo
  , _stateCacheRecords :: Map (Id Table) RecordCache.State
  , _stateSessionKey   :: SessionKey
  , _stateProjectId    :: Maybe (Id Project)
  , _stateTableId      :: Maybe (Id Table)

  -- columns
  , _stateColumns      :: Map (Id Column) Column.State

  -- projects/tables/cells/records
  , _stateProjects     :: Map (Id Project) Project
  , _stateTables       :: Map (Id Table) Table
  , _stateCells        :: Map Coords CellContent
  , _stateRecords      :: Map (Id Record) Record

  -- for column config
  , _stateTableCache   :: TableCache
  }

data LoggedOutState
  = LoggedOutLoginForm
  | LoggedOutSignupForm

makeLenses ''State
makeLenses ''LoggedInState
makePrisms ''SessionState

-- execute action in case the user is logged in and error otherwise
forLoggedIn :: State -> (LoggedInState -> IO LoggedInState) -> IO State
forLoggedIn st action = case st ^. stateSession of
  StateLoggedIn  liSt -> mkStateLoggedIn st <$> action liSt
  StateLoggedOut _    -> do
    -- TODO: proper error message
    putStrLn "inconsistent client state: unexpected: not logged in"
    pure st

-- execute action in case the user is logged in and error otherwise
-- use the state read-only for the action
forLoggedIn_ :: State -> (LoggedInState -> IO ()) -> IO ()
forLoggedIn_ st action = case st ^. stateSession of
  StateLoggedIn  liSt -> action liSt
  StateLoggedOut _    ->
    -- TODO: proper error message
    putStrLn "inconsistent client state: unexpected: not logged in"

-- execute action in case the user is logged in and error otherwise
-- in this variant, the action takes care of transforming the
-- LoggedInState to State
forLoggedIn' :: State -> (LoggedInState -> IO State) -> IO State
forLoggedIn' st action = case st ^. stateSession of
  StateLoggedIn  liSt -> action liSt
  StateLoggedOut _    -> do
    -- TODO: proper error message
    putStrLn "inconsistent client state: unexpected: not logged in"
    pure st

mkStateLoggedIn :: State -> LoggedInState -> State
mkStateLoggedIn st liSt = st & stateSession .~ StateLoggedIn liSt

initLoggedInState :: SessionKey -> UserInfo -> LoggedInState
initLoggedInState sKey userInfo = LoggedInState
  { _stateUserInfo     = userInfo
  , _stateCacheRecords = Map.empty
  , _stateSessionKey   = sKey
  , _stateProjectId    = Nothing
  , _stateTableId      = Nothing
  , _stateColumns      = Map.empty
  , _stateProjects     = Map.empty
  , _stateTables       = Map.empty
  , _stateCells        = Map.empty
  , _stateRecords      = Map.empty
  , _stateTableCache   = Map.empty
  }

store :: ReactStore State
store = mkStore State
  { _stateWebSocket = Nothing
  , _stateMessage   = Nothing
  , _stateSession   = StateLoggedOut LoggedOutLoginForm
  }

instance StoreData State where
  type StoreAction State = Action
  transform action st = case action of

      MessageAction a ->
        pure $ st & stateMessage .~ Message.runAction a

      GlobalInit wsUrl -> do
        ws <- jsonWebSocketNew wsUrl $ pure . dispatch . \case
          WsDownCellsChanged cs       -> TableUpdateCells cs
          WsDownColumnsChanged cs     -> TableUpdateColumns cs
          WsDownRecordCreated t r dat -> RecordCacheAction t $ RecordCache.Add r dat
          WsDownRecordDeleted t r     -> RecordCacheAction t $ RecordCache.Delete r
        case st ^. stateSession of
          StateLoggedIn liSt ->
            request api (Proxy :: Proxy Api.ProjectList)
              (session $ liSt ^. stateSessionKey) $ mkCallback $
                \projects -> [ProjectsSet projects]
          StateLoggedOut _ -> pure ()
        pure $ st & stateWebSocket .~ Just ws

      GlobalSendWebSocket msg -> do
        for_ (st ^. stateWebSocket) $ \ws -> jsonWebSocketSend msg ws
        pure st

      -- Session

      Signup signupData -> do
        request api (Proxy :: Proxy Api.AuthSignup) signupData $ mkCallback $ \case
          SignupSuccess userInfo ->
            [ LoggedIn userInfo
            , MessageAction $ Message.SetSuccess "Successfully signed up."
            ]
          SignupFailed txt ->
            [ MessageAction $ Message.SetWarning txt
            ]
        pure st

      ToSignupForm -> pure $ st & stateSession .~ StateLoggedOut LoggedOutSignupForm

      Login loginData -> do
        request api (Proxy :: Proxy Api.AuthLogin) loginData $ mkCallback $ \case
          LoginSuccess userInfo ->
            [ LoggedIn userInfo
            , MessageAction $ Message.SetSuccess "Successfully logged in."
            ]
          LoginFailed txt ->
            [ MessageAction $ Message.SetWarning txt
            ]
        pure st

      LoggedIn userInfo@(UserInfo _ _ sKey) -> do
        request api (Proxy :: Proxy Api.ProjectList)
            (session sKey) $ mkCallback $ \projects -> [ProjectsSet projects]
        pure $ mkStateLoggedIn st $ initLoggedInState sKey userInfo

      Logout -> do
        forLoggedIn_ st $ \liSt ->
          request api (Proxy :: Proxy Api.AuthLogout)
                      (session $ liSt ^. stateSessionKey) $ mkCallback $
                      const [MessageAction $ Message.SetSuccess "Successfully logged out."]
        pure $ st & stateSession .~ StateLoggedOut LoggedOutLoginForm

      -- Cache

      RecordCacheAction tableId a ->
        forLoggedIn st $ \liSt ->
          liSt & stateCacheRecords . at tableId . _Just %%~ \cache ->
            RecordCache.runAction mkCallback (liSt ^. stateSessionKey) tableId a cache

      -- Column

      ColumnAction columnId a ->
        forLoggedIn st $ \liSt ->
          liSt & stateColumns . at columnId . _Just %%~ \col ->
            Column.runAction mkCallback (liSt ^. stateSessionKey) columnId a col

      -- Projects

      ProjectsSet ps ->
        forLoggedIn st $ \liSt -> do
          _ <- forkIO $ case ps of
            []             -> pure ()
            Entity i _ : _ -> alterStore store $ ProjectsLoadProject i
          pure $ liSt & stateProjects .~ projectsMap
            where
              projectsMap = Map.fromList $ map entityToTuple ps

      ProjectsCreate projectName -> do
        forLoggedIn_ st $ \liSt ->
          request api (Proxy :: Proxy Api.ProjectCreate)
                  (session $ liSt ^. stateSessionKey)
                  projectName $
                  mkCallback $ \project -> [ProjectsAdd project]
        pure st

      ProjectsAdd (Entity i p) ->
        forLoggedIn st $ \liSt ->
          pure $ liSt & stateProjects . at i .~ Just p

      ProjectsLoadProject i ->
        forLoggedIn st $ \liSt -> do
          request api (Proxy :: Proxy Api.TableList)
                      (session $ liSt ^. stateSessionKey) i $ mkCallback $
                      \tables -> [TablesSet tables]
          pure $ liSt & stateProjectId .~ Just i

      -- Project

      ProjectSetName i name ->
        forLoggedIn st $ \liSt -> do
          request api (Proxy :: Proxy Api.ProjectSetName)
                      (session $ liSt ^. stateSessionKey)
                      i name $ mkCallback $ const []
          pure $ liSt & stateProjects . at i . _Just . projectName .~ name

      ProjectDelete projectId ->
        forLoggedIn' st $ \liSt -> do
          request api (Proxy :: Proxy Api.ProjectDelete)
                      (session $ liSt ^. stateSessionKey) projectId $ mkCallback $
                      const []

          if liSt ^. stateProjectId == Just projectId
            then do
              let nextProject = Map.lookupLT projectId (liSt ^. stateProjects)
                          <|> Map.lookupGT projectId (liSt ^. stateProjects)
                  liSt' = liSt & stateProjects %~ Map.delete projectId
                               & stateTables %~ Map.filter (\table -> (table ^. tableProjectId) /= projectId)
                               & stateColumns .~ Map.empty
                               & stateCells .~ Map.empty
                               & stateRecords .~ Map.empty
                               & stateTableId .~ Nothing
                               & stateProjectId .~ (fst <$> nextProject)
                  st' = mkStateLoggedIn st liSt'
              case nextProject of
                Just (nextProjectId, _) ->
                  React.Flux.transform (ProjectsLoadProject nextProjectId) st'
                Nothing ->
                  pure st'
            else
              pure $ mkStateLoggedIn st $
                liSt & stateProjects %~ Map.delete projectId

      -- Tables

      TablesSet ts ->
        forLoggedIn st $ \liSt -> do
          _ <- forkIO $ case ts of
            []             -> pure ()
            Entity i _ : _ -> alterStore store $ TablesLoadTable i
          pure $ liSt & stateTables .~ tablesMap
            where
              tablesMap = Map.fromList $ map entityToTuple ts

      TablesCreate table -> do
        forLoggedIn_ st $ \liSt ->
          request api (Proxy :: Proxy Api.TableCreate)
                      (session $ liSt ^. stateSessionKey)
                      table $ mkCallback $
                      \tableId -> [TablesAdd $ Entity tableId table]
        pure st

      TablesAdd (Entity i t) ->
        forLoggedIn st $ \liSt ->
          pure $ liSt & stateTables . at i .~ Just t

      TablesLoadTable i ->
        forLoggedIn st $ \liSt -> do
          request api (Proxy :: Proxy Api.TableGetWhole)
                      (session $ liSt ^. stateSessionKey) i $ mkCallback $
                      \table -> [ TableSet table
                                , MessageAction Message.Unset
                                ]
          pure $ liSt & stateTableId .~ Just i

      -- Table

      TableSet (cols, recs, entries) ->
        forLoggedIn st $ \liSt -> pure $
          liSt & stateColumns .~ (Column.mkState <$> Map.fromList (map entityToTuple cols))
               & stateRecords .~ Map.fromList (map entityToTuple recs)
               & stateCells   .~ fillEntries entries Map.empty

      TableUpdateCells cells ->
        forLoggedIn st $ \liSt -> pure $
          let toEntry (Cell content (Aspects _ c r)) = (c, r, content)
              setRecordInCache liSt'' (Cell content (Aspects t c r)) =
                liSt'' & stateCacheRecords . at t . _Just
                     . RecordCache.recordCache . at r . _Just
                     . at c . _Just . _2 .~ content
              liSt' = foldl' setRecordInCache liSt cells
          in liSt' & stateCells %~ fillEntries (map toEntry cells)

      TableUpdateColumns entities ->
          forLoggedIn st $ \liSt ->
            pure $ liSt & stateColumns %~ \cols ->
              foldl' acc cols $ filter (tableColumn liSt) entities
        where
          tableColumn liSt (Entity _ column) =
            liSt ^. stateTableId == Just (column ^. columnTableId)
          acc cols' (Entity columnId column) =
            Map.insert columnId (Column.mkState column) cols'

      TableAddColumn col -> do
        forLoggedIn_ st $ \liSt ->
          request api (Proxy :: Proxy Api.ColumnCreate)
                      (session $ liSt ^. stateSessionKey) col $ mkCallback $
                      \column -> [TableAddColumnDone column]
        pure st

      TableAddColumnDone (Entity i c, cells) ->
        forLoggedIn st $ \liSt -> pure $
          liSt & stateColumns %~ Map.insert i (Column.mkState c)
               & stateCells %~ fillEntries (map toCellUpdate cells)

      TableDeleteColumn i ->
        forLoggedIn st $ \liSt -> do
          request api (Proxy :: Proxy Api.ColumnDelete)
                      (session $ liSt ^. stateSessionKey) i $ mkCallback $ const []
          pure $ liSt & stateColumns %~ Map.delete i
                      & stateCells %~ Map.filterWithKey (\(Coords c _) _ -> c /= i)

      TableAddRecord -> do
        forLoggedIn_ st $ \liSt ->
          for_ (liSt ^. stateTableId) $ \table ->
            request api (Proxy :: Proxy Api.RecordCreate)
                        (session $ liSt ^. stateSessionKey) table $ mkCallback $
                        \record -> [TableAddRecordDone record]
        pure st

      TableAddRecordDone (Entity i r, cells) ->
        forLoggedIn st $ \liSt -> pure $
          liSt & stateRecords %~ Map.insert i r
               & stateCells %~ fillEntries (map toCellUpdate cells)

      TableDeleteRecord i ->
        forLoggedIn st $ \liSt -> do
          request api (Proxy :: Proxy Api.RecordDelete)
                      (session $ liSt ^. stateSessionKey) i $ mkCallback $ const []
          pure $ liSt & stateRecords %~ Map.delete i
                      & stateCells %~ Map.filterWithKey (\(Coords _ r) _ -> r /= i)

      TableSetName i name ->
        forLoggedIn st $ \liSt -> do
          request api (Proxy :: Proxy Api.TableSetName)
                      (session $ liSt ^. stateSessionKey)
                      i name $ mkCallback $ const []
          pure $ liSt & stateTables . at i . _Just . tableName .~ name

      TableDelete tableId ->
        forLoggedIn' st $ \liSt -> do
          request api (Proxy :: Proxy Api.TableDelete)
                      (session $ liSt ^. stateSessionKey)
                      tableId $ mkCallback $ const []

          if liSt ^. stateTableId == Just tableId
            then do
              let nextTable = Map.lookupLT tableId (liSt ^. stateTables)
                          <|> Map.lookupGT tableId (liSt ^. stateTables)

                  liSt' = liSt & stateTables %~ Map.delete tableId
                          & stateColumns .~ Map.empty
                          & stateCells .~ Map.empty
                          & stateRecords .~ Map.empty
                          & stateTableId .~ (fst <$> nextTable)
                  st' = mkStateLoggedIn st liSt'
              case nextTable of
                Just (nextTableId, _) ->
                  React.Flux.transform (TablesLoadTable nextTableId) st'
                Nothing -> pure st'
            else
              pure $ mkStateLoggedIn st $
                liSt & stateTables %~ Map.delete tableId

      -- Column

      GetTableCache -> do
          forLoggedIn_ st $ \liSt ->
            when (Map.null $ liSt ^. stateTableCache) $
              request api (Proxy :: Proxy Api.TableListGlobal)
                      (session $ liSt ^. stateSessionKey) $
                      mkCallback $
                      \tables -> [SetTableCache $ toTableMap tables]
          pure st
        where
          toTableMap = Map.fromList . map entityToPair
          entityToPair (Entity tableId table) = (tableId, table ^. tableName)

      SetTableCache m ->
        forLoggedIn st $ \liSt -> pure $ liSt & stateTableCache .~ m

      -- Cell

      CellSetValue c r val ->
        forLoggedIn st $ \liSt -> do
          request api (Proxy :: Proxy Api.CellSet)
                      (session $ liSt ^. stateSessionKey) c r val $ mkCallback $
                      const []
          pure $ liSt & stateCells %~ fillEntries [(c, r, CellValue val)]

    where
      fillEntries entries m = foldl' acc m $ map toCoords entries
        where
          toCoords (colId, recId, val) = (Coords colId recId, val)
          acc m' (c, v) = Map.insert c v m'

toCellUpdate :: Entity Cell -> (Id Column, Id Record, CellContent)
toCellUpdate (Entity _ (Cell content (Aspects _ c r))) = (c, r, content)

dispatch :: Action -> [SomeStoreAction]
dispatch a = [SomeStoreAction store a]

mkCallback :: (a -> [Action])
           -> HandleResponse a
mkCallback cbSuccess = pure . \case
  Left (401, e) -> dispatch $ MessageAction $ Message.SetWarning $
                    "Unauthorized. Are you logged in?" <>
                    " (401) " <> Text.pack e
  Left (n, e)   -> dispatch $ MessageAction $ Message.SetError $
                    " (" <> (Text.pack . show) n <> ") " <> Text.pack e
  Right x       -> SomeStoreAction store <$> cbSuccess x
