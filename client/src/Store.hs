{-# LANGUAGE TemplateHaskell #-}

module Store where

import           Control.Applicative       ((<|>))
import           Control.Concurrent        (forkIO)
import           Control.Lens
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

import           Action                    (Action (..), api, session)
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

  -- data LoggedInState = LoggedInState

data LoggedInState = LoggedInState
  { _stateUserInfo    :: UserInfo
  , _stateSessionKey  :: SessionKey
  , _stateProjectView :: ProjectViewState
  }

data ProjectViewState
  = StateProjectOverview ProjectOverviewState
  | StateProjectDetail ProjectDetailState

type ProjectOverviewState = Map (Id Project) Project

-- TODO: maybe put tableId and tables one level deeper into project?
data ProjectDetailState = ProjectDetailState
  { _stateProjectId    :: Id Project
  , _stateProject      :: Project
  , _stateCacheRecords :: Map (Id Table) RecordCache.State
  , _stateTableId      :: Maybe (Id Table)
  , _stateColumns      :: Map (Id Column) Column.State
  , _stateTables       :: Map (Id Table) Table
  , _stateCells        :: Map Coords CellContent
  , _stateRecords      :: Map (Id Record) Record
}

mkProjectDetailState :: Id Project -> Project -> ProjectDetailState
mkProjectDetailState i p = ProjectDetailState
  { _stateProjectId    = i
  , _stateProject      = p
  , _stateCacheRecords = Map.empty
  , _stateTableId      = Nothing
  , _stateColumns      = Map.empty
  , _stateTables       = Map.empty
  , _stateCells        = Map.empty
  , _stateRecords      = Map.empty
  }

data LoggedOutState
  = LoggedOutLoginForm
  | LoggedOutSignupForm

makeLenses ''State
makeLenses ''LoggedInState
makePrisms ''SessionState
makePrisms ''ProjectViewState
makeLenses ''ProjectDetailState

forProjectDetail :: State -> (SessionKey -> ProjectDetailState -> IO ProjectDetailState) -> IO State
forProjectDetail st action =
  forLoggedIn st $ \liSt -> case liSt ^. stateProjectView of
      StateProjectDetail pdSt -> mkStateProjectDetail liSt <$> action (liSt ^. stateSessionKey) pdSt
      StateProjectOverview _ -> do
        -- TODO: proper error message
        putStrLn "inconsistent client state: unexpected: project overview"
        pure liSt

forProjectDetail_ :: State -> (SessionKey -> ProjectDetailState -> IO ()) -> IO ()
forProjectDetail_ st action =
  forLoggedIn_ st $ \liSt -> case liSt ^. stateProjectView of
      StateProjectDetail pdSt -> action (liSt ^. stateSessionKey) pdSt
      StateProjectOverview _ -> do
        -- TODO: proper error message
        putStrLn "inconsistent client state: unexpected: project overview"

mkStateProjectDetail :: LoggedInState -> ProjectDetailState -> LoggedInState
mkStateProjectDetail liSt pdSt = liSt & stateProjectView .~ StateProjectDetail pdSt

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
  , _stateSessionKey   = sKey
  , _stateProjectView  = StateProjectOverview Map.empty
  }

setProjectOverview :: State -> SessionKey -> IO ()
setProjectOverview st sKey = do
  request api (Proxy :: Proxy Api.ProjectList)
    (session sKey) $ mkCallback $
    \projects -> [ProjectsSet projects]

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
            setProjectOverview st (liSt ^. stateSessionKey)
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
        setProjectOverview st sKey
        pure $ mkStateLoggedIn st $ initLoggedInState sKey userInfo

      Logout -> do
        forLoggedIn_ st $ \liSt ->
          request api (Proxy :: Proxy Api.AuthLogout)
                      (session $ liSt ^. stateSessionKey) $ mkCallback $
                      const [MessageAction $ Message.SetSuccess "Successfully logged out."]
        pure $ st & stateSession .~ StateLoggedOut LoggedOutLoginForm

      -- Cache

      RecordCacheAction tableId a ->
        forProjectDetail st $ \sKey pdSt ->
          pdSt & stateCacheRecords . at tableId . _Just %%~ \cache ->
            RecordCache.runAction mkCallback sKey tableId a cache

      -- Column

      ColumnAction columnId a ->
        forProjectDetail st $ \sKey pdSt ->
          pdSt & stateColumns . at columnId . _Just %%~ \col ->
            Column.runAction mkCallback sKey columnId a col

      -- Projects
      SetProjectOverview sKey -> do
        setProjectOverview st sKey
        pure st

      ProjectsSet ps ->
        forLoggedIn st $ \liSt ->
          liSt & stateProjectView .~ StateProjectOverview projectsMap
            where
              projectsMap = Map.fromList $ map entityToTuple ps

      ProjectsCreate name -> do
        forLoggedIn_ st $ \liSt ->
          request api (Proxy :: Proxy Api.ProjectCreate)
                  (session $ liSt ^. stateSessionKey)
                  name $
                  mkCallback $ \project -> [ProjectsAdd project]
        pure st

      ProjectsAdd (Entity i p) ->
        forLoggedIn st $ \liSt ->
          pure $ liSt & stateProjectView .~  mkProjectDetailState i p

      -- Project

      ProjectsLoadProject i ->
        forLoggedIn_ st $ \liSt ->
          request api (Proxy :: Proxy Api.ProjectLoad)
                      (session $ liSt ^. stateSessionKey) i $ mkCallback $
                      \(project, tables) -> [ProjectLoadDone project tables]

      ProjectLoadDone p ts ->
        forLoggedIn st $ \liSt -> do
          _ <- forkIO $ case ts of
            []             -> pure ()
            Entity i _ : _ -> alterStore store $ TablesLoadTable i
          pure $ liSt & stateProjectView .~
            (mkProjectDetailState i p & stateTables .~ tablesMap)
            where
              tablesMap = Map.fromList $ map entityToTuple ts


      ProjectSetName name ->
        forProjectDetail st $ \sKey pdSt -> do
          request api (Proxy :: Proxy Api.ProjectSetName)
                      (session sKey)
                      (pdSt ^. stateProjectId) name $ mkCallback $ const []
          pure $ pdSt & stateProject . projectName .~ name

      ProjectDelete projectId ->
        forLoggedIn st $ \liSt -> do
          let sKey = liSt ^. stateSessionKey
          request api (Proxy :: Proxy Api.ProjectDelete)
                      (session sKey) projectId $ mkCallback $
                      const [SetProjectOverview sKey]
          pure $ liSt & stateProjects %~ Map.delete projectId

      -- Tables

      TablesCreate table -> do
        forLoggedIn_ st $ \liSt ->
          request api (Proxy :: Proxy Api.TableCreate)
                      (session $ liSt ^. stateSessionKey)
                      table $ mkCallback $
                      \tableId -> [TablesAdd $ Entity tableId table]
        pure st

      TablesAdd (Entity i t) ->
        forProjectDetail st $ \sKey pdSt ->
          pure $ pdSt & stateTables . at i .~ Just t
                      & stateTableId .~ Just i

      TablesLoadTable i ->
        forProjectDetail st $ \sKey pdSt -> do
          request api (Proxy :: Proxy Api.TableGetWhole)
                      (session sKey) i $ mkCallback $
                      \table -> [ TableSet table ]
          pure $ pdSt & stateTableId .~ Just i

      -- Table

      TableSet (cols, recs, entries) ->
        forProjectDetail st $ \_ pdSt -> pure
          pdSt & stateColumns .~ Map.fromList (map entityToTuple cols)
               & stateRecords .~ Map.fromList (map entityToTuple recs)
               & stateCells   .~ fillEntries entries Map.empty

      TableUpdateCells cells ->
        forProjectDetail st $ \_ pdSt -> pure $
          let toEntry (Cell content (Aspects _ c r)) = (c, r, content)
              setRecordInCache pdSt'' (Cell content (Aspects t c r)) =
                pdSt'' & stateCacheRecords . at t . _Just
                     . RecordCache.recordCache . at r . _Just
                     . at c . _Just . _2 .~ content
              pdSt' = foldl' setRecordInCache pdSt cells
          in pdSt' & stateCells %~ fillEntries (map toEntry cells)

      TableUpdateColumns entities ->
        forProjectDetail st $ \_ pdSt ->
            pure $ pdSt & stateColumns %~ \cols ->
              foldl' acc cols $ filter (tableColumn pdSt) entities
        where
          tableColumn pdSt (Entity _ column) =
            pdSt ^. stateTableId == Just (column ^. columnTableId)
          acc cols' (Entity columnId column) =
            Map.insert columnId column cols'

      TableAddColumn col -> do
        forLoggedIn_ st $ \liSt ->
          request api (Proxy :: Proxy Api.ColumnCreate)
                      (session $ liSt ^. stateSessionKey) col $ mkCallback $
                      \column -> [TableAddColumnDone column]
        pure st

      TableAddColumnDone (Entity i c, cells) ->
        forProjectDetail st $ \_ pdSt -> pure $
          pdSt & stateColumns %~ Map.insert i c
               & stateCells %~ fillEntries (map toCellUpdate cells)

      TableDeleteColumn i ->
        forProjectDetail st $ \sKey pdSt -> do
          request api (Proxy :: Proxy Api.ColumnDelete)
                      (session sKey) i $ mkCallback $ const []
          pure $ pdSt & stateColumns %~ Map.delete i
                      & stateCells %~ Map.filterWithKey (\(Coords c _) _ -> c /= i)

      TableAddRecord -> do
        forProjectDetail st $ \sKey pdSt ->
          for_ (pdSt ^. stateTableId) $ \table ->
            request api (Proxy :: Proxy Api.RecordCreate)
                        (session sKey) table $ mkCallback $
                        \record -> [TableAddRecordDone record]
        pure st

      TableAddRecordDone (Entity i r, cells) ->
        forProjectDetail st $ \_ pdSt -> pure $
          pdSt & stateRecords %~ Map.insert i r
               & stateCells %~ fillEntries (map toCellUpdate cells)

      TableDeleteRecord i ->
        forProjectDetail st $ \sKey pdSt -> do
          request api (Proxy :: Proxy Api.RecordDelete)
                      (session sKey) i $ mkCallback $ const []
          pure $ pdSt & stateRecords %~ Map.delete i
                      & stateCells %~ Map.filterWithKey (\(Coords _ r) _ -> r /= i)

      TableSetName i name ->
        forProjectDetail st $ \sKey pdSt -> do
          request api (Proxy :: Proxy Api.TableSetName)
                      (session sKey)
                      i name $ mkCallback $ const []
          pure $ pdSt & stateTables . at i . _Just . tableName .~ name

      TableDelete tableId ->
        forProjectDetail st $ \sKey pdSt -> do
          request api (Proxy :: Proxy Api.TableDelete)
                      (session sKey)
                      tableId $ mkCallback $ const []

          if pdSt ^. stateTableId == Just tableId
            then do
              let nextTable = Map.lookupLT tableId (pdSt ^. stateTables)
                          <|> Map.lookupGT tableId (pdSt ^. stateTables)

                  pdSt' = pdSt & stateTables %~ Map.delete tableId
                          & stateColumns .~ Map.empty
                          & stateCells .~ Map.empty
                          & stateRecords .~ Map.empty
                          & stateTableId .~ (fst <$> nextTable)
                  st' = mkStateLoggedIn st pdSt'
              case nextTable of
                Just (nextTableId, _) ->
                  React.Flux.transform (TablesLoadTable nextTableId) st'
                Nothing -> pure st'
            else
              pure $ mkStateLoggedIn st $
                pdSt & stateTables %~ Map.delete tableId

      -- Cell

      CellSetValue c r val ->
        forProjectDetail st $ \sKey pdSt -> do
          request api (Proxy :: Proxy Api.CellSet)
                      (session sKey) c r val $ mkCallback $
                      const []
          pure $ pdSt & stateCells %~ fillEntries [(c, r, CellValue val)]

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
