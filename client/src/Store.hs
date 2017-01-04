{-# LANGUAGE TemplateHaskell #-}

module Store where

import           Control.Applicative       ((<|>))
import           Control.DeepSeq           (NFData)
import           Control.Lens
import           Control.Monad             (void)
import           Control.Monad.IO.Class    (liftIO)

import           Data.Foldable             (foldl', for_)
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Monoid               ((<>))
import           Data.Proxy
import qualified Data.Text                 as Text

import           React.Flux
import           React.Flux.Addons.Free
import           React.Flux.Addons.Servant (HandleResponse, request)

import qualified Lib.Api.Rest              as Api
import           Lib.Api.WebSocket         (WsDownMessage (..))
import           Lib.Model
import           Lib.Model.Auth            (GetUserInfoResponse (..),
                                            LoginResponse (..), SessionKey,
                                            SignupResponse (..),
                                            UserInfo (UserInfo))
import           Lib.Model.Cell            (Cell (..), CellContent (..))
import           Lib.Model.Column          (Column)
import           Lib.Model.Project
import           Lib.Model.Row
import           Lib.Model.Table
import           Lib.Types

import           Action                    (Action (..), api, session)
import qualified Store.Message             as Message
import qualified Store.RowCache            as RowCache
import           Store.Session             (clearSession, persistSession,
                                            recoverSession)
import           WebSocket

data Coords = Coords (Id Column) (Id Row)
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

mkLoggedInState :: SessionKey -> UserInfo -> LoggedInState
mkLoggedInState sKey userInfo = LoggedInState
  { _stateUserInfo     = userInfo
  , _stateSessionKey   = sKey
  , _stateProjectView  = StateProjectOverview Map.empty
  }

data ProjectViewState
  = StateProjectOverview ProjectOverviewState
  | StateProjectDetail ProjectDetailState

type ProjectOverviewState = Map (Id ProjectClient) ProjectClient

-- TODO: maybe put tableId and tables one level deeper into project?
data ProjectDetailState = ProjectDetailState
  { _stateProjectId :: Id ProjectClient
  , _stateProject   :: ProjectClient
  , _stateCacheRows :: Map (Id Table) RowCache.State
  , _stateTableId   :: Maybe (Id Table)
  , _stateColumns   :: Map (Id Column) Column
  , _stateTables    :: Map (Id Table) Table
  , _stateCells     :: Map Coords CellContent
  , _stateRows      :: Map (Id Row) Row
}

mkProjectDetailState :: Id ProjectClient -> ProjectClient -> ProjectDetailState
mkProjectDetailState i p = ProjectDetailState
  { _stateProjectId = i
  , _stateProject   = p
  , _stateCacheRows = Map.empty
  , _stateTableId   = Nothing
  , _stateColumns   = Map.empty
  , _stateTables    = Map.empty
  , _stateCells     = Map.empty
  , _stateRows      = Map.empty
  }

data LoggedOutState
  = LoggedOutLoginForm
  | LoggedOutSignupForm
  | LoggedOutUninitialized

makeLenses ''State
makeLenses ''LoggedInState
makePrisms ''SessionState
makePrisms ''ProjectViewState
makeLenses ''ProjectDetailState

--------------------------------------------------------------------------------

forProjectDetail :: (SessionKey -> ProjectDetailState -> App a) -> App a
forProjectDetail action =
  forLoggedIn $ \liSt -> case liSt ^. stateProjectView of
    StateProjectDetail pdSt -> action (liSt ^. stateSessionKey) pdSt
    StateProjectOverview _ -> do
      showMessage $ Message.SetError
        "inconsistent client state: unexpected: project overview"
      halt

forProjectDetail' :: (SessionKey -> ProjectDetailState -> App ProjectDetailState) -> App ()
forProjectDetail' action = do
  new <- forProjectDetail action
  stateSession . _StateLoggedIn . stateProjectView . _StateProjectDetail .= new

forProjectDetail_ :: (SessionKey -> ProjectDetailState -> App a) -> App ()
forProjectDetail_ = void . forProjectDetail

-- | Execute action in case the user is logged in and error otherwise
-- throw an error.
forLoggedIn :: (LoggedInState -> App a) -> App a
forLoggedIn action = use stateSession >>= \case
  StateLoggedIn  liSt -> action liSt
  StateLoggedOut _    -> do
    showMessage $ Message.SetError
      "inconsistent client state: unexpected: not logged in"
    halt

forLoggedIn' :: (LoggedInState -> App LoggedInState) -> App ()
forLoggedIn' action = do
  new <- forLoggedIn action
  stateSession .= StateLoggedIn new

forLoggedIn_ :: (LoggedInState -> App a) -> App ()
forLoggedIn_ = void . forLoggedIn

--------------------------------------------------------------------------------

setProjects :: [Entity ProjectClient] -> App ()
setProjects ps =
  forLoggedIn' $ \liSt ->
    pure $ liSt & stateProjectView .~ StateProjectOverview projectsMap
      where
        projectsMap = Map.fromList $ map entityToTuple ps

setProjectDetailState :: ProjectDetailState -> App ()
setProjectDetailState pdSt =
  stateSession . _StateLoggedIn . stateProjectView .= StateProjectDetail pdSt

setProjectOverview :: SessionKey -> App ()
setProjectOverview sKey = do
  projects <- ajax' $ request api (Proxy :: Proxy Api.ProjectList) (session sKey)
  setProjects projects

ajax' :: NFData a => (HandleResponse a -> IO ()) -> App a
ajax' go = ajax go >>= \case
  Left (401, e) -> do
    showMessage $ Message.SetWarning $
      "Unauthorized. Are you logged in?" <>
      " (401) " <> Text.pack e
    halt
  Left (n, e) -> do
    showMessage $ Message.SetError $
      " (" <> (Text.pack . show) n <> ") " <> Text.pack e
    halt
  Right x -> pure x

performLogin :: UserInfo -> App ()
performLogin userInfo@(UserInfo _ _ sKey) = do
  liftIO $ persistSession sKey
  setProjectOverview sKey
  stateSession .= StateLoggedIn (mkLoggedInState sKey userInfo)

showMessage :: Message.Action -> App ()
showMessage a = stateMessage .= Message.runAction a

withRowCache :: Id Table -> RowCache.Action -> App ()
withRowCache tableId a =
  forProjectDetail' $ \_ pdSt ->
    pure $ pdSt &
      stateCacheRows . at tableId . _Just %~ RowCache.runAction tableId a

--------------------------------------------------------------------------------

store :: ReactStore State
store = mkStore State
  { _stateWebSocket = Nothing
  , _stateMessage   = Nothing
  , _stateSession   = StateLoggedOut LoggedOutUninitialized
  }

dispatch :: Action -> [SomeStoreAction]
dispatch a = [SomeStoreAction store $ freeFluxDispatch a]

type App = FreeFlux State

instance StoreData State where
  type StoreAction State = FreeFluxAction State Action
  transform = freeFluxTransform store update

update :: Action -> App ()
update = \case

  MessageAction a ->
    stateMessage .= Message.runAction a

  GlobalInit wsUrl -> do
    ws <- jsonWebSocketNew wsUrl $ pure . dispatch . \case
      -- WsDownCellsChanged cs    -> TableUpdateCells cs
      -- WsDownColumnsChanged cs  -> TableUpdateColumns cs
      WsDownRowCreated t r dat -> undefined -- RowCacheAction t $ RowCache.Add r dat
      WsDownRowDeleted t r     -> undefined -- RowCacheAction t $ RowCache.Delete r
    stateWebSocket .= Just ws

    use stateSession >>= \case

      StateLoggedIn liSt ->
        setProjectOverview (liSt ^. stateSessionKey)

      StateLoggedOut LoggedOutUninitialized ->
        liftIO recoverSession >>= \case
          Just sessionKey -> do
            result <- ajax' $ request api (Proxy :: Proxy Api.AuthGetUserInfo) sessionKey
            case result of
              GetUserInfoSuccess userInfo -> do
                setProjectOverview sessionKey
                stateSession .= StateLoggedIn (mkLoggedInState sessionKey userInfo)
              GetUserInfoFailed _ -> do
                liftIO clearSession
                stateSession .= StateLoggedOut LoggedOutLoginForm
                showMessage $ Message.SetWarning "Failed to restore local session"
          Nothing ->
            stateSession .= StateLoggedOut LoggedOutLoginForm

      StateLoggedOut _ -> pure ()

  GlobalSendWebSocket msg -> do
    mWS <- use stateWebSocket
    for_ mWS $ jsonWebSocketSend msg

  -- Session -------------------------------------------------------------------

  Signup signupData -> do
    result <- ajax' $ request api (Proxy :: Proxy Api.AuthSignup) signupData
    case result of
      SignupSuccess userInfo -> do
        performLogin userInfo
        showMessage $ Message.SetSuccess "Successfully signed up."
      SignupFailed txt ->
        showMessage $ Message.SetWarning txt

  ToSignupForm ->
    stateSession .= StateLoggedOut LoggedOutSignupForm

  ToLoginForm ->
    stateSession .= StateLoggedOut LoggedOutLoginForm

  Login loginData -> do
    result <- ajax' $ request api (Proxy :: Proxy Api.AuthLogin) loginData
    case result of
      LoginSuccess userInfo -> do
        performLogin userInfo
        showMessage $ Message.SetSuccess "Successfully logged in."
      LoginFailed txt ->
        showMessage $ Message.SetWarning txt

  Logout -> do
    liftIO $ clearSession
    forLoggedIn_ $ \liSt -> do
      ajax' $ request api (Proxy :: Proxy Api.AuthLogout)
                          (session $ liSt ^. stateSessionKey)
      showMessage $ Message.SetSuccess "Successfully logged out."
      stateSession .= StateLoggedOut LoggedOutLoginForm

  -- Cache ---------------------------------------------------------------------

  RowCacheGet tableId ->
    forProjectDetail_ $ \sKey pdSt ->
      case pdSt ^. stateCacheRows . at tableId of
        Just _  -> pure ()
        -- cache not yet initialized: no key in map => Nothing
        Nothing -> do
          records <- ajax' $ request api (Proxy :: Proxy Api.RowListWithData)
            (session sKey) tableId
          withRowCache tableId $ RowCache.Set records

  -- Project Overview ----------------------------------------------------------

  SetProjectOverview sKey ->
    setProjectOverview sKey

  ProjectsCreate name ->
    forLoggedIn' $ \liSt -> do
      Entity i p <- ajax' $ request api (Proxy :: Proxy Api.ProjectCreate)
                                        (session $ liSt ^. stateSessionKey) name
      pure $ liSt &
        stateProjectView .~ StateProjectDetail (mkProjectDetailState i p)

  ProjectsLoadProject i ->
    forLoggedIn_ $ \liSt -> do
      (p, ts) <- ajax' $ request api (Proxy :: Proxy Api.ProjectLoad)
                 (session $ liSt ^. stateSessionKey) i
      -- Load first table if exists
      case ts of
        []               -> pure ()
        Entity tId _ : _ -> liftIO $ alterStore store $ freeFluxDispatch $ TablesLoadTable tId

      let tablesMap = Map.fromList $ map entityToTuple ts
      setProjectDetailState $
        mkProjectDetailState i p & stateTables .~ tablesMap

  ProjectSetName name ->
    forProjectDetail' $ \sKey pdSt -> do
      ajax' $ request api (Proxy :: Proxy Api.ProjectSetName)
                          (session sKey) (pdSt ^. stateProjectId) name
      pure $ pdSt & stateProject . projectClientName .~ name

  ProjectDelete projectId ->
    forLoggedIn_ $ \liSt -> do
      let sKey = liSt ^. stateSessionKey
      ajax' $ request api (Proxy :: Proxy Api.ProjectDelete)
                          (session sKey) projectId
      liftIO $ alterStore store $ freeFluxDispatch $ SetProjectOverview sKey

  -- Tables --------------------------------------------------------------------

  TablesCreate projectId name ->
    forProjectDetail_ $ \sKey _ ->
      ajax' $ request api (Proxy :: Proxy Api.TableCreate)
                          (session sKey) projectId name

  -- TODO: move to websocket event
  -- TablesAdd (Entity i t) ->
  --   forProjectDetail st $ \_ pdSt ->
  --     pure $ pdSt & stateTables . at i .~ Just t
  --                 & stateTableId .~ Just i

  TablesLoadTable tableId ->
    forProjectDetail' $ \sKey pdSt -> do
      (cols, rows, cells) <- ajax' $
        request api (Proxy :: Proxy Api.TableGetWhole) (session sKey) tableId
      pure $ pdSt & stateColumns .~ Map.fromList (map entityToTuple cols)
                  & stateRows    .~ Map.fromList (map entityToTuple rows)
                  & stateCells   .~ fillEntries cells Map.empty
                  & stateTableId .~ Just tableId

  -- Table

  -- TODO: move to websocket event
  -- TableUpdateCells cells ->
  --   forProjectDetail st $ \_ pdSt -> pure $
  --     let toEntry (Cell content _ c r) = (c, r, content)
  --         setRowInCache pdSt'' (Cell content (Aspects t c r)) =
  --           pdSt'' & stateCacheRows . at t . _Just
  --                . RowCache.recordCache . at r . _Just
  --                . at c . _Just . _2 .~ content
  --         pdSt' = foldl' setRowInCache pdSt cells
  --     in pdSt' & stateCells %~ fillEntries (map toEntry cells)

  -- TODO: move to websocket event
  -- TableUpdateColumns entities ->
  --   forProjectDetail st $ \_ pdSt ->
  --       pure $ pdSt & stateColumns %~ \cols ->
  --         foldl' acc cols $ filter (tableColumn pdSt) entities
  --   where
  --     tableColumn pdSt (Entity _ column) =
  --       pdSt ^. stateTableId == Just (column ^. columnTableId)
  --     acc cols' (Entity columnId column) =
  --       Map.insert columnId column cols'

  TableCreateDataCol ->
    forProjectDetail_ $ \sKey pdSt ->
      for_ (pdSt ^. stateTableId) $ \tableId ->
        ajax' $ request api (Proxy :: Proxy Api.DataColCreate)
                            (session sKey) tableId

  TableCreateReportCol ->
    forProjectDetail_ $ \sKey pdSt ->
      for_ (pdSt ^. stateTableId) $ \tableId ->
        ajax' $ request api (Proxy :: Proxy Api.ReportColCreate)
                            (session sKey) tableId

  -- TODO: move to websocket event
  -- TableAddColumnDone (Entity i c, cells) ->
  --   forProjectDetail st $ \_ pdSt -> pure $
  --     pdSt & stateColumns %~ Map.insert i c
  --          & stateCells %~ fillEntries (map toCellUpdate cells)

  TableRenameColumn columnId name ->
    forProjectDetail_ $ \sKey _ ->
      ajax' $ request api (Proxy :: Proxy Api.ColumnSetName)
                          (session sKey) columnId name

  TableUpdateDataCol columnId dt inpTyp src ->
    forProjectDetail_ $ \sKey _ ->
      ajax' $ request api (Proxy :: Proxy Api.DataColUpdate)
                          (session sKey) columnId (dt, inpTyp, src)

  TableUpdateReportCol columnId template format lang ->
    forProjectDetail_ $ \sKey _ ->
      ajax' $ request api (Proxy :: Proxy Api.ReportColUpdate)
                          (session sKey) columnId (template, format, lang)

  TableDeleteColumn columnId ->
    forProjectDetail_ $ \sKey _ ->
      ajax' $ request api (Proxy :: Proxy Api.ColumnDelete)
                          (session sKey) columnId
      -- TODO: move to websocket event
      -- pure $ pdSt & stateColumns %~ Map.delete i
      --             & stateCells %~ Map.filterWithKey (\(Coords c _) _ -> c /= i)

  TableAddRow ->
    forProjectDetail_ $ \sKey pdSt ->
      for_ (pdSt ^. stateTableId) $ \tableId ->
        ajax' $ request api (Proxy :: Proxy Api.RowCreate)
                            (session sKey) tableId

  -- TODO: move to websocket event
  -- TableAddRowDone (Entity i r, cells) ->
  --   forProjectDetail st $ \_ pdSt -> pure $
  --     pdSt & stateRows %~ Map.insert i r
  --          & stateCells %~ fillEntries (map toCellUpdate cells)

  TableDeleteRow rowId ->
    forProjectDetail_ $ \sKey _ ->
      ajax' $ request api (Proxy :: Proxy Api.RowDelete)
                          (session sKey) rowId

      -- TODO: move to websocket event
      -- pure $ pdSt & stateRows %~ Map.delete i
      --             & stateCells %~ Map.filterWithKey (\(Coords _ r) _ -> r /= i)

  TableSetName tableId name ->
    forProjectDetail_ $ \sKey _ ->
      ajax' $ request api (Proxy :: Proxy Api.TableSetName)
                          (session sKey) tableId name
      -- TODO: move to websocket event
      -- pure $ pdSt & stateTables . at i . _Just . tableName .~ name

  TableDelete tableId ->
    forProjectDetail_ $ \sKey _ ->
      ajax' $ request api (Proxy :: Proxy Api.TableDelete)
                          (session sKey) tableId

      -- TODO: to websocket event
      -- if pdSt ^. stateTableId == Just tableId
      --   then do
      --     let nextTable = Map.lookupLT tableId (pdSt ^. stateTables)
      --                 <|> Map.lookupGT tableId (pdSt ^. stateTables)


      --         st' = setProjectDetailState st $ pdSt
      --                 & stateTables %~ Map.delete tableId
      --                 & stateColumns .~ Map.empty
      --                 & stateCells .~ Map.empty
      --                 & stateRows .~ Map.empty
      --                 & stateTableId .~ (fst <$> nextTable)

      --     case nextTable of
      --       Just (nextTableId, _) ->
      --         React.Flux.transform (TablesLoadTable nextTableId) st'
      --       Nothing -> pure st'
      --   else
      --     pure $ setProjectDetailState st $ pdSt & stateTables %~ Map.delete tableId

  -- Cell

  CellSetValue c r val ->
    forProjectDetail_ $ \sKey _ ->
      ajax' $ request api (Proxy :: Proxy Api.CellSet)
                          (session sKey) c r val

fillEntries :: [(Id Column, Id Row, a)] -> Map Coords a -> Map Coords a
fillEntries entries m = foldl' acc m $ map toCoords entries
  where
    toCoords (colId, recId, val) = (Coords colId recId, val)
    acc m' (c, v) = Map.insert c v m'

toCellUpdate :: Entity Cell -> (Id Column, Id Row, CellContent)
toCellUpdate (Entity _ (Cell content _ c r)) = (c, r, content)
