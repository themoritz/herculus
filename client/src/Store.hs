{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Store where

import           Control.Concurrent        (forkIO)
import           Control.Lens              hiding (op)
import           Control.Monad             (void)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.State       (get)

import           Data.Foldable             (for_)
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Monoid               ((<>))
import           Data.Proxy
import qualified Data.Text                 as Text

import           React.Flux
import           React.Flux.Addons.Free
import           React.Flux.Addons.Servant (request)

import qualified Lib.Api.Rest              as Api
import           Lib.Api.WebSocket         (WsDownMessage (..))
import           Lib.Model
import           Lib.Model.Auth            (ChangePwdResponse (..),
                                            GetUserInfoResponse (..),
                                            LoginResponse (..), SessionKey,
                                            SignupResponse (..),
                                            UserInfo (UserInfo))
import           Lib.Model.Cell
import           Lib.Model.Column
import           Lib.Model.Project
import           Lib.Types

import           Action                    (Action (..), session)
import qualified Project
import qualified Store.Message             as Message
import           Store.Session             (clearSession, persistSession,
                                            recoverSession)
import           Store.Types
import           WebSocket

data CellInfo = CellInfo
  { ciCol     :: Column
  , ciContent :: CellContent
  } deriving (Eq, Show)

data State = State
  -- session-agnostic state
  { _stateWebSocket :: Maybe JSWebSocket
  , _stateMessage   :: Message.State
  -- session state
  , _stateSession   :: SessionState
  } deriving (Show)

data SessionState
  = StateLoggedIn  LoggedInState
  | StateLoggedOut LoggedOutState
  deriving (Show)

  -- data LoggedInState = LoggedInState

data LoggedInState = LoggedInState
  { _liStUserInfo         :: UserInfo
  , _liStSessionKey       :: SessionKey
  , _liStSubState         :: LoggedInSubState
  , _liStUserSettingsShow :: Bool
  } deriving (Show)

mkLoggedInState :: SessionKey -> UserInfo -> LoggedInState
mkLoggedInState sKey userInfo = LoggedInState
  { _liStUserInfo         = userInfo
  , _liStSessionKey       = sKey
  , _liStSubState         = LiStProjectOverview Map.empty
  , _liStUserSettingsShow = False
  }

data LoggedInSubState
  = LiStProjectOverview ProjectOverviewState
  | LiStProjectDetail Project.State
  | LiStChangePassword Bool -- Bool flag: True: valid old password or initial, False -> invalid old password
  deriving (Show)

type ProjectOverviewState = Map (Id ProjectClient) ProjectClient

data LoggedOutState
  = LoggedOutLoginForm
  | LoggedOutSignupForm
  | LoggedOutUninitialized
  deriving (Show)

makeLenses ''State
makeLenses ''LoggedInState
makePrisms ''SessionState
makePrisms ''LoggedInSubState

--------------------------------------------------------------------------------

-- | Execute action in case the user is logged in and error otherwise
-- throw an error.
forLoggedIn :: (LoggedInState -> DSL a) -> DSL a
forLoggedIn action = use stateSession >>= \case
  StateLoggedIn  liSt -> action liSt
  StateLoggedOut _    -> do
    let msg = "inconsistent client state: unexpected: not logged in"
    showMessage $ Message.SetError $ Text.pack msg
    halt msg

forLoggedIn' :: (LoggedInState -> DSL LoggedInState) -> DSL ()
forLoggedIn' action = do
  new <- forLoggedIn action
  stateSession .= StateLoggedIn new

forLoggedIn_ :: (LoggedInState -> DSL a) -> DSL ()
forLoggedIn_ = void . forLoggedIn

--------------------------------------------------------------------------------

setProjects :: [Entity ProjectClient] -> DSL ()
setProjects ps =
  forLoggedIn' $ \liSt ->
    pure $ liSt & liStSubState .~ LiStProjectOverview projectsMap
      where
        projectsMap = Map.fromList $ map entityToTuple ps

setProjectDetailState :: Project.State -> DSL ()
setProjectDetailState pdSt =
  stateSession . _StateLoggedIn . liStSubState .= LiStProjectDetail pdSt

setProjectOverview :: SessionKey -> DSL ()
setProjectOverview sKey = do
  projects <- apiCall $ request api (Proxy :: Proxy Api.ProjectList) (session sKey)
  setProjects projects

performLogin :: UserInfo -> DSL ()
performLogin userInfo@(UserInfo _ _ sKey) = do
  liftIO $ persistSession sKey
  stateSession .= StateLoggedIn (mkLoggedInState sKey userInfo)
  setProjectOverview sKey

showMessage :: Message.Action -> DSL ()
showMessage a = stateMessage .= Message.runAction a


--------------------------------------------------------------------------------

store :: ReactStore State
store = mkStore State
  { _stateWebSocket = Nothing
  , _stateMessage   = Nothing
  , _stateSession   = StateLoggedOut LoggedOutUninitialized
  }

dispatch :: Action -> [SomeStoreAction]
dispatch a = [SomeStoreAction store $ freeFluxDispatch a]

type DSL = FreeFlux State

instance MonadStore DSL where
  apiCall go = ajax go >>= \case
    Left (401, e) -> do
      let msg = "Unauthorized. Are you logged in? (401) " <> e
      showMessage $ Message.SetWarning $ Text.pack msg
      halt msg
    Left (n, e) -> do
      let msg = " (" <> show n <> ") " <> e
      showMessage $ Message.SetError $ Text.pack msg
      halt msg
    Right x -> pure x

instance Project.HasProjectState DSL where
  projectState f = do
    st <- get
    case st ^? stateSession . _StateLoggedIn . liStSubState . _LiStProjectDetail of
      Just p -> do
        let (a, p') = f p
        stateSession . _StateLoggedIn . liStSubState . _LiStProjectDetail .= p'
        pure a
      Nothing -> do
        let msg = "inconsistent client state. cannot access project detail"
        showMessage $ Message.SetError $ Text.pack msg
        halt msg

instance StoreData State where
  type StoreAction State = FreeFluxAction State Action
  transform = freeFluxTransform store eval

eval :: Action -> DSL ()
eval = \case

  MessageAction a ->
    stateMessage .= Message.runAction a

  GlobalInit wsUrl -> do
    ws <- jsonWebSocketNew wsUrl $ pure . dispatch . ApplyWebSocketMsg
    stateWebSocket .= Just ws

    use stateSession >>= \case

      StateLoggedIn liSt ->
        setProjectOverview (liSt ^. liStSessionKey)

      StateLoggedOut LoggedOutUninitialized ->
        liftIO recoverSession >>= \case
          Just sessionKey -> do
            result <- apiCall $ request api (Proxy :: Proxy Api.AuthGetUserInfo) sessionKey
            case result of
              GetUserInfoSuccess userInfo -> do
                stateSession .= StateLoggedIn (mkLoggedInState sessionKey userInfo)
                setProjectOverview sessionKey
              GetUserInfoFailed _ -> do
                liftIO clearSession
                stateSession .= StateLoggedOut LoggedOutLoginForm
                showMessage $ Message.SetWarning "Failed to restore local session"
          Nothing ->
            stateSession .= StateLoggedOut LoggedOutLoginForm

      StateLoggedOut _ -> pure ()

  ApplyWebSocketMsg msg -> case msg of
    WsDownProjectDiff cellDiff columnDiff rowDiff tableDiff ->
      runProjectAction $ Project.ApplyDiff cellDiff columnDiff rowDiff tableDiff

  GlobalSendWebSocket msg -> do
    mWS <- use stateWebSocket
    for_ mWS $ jsonWebSocketSend msg

  -- Session -------------------------------------------------------------------

  Signup signupData -> do
    result <- apiCall $ request api (Proxy :: Proxy Api.AuthSignup) signupData
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
    result <- apiCall $ request api (Proxy :: Proxy Api.AuthLogin) loginData
    case result of
      LoginSuccess userInfo -> do
        performLogin userInfo
        showMessage $ Message.SetSuccess "Successfully logged in."
      LoginFailed txt ->
        showMessage $ Message.SetWarning txt

  Logout -> do
    liftIO clearSession
    forLoggedIn_ $ \liSt -> do
      apiCall $ request api (Proxy :: Proxy Api.AuthLogout)
                          (session $ liSt ^. liStSessionKey)
      showMessage $ Message.SetSuccess "Successfully logged out."
      stateSession .= StateLoggedOut LoggedOutLoginForm

  ToggleUserSettingsDialog ->
    forLoggedIn' $ \liSt ->
      pure $ liSt & liStUserSettingsShow .~
        not (liSt ^. liStUserSettingsShow)

  ToChangePasswordForm ->
    forLoggedIn' $ \liSt ->
      pure $ liSt & liStSubState .~ LiStChangePassword True

  ChangePassword changePwdData ->
    forLoggedIn' $ \liSt ->
      apiCall (request api (Proxy :: Proxy Api.AuthChangePassword)
                           (session $ liSt ^. liStSessionKey)
                           changePwdData
            ) >>= \case
        ChangePwdSuccess -> do
          showMessage $ Message.SetSuccess "Successfully changed password."
          eval $ SetProjectOverview (liSt ^. liStSessionKey)
          pure liSt
        ChangePwdFailure msg -> do
          showMessage $ Message.SetWarning msg
          pure $ liSt & liStSubState .~ LiStChangePassword False

  -- Project Overview ----------------------------------------------------------

  SetProjectOverview sKey ->
    setProjectOverview sKey

  ProjectsCreate name ->
    forLoggedIn' $ \liSt -> do
      Entity i p <- apiCall $ request api (Proxy :: Proxy Api.ProjectCreate)
                                        (session $ liSt ^. liStSessionKey) name
      pure $ liSt &
        liStSubState .~ LiStProjectDetail (Project.mkState i p)

  ProjectsLoadProject i ->
    forLoggedIn_ $ \liSt -> do
      (p, ts) <- apiCall $ request api (Proxy :: Proxy Api.ProjectLoad)
                 (session $ liSt ^. liStSessionKey) i
      let tablesMap = Map.fromList $ map entityToTuple ts
      setProjectDetailState $
        Project.mkState i p & Project.stateTables .~ tablesMap
      -- Load first table if exists
      case ts of
        []               -> pure ()
        Entity tId _ : _ -> runProjectAction $ Project.LoadTable tId

  ProjectDelete projectId ->
    forLoggedIn_ $ \liSt -> do
      let sKey = liSt ^. liStSessionKey
      apiCall $ request api (Proxy :: Proxy Api.ProjectDelete)
                          (session sKey) projectId
      void $ liftIO $ forkIO $
        alterStore store $ freeFluxDispatch $ SetProjectOverview sKey

  ProjectAction action -> runProjectAction action

runProjectAction :: Project.Action -> DSL ()
runProjectAction action = forLoggedIn_ $ \liSt ->
  Project.run (session $ liSt ^. liStSessionKey) action

