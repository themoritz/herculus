{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Store where

import           Control.DeepSeq           (NFData)
import           Control.Lens              hiding (op)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.State       (get)

import           Data.Foldable             (for_)
import           Data.Monoid               ((<>))
import           Data.Proxy
import           Data.Text                 (Text, pack, unpack)

import           GHC.Generics

import           React.Flux
import           React.Flux.Addons.Free
import           React.Flux.Addons.Servant (request)

import qualified Lib.Api.Rest              as Api
import           Lib.Api.WebSocket         (WsDownMessage (..),
                                            WsUpMessage (..))
import           Lib.Model.Auth            (GetUserInfoResponse (..),
                                            LoginData (..), LoginResponse (..),
                                            SignupData (..),
                                            SignupResponse (..),
                                            UserInfo (UserInfo))

import qualified LoggedIn
import qualified Project
import qualified Store.Message             as Message
import           Store.Session             (clearSession, persistSession,
                                            recoverSession)
import           Store.Types
import           WebSocket

--------------------------------------------------------------------------------

data State = State
  -- session-agnostic state
  { _stateWebSocket :: Maybe JSWebSocket
  , _stateMessage   :: Message.State
  -- session state
  , _stateSession   :: SessionState
  } deriving (Show)

data SessionState
  = StateLoggedIn  LoggedIn.State
  | StateLoggedOut LoggedOutState
  deriving (Show)

data LoggedOutState
  = LoggedOutLoginForm
  | LoggedOutSignupForm
  | LoggedOutUninitialized
  deriving (Show)

makeLenses ''State
makePrisms ''SessionState

--------------------------------------------------------------------------------

data Action
  -- Global
  = Init Text -- WebSocket URL
  | WebSocketApplyMsg WsDownMessage
  | WebSocketOpened
  | WebSocketClosed
  | WebSocketSend WsUpMessage
  -- Session
  | ToSignupForm
  | ToLoginForm
  | Signup SignupData
  | Login LoginData
  | MessageAction Message.Action
  | LoggedInAction LoggedIn.Action
  deriving (Generic, Show)

instance NFData Action

--------------------------------------------------------------------------------

store :: ReactStore State
store = mkStore State
  { _stateWebSocket = Nothing
  , _stateMessage   = Nothing
  , _stateSession   = StateLoggedOut LoggedOutUninitialized
  }

dispatch :: Action -> [SomeStoreAction]
dispatch a = [SomeStoreAction store $ freeFluxDispatch a]

dispatchLoggedIn :: LoggedIn.Action -> [SomeStoreAction]
dispatchLoggedIn = dispatch . LoggedInAction

dispatchProject :: Project.Action -> [SomeStoreAction]
dispatchProject = dispatchLoggedIn . LoggedIn.ProjectAction

dispatchProjectCommand :: Api.Command -> [SomeStoreAction]
dispatchProjectCommand = dispatchProject . Project.RunCommand

type DSL = FreeFlux State

instance MonadStore DSL where
  apiCall go = ajax go >>= \case
    Left (401, e) -> do
      let msg = "Unauthorized. Are you logged in? (401) " <> e
      showMessage $ Message.SetWarning $ pack msg
      halt msg
    Left (n, e) -> do
      let msg = " (" <> show n <> ") " <> e
      showMessage $ Message.SetError $ pack msg
      halt msg
    Right x -> pure x

  sendWS = eval . WebSocketSend

  showMessage a = stateMessage .= Message.runAction a

  haltMessage msg = do
    showMessage $ Message.SetError msg
    halt (unpack msg)

instance StoreData State where
  type StoreAction State = FreeFluxAction State Action
  transform = freeFluxTransform store eval

--------------------------------------------------------------------------------

eval :: Action -> DSL ()
eval = \case

  Init wsUrl -> do
    ws <- liftIO $ jsonWebSocketNew wsUrl
      (pure . dispatch . WebSocketApplyMsg)
      (pure . dispatch $ WebSocketOpened)
      (pure . dispatch $ WebSocketClosed)
    stateWebSocket .= Just ws

    use stateSession >>= \case

      StateLoggedIn _ ->
        eval $ LoggedInAction LoggedIn.ToProjectOverview

      StateLoggedOut LoggedOutUninitialized ->
        liftIO recoverSession >>= \case
          Just sessionKey ->
            apiCall (request api (Proxy :: Proxy Api.AuthGetUserInfo)
                                 sessionKey
                    ) >>= \case
              GetUserInfoSuccess userInfo -> performLogin userInfo
              GetUserInfoFailed _ -> do
                liftIO clearSession
                stateSession .= StateLoggedOut LoggedOutLoginForm
                showMessage $
                  Message.SetWarning "Failed to restore local session"
          Nothing ->
            stateSession .= StateLoggedOut LoggedOutLoginForm

      StateLoggedOut _ -> pure ()

  WebSocketApplyMsg msg -> case msg of

    WsDownAuthResponse response -> case response of
      GetUserInfoFailed err -> showMessage $ Message.SetWarning $
        "WebSocket authentication failed: " <> err
      _ -> pure ()

    WsDownSubscribeError err -> showMessage $ Message.SetWarning $
      "Subscribing to project failed: " <> err

    WsDownProjectDiff projectId cellDiff columnDiff rowDiff tableDiff -> do
      st <- use stateSession
      if st ^? _StateLoggedIn
             . LoggedIn.stateSubState . LoggedIn._ProjectDetail
             . Project.stateProjectId == Just projectId
        then eval $ LoggedInAction $ LoggedIn.ProjectAction $
               Project.ApplyDiff cellDiff columnDiff rowDiff tableDiff
        else showMessage $ Message.SetWarning
               "Received projectDiff while not viewing that project."

  WebSocketOpened -> do
    showMessage Message.Unset
    -- We need to authenticate the new websocket connection
    st <- use stateSession
    case st of
      StateLoggedIn liSt -> do
        sendWS $ WsUpAuthenticate (liSt ^. LoggedIn.stateSessionKey)
        -- If the user has a project opened, we reload it to catch up with
        -- potential changes that happened during the time the websocket
        -- connection was lost.
        case liSt ^. LoggedIn.stateSubState of
          LoggedIn.ProjectDetail pdSt -> do
            let projectId = pdSt ^. Project.stateProjectId
            sendWS $ WsUpSubscribe projectId
            eval $ LoggedInAction $ LoggedIn.ToProject projectId
          _ -> pure ()
      _ -> pure ()

  WebSocketClosed ->
    showMessage $ Message.SetWarning
      "WebSocket connection lost. Trying to reconnect..."

  WebSocketSend msg -> do
    mWS <- use stateWebSocket
    liftIO $ for_ mWS $ jsonWebSocketSend msg

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

  MessageAction a ->
    stateMessage .= Message.runAction a

  LoggedInAction action -> LoggedIn.run env action
    where
      env = LoggedIn.Env
        { LoggedIn.accessState = \f -> do
            st <- get
            case st ^? stateSession . _StateLoggedIn of
              Just p -> do
                let (a, p') = f p
                stateSession . _StateLoggedIn .= p'
                pure a
              Nothing ->
                haltMessage $ "Inconsistent client state: "
                           <> "Cannot access loggedin state."
        , LoggedIn.toLoginForm = do
            liftIO clearSession
            eval ToLoginForm
        }

performLogin :: UserInfo -> DSL ()
performLogin userInfo@(UserInfo _ _ sKey) = do
  liftIO $ persistSession sKey
  stateSession .= StateLoggedIn (LoggedIn.mkState sKey userInfo)
  sendWS $ WsUpAuthenticate sKey
  eval $ LoggedInAction LoggedIn.ToProjectOverview
