module Herculus.WebSocket where

import Prelude
import Control.Coroutine as CR
import Halogen as H
import Halogen.HTML as HH
import Halogen.Query.EventSource as ES
import WebSocket as WS
import Control.Monad.Aff (Aff, forkAff, later', runAff)
import Control.Monad.Aff.AVar (AVar, makeVar, peekVar, putVar, takeVar)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (Ref, newRef, readRef, writeRef)
import Control.Monad.Eff.Var (set)
import Control.Monad.Rec.Class (forever)
import Control.Monad.State (get, put)
import Control.Monad.Trans.Class (lift)
import DOM.Websocket.Event.Types (MessageEvent)
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Generic.Aeson (decodeJson, encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Generic (class Generic)
import Data.Maybe (Maybe(..))
import Halogen.Query.HalogenM (halt)
import Herculus.Monad (Herc, HercEffects, HercEnv)

data Query mi a
  = Initialize a
  | Finalize a
  | Connect a
  | Send mi a
  | OnOpen a
  | OnClose a
  | OnMessage MessageEvent (H.SubscribeStatus -> a)

type Vars =
  { queue :: AVar String -- TODO: use a proper queue
  , connection :: Ref (Maybe WS.Connection)
  , open :: AVar Unit
  , closed :: Ref Boolean
  }

type State = Maybe Vars

data Output mo
  = Opened
  | Closed
  | Message mo

webSocket
  :: forall mi mo
   . (Generic mi, Generic mo)
  => String -> HercEnv
  -> H.Component HH.HTML (Query mi) Unit (Output mo) Herc
webSocket url { notify } = H.lifecycleComponent
  { initialState: const Nothing
  , receiver: const Nothing
  , render: const (HH.text "")
  , eval
  , initializer: Just (H.action Initialize)
  , finalizer: Just (H.action Finalize)
  }

  where

  eval :: Query mi ~> H.ComponentDSL State (Query mi) (Output mo) Herc
  eval (Initialize next) = do
    queue <- liftAff makeVar
    open <- liftAff makeVar
    conn <- liftEff $ newRef Nothing
    closed <- liftEff $ newRef true
    put $ Just
      { queue: queue
      , open: open
      , connection: conn
      , closed: closed
      }
    -- Consume message queue
    liftAff $ forkAff $ forever do
      m <- peekVar queue
      peekVar open
      mConn <- liftEff $ readRef conn
      case mConn of
        Nothing -> pure unit
        Just (WS.Connection socket) -> do
          msg <- takeVar queue
          liftEff $ socket.send (WS.Message msg)
    eval (Connect next)

  eval (Finalize next) = do
    st <- get
    case st of
      Nothing -> halt "WebSocket not initialized."
      Just vars -> do
        mConn <- liftEff $ readRef vars.connection
        case mConn of
          Nothing -> pure unit
          Just (WS.Connection socket) -> liftEff socket.close
    pure next

  eval (Connect next) = do
    st <- get
    case st of
      Nothing -> halt "WebSocket not initialized."
      Just vars ->
        H.subscribe $ ES.hoist liftAff $ ES.EventSource $
          pure { producer: wsService url vars
               , done: pure unit
               }
    pure next

  eval (Send msg next) = do
    st <- get
    case st of
      Nothing -> halt "Websocket message bus not ready."
      Just vars ->
        liftAff $ putVar vars.queue $ stringify $ encodeJson msg
    pure next

  eval (OnOpen next) = do
    H.raise Opened
    pure next

  eval (OnClose next) = do
    H.raise Closed
    pure next

  eval (OnMessage event reply) = do
    let
      message = WS.runMessage $ WS.runMessageEvent event
    case jsonParser message >>= decodeJson of
      Left e -> notify e
      Right mo -> H.raise (Message mo)
    pure $ reply ES.Listening

wsService
  :: forall mi
   . String
  -> Vars
  -> CR.Producer (Query mi ES.SubscribeStatus) (Aff HercEffects) Unit
wsService url vars = do
  conn@(WS.Connection socket) <- lift $ liftEff $ do
    c <- WS.newWebSocket (WS.URL url) []
    writeRef vars.connection (Just c)
    pure c
  let
    runAff' = runAff (const (pure unit)) pure
  ES.produce \emit -> do
    -- Set `open` to true on open
    set socket.onopen \_ -> do
      runAff' $ do
        putVar vars.open unit
        liftEff $ writeRef vars.closed false
      emit (Left (OnOpen ES.Listening))
    -- Emit `Received` on message
    set socket.onmessage \event ->
      emit (Left (OnMessage event id))
    -- Set `open` to false on close and reconnect
    set socket.onclose \_ -> do
      runAff' $ do
        cannotClose <- liftEff $ readRef vars.closed
        if cannotClose
          then pure unit
          else do takeVar vars.open
                  liftEff $ writeRef vars.closed true
      writeRef vars.connection Nothing
      emit (Left (OnClose ES.Listening))
      runAff' $ later' 1000 $
        liftEff $ emit (Left (Connect ES.Done))
      pure unit
