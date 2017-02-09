module Herculus.App where

import Prelude
import Control.Monad.Aff.Bus as Bus
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Herculus.Play as Play
import Herculus.WebSocket as WebSocket
import Control.Coroutine (emit)
import Control.Monad.Reader (ask)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Halogen (liftAff)
import Halogen.Component.ChildPath (type (\/), type (<\/>))
import Halogen.Query.EventSource (EventSource(..), SubscribeStatus(..))
import Herculus.Monad (ApiT, AppM, Env, getAuthToken, runApiT)
import Lib.Api.WebSocket (WsDownMessage, WsUpMessage(..))
import Servant.PureScript.Affjax (errorToString)

data Query a
  = Initialize a
  | Notify String a
  | Test a
  | PrintWsMessage (WebSocket.Message WsDownMessage) a

type Wiring =
  { notificationBus :: Bus.BusRW String
  }

type State =
  { error :: String
  , msg :: String
  , wiring :: Maybe Wiring
  }

type ChildQuery = WebSocket.Query WsUpMessage <\/> Play.Query <\/> Const Void
type ChildSlot = Unit \/ Unit \/ Void

app :: forall i o eff. H.Component HH.HTML Query i o (AppM eff)
app = H.lifecycleParentComponent
  { initialState: const initialState
  , render
  , eval
  , receiver: const Nothing
  , initializer: Just (H.action Initialize)
  , finalizer: Nothing
  }

  where

    initialState :: State
    initialState =
      { error: "None"
      , msg: ""
      , wiring: Nothing
      }

    render :: State -> H.ParentHTML Query ChildQuery ChildSlot (AppM eff)
    render st = case st.wiring of
      Nothing -> HH.text "loading"
      Just { notificationBus } -> HH.div_
        [ HH.slot' CP.cp2 unit (Play.play $ env notificationBus) unit absurd
        , HH.hr_
        , HH.text st.error
        , HH.hr_
        , HH.button
          [ HE.onClick (HE.input_ Test) ]
          [ HH.text "Send" ]
        , HH.text st.msg
        , let
            ws = WebSocket.webSocket "ws://localhost:3000/websocket"
                                     (env notificationBus)
          in
            HH.slot' CP.cp1 unit ws unit (Just <<< H.action <<< PrintWsMessage)
        ]

    eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot o (AppM eff)
    eval (Initialize next) = do
      bus <- liftAff Bus.make
      H.modify _{ wiring = Just { notificationBus: bus } }
      H.subscribe $ EventSource do
        let go = forever do
              e <- lift $ liftAff $ Bus.read bus
              emit (Notify e Listening)
        pure { producer: go, done: pure unit }
      pure next

    eval (Notify e next) = do
      H.modify _{ error = e }
      pure next

    eval (Test next) = do
      H.query' CP.cp1 unit (H.action (WebSocket.Send $ WsUpAuthenticate "ff"))
      pure next

    eval (PrintWsMessage msg next) = do
      H.modify _{ msg = case msg of
                     WebSocket.Message _ -> "received message"
                     WebSocket.Opened -> "opened"
                     WebSocket.Closed -> "closed"
                }
      pure next

-- Utils -----------------------------------------------------------------------

env :: forall r. Bus.BusW' r String -> Env
env bus =
  { withApi
  , notify
  }

  where
  notify
    :: forall s i o eff
     . String -> H.ComponentDSL s i o (AppM eff) Unit
  notify msg = liftAff $ Bus.write msg bus

  withApi
    :: forall s i o eff a
     . ApiT (AppM eff) a
    -> (a -> H.ComponentDSL s i o (AppM eff) Unit)
    -> H.ComponentDSL s i o (AppM eff) Unit
  withApi call handler = do
    baseUrl <- ask
    token <- getAuthToken
    result <- H.lift $ runApiT baseUrl token call
    case result of
      Left e -> notify $ errorToString e
      Right a -> handler a
