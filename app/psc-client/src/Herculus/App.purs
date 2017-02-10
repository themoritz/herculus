module Herculus.App where

import Herculus.Prelude
import Control.Monad.Aff.Bus as Bus
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Herculus.Ace as Ace
import Herculus.Play as Play
import Herculus.WebSocket as WebSocket
import Control.Coroutine (emit)
import Control.Monad.Aff.Console (log)
import Control.Monad.Rec.Class (forever)
import Halogen.Component.ChildPath (type (\/), type (<\/>))
import Herculus.Monad (ApiT, Herc, HercEnv, getApiUrl, getAuthToken, getWebSocketUrl, runApiT)
import Lib.Api.WebSocket (WsDownMessage, WsUpMessage(..))
import Servant.PureScript.Affjax (errorToString)

data Query a
  = Initialize a
  | Notify String a
  | Test a
  | PrintWsMessage (WebSocket.Output WsDownMessage) a
  | SetText String a

type Wiring =
  { notificationBus :: Bus.BusRW String
  , webSocketUrl :: String
  }

type State =
  { error :: String
  , msg :: String
  , wiring :: Maybe Wiring
  , text :: String
  }

type ChildQuery =
      WebSocket.Query WsUpMessage
 <\/> Play.Query
 <\/> Ace.Query
 <\/> Const Void

type ChildSlot = Unit \/ Unit \/ Unit \/ Void

app :: forall i o. H.Component HH.HTML Query i o Herc
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
      , text: "Editor"
      , wiring: Nothing
      }

    render :: State -> H.ParentHTML Query ChildQuery ChildSlot Herc
    render st = case st.wiring of
      Nothing -> HH.text "loading"
      Just { webSocketUrl, notificationBus } -> HH.div_
        [ HH.slot' CP.cp2 unit (Play.play $ env notificationBus) unit absurd
        , HH.hr_
        , HH.text st.error
        , HH.hr_
        , HH.button
          [ HE.onClick (HE.input_ Test) ]
          [ HH.text "Send!" ]
        , HH.text st.msg
        , let
            ws = WebSocket.webSocket webSocketUrl
                                     (env notificationBus)
          in
            HH.slot' CP.cp1 unit ws unit (Just <<< H.action <<< PrintWsMessage)
        , HH.hr_
        , HH.input
            [ HE.onValueInput (HE.input SetText)
            , HP.value st.text
            ]
        , HH.slot' CP.cp3 unit Ace.ace st.text $ case _ of
            Ace.TextChanged t -> Just $ H.action $ SetText t
        ]

    eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot o Herc
    eval (Initialize next) = do
      liftAff $ log "Hallo"
      bus <- liftAff Bus.make
      webSocketUrl <- lift getWebSocketUrl
      modify _{ wiring = Just
                  { notificationBus: bus
                  , webSocketUrl: webSocketUrl
                  }
              }
      H.subscribe $ ES.EventSource do
        let go = forever do
              e <- lift $ liftAff $ Bus.read bus
              emit (Notify e ES.Listening)
        pure { producer: go, done: pure unit }
      pure next

    eval (Notify e next) = do
      modify _{ error = e }
      pure next

    eval (Test next) = do
      H.query' CP.cp1 unit (H.action (WebSocket.Send $ WsUpAuthenticate "ff"))
      pure next

    eval (PrintWsMessage msg next) = do
      modify _{ msg = case msg of
                     WebSocket.Message _ -> "received message"
                     WebSocket.Opened -> "opened"
                     WebSocket.Closed -> "closed"
                }
      pure next

    eval (SetText text next) = do
      modify _{ text = text }
      pure next

-- Utils -----------------------------------------------------------------------

env :: forall r. Bus.BusW' r String -> HercEnv
env bus =
  { withApi
  , notify
  }

  where
  notify
    :: forall s i o
     . String -> H.ComponentDSL s i o Herc Unit
  notify msg = liftAff $ Bus.write msg bus

  withApi
    :: forall s i o a
     . ApiT Herc a
    -> (a -> H.ComponentDSL s i o Herc Unit)
    -> H.ComponentDSL s i o Herc Unit
  withApi call handler = do
    apiUrl <- lift getApiUrl
    token <- lift getAuthToken
    result <- H.lift $ runApiT apiUrl token call
    case result of
      Left e -> notify $ errorToString e
      Right a -> handler a
