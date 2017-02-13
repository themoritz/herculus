module Herculus.App where

import Herculus.Prelude
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Herculus.Ace as Ace
import Herculus.Notifications as Notify
import Herculus.Notifications.Types as NotifyTypes
import Herculus.Play as Play
import Herculus.WebSocket as WebSocket
import Halogen.Component.ChildPath (type (\/), type (<\/>))
import Herculus.Monad (Herc)
import Lib.Api.WebSocket (WsDownMessage, WsUpMessage(..))

data Query a
  = Notify NotifyTypes.Config a
  | Test a
  | PrintWsMessage (WebSocket.Output WsDownMessage) a
  | SetText String a

type State =
  { msg :: String
  , text :: String
  }

type ChildQuery =
      WebSocket.Query WsUpMessage
 <\/> Play.Query
 <\/> Ace.Query
 <\/> Notify.Query
 <\/> Const Void

type ChildSlot = Unit \/ Unit \/ Unit \/ Unit \/ Void

app :: forall i o. H.Component HH.HTML Query i o Herc
app = H.parentComponent
  { initialState: const initialState
  , render
  , eval
  , receiver: const Nothing
  }

  where

    initialState :: State
    initialState =
      { msg: ""
      , text: "Editor"
      }

    render :: State -> H.ParentHTML Query ChildQuery ChildSlot Herc
    render st = HH.div_
        [ HH.slot' CP.cp2 unit Play.play unit absurd
        , HH.hr_
        , HH.slot' CP.cp4 unit Notify.notifications unit absurd
        , HH.hr_
        , HH.button
          [ HE.onClick (HE.input_ Test) ]
          [ HH.text "Send!" ]
        , HH.text st.msg
        , HH.slot' CP.cp1 unit WebSocket.webSocket unit (Just <<< H.action <<< PrintWsMessage)
        , HH.hr_
        , HH.input
            [ HE.onValueInput (HE.input SetText)
            , HP.value st.text
            ]
        , HH.slot' CP.cp3 unit Ace.ace st.text $ case _ of
            Ace.TextChanged t -> Just $ H.action $ SetText t
        ]

    eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot o Herc
    eval (Notify cfg next) = do
      H.query' CP.cp4 unit (H.action (Notify.Push cfg))
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
