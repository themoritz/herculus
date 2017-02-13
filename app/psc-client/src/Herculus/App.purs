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
import Herculus.LogIn as LogIn
import Lib.Api.Schema.Auth (UserInfo(..))
import Lib.Api.WebSocket (WsDownMessage, WsUpMessage(..))

data Query a
  = Notify NotifyTypes.Config a
  | Goto View a

data View
  = LoggedIn UserInfo
  | SignUp
  | LogIn
  | ForgotPassword
  | Initializing

type State =
  { view :: View
  }

type ChildQuery =
      Notify.Query
 <\/> LogIn.Query
 <\/> Const Void

type ChildSlot = Unit \/ Unit \/ Void

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
      { view: LogIn
      }

    render :: State -> H.ParentHTML Query ChildQuery ChildSlot Herc
    render st = case st.view of
      LoggedIn ui -> HH.text "Logged in"
      SignUp -> HH.text "signup"
      LogIn -> HH.slot' CP.cp2 unit LogIn.comp unit \(LogIn.LoggedIn ui) ->
        Just $ Goto (LoggedIn ui) unit
      ForgotPassword -> HH.text "Forgot pw"
      Initializing -> HH.text "Initializing"

    eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot o Herc
    eval (Notify cfg next) = do
      H.query' CP.cp1 unit (H.action $ Notify.Push cfg)
      pure next

    eval (Goto view next) = do
      modify _{ view = view }
      pure next
