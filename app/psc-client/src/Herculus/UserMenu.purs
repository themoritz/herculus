module Herculus.UserMenu where

import Herculus.Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Herculus.PopupMenu as Popup
import Herculus.Router as R
import Data.String (length, take)
import Herculus.Monad (Herc, gotoRoute, setAuthToken)
import Herculus.Utils (faIcon_)
import Lib.Api.Schema.Auth (UserInfo(..))

data Query a
  = Update Input a
  | OnSelect Value a
  | Toggle a

type State = UserInfo

type Input = UserInfo

data Value
  = Logout
  | ChangePW

entries :: String -> Array (Popup.Entry Value)
entries name =
  [ { icon: "power-off"
    , label: "Log out (" <> shortName <> ")"
    , value: Logout
    }
  , { icon: "pencil"
    , label: "Change password"
    , value: ChangePW
    }
  ]
  where
    shortName = case length name > 12 of
      true -> take 9 name <> "..."
      false -> name

comp :: H.Component HH.HTML Query Input Void Herc
comp = H.parentComponent
  { initialState: id
  , receiver: Just <<< H.action <<< Update
  , render
  , eval
  }

  where

  render :: State -> H.ParentHTML Query (Popup.Query Value) Unit Herc
  render (UserInfo ui) = HH.span_
    [ HH.button
      [ HP.class_ (H.ClassName "navigation__button")
      , HE.onClick (HE.input_ Toggle) ]
      [ faIcon_ "cog" ]
    , let
        input =
          { position: Popup.BelowLeft
          , entries: entries ui._uiUserName
          }
      in
        HH.slot unit Popup.comp input (Just <<< H.action <<< OnSelect)
    ]

  eval :: Query ~> H.ParentDSL State Query (Popup.Query Value) Unit Void Herc
  eval (Update inp next) = do
    put inp
    pure next

  eval (OnSelect val next) = do
    case val of
      Logout -> do
        setAuthToken ""
        gotoRoute R.LogIn
      ChangePW ->
        gotoRoute $ R.LoggedIn R.ChangePassword
    pure next

  eval (Toggle next) = do
    H.query unit (H.action Popup.Toggle)
    pure next
