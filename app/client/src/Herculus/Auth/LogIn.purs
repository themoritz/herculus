module Herculus.Auth.LogIn where

import Herculus.Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Herculus.Notifications.Types as N
import Lib.Api.Rest as Api
import Herculus.Monad (Herc, notify, setAuthToken, withApi, gotoRoute)
import Herculus.Utils (cldiv_)
import Herculus.Utils.Forms (renderRow, renderSubmit)
import Lib.Api.Schema.Auth (LoginData(LoginData), LoginResponse(LoginSuccess, LoginFailed), UserInfo(..))
import Lib.Model.Auth (Email(..))
import Herculus.Router as R

data Query a
  = SetEmail String a
  | SetPassword String a
  | PerformLogin a

type State =
  { email :: String
  , password :: String
  }

comp :: H.Component HH.HTML Query Unit Void Herc
comp = H.component
  { initialState: const
      { email: ""
      , password: ""
      }
  , render
  , eval
  , receiver: const Nothing
  }

  where

  render :: State -> H.ComponentHTML Query
  render st = cldiv_ "p3"
    [ HH.h1
      [ HP.class_ (HH.ClassName "h2 m0 mb3") ]
      [ HH.text "Login" ]
    , HH.table_
      [ HH.tbody_
        [ renderRow "Email" "email" false st.email
                    (HE.input SetEmail) (H.action PerformLogin)
        , renderRow "Password" "password" true st.password
                    (HE.input SetPassword) (H.action PerformLogin)
        , renderSubmit PerformLogin
        ]
      ]
    , HH.text "Not registered yet? "
    , HH.a
      [ HP.href (R.getLink R.SignUp)
      , HP.class_ (HH.ClassName "link")
      ]
      [ HH.text "Sign up" ]
    , HH.text "."
    , HH.br_
    , HH.a
      [ HP.href (R.getLink R.ForgotPassword)
      , HP.class_ (HH.ClassName "link") ]
      [ HH.text "Forgot your password?" ]
    ]

  eval :: Query ~> H.ComponentDSL State Query Void Herc
  eval (SetEmail email next) = do
    modify _{ email = email }
    pure next

  eval (SetPassword pw next) = do
    modify _{ password = pw }
    pure next

  eval (PerformLogin next) = do
    { email, password } <- H.get
    let
      ld =
        { ldEmail: Email { unEmail: email }
        , ldPassword: password
        }
    withApi (Api.postAuthLogin $ LoginData ld) case _ of
      LoginFailed msg -> notify
        { kind: N.Warn
        , message: "Login failed."
        , detail: Just msg
        }
      LoginSuccess (UserInfo userInfo) -> do
        setAuthToken userInfo._uiSessionKey
        gotoRoute $ R.LoggedIn R.ProjectOverview
        
    pure next
