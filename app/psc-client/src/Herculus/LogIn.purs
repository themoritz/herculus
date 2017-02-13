module Herculus.LogIn where

import Herculus.Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Herculus.Notifications.Types as N
import Lib.Api.Rest as Api
import DOM.Event.KeyboardEvent (KeyboardEvent, code)
import Halogen.HTML.Properties (InputType(..))
import Herculus.Monad (Herc, notify, withApi)
import Herculus.Utils (cldiv_)
import Lib.Api.Schema.Auth (LoginData(LoginData), LoginResponse(LoginSuccess, LoginFailed), UserInfo)
import Lib.Model.Auth (Email(..))

data Query a
  = SetEmail String a
  | SetPassword String a
  | PerformLogin a

data Output = LoggedIn UserInfo

type State =
  { email :: String
  , password :: String
  }

comp :: H.Component HH.HTML Query Unit Output Herc
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
  render st = cldiv_ "form"
    [ HH.h1_ [ HH.text "Login" ]
    , HH.table_
      [ HH.tbody_
        [ renderRow "Email" "email" false st.email (HE.input SetEmail)
        , renderRow "Password" "password" true st.password (HE.input SetPassword)
        , HH.tr_
          [ HH.td
            [ HP.class_ (H.ClassName "submit")
            , HP.colSpan 2
            ]
            [ HH.button
              [ HP.class_ (H.ClassName "enabled")
              , HE.onClick (HE.input_ PerformLogin)
              ]
              [ HH.text "Submit" ]
            ]
          ]
        , HH.tr_
          [ HH.td
            [ HP.colSpan 2
            ]
            [ HH.text "Not registered yet? "
            , HH.a
              [ HP.href "#signup" ]
              [ HH.text "Sign up" ]
            , HH.text "."
            , HH.br_
            , HH.a
              [ HP.href "#reset-password" ]
              [ HH.text "Forgot you password?" ]
            ]
          ]
        ]
      ]
    ]

    where

    renderRow
      :: String
      -> String
      -> Boolean
      -> String
      -> (String -> Maybe (Query Unit))
      -> H.ComponentHTML Query
    renderRow label name password value query = HH.tr_
      [ HH.td_
        [ HH.label
          [ HP.for name ]
          [ HH.text label ]
        ]
      , HH.td_
        [ HH.input
          [ HP.class_ (H.ClassName "auth-input")
          , HP.type_ (if password then InputPassword else InputText)
          , HP.value value
          , HP.name name
          , HE.onValueInput query
          , HE.onKeyDown \e -> if keyEnter e
              then Just $ H.action PerformLogin
              else Nothing
          ]
        ]
      ]

    keyEnter :: KeyboardEvent -> Boolean
    keyEnter e = code e == "13"

  eval :: Query ~> H.ComponentDSL State Query Output Herc
  eval (SetEmail email next) = do
    modify _{ email = email }
    pure next
  eval (SetPassword pw next) = do
    modify _{ password = pw }
    pure next
  eval (PerformLogin next) = do
    { email, password } <- get
    let
     ld =
       { ldEmail: Email { unEmail: email }
       , ldPassword: password
       }
    withApi (Api.postAuthLogin $ LoginData ld) case _ of
      LoginFailed msg -> notify
        { kind: N.Warn
        , message: "Login failed"
        , detail: Just msg
        }
      LoginSuccess userInfo -> H.raise $ LoggedIn userInfo
    pure next
