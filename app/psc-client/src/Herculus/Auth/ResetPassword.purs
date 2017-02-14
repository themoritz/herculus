module Herculus.Auth.ResetPassword where

import Herculus.Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Herculus.Notifications.Types as N
import Herculus.Router as R
import Lib.Api.Rest as Api
import Herculus.Monad (Herc, notify, withApi)
import Herculus.Utils (cldiv_)
import Herculus.Utils.Forms (renderRow, renderSubmit)

data Query a
  = SetEmail String a
  | SendLink a

data Output = Done

type State =
  { email :: String
  }

comp :: H.Component HH.HTML Query Unit Output Herc
comp = H.component
  { initialState: const
      { email: ""
      }
  , render
  , eval
  , receiver: const Nothing
  }

  where

  render :: State -> H.ComponentHTML Query
  render st = cldiv_ "auth-form"
    [ HH.h1_ [ HH.text "Reset Password" ]
    , HH.p_ [ HH.text "Please enter the email address you signed up with." ]
    , HH.table_
      [ HH.tbody_
        [ renderRow "Email" "email" false st.email
                    (HE.input SetEmail) (H.action SendLink)
        , renderSubmit SendLink
        ]
      ]
    , HH.a
      [ HP.href (R.getLink R.LogIn) ]
      [ HH.text "Back to login" ]
    ]

  eval :: Query ~> H.ComponentDSL State Query Output Herc
  eval (SetEmail email next) = do
    modify _{ email = email }
    pure next

  eval (SendLink next) = do
    { email } <- get
    withApi (Api.postAuthSendResetLink email) \_ -> do
      notify
        { kind: N.Success
        , message: "An email with a reset link will be sent to the given \
            \address (provided it exists). Please also check your spam folder."
        , detail: Nothing
        }
      H.raise Done
    pure next
