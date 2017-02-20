module Herculus.Auth.SignUp where

import Herculus.Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Herculus.Notifications.Types as N
import Herculus.Router as R
import Lib.Api.Rest as Api
import Data.String (length)
import Herculus.Monad (Herc, gotoRoute, notify, setAuthToken, withApi)
import Herculus.Utils (cldiv_)
import Herculus.Utils.Forms (renderRow, renderSubmit)
import Lib.Api.Schema.Auth (SignupData(SignupData), SignupResponse(SignupSuccess, SignupFailed), UserInfo(..))
import Lib.Model.Auth (Email(..))

data Query a
  = SetIntention String a
  | SetName String a
  | SetEmail String a
  | SetPassword String a
  | SetPwConfirm String a
  | PerformSignup a

type State =
  { intention :: String
  , name :: String
  , email :: String
  , password :: String
  , pwConfirm :: String
  , validationError :: Maybe String
  }

comp :: H.Component HH.HTML Query Unit Void Herc
comp = H.component
  { initialState: const
      { intention: ""
      , name: ""
      , email: ""
      , password: ""
      , pwConfirm: ""
      , validationError: Nothing
      }
  , render
  , eval
  , receiver: const Nothing
  }

  where

  render :: State -> H.ComponentHTML Query
  render st = cldiv_ "auth-form"
    [ HH.h1_ [ HH.text "Signup" ]
    , HH.table_
      [ HH.tbody_
        [ HH.tr_
          [ HH.td_
            [ HH.label
              [ HP.for "intention" ]
              [ HH.text "What are you planning to use Herculus for? Describe a \
                 \project, idea, use case, or state that you are just going \
                 \to play around."
              ]
            ]
          , HH.td_
            [ HH.textarea
              [ HP.class_ (H.ClassName "auth-form__input")
              , HP.value st.intention
              , HP.rows 4
              , HP.name "intention"
              , HE.onValueInput (HE.input SetIntention)
              ]
            ]
          ]
        , renderRow "Name" "name" false st.name
                    (HE.input SetName) (H.action PerformSignup)
        , renderRow "Email" "email" false st.email
                    (HE.input SetEmail) (H.action PerformSignup)
        , renderRow "Password" "password" true st.password
                    (HE.input SetPassword) (H.action PerformSignup)
        , renderRow "Password (again)" "pwConfirm" true st.pwConfirm
                    (HE.input SetPwConfirm) (H.action PerformSignup)
        , renderSubmit PerformSignup
        ]
      ]
    , HH.text $ fromMaybe "" st.validationError
    , HH.p_
      [ HH.text "Please be aware that this is a beta version. \
          \We are still in the progress of implementing a lot of features. \
          \Bugs may occur, and you may lose your data."
      ]
    , HH.p_
      [ HH.text "Your feedback to "
      , HH.strong_ [ HH.text "moritz@herculus.io" ]
      , HH.text " is highly appreciated."
      ]
    , HH.a
      [ HP.href (R.getLink R.LogIn) ]
      [ HH.text "Back to login" ]
    ]

  eval :: Query ~> H.ComponentDSL State Query Void Herc
  eval (SetIntention val next) = do
    modify _{ intention = val }
    pure next

  eval (SetName val next) = do
    modify _{ name = val }
    pure next

  eval (SetEmail email next) = do
    modify _{ email = email }
    pure next

  eval (SetPassword pw next) = do
    modify _{ password = pw }
    pure next

  eval (SetPwConfirm pw next) = do
    modify _{ pwConfirm = pw }
    pure next

  eval (PerformSignup next) = do
    st <- H.get
    let
      result = runExcept do
        when (st.name == "") $
          throwError "Please enter a name."
        when (length st.password < 6) $
          throwError "The password must be at least 6 characters long."
        when (st.password /= st.pwConfirm) $
          throwError "The passwords don't match."
        pure $ SignupData
          { suUserName: st.name
          , suEmail: Email { unEmail: st.email }
          , suPassword: st.password
          , suIntention: st.pwConfirm
          }
    case result of
      Left msg -> modify _{ validationError = Just msg }
      Right signUpData ->
        withApi (Api.postAuthSignup signUpData) case _ of
          SignupFailed msg -> notify
            { kind: N.Warn
            , message: "Sign up failed."
            , detail: Just msg
            }
          SignupSuccess (UserInfo userInfo) -> do
            setAuthToken userInfo._uiSessionKey
            gotoRoute $ R.LoggedIn R.ProjectOverview
    pure next
