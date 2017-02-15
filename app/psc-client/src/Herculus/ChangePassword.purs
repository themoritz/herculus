module Herculus.ChangePassword where

import Herculus.Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

import Herculus.Notifications.Types as N
import Herculus.Router as R
import Lib.Api.Rest as Api
import Data.String (length)
import Herculus.Monad (Herc, gotoRoute, notify, withApi)
import Herculus.Utils (cldiv_)
import Herculus.Utils.Forms (renderRow, renderSubmit)
import Lib.Api.Schema.Auth (ChangePwdData(..), ChangePwdResponse(..))

data Query a
  = SetOld String a
  | SetNew String a
  | SetNewConfirm String a
  | Submit a

type State =
  { old :: String
  , new :: String
  , newConfirm :: String
  , validationError :: Maybe String
  }

comp :: H.Component HH.HTML Query Unit Void Herc
comp = H.component
  { initialState: const
      { old: ""
      , new: ""
      , newConfirm: ""
      , validationError: Nothing
      }
  , render
  , eval
  , receiver: const Nothing
  }

  where

  render :: State -> H.ComponentHTML Query
  render st = cldiv_ "auth-form"
    [ HH.h1_ [ HH.text "Change Password" ]
    , HH.table_
      [ HH.tbody_
        [ renderRow "Old password" "old" true st.old
                    (HE.input SetOld) (H.action Submit)
        , renderRow "New password" "password" true st.new
                    (HE.input SetNew) (H.action Submit)
        , renderRow "Confirm new password" "password-confirm" true st.newConfirm
                    (HE.input SetNewConfirm) (H.action Submit)
        , renderSubmit Submit
        ]
      ]
    , HH.text $ fromMaybe "" st.validationError
    ]

  eval :: Query ~> H.ComponentDSL State Query Void Herc
  eval (SetOld val next) = do
    modify _{ old = val }
    pure next

  eval (SetNew val next) = do
    modify _{ new = val }
    pure next

  eval (SetNewConfirm val next) = do
    modify _{ newConfirm = val }
    pure next

  eval (Submit next) = do
    st <- get
    let
      result = runExcept do
        when (length st.new < 6) $
          throwError "The password must be at least 6 characters long."
        when (st.new /= st.newConfirm) $
          throwError "The passwords don't match."
        pure $ ChangePwdData
          { cpdOldPassword: st.old
          , cpdNewPassword: st.new
          }
    case result of
      Left msg -> modify _{ validationError = Just msg }
      Right changePwdData ->
        withApi (Api.postAuthChangePassword changePwdData) case _ of
          ChangePwdFailure msg -> notify
            { kind: N.Warn
            , message: "Could not change password."
            , detail: Just msg
            }
          ChangePwdSuccess -> do
            notify
              { kind: N.Success
              , message: "Successfully change password."
              , detail: Nothing
              }
            gotoRoute $ R.LoggedIn R.ProjectOverview
    pure next
