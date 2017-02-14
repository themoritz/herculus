module Herculus.LoggedIn where

import Herculus.Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Herculus.Notifications.Types as N
import Herculus.Router as R
import Lib.Api.Rest as Api
import Herculus.Monad (Herc, notify, withApi)
import Lib.Api.Schema.Auth (GetUserInfoResponse(..), UserInfo(..))


data Query a
  = Initialize a
  | Goto R.LoggedIn a

type State =
  { view :: R.LoggedIn
  , userInfo :: Maybe UserInfo
  }

type Input = R.LoggedIn

type ChildQuery =
  Const Void

type ChildSlot = Void

comp :: H.Component HH.HTML Query Input Void Herc
comp = H.lifecycleParentComponent
  { initialState: \view ->
     { view
     , userInfo: Nothing
     }
  , render
  , eval
  , receiver: Just <<< H.action <<< Goto
  , initializer: Just (H.action Initialize)
  , finalizer: Nothing
  }

  where

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot Herc
  render st = case st.view of
    R.ProjectOverview -> case st.userInfo of
      Nothing -> HH.text "Overview"
      Just (UserInfo ui) -> HH.text $ ui._uiUserName
    R.ProjectDetail pd -> HH.text "Details"

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void Herc
  eval (Initialize next) = do
    withApi Api.getAuthUserInfo $ case _ of
      GetUserInfoSuccess ui -> 
        modify _{ userInfo = Just ui }
      GetUserInfoFailed msg -> do
        notify
          { kind: N.Error
          , message: "Could not get user info."
          , detail: Just msg
          }
        liftEff $ R.setPath R.LogIn
    pure next
    
  eval (Goto view next) = do
    modify _{ view = view }
    pure next