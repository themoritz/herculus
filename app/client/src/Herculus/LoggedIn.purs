module Herculus.LoggedIn where

import Herculus.Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Herculus.ChangePassword as ChangePw
import Herculus.Project as Project
import Herculus.ProjectOverview as PO
import Herculus.Router as R
import Herculus.UserMenu as UserMenu
import Lib.Api.Rest as Api
import Halogen.Component.ChildPath (type (<\/>), type (\/), cp1, cp2, cp3, cp4)
import Herculus.Monad (Herc, gotoRoute, withApi)
import Herculus.Utils (faIcon_)
import Herculus.Utils.Templates (app, plainApp)
import Lib.Api.Schema.Auth (GetUserInfoResponse(GetUserInfoFailed, GetUserInfoSuccess), UserInfo)


data Query a
  = Initialize a
  | Goto R.LoggedIn a
  | ToOverview a

type State =
  { view :: R.LoggedIn
  , userInfo :: Maybe UserInfo
  }

type Input = R.LoggedIn

type ChildQuery =
  Project.Query <\/>
  PO.Query <\/>
  ChangePw.Query <\/>
  UserMenu.Query <\/>
  Const Void

type ChildSlot =
  Unit \/
  Unit \/
  Unit \/
  Unit \/
  Void 

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
  render st = case st.userInfo of
    Nothing -> plainApp $ HH.text ""
    Just ui ->
      let
        uiSlot = HH.slot' cp4 unit UserMenu.comp ui absurd
        overviewButton = HH.button
          [ HE.onClick $ HE.input_ ToOverview
          , HP.class_ (H.ClassName "button--navigation")
          ]
          [ faIcon_ "th-large"
          , HH.text " Projects"
          ]
      in
        case st.view of
          R.ProjectOverview -> app
            [ uiSlot ]
            [ ]
            (HH.div_
             [ HH.slot' cp2 unit PO.comp unit absurd
             ]
            )
          R.ProjectDetail p ->
            HH.slot' cp1 unit Project.comp (Project.Input ui p) absurd
          R.ChangePassword -> app
            [ overviewButton
            , uiSlot
            ]
            [ ]
            (HH.slot' cp3 unit ChangePw.comp unit absurd)

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void Herc
  eval (Initialize next) = do
    withApi Api.getAuthUserInfo $ case _ of
      GetUserInfoSuccess ui -> 
        modify _{ userInfo = Just ui }
      GetUserInfoFailed _ ->
        liftEff $ R.setPath R.LogIn
    pure next
    
  eval (Goto view next) = do
    modify _{ view = view }
    pure next

  eval (ToOverview next) = do
    gotoRoute $ R.LoggedIn R.ProjectOverview
    pure next
