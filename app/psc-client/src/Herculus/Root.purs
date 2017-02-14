module Herculus.Root where

import Herculus.Prelude
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Herculus.Notifications as Notify
import Herculus.Notifications.Types as NotifyTypes
import Halogen.Component.ChildPath (type (\/), type (<\/>))
import Herculus.Monad (Herc)
import Herculus.Router as R
import Herculus.Auth.LogIn as LogIn
import Herculus.Auth.SignUp as SignUp
import Herculus.Auth.ResetPassword as ResetPw
import Herculus.LoggedIn as LoggedIn

data Query a
  = Notify NotifyTypes.Config a
  | Goto R.Root a

type State =
  { view :: R.Root
  }

type ChildQuery =
      Notify.Query
 <\/> LogIn.Query
 <\/> SignUp.Query
 <\/> ResetPw.Query
 <\/> LoggedIn.Query
 <\/> Const Void

type ChildSlot = Unit \/ Unit \/ Unit \/ Unit \/ Unit \/ Void

comp :: forall i o. H.Component HH.HTML Query i o Herc
comp = H.parentComponent
  { initialState: const initialState
  , render
  , eval
  , receiver: const Nothing
  }

  where

  initialState :: State
  initialState =
    { view: R.LoggedIn R.ProjectOverview
    }

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot Herc
  render st = HH.div_
    [ HH.slot' CP.cp1 unit Notify.comp unit absurd
    , case st.view of
        R.LoggedIn sub   -> HH.slot' CP.cp5 unit LoggedIn.comp sub absurd
        R.SignUp         -> HH.slot' CP.cp3 unit SignUp.comp unit absurd
        R.LogIn          -> HH.slot' CP.cp2 unit LogIn.comp unit absurd
        R.ForgotPassword -> HH.slot' CP.cp4 unit ResetPw.comp unit absurd
    ]

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot o Herc
  eval (Notify cfg next) = do
    H.query' CP.cp1 unit (H.action $ Notify.Push cfg)
    pure next

  eval (Goto view next) = do
    modify _{ view = view }
    pure next
