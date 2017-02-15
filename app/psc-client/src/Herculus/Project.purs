module Herculus.Project where

import Herculus.Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Herculus.Router as R
import Herculus.UserMenu as UserMenu
import Herculus.WebSocket as WS
import Halogen.Component.ChildPath (type (<\/>), type (\/), cp1)
import Herculus.Monad (Herc, gotoRoute)
import Herculus.Project.Data (Diff, ProjectData)
import Herculus.Utils (faIcon_)
import Herculus.Utils.Templates (app)
import Lib.Api.Schema.Auth (UserInfo)
import Lib.Api.Schema.Column (Column)
import Lib.Api.WebSocket (WsUpMessage)
import Lib.Custom (Id, ProjectTag)
import Lib.Model (Entity)
import Lib.Model.Cell (Cell)
import Lib.Model.Row (Row)
import Lib.Model.Table (Table)

data Query a
  = Initialize a
  | Update Input a
  | OpenTable (Id Table) a
  | LoadProject (Id ProjectTag) a
  | SetName String a
  | ToOverview a
  | ApplyDiff (Diff (Entity Cell))
              (Diff Column)
              (Diff (Entity Row))
              (Diff (Entity Table))
              a

data Input = Input UserInfo R.Project

type State =
  { projectData :: Maybe ProjectData
  , view :: Maybe (Id Table)
  , projectId :: Id ProjectTag
  , userInfo :: UserInfo
  }

type ChildQuery =
  UserMenu.Query <\/>
  WS.Query WsUpMessage <\/>
  Const Void

type ChildSlot =
  Unit \/
  Unit \/
  Void

comp :: H.Component HH.HTML Query Input Void Herc
comp = H.lifecycleParentComponent
  { initialState
  , render
  , eval
  , receiver: Just <<< H.action <<< Update
  , initializer: Just (H.action Initialize)
  , finalizer: Nothing
  }

  where

  initialState :: Input -> State
  initialState (Input ui (R.Project p mT)) =
    { projectData: Nothing
    , view: mT
    , projectId: p
    , userInfo: ui
    }

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot Herc
  render st =
    let
      uiSlot = HH.slot' cp1 unit UserMenu.comp st.userInfo absurd
      overviewButton = HH.button
        [ HE.onClick $ HE.input_ ToOverview
        , HP.class_ (H.ClassName "navigation__button")
        ]
        [ faIcon_ "th-large"
        , HH.text " Projects"
        ]
      body = HH.text "Body"
      tables = HH.text "tables"
      project = HH.text "project"
    in
      app
      [ overviewButton
      , uiSlot
      ]
      [ tables
      , project
      ]
      body

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void Herc
  eval (Initialize next) = do
    pure next

  eval (Update (Input ui (R.Project p mT)) next) = do
    pure next

  eval (OpenTable i next) = do
    pure next

  eval (LoadProject i next) = do
    pure next

  eval (SetName name next) = do
    pure next

  eval (ToOverview next) = do
    gotoRoute $ R.LoggedIn R.ProjectOverview
    pure next

  eval (ApplyDiff cellDiff columnDiff rowDiff tableDiff next) = do
    pure next
