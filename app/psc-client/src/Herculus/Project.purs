module Herculus.Project where

import Herculus.Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Herculus.Notifications.Types as N
import Herculus.Router as R
import Herculus.UserMenu as UserMenu
import Herculus.WebSocket as WS
import Lib.Api.Rest as Api
import Control.Monad.State (runState)
import Control.Monad.Writer (execWriterT)
import Data.Lens (Lens', _Just, lens, (.=))
import Data.Maybe.First (First(..))
import Halogen.Component.ChildPath (type (<\/>), type (\/), cp1, cp2)
import Herculus.Monad (Herc, getAuthToken, gotoRoute, notify, withApi)
import Herculus.Project.Data (Diff, ProjectData, applyDiff, mkProjectData)
import Herculus.Utils (faIcon_)
import Herculus.Utils.Templates (app)
import Lib.Api.Schema.Auth (UserInfo)
import Lib.Api.Schema.Column (Column)
import Lib.Api.Schema.Project (Project, projectName)
import Lib.Api.WebSocket (WsDownMessage(..), WsUpMessage(..))
import Lib.Custom (Id, ProjectTag)
import Lib.Model (Entity)
import Lib.Model.Cell (Cell)
import Lib.Model.Row (Row)
import Lib.Model.Table (Table)

data Query a
  = Update Input a
  | OpenTable (Id Table) a
  | SetName String a
  | ToOverview a
  | HandleWebSocket (WS.Output WsDownMessage) a
  | ApplyDiff (Diff (Entity Cell))
              (Diff Column)
              (Diff (Entity Row))
              (Diff (Entity Table))
              a

data Input = Input UserInfo R.Project

type State =
  { projectData :: ProjectData
  , view :: Maybe (Id Table)
  , projectId :: Id ProjectTag
  , _project :: Maybe Project
  , userInfo :: UserInfo
  , disconnected :: Boolean
  }

project :: Lens' State (Maybe Project)
project = lens _._project _{ _project = _ }

type ChildQuery =
  UserMenu.Query <\/>
  WS.Query WsUpMessage <\/>
  Const Void

type ChildSlot =
  Unit \/
  Unit \/
  Void

comp :: H.Component HH.HTML Query Input Void Herc
comp = H.parentComponent
  { initialState
  , render
  , eval
  , receiver: Just <<< H.action <<< Update
  }

  where

  initialState :: Input -> State
  initialState (Input ui (R.Project p mT)) =
    { projectData: mkProjectData
    , view: mT
    , projectId: p
    , _project: Nothing
    , userInfo: ui
    , disconnected: false
    }

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot Herc
  render st =
    let
      overviewButton = HH.button
        [ HE.onClick $ HE.input_ ToOverview
        , HP.class_ (H.ClassName "navigation__button")
        ]
        [ faIcon_ "th-large"
        , HH.text " Projects"
        ]
      body = HH.text "Body"
      tables = HH.text "tables"
      projectName = HH.text "project"
    in
      app
      [ case st.disconnected of
          true -> HH.text "Disconnected"
          false -> HH.text ""
      , overviewButton
      , HH.slot' cp1 unit UserMenu.comp st.userInfo absurd
      , HH.slot' cp2 unit WS.comp unit (Just <<< H.action <<< HandleWebSocket)
      ]
      [ tables
      , projectName
      ]
      body

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void Herc
  eval (Update (Input ui (R.Project p mT)) next) = do
    pure next

  eval (OpenTable i next) = do
    modify _{ view = Just i }
    pure next

  eval (SetName name next) = do
    { projectId } <- get
    withApi (Api.postProjectSetNameByProjectId name projectId) \_ ->
      project <<< _Just <<< projectName .= name
    pure next

  eval (ToOverview next) = do
    gotoRoute $ R.LoggedIn R.ProjectOverview
    pure next

  eval (HandleWebSocket output next) = do
    case output of
      WS.Opened -> do
        modify _{ disconnected = false }
        i <- gets _.projectId
        getAuthToken >>= case _ of
          Nothing -> gotoRoute R.LogIn
          Just token -> do
            H.query' cp2 unit (H.action $ WS.Send $ WsUpSubscribe token i)
            pure unit
      WS.Closed ->
        modify _{ disconnected = true }
      WS.Message msg -> case msg of
        WsDownSubscribeError e ->
          notify
            { kind: N.Error
            , message: "Could not subscribe to project updates."
            , detail: Just e
            }
        WsDownProjectDiff projectId cellDiff columnDiff rowDiff tableDiff -> do
          { view, projectData } <- get
          let
            m = applyDiff view tableDiff columnDiff rowDiff cellDiff
          case runState (execWriterT m) projectData of
            Tuple (First newView) newProjectData ->
              modify _
                { view = newView
                , projectData = newProjectData
                }
    pure next

  eval (ApplyDiff cellDiff columnDiff rowDiff tableDiff next) = do
    pure next
