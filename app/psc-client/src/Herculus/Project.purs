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
import Control.Monad.State (execState, runState)
import Control.Monad.Writer (execWriterT)
import DOM.Event.KeyboardEvent (code)
import Data.Lens (Lens', _Just, lens, view, (.=))
import Data.Maybe.First (First(..))
import Data.String (length)
import Debug.Trace (traceAny, traceAnyM)
import Halogen.Component.ChildPath (type (<\/>), type (\/), cp1, cp2)
import Herculus.Monad (Herc, getAuthToken, gotoRoute, notify, withApi)
import Herculus.Project.Data (Diff, ProjectData, applyDiff, mkProjectData, prepare)
import Herculus.Utils (cldiv_, faIcon_)
import Herculus.Utils.Templates (app)
import Lib.Api.Rest (deleteProjectDeleteByProjectId, getProjectLoadByProjectId, postProjectSetNameByProjectId) as Api
import Lib.Api.Schema.Auth (UserInfo)
import Lib.Api.Schema.Column (Column)
import Lib.Api.Schema.Project (Project(..), projectName)
import Lib.Api.Schema.Project (ProjectData(..)) as Api
import Lib.Api.WebSocket (WsDownMessage(..), WsUpMessage(..))
import Lib.Custom (Id, ProjectTag)
import Lib.Model (Entity)
import Lib.Model.Cell (Cell)
import Lib.Model.Row (Row)
import Lib.Model.Table (Table)

data Query a
  = Initialize a
  | Update Input a
  | OpenTable (Id Table) a
  | StartEditName a
  | CancelEditName a
  | SetName String a
  | SaveName a
  | DeleteProject a
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
  , tmpProjectName :: Maybe String
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
    { projectData: mkProjectData
    , view: mT
    , projectId: p
    , _project: Nothing
    , userInfo: ui
    , disconnected: false
    , tmpProjectName: Nothing
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

      projectName = cldiv_ "project-name" case st._project, st.tmpProjectName of
        Just (Project p), Nothing ->
          [ HH.span_
            [ HH.text (p._projectName) ]
          , HH.text " "
          , HH.button
            [ HP.class_ (H.ClassName "button--pure  button--on-dark")
            , HP.title "Change project name"
            , HE.onClick (HE.input_ StartEditName)
            ]
            [ faIcon_ "pencil" ]
          , HH.button
            [ HP.class_ (H.ClassName "button--pure  button--on-dark")
            , HP.title "Delete project (careful!)"
            , HE.onClick (HE.input_ DeleteProject)
            ]
            [ faIcon_ "times" ]
          ]
        _, Just name ->
          [ HH.input
            [ HP.value name
            , HP.class_ (H.ClassName "header-input")
            , HP.autofocus true
            , HE.onValueInput (HE.input SetName)
            , HE.onKeyDown \e -> case code e of
                "Enter"  -> Just (H.action SaveName)
                "Escape" -> Just (H.action CancelEditName)
                _        -> Nothing
            -- TODO: investigate DOMException
            -- , HE.onBlur (HE.input_ CancelEditName)
            ]
          ]
        _, _ -> []

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
  eval (Initialize next) = do
    { userInfo, projectId, view } <- get
    update userInfo projectId view
    pure next

  eval (Update (Input ui (R.Project p mT)) next) = do
    update ui p mT
    pure next

  eval (OpenTable i next) = do
    modify _{ view = Just i }
    pure next

  eval (StartEditName next) = do
    p <- gets _._project
    modify _
      { tmpProjectName = view projectName <$> p
      }
    pure next

  eval (CancelEditName next) = do
    modify _{ tmpProjectName = Nothing }
    pure next

  eval (SetName name next) = do
    modify _{ tmpProjectName = Just name }
    pure next

  eval (SaveName next) = do
    { projectId, tmpProjectName } <- get
    case tmpProjectName of
      Nothing -> pure unit
      Just name -> when (length name > 0) $
        withApi (Api.postProjectSetNameByProjectId name projectId) \_ -> do
          project <<< _Just <<< projectName .= name
          modify _{ tmpProjectName = Nothing }
    pure next

  eval (DeleteProject next) = do
    { projectId } <- get
    withApi (Api.deleteProjectDeleteByProjectId projectId) \_ ->
      gotoRoute $ R.LoggedIn R.ProjectOverview
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

  update
    :: UserInfo
    -> Id ProjectTag
    -> Maybe (Id Table)
    -> H.ParentDSL State Query ChildQuery ChildSlot Void Herc Unit
  update ui p mT =
    withApi (Api.getProjectLoadByProjectId p) \(Api.ProjectData pd) ->
      modify _
        { view = mT
        , projectId = p
        , userInfo = ui
        , _project = Just pd._pdProject
        , projectData =
            let
              m = prepare pd._pdTables pd._pdColumns pd._pdRows pd._pdCells
            in
              execState m mkProjectData
        }
