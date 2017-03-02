module Herculus.Project where

import Herculus.Prelude
import Data.Map as Map
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Herculus.Grid as Grid
import Herculus.Notifications.Types as N
import Herculus.Project.TableList as TL
import Herculus.Router as R
import Herculus.UserMenu as UserMenu
import Herculus.WebSocket as WS
import Control.Monad.State (execState, runState)
import Control.Monad.Writer (execWriterT)
import DOM.Event.KeyboardEvent (code)
import Data.Array (head)
import Data.Lens (Lens', _Just, lens, view, (.=))
import Data.Map (Map)
import Data.Maybe.First (First(..))
import Data.String (length)
import Halogen.Component.ChildPath (type (<\/>), type (\/), cp1, cp2, cp3, cp4)
import Herculus.Monad (Herc, getAuthToken, gotoRoute, notify, withApi)
import Herculus.Project.Data (ProjectData, applyDiff, descTable, mkProjectData, prepare)
import Herculus.Project.TableList (Output(..))
import Herculus.Utils (cldiv_, clspan_, faIcon_, focusElement)
import Herculus.Utils.Templates (app)
import Lib.Api.Rest as Api
import Lib.Api.Schema.Auth (UserInfo)
import Lib.Api.Schema.Project (Command, Project(..), projectId, projectName)
import Lib.Api.Schema.Project (ProjectData(..)) as Schema
import Lib.Api.WebSocket (WsDownMessage(..), WsUpMessage(..))
import Lib.Custom (ColumnTag, Id, ProjectTag)
import Lib.Model (Entity(..))
import Lib.Model.Table (Table, tableName)

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
  | RunCommand Command a
  | ResizeColumn (Id ColumnTag) Int a
  | ReorderColumns (Array (Id ColumnTag)) a
  | HandleWebSocket (WS.Output WsDownMessage) a

data Input = Input UserInfo R.Project

type State =
  { projectData :: ProjectData
  , columnSizes :: Map (Id ColumnTag) Int
  , columnOrders :: Map (Id Table) (Array (Id ColumnTag))
  , view :: Maybe (Id Table)
  , projId :: Id ProjectTag
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
  TL.Query <\/>
  Grid.Query <\/>
  Const Void

type ChildSlot =
  Unit \/
  Unit \/
  Unit \/
  Unit \/
  Void

projectNameRef :: H.RefLabel
projectNameRef = H.RefLabel "projectNameRef" 

comp :: H.Component HH.HTML Query Input Void Herc
comp = H.lifecycleParentComponent
  { initialState
  , render
  , eval
  , receiver: Just <<< H.action <<< Update
  , initializer: Just (H.action Initialize)
  , finalizer: Nothing
  }

initialState :: Input -> State
initialState (Input ui (R.Project p mT)) =
  { projectData: mkProjectData
  , columnSizes: Map.empty
  , columnOrders: Map.empty
  , view: mT
  , projId: p
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
      , HP.class_ (H.ClassName "button--navigation")
      ]
      [ faIcon_ "th-large"
      , HH.text " Projects"
      ]

    body =
      let
        mInput = do
          t <- st.view
          desc <- Map.lookup t st.projectData._pdTables
          pure 
            { cells: st.projectData._pdCells
            , cols: desc._descColumns
            , rows: Map.toAscUnfoldable desc._descRows
            , tables: Map.toAscUnfoldable st.projectData._pdTables <#> \(Tuple i t) ->
                { value: i, label: t ^. descTable <<< tableName }
            , rowCache: st.projectData._pdRowCache
            , tableId: t
            , projectId: st.projId
            , colSizes: st.columnSizes
            , colOrder: fromMaybe [] $ Map.lookup t st.columnOrders
            }
      in
        case mInput of
          Nothing -> HH.div_ []
          Just input -> 
            HH.slot' cp4 unit Grid.comp input \o ->
              Just $ H.action $ case o of
                Grid.Command cmd -> RunCommand cmd
                Grid.ResizeColumn colId size -> ResizeColumn colId size
                Grid.ReorderColumns order -> ReorderColumns order

    projectName = cldiv_ "project-name" case st._project, st.tmpProjectName of
      Just (Project p), Nothing ->
        [ HH.span_
          [ HH.text (p._projectName) ]
        , HH.text " "
        , HH.button
          [ HP.class_ (H.ClassName "button--pure button--on-dark gray ml1 align-middle")
          , HP.title "Change project name"
          , HE.onClick (HE.input_ StartEditName)
          ]
          [ faIcon_ "pencil" ]
        , HH.button
          [ HP.class_ (H.ClassName "button--pure button--on-dark gray ml1 align-middle")
          , HP.title "Delete project (careful!)"
          , HE.onClick (HE.input_ DeleteProject)
          ]
          [ faIcon_ "times" ]
        ]
      _, Just name ->
        [ HH.input
          [ HP.value name
          , HP.class_ (H.ClassName "header-input")
          , HP.ref projectNameRef
          , HE.onValueInput (HE.input SetName)
          , HE.onKeyDown \e -> case code e of
              "Enter"  -> Just (H.action SaveName)
              "Escape" -> Just (H.action CancelEditName)
              _        -> Nothing
          , HE.onBlur (HE.input_ CancelEditName)
          ]
        ]
      _, _ -> []

  in
    app
    [ case st.disconnected of
        true -> clspan_ "orange bold pr2 pulsing"
          [ faIcon_ "wifi pr1"
          , HH.text "Reconnecting..."
          ]
        false -> HH.text ""
    , overviewButton
    , HH.slot' cp1 unit UserMenu.comp st.userInfo absurd
    , HH.slot' cp2 unit WS.comp unit
               (Just <<< H.action <<< HandleWebSocket)
    ]
    [ HH.slot' cp3 unit TL.comp
               { tables: st.projectData._pdTables
               , selected: st.view
               }
               case _ of
                 Command c -> Just (H.action (RunCommand c))
                 SelectTable t -> Just (H.action (OpenTable t))
    , projectName
    ]
    body

eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void Herc
eval (Initialize next) = do
  { userInfo, projId, view } <- H.get
  update userInfo projId view
  pure next

eval (Update (Input ui (R.Project p mT)) next) = do
  update ui p mT
  pure next

eval (OpenTable i next) = do
  { projId } <- H.get
  gotoRoute $
    R.LoggedIn $ R.ProjectDetail $ R.Project projId (Just i)
  pure next

eval (StartEditName next) = do
  p <- H.gets _._project
  modify _
    { tmpProjectName = view projectName <$> p
    }
  focusElement projectNameRef
  pure next

eval (CancelEditName next) = do
  modify _{ tmpProjectName = Nothing }
  pure next

eval (SetName name next) = do
  modify _{ tmpProjectName = Just name }
  pure next

eval (SaveName next) = do
  { projId, tmpProjectName } <- H.get
  case tmpProjectName of
    Nothing -> pure unit
    Just name -> when (length name > 0) $
      withApi (Api.postProjectSetNameByProjectId name projId) \_ -> do
        project <<< _Just <<< projectName .= name
        modify _{ tmpProjectName = Nothing }
  pure next

eval (DeleteProject next) = do
  { projId } <- H.get
  withApi (Api.deleteProjectDeleteByProjectId projId) \_ ->
    gotoRoute $ R.LoggedIn R.ProjectOverview
  pure next

eval (ToOverview next) = do
  gotoRoute $ R.LoggedIn R.ProjectOverview
  pure next

eval (HandleWebSocket output next) = do
  case output of
    WS.Opened -> do
      { disconnected, projId } <- H.get
      when disconnected do
        modify _{ disconnected = false }
      getAuthToken >>= case _ of
        Nothing -> gotoRoute R.LogIn
        Just token -> do
          H.query' cp2 unit (H.action $ WS.Send $ WsUpSubscribe token projId)
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
      WsDownProjectDiff _ cellDiff columnDiff rowDiff tableDiff -> do
        { view, projectData } <- H.get
        let
          m = applyDiff view tableDiff columnDiff rowDiff cellDiff
        case runState (execWriterT m) projectData of
          Tuple (First action) newProjectData -> do
            let newView = action <|> view
            modify _
              { view = newView
              , projectData = newProjectData
              }
  pure next

eval (RunCommand cmd next) = do
  p <- H.gets _.projId
  withApi (Api.postProjectRunCommandByProjectId cmd p) (const $ pure unit)
  pure next

eval (ResizeColumn colId size next) = do
  modify \st -> st
    { columnSizes = Map.insert colId size st.columnSizes }
  withApi (Api.postProjectColSetWidthByColumnId size colId)
    (const $ pure unit)
  pure next

eval (ReorderColumns order next) = do
  { view } <- get
  for_ view \tableId -> do
    modify \st -> st
      { columnOrders = Map.insert tableId order st.columnOrders }
    withApi (Api.postProjectReorderColsByTableId order tableId)
      (const $ pure unit)
  pure next

update
  :: UserInfo
  -> Id ProjectTag
  -> Maybe (Id Table)
  -> H.ParentDSL State Query ChildQuery ChildSlot Void Herc Unit
update ui p mT = do
  currentProjectId <- H.gets \st -> map (view projectId) st._project
  case Just p == currentProjectId of
    false -> withApi (Api.getProjectLoadByProjectId p) \(Schema.ProjectData pd) -> do
      let firstTable = (\(Entity e) -> e.entityId) <$> head pd._pdTables 
      modify _
        { view = mT <|> firstTable
        , projId = p
        , userInfo = ui
        , _project = Just pd._pdProject
        , projectData =
            let
              m = prepare pd._pdTables pd._pdColumns pd._pdRows pd._pdCells
            in
              execState m mkProjectData
        , columnSizes = Map.fromFoldable pd._pdColumnSizes
        , columnOrders = Map.fromFoldable pd._pdColumnOrders
        }
    true -> modify _
      { view = mT
      , userInfo = ui
      }
