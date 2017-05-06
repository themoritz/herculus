module Herculus.Project where

import Herculus.Prelude
import CSS as CSS
import Data.Map as Map
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Herculus.Config as Config
import Herculus.Grid as Grid
import Herculus.Notifications.Types as N
import Herculus.Project.Settings as Settings
import Herculus.Project.TableList as TL
import Herculus.Router as R
import Herculus.UserMenu as UserMenu
import Herculus.WebSocket as WS
import Lib.Api.Rest as Api
import Control.Monad.State (execState, runState)
import Control.Monad.Writer (execWriterT)
import Data.Array (head, singleton)
import Data.Int (toNumber)
import Data.Lens (Lens', _Just, lens, view, (.=))
import Data.Map (Map)
import Data.Maybe.First (First(..))
import Halogen.Component.ChildPath (type (<\/>), type (\/), cp1, cp2, cp3, cp4, cp5, cp6)
import Herculus.Monad (Herc, getAuthToken, gotoRoute, notify, withApi)
import Herculus.Project.Data (ProjectData, applyDiff, descTable, mkProjectData, prepare)
import Herculus.Project.TableList (Output(..))
import Herculus.Utils (cldiv, cldiv_, clspan_, faIcon_)
import Herculus.Utils.Templates (app)
import Lib.Api.Schema.Auth (UserInfo)
import Lib.Api.Schema.Project (Command, Project, projectId, projectName)
import Lib.Api.Schema.Project (ProjectData(..)) as Schema
import Lib.Api.WebSocket (WsDownMessage(..), WsUpMessage(..))
import Lib.Custom (ColumnTag, Id, ProjectTag)
import Lib.Model (Entity(..))
import Lib.Model.Table (Table, tableName)

data Query a
  = Initialize a
  | Update Input a
  | OpenTable (Id Table) a
  | ToOverview a
  | RunCommands (Array Command) a
  | SaveName String a
  | Delete a
  | ResizeColumn (Id ColumnTag) Int a
  | ReorderColumns (Array (Id ColumnTag)) a
  | EditColumn (Id ColumnTag) a
  | ConfigClose a
  | HandleWebSocket (WS.Output WsDownMessage) a

data Input = Input UserInfo R.Project

type State =
  { projectData :: ProjectData
  , columnSizes :: Map (Id ColumnTag) Int
  , columnOrders :: Map (Id Table) (Array (Id ColumnTag))
  , view :: Maybe (Id Table)
  , projId :: Id ProjectTag
  , _project :: Maybe Project
  , configWidth :: Int
  , configOpen :: Boolean
  , userInfo :: UserInfo
  , disconnected :: Boolean
  }

project :: Lens' State (Maybe Project)
project = lens _._project _{ _project = _ }

type ChildQuery =
  UserMenu.Query <\/>
  WS.Query WsUpMessage <\/>
  TL.Query <\/>
  Grid.Query <\/>
  Settings.Query <\/>
  Config.Query <\/>
  Const Void

type ChildSlot =
  Unit \/
  Unit \/
  Unit \/
  Unit \/
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

initialState :: Input -> State
initialState (Input ui (R.Project p mT)) =
  { projectData: mkProjectData
  , columnSizes: Map.empty
  , columnOrders: Map.empty
  , view: mT
  , projId: p
  , _project: Nothing
  , configWidth: 600
  , configOpen: false
  , userInfo: ui
  , disconnected: false
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
          Just input -> cldiv_ "absolute top-0 right-0 bottom-0 left-0"
            [ cldiv "absolute top-0 left-0 bottom-0"
              [ HC.style do
                  CSS.right $ CSS.px $ toNumber $
                    if st.configOpen then st.configWidth else 0
              ]
              [ HH.slot' cp4 unit Grid.comp input \o ->
                  Just $ H.action $ case o of
                    Grid.Commands cmds -> RunCommands cmds
                    Grid.OpenColumnConfig c -> EditColumn c
                    Grid.ResizeColumn colId size -> ResizeColumn colId size
                    Grid.ReorderColumns order -> ReorderColumns order
              ]
            , if st.configOpen
              then cldiv "absolute top-0 right-0 bottom-0 config"
                   [ HC.style do
                       CSS.width $ CSS.px $ toNumber $ st.configWidth
                   ]
                   [ HH.slot' cp6 unit Config.comp
                       { cols: Map.unions $ _._descColumns <$> st.projectData._pdTables
                       , tables: input.tables
                       , projectId: input.projectId
                       }
                       \o -> Just $ H.action $ case o of
                         Config.Close -> ConfigClose
                   ]
              else HH.text ""
            ]

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
                 Command c -> Just (H.action (RunCommands $ singleton c))
                 SelectTable t -> Just (H.action (OpenTable t))
    , case st._project of
        Nothing -> HH.text ""
        Just p -> HH.slot' cp5 unit Settings.comp { name: p ^. projectName } $
                    case _ of
                      Settings.SaveName name -> Just (H.action $ SaveName name)
                      Settings.Delete -> Just (H.action Delete)
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
          _ <- H.query' cp2 unit (H.action $ WS.Send $ WsUpSubscribe token projId)
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

eval (RunCommands cmds next) = do
  p <- H.gets _.projId
  withApi (Api.postProjectRunCommandsByProjectId cmds p) (const $ pure unit)
  pure next

eval (SaveName name next) = do
  { projId } <- get
  withApi (Api.postProjectSetNameByProjectId name projId) \_ -> do
    project <<< _Just <<< projectName .= name
  pure next

eval (Delete next) = do
  { projId } <- H.get
  withApi (Api.deleteProjectDeleteByProjectId projId) \_ ->
    gotoRoute $ R.LoggedIn R.ProjectOverview
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

eval (EditColumn c next) = do
  modify _{ configOpen = true }
  _ <- H.query' cp6 unit $ H.action $ Config.EditColumn c
  pure next

eval (ConfigClose next) = do
  modify _{ configOpen = false }
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
