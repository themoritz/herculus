{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Views where

import           Control.DeepSeq       (NFData)
import           Control.Lens          hiding (view)
import           Data.Foldable         (for_)
import qualified Data.IntMap.Strict    as IntMap
import           Data.Map.Strict       (Map)
import qualified Data.Map.Strict       as Map
import           Data.Monoid           (mempty, (<>))
import           Data.Text             (Text)
import qualified Data.Text             as Text
import           GHC.Generics          (Generic)
import           React.Flux
import           React.Flux.Internal   (toJSString)

import           Lib.Model.Project
import           Lib.Model.Table
import           Lib.Types

import           Action                (Action (..))
import           Store                 (LoggedOutState (..),
                                        ProjectViewState (..),
                                        SessionState (..), State, dispatch,
                                        stateCells, stateColumns, stateMessage,
                                        stateProject, stateProjectId,
                                        stateProjectView, stateRows,
                                        stateSession, stateSessionKey,
                                        stateTableId, stateTables, store)
import           Views.Auth            (login_, signup_)
import           Views.Combinators     (clspan_, inputNew_)
import           Views.Common          (keyENTER, keyESC)
import           Views.ProjectOverview (projectsOverview_)
import           Views.Table           (TableGridProps (..), tableGrid_)

import           Store.Message         (Message (Message))
import qualified Store.Message         as Message

app :: ReactView ()
app = defineControllerView "app" store $ \st () ->
    cldiv_ "container" $ do
      appHeader_ st
      for_ (st ^. stateMessage) message_
      case st ^. stateSession of
        StateLoggedOut LoggedOutLoginForm -> login_
        StateLoggedOut LoggedOutSignupForm -> signup_
        StateLoggedOut LoggedOutUninitialized -> "Failed to load. Please reload."
        StateLoggedIn liSt ->
          case liSt ^. stateProjectView of
            StateProjectOverview ps -> cldiv_ "overview" $ projectsOverview_ ps
            StateProjectDetail pdSt ->
              if null $ pdSt ^. stateTables
                then cldiv_ "" mempty
                else projectDetailView_ pdSt $ liSt ^. stateSessionKey
      appFooter_ st
  where
    projectDetailView_ pdSt sKey =
      let cols = pdSt ^. stateColumns
          recs = pdSt ^. stateRows
          -- TODO: put tables deeper into view state hierarchy under projectId
          tableGridProps = TableGridProps
            { _cells      = pdSt ^. stateCells
            , _colByIndex = IntMap.fromList $ zip [0..] (Map.toList cols)
            , _recByIndex = IntMap.fromList $ zip [0..] (Map.toList recs)
            , _tableId    = pdSt ^. stateTableId
            , _projectId  = pdSt ^. stateProjectId
            , _sKey       = sKey
            , _tables     = (^. tableName) <$> (pdSt ^. stateTables)
            }
      in  cldiv_ "tableGrid" $ tableGrid_ tableGridProps

-- header

appHeader_ :: State -> ReactElementM eh ()
appHeader_ !st = view appHeader st mempty

appHeader :: ReactView State
appHeader = defineView "header" $ \st ->
  cldiv_ "menubar" $ do
    cldiv_ "title" $ do
      cldiv_ "logo" $ img_
        [ "src" $= "img/herculus.svg"
        , "alt" $= "logo"
        ] mempty
      cldiv_ "text" "Herculus"
    case st ^. stateSession of
      StateLoggedIn liSt ->
        case liSt ^. stateProjectView of
          StateProjectOverview _  ->
            cldiv_ "navigation" $ btnLogout_
          StateProjectDetail pdSt -> do
            tables_ (pdSt ^. stateTables) (pdSt ^. stateTableId) (pdSt ^. stateProjectId)
            projectNameComp_ (pdSt ^. stateProjectId) $ pdSt ^. stateProject
            cldiv_ "navigation" $ do
              btnLogout_
              button_
                [ classNames [ ("backToOverview", True)]
                , onClick $ \_ _ -> dispatch $ SetProjectOverview (liSt ^. stateSessionKey)
                ] $ do
                  faIcon_ "th-large"
                  " Projects"
      StateLoggedOut _  -> pure ()

data ProjectNameState = ProjectNameState
  { pnsEditable  :: Bool
  , pnsName      :: Text
  , pnsNameError :: Bool
  } deriving (Generic, Show, NFData)

btnLogout_ :: ReactElementM ViewEventHandler ()
btnLogout_ = button_
  [ classNames [ ("logout", True) ]
  , onClick $ \_ _ -> dispatch Logout
  ] $ do
    faIcon_ "power-off"
    " Log Out"

initalProjectName :: ProjectNameState
initalProjectName = ProjectNameState False "" False

projectNameComp_ :: Id ProjectClient -> ProjectClient -> ReactElementM eh ()
projectNameComp_ !projectId !project =
  view projectNameComp (projectId, project) mempty

projectNameComp :: ReactView (Id ProjectClient, ProjectClient)
projectNameComp = defineStatefulView "project name" initalProjectName $
    \ProjectNameState{..} (projectId, project) ->
      cldiv_ "projectName" $ if pnsEditable
        then
          div_ $ input_
              [ "value" &= pnsName
              , "autoFocus" &= True
              , onChange $ \evt st ->
                let value = target evt "value"
                in  ([], Just st { pnsName = value,
                                   pnsNameError = Text.null value})
              , onKeyDown inputKeyDownHandler
              ]
        else do
          span_ $ elemText $ project ^. projectClientName
          " "
          button_
            [ classNames [ ("pure"   , True)
                         , ("on-dark", True)
                         ]
            , onClick $ \ev _ st ->
              ([stopPropagation ev],
               Just st { pnsEditable = True
                       , pnsName = project ^. projectClientName
                       }
              )
            ] $ faIcon_ "pencil"
          button_
            [ classNames [ ("pure"   , True)
                         , ("on-dark", True)
                         ]
            , onClick $ \ev _ _ ->
              (stopPropagation ev :
                 dispatch (ProjectDelete projectId), Nothing)
            ] $ faIcon_ "times"
  where
    -- saveHandler :: TileState -> ([SomeStoreAction], Maybe TileState)
    saveHandler st@ProjectNameState{..} =
      (dispatch $ ProjectSetName pnsName, Just st { pnsEditable = False })

    inputKeyDownHandler _ evt st@ProjectNameState{..}
      | keyENTER evt && not (Text.null pnsName) = saveHandler st
      | keyESC evt = ([] , Just st { pnsEditable = False })
      | otherwise = ([], Just st)


message_ :: Message -> ReactElementM eh ()
message_ !msg = view message msg mempty

message :: ReactView Message
message = defineView "message" $ \(Message content typ) ->
  let (cls, icon, txt) = case typ of
        Message.Info    -> ("info"   , "exclamation-circle"  , "Information")
        Message.Success -> ("success", "check-circle-o"      , "Success"    )
        Message.Warning -> ("warning", "exclamation-triangle", "Warning"    )
        Message.Error   -> ("error"  , "times-circle-o"      , "Error"      )
  in  cldiv_ ("message " <> cls) $ do
        cldiv_ "header" $ do
          cldiv_ "title" $ do
            clspan_ "symbol" $ faIcon_ icon
            txt
          cldiv_ "button" $ button_
            [ "className" $= "pure"
            , onClick $ \_ _ -> dispatch $ MessageAction Message.Unset
            ] $ faIcon_ "times"
        cldiv_ "content" $ elemText content

appFooter_ :: State -> ReactElementM eh ()
appFooter_ !st = view appFooter st mempty

appFooter :: ReactView State
appFooter = defineView "footer" $ \_ ->
  cldiv_ "footer" $ do
    "Contact us at "
    a_ [ "href" $= "mailto:hi@herculus.io"
      , "className" $= "on-dark"
      ] "hi@herculus.io"

--

tables_ :: Map (Id Table) Table -> Maybe (Id Table) -> Id ProjectClient -> ReactElementM eh ()
tables_ !ts !mTbl !prj = view tables (ts, mTbl, prj) mempty

tables :: ReactView (Map (Id Table) Table, Maybe (Id Table), Id ProjectClient)
tables = defineView "tables" $ \(ts, mTbl, projId) ->
  cldiv_ "tables" $ do
    ul_ $ for_ (Map.toList ts) $
      \(tableId, table') -> table_' tableId table' (Just tableId == mTbl)
    inputNew_ "Add table..." (dispatch . TablesCreate projId)

--

table_' :: Id Table -> Table -> Bool -> ReactElementM eh ()
table_' !tableId !table' !selected =
  viewWithSKey table (toJSString $ show tableId) (tableId, table', selected) mempty

data TableViewState = TableViewState
  { tEditable  :: Bool
  , tName      :: Text
  , tNameError :: Bool
  } deriving (Generic, Show, NFData)

initialTableViewState :: TableViewState
initialTableViewState = TableViewState False "" False

table :: ReactView (Id Table, Table, Bool)
table = defineStatefulView "table" initialTableViewState $ \state (tableId, table', selected) ->
  let saveHandler st = (dispatch $ TableSetName tableId (tName st), Just st { tEditable = False })
      inputKeyDownHandler _ evt st
        | keyENTER evt && not (Text.null $ tName st) = saveHandler st
        | keyESC evt = ([] , Just st { tEditable = False })
        | otherwise = ([], Just st)
  in li_ [ classNames
             [ ("active", selected)
             , ("link", True)
             ]
         , onClick $ \_ _ _ -> (dispatch $ TablesLoadTable tableId, Nothing)
         ] $
     if tEditable state
     then div_ $ input_
            [ "value" &= tName state
            , onChange $ \ev st ->
              let value = target ev "value"
              in  ([], Just st { tName = value, tNameError = Text.null value})
            , onKeyDown inputKeyDownHandler
            ]
     else div_ $ do
       span_ $ elemText $ table' ^. tableName

       button_
         [ "className" $= "pure on-dark"
         , onClick $ \ev _ st -> ([stopPropagation ev], Just st { tEditable = True, tName = table'  ^. tableName })
         ] $ faIcon_ "pencil"

       button_
         [ "className" $= "pure on-dark"
         , onClick $ \ev _ _ -> (stopPropagation ev : dispatch (TableDelete tableId), Nothing)
         ] $ faIcon_ "times"
