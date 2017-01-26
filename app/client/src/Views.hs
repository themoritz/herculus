{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Views where

import           Control.DeepSeq       (NFData)
import           Control.Lens          hiding (view)
import           Control.Monad         (when)
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

import           Lib.Api.Rest          (Command (..))
import           Lib.Model.Auth        (uiUserName)
import           Lib.Model.Project
import           Lib.Model.Table
import           Lib.Types

import qualified LoggedIn
import qualified Project
import           Store                 (Action (MessageAction),
                                        LoggedOutState (..), SessionState (..),
                                        State, dispatch, dispatchLoggedIn,
                                        dispatchProject, dispatchProjectCommand,
                                        stateMessage, stateSession, store)
import           Views.Auth            (changePassword_, login_, signup_)
import           Views.Combinators     (clspan_, inputNew_, menuItem_)
import           Views.Common          (keyENTER, keyESC)
import           Views.Foreign         (onClickOutside_)
import           Views.ProjectOverview (projectsOverview_)
import           Views.Table           (TableGridProps (..), tableGrid_)

import           Store.Message         (Message (Message))
import qualified Store.Message         as Message

app :: ReactView ()
app = defineControllerView "app" store $ \st () ->
    cldiv_ "container" $ do
      appHeader_ st
      for_ (st ^. stateMessage) message_
      cldiv_ "content-body" $ case st ^. stateSession of
        StateLoggedOut LoggedOutLoginForm -> login_
        StateLoggedOut LoggedOutSignupForm -> signup_
        StateLoggedOut LoggedOutUninitialized -> cldiv_ "form" "Failed to load. Please reload."
        StateLoggedIn liSt ->
          case liSt ^. LoggedIn.stateSubState of
            LoggedIn.ProjectOverview ps -> projectsOverview_ ps
            LoggedIn.ProjectDetail pdSt ->
              if null $ pdSt ^. Project.stateTables
                then mempty
                else projectDetailView_ pdSt $ liSt ^. LoggedIn.stateSessionKey
            LoggedIn.ChangePasswordForm valid -> changePassword_ valid
      appFooter_ st
  where
    projectDetailView_ pdSt sKey =
      let tableGridProps = do
            tableId <- pdSt ^. Project.stateCurrentTable
            tableDesc <- pdSt ^. Project.stateTables . at tableId
            let cols = Map.toList (tableDesc ^. Project.descColumns)
                rows = Map.toList (tableDesc ^. Project.descRows)
            -- TODO: put tables deeper into view state hierarchy under projectId
            pure TableGridProps
              { _cells            = pdSt ^. Project.stateCells
              , _colByIndex       = IntMap.fromList $ zip [0..] cols
              , _rowByIndex       = IntMap.fromList $ zip [0..] rows
              , _tableId          = tableId
              , _projectId        = pdSt ^. Project.stateProjectId
              , _showNewColDialog = pdSt ^. Project.stateNewColShow
              , _sKey             = sKey
              , _tables           = (^. Project.descTable . tableName) <$> (pdSt ^. Project.stateTables)
              }
      in  for_ tableGridProps tableGrid_

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
        cldiv_ "beta" "beta"
      case st ^. stateSession of
        StateLoggedIn liSt ->
          case liSt ^. LoggedIn.stateSubState of
            LoggedIn.ProjectDetail pdSt -> do
              tables_ (fmap (^. Project.descTable) (pdSt ^. Project.stateTables))
                      (pdSt ^. Project.stateCurrentTable)
              projectNameComp_ (pdSt ^. Project.stateProjectId) $ pdSt ^. Project.stateProject
              cldiv_ "navigation" $ do
                btnUserSettings_ liSt
                btnProjectOverView_
            LoggedIn.ChangePasswordForm _ ->
              cldiv_ "navigation" $ do
                btnUserSettings_ liSt
                btnProjectOverView_
            _  ->
              cldiv_ "navigation" $ btnUserSettings_ liSt
        StateLoggedOut _  -> pure ()
  where
    btnUserSettings_ :: LoggedIn.State -> ReactElementM ViewEventHandler ()
    btnUserSettings_ liSt = cldiv_ "user-settings" $ do
        button_
          [ onClick $ \_ _ -> if isOpen then [] else toggle
          ] $ faIcon_ "cog"
        when isOpen $ onClickOutside_ (\(_ :: Text) -> toggle) $
          cldiv_ "small-menu" $ do
            menuItem_ "power-off"
                      (elemText $ "Log out (" <> shortName <> ")")
                      (toggle <> dispatchLoggedIn LoggedIn.Logout)
            menuItem_ "pencil" "Change password"
                      (toggle <> dispatchLoggedIn LoggedIn.ToChangePasswordForm)
      where
        isOpen = liSt ^. LoggedIn.stateUserSettingsShow
        toggle = dispatchLoggedIn LoggedIn.ToggleUserSettingsDialog
        shortName =
          if Text.length userName > 12
          then Text.take 9 userName <> "..."
          else userName
        userName = liSt ^. LoggedIn.stateUserInfo . uiUserName

    btnProjectOverView_ :: ReactElementM ViewEventHandler ()
    btnProjectOverView_ = button_
      [ onClick $ \_ _ ->
          dispatchLoggedIn LoggedIn.ToProjectOverview
      ] $ do
        faIcon_ "th-large"
        " Projects"

data ProjectNameState = ProjectNameState
  { pnsEditable  :: Bool
  , pnsName      :: Text
  , pnsNameError :: Bool
  } deriving (Generic, Show, NFData)

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
              , onBlur $ \_ _ st -> ([], Just st { pnsEditable = False })
              ]
        else do
          span_ $ elemText $ project ^. projectClientName
          " "
          button_
            [ classNames [ ("pure"   , True)
                         , ("on-dark", True)
                         ]
            , "title" $= "Change project name"
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
            , "title" $= "Delete project (careful!)"
            , onClick $ \ev _ _ ->
              (stopPropagation ev :
                 dispatchLoggedIn (LoggedIn.DeleteProject projectId), Nothing)
            ] $ faIcon_ "times"
  where
    -- saveHandler :: TileState -> ([SomeStoreAction], Maybe TileState)
    saveHandler st@ProjectNameState{..} =
      (dispatchProject $ Project.SetName pnsName, Just st { pnsEditable = False })

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

tables_ :: Map (Id Table) Table -> Maybe (Id Table) -> ReactElementM eh ()
tables_ !ts !mTbl = view tables (ts, mTbl) mempty

tables :: ReactView (Map (Id Table) Table, Maybe (Id Table))
tables = defineView "tables" $ \(ts, mTbl) ->
  cldiv_ "tables" $ do
    ul_ $ for_ (Map.toList ts) $
      \(tableId, table') -> table_' tableId table' (Just tableId == mTbl)
    inputNew_ "Add table..." (dispatchProjectCommand . CmdTableCreate)

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
  let saveHandler st = (dispatchProjectCommand $ CmdTableSetName tableId (tName st), Just st { tEditable = False })
      inputKeyDownHandler _ evt st
        | keyENTER evt && not (Text.null $ tName st) = saveHandler st
        | keyESC evt = ([] , Just st { tEditable = False })
        | otherwise = ([], Just st)
  in li_ [ classNames
             [ ("active", selected)
             , ("link", True)
             ]
         , onClick $ \_ _ _ -> (dispatchProject $ Project.OpenTable tableId, Nothing)
         ] $
     if tEditable state
     then input_
            [ "value" &= tName state
            , "autoFocus" &= True
            , onChange $ \ev st ->
              let value = target ev "value"
              in  ([], Just st { tName = value, tNameError = Text.null value})
            , onKeyDown inputKeyDownHandler
            , onBlur $ \_ _ st -> ([], Just st { tEditable = False })
            ]
     else do
       span_ $ elemText $ table' ^. tableName

       when selected $ do
         button_
           [ "className" $= "pure on-dark"
           , "title" $= "Change table name"
           , onClick $ \ev _ st -> ([stopPropagation ev], Just st { tEditable = True, tName = table'  ^. tableName })
           ] $ faIcon_ "pencil"

         button_
           [ "className" $= "pure on-dark"
           , "title" $= "Delete table (careful!)"
           , onClick $ \ev _ _ -> (stopPropagation ev : dispatchProjectCommand (CmdTableDelete tableId), Nothing)
           ] $ faIcon_ "times"
