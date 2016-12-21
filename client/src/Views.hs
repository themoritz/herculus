{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Views where

import           Control.DeepSeq     (NFData)
import           Control.Lens        hiding (view)
import           Data.Foldable       (for_)
import qualified Data.IntMap.Strict  as IntMap
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import           Data.Monoid         ((<>), mempty)
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           GHC.Generics        (Generic)
import           React.Flux
import           React.Flux.Internal (toJSString)

import           Lib.Model.Auth      (uiUserName)
import           Lib.Model.Project
import           Lib.Model.Table
import           Lib.Types

import           Action              (Action (..))
import           Helper              (keyENTER, keyESC)
import           Store               (LoggedOutState (..),
                                      ProjectViewState (..), SessionState (..),
                                      State, dispatch, stateCells, stateColumns,
                                      stateMessage, stateProjectId,
                                      stateProjectView, stateRecords,
                                      stateSession, stateSessionKey,
                                      stateTableId, stateTables, stateUserInfo,
                                      store)
import           Views.Auth          (login_, logout_, signup_)
import           Views.Combinators   (clspan_, inputNew_)
import           Views.Table         (TableGridProps (..), tableGrid_)
import           Views.ProjectOverview (projectsOverview_)

import           Store.Message       (Message (Message))
import qualified Store.Message       as Message

app :: ReactView ()
app = defineControllerView "app" store $ \st () ->
  cldiv_ "container" $ do
    appHeader_ st
    for_ (st ^. stateMessage) message_
    case st ^. stateSession of
      StateLoggedOut LoggedOutLoginForm -> login_
      StateLoggedOut LoggedOutSignupForm -> signup_
      StateLoggedIn liSt ->
        case liSt ^. stateProjectView of
          StateProjectOverview ps  ->
            cldiv_ "overview" $ projectsOverview_ ps
          StateProjectDetail pdSt ->
            case (pdSt ^. stateProjectId, null $ pdSt ^. stateTables) of
              (projectId, False) ->
                let cols = pdSt ^. stateColumns
                    recs = pdSt ^. stateRecords
                    -- TODO: put tables deeper into view state hierarchy under projectId
                    tableGridProps = TableGridProps
                      { _cells      = pdSt ^. stateCells
                      , _colByIndex = IntMap.fromList $ zip [0..] (Map.toList cols)
                      , _recByIndex = IntMap.fromList $ zip [0..] (Map.toList recs)
                      , _tableId    = pdSt ^. stateTableId
                      , _projectId  = projectId
                      , _sKey       = liSt ^. stateSessionKey
                      }
                in  cldiv_ "tableGrid" $ tableGrid_ tableGridProps
              _ -> cldiv_ "" mempty

    appFooter_ st

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
      StateLoggedIn liSt -> do
        case liSt ^. stateProjectView of
          StateProjectOverview _  -> pure ()
          StateProjectDetail pdSt -> do
            tables_ (pdSt ^. stateTables) (pdSt ^. stateTableId) (pdSt ^. stateProjectId)
            button_
              [ classNames
                [ ("pure", True)
                , ("backToOverview", True)
                , ("on-dark", True)
                ]
              , onClick $ \_ _ -> dispatch $ SetProjectOverview (liSt ^. stateSessionKey)
              ] $ do
                faIcon_ "square-o"
                " Projects"
        logout_ $ liSt ^. stateUserInfo . uiUserName
      StateLoggedOut _  -> pure ()

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

tables_ :: Map (Id Table) Table -> Maybe (Id Table) -> Id Project -> ReactElementM eh ()
tables_ !ts !mTbl !prj = view tables (ts, mTbl, prj) mempty

tables :: ReactView (Map (Id Table) Table, Maybe (Id Table), Id Project)
tables = defineView "tables" $ \(ts, mTbl, projId) ->
  cldiv_ "tables" $ do
    ul_ $ for_ (Map.toList ts) $
      \(tableId, table') -> table_' tableId table' (Just tableId == mTbl)
    inputNew_ "Add table..." (dispatch . TablesCreate . Table projId)

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
            [ classNames
              [ ("inp", True)
              , ("inp-error", tNameError state)
              ]
            , "value" &= tName state
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
