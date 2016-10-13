{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Views where

import           Control.Lens        hiding (view)

import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map

import           Data.Foldable       (for_)
import           Data.Text           (Text)
import qualified Data.Text           as Text

import           React.Flux
import           React.Flux.Internal (toJSString)

import           Lib.Model.Project
import           Lib.Model.Table
import           Lib.Types

import           Store
import           Views.Auth          (login_, logout_)
import           Views.Table         (tableGrid_)

import           Helper              (keyENTER, keyESC)

import           Control.DeepSeq     (NFData)
import           GHC.Generics        (Generic)

app :: ReactView ()
app = defineControllerView "app" store $ \st () ->
  cldiv_ "container" $ do
    appHeader_ st
    appContent_ st
    appFooter_ st

-- header

appHeader_ :: State -> ReactElementM eh ()
appHeader_ !st = view appHeader st mempty

appHeader :: ReactView State
appHeader = defineView "login" $ \st ->
  cldiv_ "menubar" $ do
    cldiv_ "logo" "Herculus"
    case st ^. stateSessionKey of
      Nothing -> pure ()
      Just _ -> projects_ (st ^. stateProjects) (st ^. stateProjectId)
    case st ^. stateSessionKey of
      Nothing -> pure ()
      Just _ -> case st ^. stateProjectId of
                  Nothing -> pure ()
                  Just prjId -> tables_ (st ^. stateTables) (st ^. stateTableId) prjId
    case st ^. stateError of
      Nothing -> pure ()
      Just t -> do
        h3_ "Error"
        elemText t

-- content

appFooter_ :: State -> ReactElementM eh ()
appFooter_ !st = view appFooter st mempty

appFooter :: ReactView State
appFooter = defineView "login" $ \st ->
  cldiv_ "footer" $ do
    a_ [ "href" $= "mailto:Moritz <mdrexl@fastmail.fm>, Ruben <ruben.moor@gmail.com>"
      , "className" $= "link-on-dark"
      , "target" $= "_blank"
      ] "Contact"
    case st ^. stateSessionKey of
      Nothing -> pure ()
      Just _ -> logout_ st

-- content

appContent_ :: State -> ReactElementM eh ()
appContent_ !st = view appContent st mempty

appContent :: ReactView State
appContent = defineView "login" $ \st ->
  cldiv_ "tableGrid" $
    case st ^. stateSessionKey of
      Nothing -> login_ st
      Just _ -> tableGrid_ st


--

projects_ :: Map (Id Project) Project -> Maybe (Id Project) -> ReactElementM eh ()
projects_ !ps !mProj = view projects (ps, mProj) mempty

projects :: ReactView (Map (Id Project) Project, Maybe (Id Project))
projects = defineView "projects" $ \(ps, mProj) ->
  cldiv_ "projects" $ do
    ul_ $ for_ (Map.toList ps) $ \(i, p) -> project_ i p (Just i == mProj)
    inputNew_ "Add project..." (dispatch . ProjectsCreate . Project)

data ProjectViewState = ProjectViewState
  { pEditable  :: Bool
  , pName      :: Text
  , pNameError :: Bool
  } deriving (Generic, Show, NFData)

initialProjectViewState :: ProjectViewState
initialProjectViewState = ProjectViewState False "" False

project_ :: Id Project -> Project -> Bool -> ReactElementM eh ()
project_ !projectId !project' !selected = viewWithSKey project (toJSString $ show projectId) (projectId, project', selected) mempty

project :: ReactView (Id Project, Project, Bool)
project = defineStatefulView "project" initialProjectViewState $ \state (projectId, project', selected) ->
  let saveHandler st = (dispatch $ ProjectSetName projectId (pName st), Just st { pEditable = False })
      inputKeyDownHandler _ evt st
        | keyENTER evt && not (Text.null $ pName st) = saveHandler st
        | keyESC evt = ([] , Just st { pEditable = False })
        | otherwise = ([], Just st)
  in li_
     [ classNames
       [ ("link", True)
       , ("active", selected)
       ]
     , onClick $ \_ _ _ -> (dispatch $ ProjectsLoadProject projectId, Nothing)
     ] $
     if pEditable state
     then div_ $
          input_
          [  classNames
             [ ("inp", True)
             , ("inp-error", pNameError state)
             ]
          , "value" &= pName state
          , onChange $ \evt st ->
              let value = target evt "value"
              in ([], Just st { pName = value, pNameError = Text.null value})
          , onKeyDown inputKeyDownHandler
          ]
     else div_ $ do
       span_ $ elemText $ project' ^. projectName
       button_
         [ "className" $= "pure link-on-dark"
         , onClick $ \ev _ st -> ([stopPropagation ev], Just st { pEditable = True, pName = project' ^. projectName})
         ] $ faIcon_ "pencil"

       button_
         [ "className" $= "pure link-on-dark"
         , onClick $ \ev _ _ -> (stopPropagation ev : dispatch (ProjectDelete projectId), Nothing)
         ] $ faIcon_ "times"
--

tables_ :: Map (Id Table) Table -> Maybe (Id Table) -> Id Project -> ReactElementM eh ()
tables_ !ts !mTbl !prj = view tables (ts, mTbl, prj) mempty

tables :: ReactView (Map (Id Table) Table, Maybe (Id Table), Id Project)
tables = defineView "tables" $ \(ts, mTbl, projId) ->
  cldiv_ "tables" $ do
    ul_ $ for_ (Map.toList ts) $ \(tableId, table') -> table_' tableId table' (Just tableId == mTbl)
    inputNew_ "Add table..." (dispatch . TablesCreate . Table projId)

--

table_' :: Id Table -> Table -> Bool -> ReactElementM eh ()
table_' !tableId !table' !selected = viewWithSKey table (toJSString $ show tableId) (tableId, table', selected) mempty

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
     then div_ $
       input_
         [  classNames
            [ ("inp", True)
            , ("inp-error", tNameError state)
            ]
         , "value" &= tName state
         , onChange $ \ev st ->
             let value = target ev "value"
             in ([], Just st { tName = value, tNameError = Text.null value})
         , onKeyDown inputKeyDownHandler
         ]
     else div_ $ do
       span_ $ elemText $ table' ^. tableName

       button_
         [ "className" $= "pure link-on-dark"
         , onClick $ \ev _ st -> ([stopPropagation ev], Just st { tEditable = True, tName = table'  ^. tableName })
         ] $ faIcon_ "pencil"

       button_
         [ "className" $= "pure link-on-dark"
         , onClick $ \ev _ _ -> (stopPropagation ev : dispatch (TableDelete tableId), Nothing)
         ] $ faIcon_ "times"
--

inputNew_ :: Text -> (Text -> [SomeStoreAction]) -> ReactElementM eh ()
inputNew_ !p !cb = view inputNew (p, cb) mempty

inputNew :: ReactView (Text, Text -> [SomeStoreAction])
inputNew = defineStatefulView "inputNew" ("" :: Text) $ \curText (p, cb) ->
  input_
    [ "placeholder" &= p
    , "value" &= curText
    , onChange $ \evt _ -> ([], Just $ target evt "value")
    , onKeyDown $ \_ evt curState ->
        if keyENTER evt && not (Text.null curState)
           then (cb curState, Just "")
           else ([], Nothing)
    ]
