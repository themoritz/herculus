module Views where

import Control.Lens hiding (view)

import Data.Text as Text
import Data.Foldable (for_)

import React.Flux
import React.Flux.Internal (toJSString)

import Lib.Types
import Lib.Model
import Lib.Model.Types

import Store

app :: ReactView ()
app = defineControllerView "app" store $ \st () ->
  cldiv_ "container" $ do
    cldiv_ "sidebar" $ do
      projects_ (st ^. stateProjects) (st ^. stateProjectId)
      case st ^. stateProjectId of
        Nothing -> pure ()
        Just prjId -> tables_ (st ^. stateTables) (st ^. stateTableId) prjId
    -- cldiv_ "table" $ do

--

projects_ :: [Entity Project] -> Maybe (Id Project) -> ReactElementM eh ()
projects_ !ps !mProj = view projects (ps, mProj) mempty

projects :: ReactView ([Entity Project], Maybe (Id Project))
projects = defineView "projects" $ \(ps, mProj) -> do
  h3_ "Projects"
  inputNew_ "New project..." (dispatch . ProjectsCreate . Project)
  ul_ $ for_ ps $ \p -> project_ p (Just (entityId p) == mProj)

project_ :: Entity Project -> Bool -> ReactElementM eh ()
project_ !p !s = viewWithSKey project (toJSString $ show $ entityId p) (p, s) mempty

project :: ReactView (Entity Project, Bool)
project = defineView "project" $ \(Entity i p, selected) ->
  li_ $ a_
    [ "href" $= "#"
    , onClick $ \e _ -> dispatch $ ProjectsLoadProject i
    ] $ if selected
           then b_ $ elemText $ projectName p
           else elemText $ projectName p

--

tables_ :: [Entity Table] -> Maybe (Id Table) -> Id Project -> ReactElementM eh ()
tables_ !ts !mTbl !prj = view tables (ts, mTbl, prj) mempty

tables :: ReactView ([Entity Table], Maybe (Id Table), Id Project)
tables = defineView "tables" $ \(ts, mTbl, projId) -> do
  h3_ "Tables"
  inputNew_ "New table..." (dispatch . TablesCreate . Table projId)
  ul_ $ for_ ts $ \t -> table'_ t (Just (entityId t) == mTbl)

table'_ :: Entity Table -> Bool -> ReactElementM eh ()
table'_ !t !s = viewWithSKey table (toJSString $ show $ entityId t) (t, s) mempty

table :: ReactView (Entity Table, Bool)
table = defineView "table" $ \(Entity i t, selected) ->
  li_ $ a_
    [ "href" $= "#"
    , onClick $ \e _ -> dispatch $ TablesLoadTable i
    ] $ if selected
           then b_ $ elemText $ tableName t
           else elemText $ tableName t

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
        if keyCode evt == 13 && not (Text.null curState) -- 13 = Enter
           then (cb curState, Just "")
           else ([], Nothing)
    ]
