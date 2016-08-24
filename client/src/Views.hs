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
      projectList_ (st ^. stateProjects) (st ^. stateProjectId)
      -- tableList_ (st ^. stateTables) (st ^. stateTableId)
    -- cldiv_ "table" $ do

--

projectList_ :: [Entity Project] -> Maybe (Id Project) -> ReactElementM eh ()
projectList_ !ps !mProj = view projectList (ps, mProj) mempty

projectList :: ReactView ([Entity Project], Maybe (Id Project))
projectList = defineStatefulView "projectList" ("" :: Text) $ \curText (ps, mProj) -> do
  h5_ "Projects"
  input_
    [ "placeholder" $= "New project..."
    , "value" &= curText
    , onChange $ \evt _ -> ([], Just $ target evt "value")
    , onKeyDown $ \_ evt curState ->
        if keyCode evt == 13 && not (Text.null curState) -- 13 = Enter
           then (dispatch $ ProjectsCreate $ Project curState, Just "")
           else ([], Nothing)
    ]
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
