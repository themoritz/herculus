-- |

module Views.ProjectOverview where

import Control.Lens ((^.))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable (for_)
import React.Flux.Internal (toJSString)
import React.Flux (ReactElementM, ReactView, view, defineView, ul_, li_, viewWithSKey, classNames, span_, elemText, onClick, div_, cldiv_, h1_, p_)

import Lib.Model.Project (Project, projectName)
import Lib.Types (Id)
import Store (dispatch)
import Action (Action (ProjectsCreate, ProjectsLoadProject))
import Views.Combinators (inputNew_)

projectsOverview_ :: Map (Id Project) Project -> ReactElementM eh ()
projectsOverview_ !ps = view projectsOverview ps mempty

projectsOverview :: ReactView (Map (Id Project) Project)
projectsOverview = defineView "projects" $ \ps -> cldiv_ "projects" $ do
  h1_ "My projects"
  div_
    [ classNames [ ("tile", True)
                 , ("new" , True)
                 ]
    ] $ p_ $ do
      "Create new ..."
      inputNew_ "Project name" (dispatch . ProjectsCreate)
  for_ (Map.toList ps) $ uncurry projectTile_

projectTile_ :: Id Project -> Project -> ReactElementM eh ()
projectTile_ !projectId !project = viewWithSKey projectTile (toJSString $ show projectId) (projectId, project) mempty

projectTile :: ReactView (Id Project, Project)
projectTile = defineView "project" $ \(projectId, project) ->
  div_
    [ classNames [ ("tile", True)]
    , onClick $ \_ _ -> dispatch $ ProjectsLoadProject projectId
    ] $ p_ $ elemText $ project ^. projectName

-- TODO: projectDetail will be become the projectView of ProjectDetailView

-- projectInfo_ :: Id Project -> Project -> ReactElementM eh ()
-- projectInfo_ !projectId !project' =
--   viewWithSKey project (toJSString $ show projectId) (projectId, project') mempty
--
-- projectInfo :: ReactView (Id Project, Project)
-- projectInfo = defineStatefulView "project" initialProjectInfoViewState $ \state (projectId, project') ->
--   let saveHandler st = (dispatch $ ProjectSetName (pName st), Just st { pEditable = False })
--       inputKeyDownHandler _ evt st
--         | keyENTER evt && not (Text.null $ pName st) = saveHandler st
--         | keyESC evt = ([] , Just st { pEditable = False })
--         | otherwise = ([], Just st)
--   in li_
--      [ classNames
--        [ ("link", True)
--        ]
--      , onClick $ \_ _ _ -> (dispatch $ ProjectsLoadProject projectId, Nothing)
--      ] $
--      if pEditable state
--      then div_ $ input_
--             [ classNames
--               [ ("inp", True)
--               , ("inp-error", pNameError state)
--               ]
--             , "value" &= pName state
--             , onChange $ \evt st ->
--                 let value = target evt "value"
--                 in ([], Just st { pName = value, pNameError = Text.null value})
--             , onKeyDown inputKeyDownHandler
--             ]
--      else div_ $ do
--        span_ $ elemText $ project' ^. projectName
--        button_
--          [ "className" $= "pure link-on-dark"
--          , onClick $ \ev _ st -> ([stopPropagation ev], Just st { pEditable = True, pName = project' ^. projectName})
--          ] $ faIcon_ "pencil"
--
--        button_
--          [ "className" $= "pure link-on-dark"
--          , onClick $ \ev _ _ -> (stopPropagation ev : dispatch (ProjectDelete projectId), Nothing)
--          ] $ faIcon_ "times"

-- data ProjectInfoViewState = ProjectInfoViewState
--   { pEditable  :: Bool
--   , pName      :: Text
--   , pNameError :: Bool
--   } deriving (Generic, Show, NFData)
--
-- initialProjectInfoViewState :: ProjectInfoViewState
-- initialProjectInfoViewState = ProjectInfoViewState False "" False

--
