module Views.ProjectOverview where

import           Control.Lens        ((^.))
import           Data.Foldable       (for_)
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           React.Flux          (ReactElementM, ReactView, classNames,
                                      cldiv_, defineView, div_, elemText, h1_,
                                      onClick, p_, span_, view, viewWithSKey)
import           React.Flux.Internal (toJSString)

import           Action              (Action (ProjectsCreate, ProjectsLoadProject))
import           Lib.Model.Project   (Project, projectName)
import           Lib.Types           (Id)
import           Store               (dispatch)
import           Views.Combinators   (inputNew_)

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
projectTile = defineView "project" $
    \(projectId, project) -> div_
      [ classNames [ ("tile", True) ]
      , onClick $ \_ _ -> dispatch $ ProjectsLoadProject projectId
      ] $ span_ $ elemText $ project ^. projectName
