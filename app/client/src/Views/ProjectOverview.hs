module Views.ProjectOverview where

import           Control.Lens        ((^.))
import           Data.Foldable       (for_)
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           React.Flux          (ReactElementM, ReactView, classNames,
                                      cldiv_, defineView, div_, elemText, h1_,
                                      onClick, p_, span_, view, viewWithSKey)
import           React.Flux.Internal (toJSString)

import           Lib.Model.Project   (ProjectClient, projectClientName)
import           Lib.Types           (Id)
import           LoggedIn            (Action (CreateProject, ToProject))
import           Store               (dispatchLoggedIn)
import           Views.Combinators   (inputNew_)

projectsOverview_ :: Map (Id ProjectClient) ProjectClient -> ReactElementM eh ()
projectsOverview_ !ps = view projectsOverview ps mempty

projectsOverview :: ReactView (Map (Id ProjectClient) ProjectClient)
projectsOverview = defineView "projects" $ \ps -> cldiv_ "projects" $ do
  h1_ "My Projects"
  div_
    [ classNames [ ("tile", True)
                 , ("new" , True)
                 ]
    ] $ p_ $ do
      "Create new ..."
      inputNew_ "Project name" (dispatchLoggedIn . CreateProject)
  for_ (Map.toList ps) $ uncurry projectTile_

projectTile_ :: Id ProjectClient -> ProjectClient -> ReactElementM eh ()
projectTile_ !projectId !project = viewWithSKey projectTile (toJSString $ show projectId) (projectId, project) mempty

projectTile :: ReactView (Id ProjectClient, ProjectClient)
projectTile = defineView "project" $
    \(projectId, project) -> div_
      [ classNames [ ("tile", True) ]
      , onClick $ \_ _ -> dispatchLoggedIn $ ToProject projectId
      ] $ span_ $ elemText $ project ^. projectClientName
