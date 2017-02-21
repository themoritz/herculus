module Herculus.ProjectOverview where

import Herculus.Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Herculus.Router as R
import DOM.Event.KeyboardEvent (code)
import Halogen.HTML.Events (onClick)
import Herculus.Monad (Herc, gotoRoute, withApi)
import Herculus.Utils (cldiv, cldiv_)
import Lib.Api.Rest (getProjectList, postProjectCreate)
import Lib.Api.Schema.Project (Project(..))
import Lib.Custom (Id, ProjectTag)

data Query a
  = Initialize a
  | SelectProject (Id ProjectTag) a
  | SetNewName String a
  | CreateProject a

type State =
  { projects :: Array Project
  , newName :: String
  }

comp :: H.Component HH.HTML Query Unit Void Herc
comp = H.lifecycleComponent
  { initialState: const
      { projects: []
      , newName: ""
      }
  , render
  , eval
  , receiver: const Nothing
  , initializer: Just (H.action Initialize)
  , finalizer: Nothing
  }

  where

  render :: State -> H.ComponentHTML Query
  render st = cldiv_ "p3" (
    [ HH.h1
      [ HP.class_ (HH.ClassName "h2 m0 mb3") ]
      [ HH.text "My Projects" ]
    , cldiv_ "project-tile mr3 mb3 p2 bg-lightgray bold"
      [ HH.text "Create new..."
      , HH.input
        [ HE.onValueInput (HE.input SetNewName)
        , HE.onKeyDown \e -> case code e of
            "Enter" -> Just (H.action CreateProject)
            _       -> Nothing
        ]
      ]
    ] <> map renderTile st.projects)

    where

    renderTile :: Project -> H.ComponentHTML Query
    renderTile (Project p) = cldiv "project-tile mr3 mb3 p2"
      [ onClick $ HE.input_ $ SelectProject p._projectId ]
      [ HH.span_
        [ HH.text p._projectName ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Void Herc
  eval (Initialize next) = do
    withApi getProjectList \ps ->
      modify _{ projects = ps }
    pure next

  eval (SelectProject p next) = do
    gotoRoute $ R.LoggedIn $ R.ProjectDetail $ R.Project p Nothing
    pure next

  eval (SetNewName val next) = do
    modify _{ newName = val }
    pure next

  eval (CreateProject next) = do
    { newName } <- H.get
    withApi (postProjectCreate newName) \(Project p) ->
      gotoRoute $ R.LoggedIn $ R.ProjectDetail $ R.Project p._projectId Nothing
    pure next
    
