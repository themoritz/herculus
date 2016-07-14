{-# LANGUAGE TemplateHaskell #-}

module Widgets.ProjectList
  ( ProjectListConfig
  , projectListConfig_newProject
  , ProjectList
  , projectList_selectProject
  , projectList
  ) where

import Control.Lens

import Data.Default
import Data.Text (Text, pack, unpack)
import Data.Map (Map)
import qualified Data.Map as Map

import Reflex.Dom

import Lib

import Api.Rest (loader, api)
import qualified Api.Rest as Api

data ProjectListConfig t = ProjectListConfig
  { _projectListConfig_newProject :: Event t Project
  }

makeLenses ''ProjectListConfig

instance Reflex t => Default (ProjectListConfig t) where
  def = ProjectListConfig
    { _projectListConfig_newProject = never
    }

data ProjectList t = ProjectList
  { _projectList_selectProject :: Event t (Id Project)
  }

makeLenses ''ProjectList

type State = Map (Id Project) Text

data Action
  = New Project
  | Set [Project]

update :: Action -> State -> State
update (New (Project i name)) ps = Map.insert i name ps
update (Set ps) _ = Map.fromList $ map (\(Project i name) -> (i, name)) ps

projectList :: MonadWidget t m => ProjectListConfig t -> m (ProjectList t)
projectList (ProjectListConfig newProject) = elClass "div" "container" $ do
  el "h4" $ text "Projects"
  createdProject <- elClass "div" "row" $ do
    name <- (fmap pack . current . _textInput_value) <$> textInput def
    create <- button "Create"
    newProj <- loader (Api.projectCreate api (Right <$> name)) create
    pure $ attachWith (flip Project) name newProj
  listProjects <- elClass "div" "row" $ button "List"
  listResult <- loader (Api.projectList api) listProjects
  projects <- foldDyn update Map.empty $ leftmost
    [ New <$> newProject
    , New <$> createdProject
    , Set <$> listResult
    ]
  projectSelect <- elClass "div" "row" $
    el "table" $ el "tbody" $ list projects $ \name ->
      el "tr" $ el "td" $ do
        (proj, _) <- elAttr' "a" ("href" =: "#") $
          dynText =<< mapDyn unpack name
        pure $ domEvent Click proj
  ProjectList . switchPromptlyDyn <$> mapDyn (leftmost . map (\(k, e) -> k <$ e) . Map.toList) projectSelect
