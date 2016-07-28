{-# LANGUAGE TemplateHaskell #-}

module Widgets.ProjectList
  ( ProjectList (..)
  , projectList
  ) where

import Data.Text (Text, pack, unpack)
import Data.Map (Map)
import qualified Data.Map as Map

import Reflex.Dom

import Lib.Types
import Lib.Model
import Lib.Model.Types

import Api.Rest (loader, api)
import qualified Api.Rest as Api

type State = Map (Id Project) Text

data Action
  = New (Entity Project)
  | Set [Entity Project]

update :: Action -> State -> State
update (New (Entity i (Project name))) ps = Map.insert i name ps
update (Set ps) _ = Map.fromList $ map (\(Entity i (Project name)) -> (i, name)) ps

data ProjectList t = ProjectList
  { _projectList_selectProject :: Event t (Id Project)
  }

projectList :: MonadWidget t m
            => Event t (Entity Project)
            -> m (ProjectList t)
projectList newProject = divClass "container" $ do
  el "h5" $ text "Projects"
  createdProject <- divClass "row" $ do
    name <- (fmap pack . current . _textInput_value) <$> textInput def
    create <- button "Create"
    newProj <- loader (Api.projectCreate api (Right . Project <$> name)) create
    pure $ attachWith (\n i -> Entity i (Project n)) name newProj
  listProjects <- getPostBuild
  listResult <- loader (Api.projectList api) listProjects
  projects <- foldDyn update Map.empty $ leftmost
    [ New <$> newProject
    , New <$> createdProject
    , Set <$> listResult
    ]
  projectSelect <- divClass "row" $
    el "ul" $ list projects $ \name ->
      el "li" $ do
        (proj, _) <- elAttr' "a" ("href" =: "#") $
          dynText =<< mapDyn unpack name
        pure $ domEvent Click proj
  ProjectList . switchPromptlyDyn <$> mapDyn (leftmost . map (\(k, e) -> k <$ e) . Map.toList) projectSelect
