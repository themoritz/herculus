module Widgets where

import Control.Lens

import Data.Default

import Reflex.Dom

import Lib

data ProjectListConfig t = ProjectListConfig
  { _projectListConfig_updateProjects :: Event t [Project]
  }

makeLenses ''ProjectListConfig

instance Reflex t => Default (ProjectListConfig t) where
  def = ProjectListConfig
    { _projectListConfig_updateProjects = never
    }

data ProjectList t = ProjectList
  { _projectList_selectProject :: Event t (Id Project)
  }

makeLenses ''ProjectList
