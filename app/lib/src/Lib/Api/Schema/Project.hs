{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
-- |

module Lib.Api.Schema.Project where

import           Lib.Prelude

import           Control.Lens            (makeLenses)

import           Data.Aeson              (FromJSON, ToJSON)
import           Data.Text               (Text)

import           Lib.Api.Schema.Column
import           Lib.Api.Schema.Compiler
import           Lib.Model
import qualified Lib.Model.Auth          as M
import qualified Lib.Model.Cell          as M
import qualified Lib.Model.Column        as M
import qualified Lib.Model.Project       as M
import qualified Lib.Model.Row           as M
import qualified Lib.Model.Table         as M
import           Lib.Types


-- | All the critical commands that should be atomic, undoable, replayable etc.
-- within a project.
data Command
  = CmdTableCreate Text
  | CmdTableSetName (Id M.Table) Text
  | CmdTableDelete (Id M.Table)
  | CmdDataColCreate (Id M.Table)
  | CmdDataColUpdate (Id M.Column) M.DataType M.IsDerived Text
  | CmdReportColCreate (Id M.Table)
  | CmdReportColUpdate (Id M.Column) Text M.ReportFormat (Maybe M.ReportLanguage)
  | CmdColumnSetName (Id M.Column) Text
  | CmdColumnDelete (Id M.Column)
  | CmdRowCreate (Id M.Table)
  | CmdRowDelete (Id M.Row)
  | CmdCellSet (Id M.Column) (Id M.Row) M.Value
  deriving (Generic, ToJSON, FromJSON, Show)

data Project = Project
  { _projectId           :: Id M.Project
  , _projectName         :: Text
  , _projectOwner        :: Id M.User
  , _projectModuleSource :: Text
  } deriving (Generic, ToJSON, FromJSON, Show)

projectFromEntity :: Entity M.Project -> Project
projectFromEntity (Entity i (M.Project name owner src _ _)) =
  Project i name owner src

makeLenses ''Project

data ProjectData = ProjectData
  { _pdProject      :: Project
  , _pdTables       :: [Entity M.Table]
  , _pdColumns      :: [Column]
  , _pdRows         :: [Entity M.Row]
  , _pdCells        :: [Entity M.Cell]
  , _pdColumnSizes  :: [(Id M.Column, Int)]
  , _pdColumnOrders :: [(Id M.Table, [Id M.Column])]
  , _pdTypes        :: [(Text, TyconInfo)]
  } deriving (Generic, ToJSON, FromJSON)

makeLenses ''ProjectData
