module Herculus.Project where

import Herculus.Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Herculus.Router as R
import Data.Map (Map)
import Herculus.Monad (Herc)
import Lib.Api.Schema.Auth (UserInfo)
import Lib.Api.Schema.Column (Column)
import Lib.Api.Schema.Project (Project)
import Lib.Custom (Id, ProjectTag)
import Lib.Model (Entity)
import Lib.Model.Cell (Cell, CellContent)
import Lib.Model.Row (Row)
import Lib.Model.Table (Table)
import Lib.Types (ChangeOp)

type Diff a = Array (Tuple ChangeOp a)

data Query a
  = Initialize a
  | Update Input a
  | OpenTable (Id Table) a
  | LoadProject (Id ProjectTag) a
  | SetName String a
  | ApplyDiff (Diff (Entity Cell))
              (Diff Column)
              (Diff (Entity Row))
              (Diff (Entity Table))
              a

data Input = Input UserInfo R.Project

data Coords = Coords (Id Column) (Id Row)

type State =
  { projectData :: Maybe
    { project :: Project
    , cells :: Map Coords CellContent
    }
  , view :: Maybe (Id Table)
  , projectId :: Id ProjectTag
  , userInfo :: UserInfo
  }

type ChildQuery =
  Const Void

type ChildSlot = Void

comp :: H.Component HH.HTML Query Input Void Herc
comp = H.lifecycleParentComponent
  { initialState
  , render
  , eval
  , receiver: Just <<< H.action <<< Update
  , initializer: Just (H.action Initialize)
  , finalizer: Nothing
  }

  where

  initialState :: Input -> State
  initialState (Input ui (R.Project p mT)) =
    { projectData: Nothing
    , view: mT
    , projectId: p
    , userInfo: ui
    }

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot Herc
  render st = HH.text "ProjectDetail"

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void Herc
  eval (Initialize next) = do
    pure next

  eval (Update (Input ui (R.Project p mT)) next) = do
    pure next

  eval (OpenTable i next) = do
    pure next

  eval (LoadProject i next) = do
    pure next

  eval (SetName name next) = do
    pure next

  eval (ApplyDiff cellDiff columnDiff rowDiff tableDiff next) = do
    pure next
