module Herculus.Config where

import Herculus.Prelude
import Data.Map as Map
import Halogen as H
import Halogen.HTML as HH
import Herculus.Config.Column as Column
import Herculus.Config.Project as Project
import Data.Map (Map)
import Halogen.Component.ChildPath (cp1, cp2, type (\/), type (<\/>))
import Herculus.Monad (Herc)
import Herculus.Utils (Options)
import Lib.Api.Schema.Column (Column)
import Lib.Api.Schema.Compiler (TyconInfo)
import Lib.Api.Schema.Project (Project, projectId)
import Lib.Custom (ColumnTag, Id)
import Lib.Model.Table (Table)

data Query a
  = Update Input a
  | EditColumn (Id ColumnTag) a
  | EditProject a
  | Close' a

type Input =
  { cols :: Map (Id ColumnTag) Column
  , tables :: Options (Id Table)
  , types :: Map String TyconInfo
  , project :: Project
  }

data Output
  = Close

type State =
  { input :: Input
  , view :: View
  }

data View
  = VClosed
  | VColumn (Id ColumnTag)
  | VProject

type Child =
  Column.Query <\/>
  Project.Query <\/>
  Const Void

type Slot =
  Id ColumnTag \/
  Unit \/
  Unit

comp :: H.Component HH.HTML Query Input Output Herc
comp = H.parentComponent
  { initialState:
    { input: _
    , view: VClosed
    }
  , render
  , eval
  , receiver: Just <<< H.action <<< Update
  }

render :: State -> H.ParentHTML Query Child Slot Herc
render st = case st.view of
  VClosed -> HH.text ""
  VColumn colId -> case Map.lookup colId st.input.cols of
    Nothing -> HH.text ""
    Just col ->
      let
        input = 
          { column: col
          , projectId: st.input.project ^. projectId
          , tables: st.input.tables
          , types: st.input.types
          }
        handler o = Just $ H.action case o of
         Column.Close -> Close'
      in
        HH.slot' cp1 colId Column.comp input handler
  VProject ->
    let
      input =
        { project: st.input.project
        }
      handler o = Just $ H.action case o of
        Project.Close -> Close'
    in
      HH.slot' cp2 unit Project.comp input handler

eval :: Query ~> H.ParentDSL State Query Child Slot Output Herc
eval = case _ of

  Update i next -> do
    modify _{ input = i }
    pure next

  EditColumn c next -> do
    modify _{ view = VColumn c }
    pure next

  EditProject next -> do
    modify _{ view = VProject }
    pure next

  Close' next -> do
    modify _{ view = VClosed }
    H.raise Close
    pure next
