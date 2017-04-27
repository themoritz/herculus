module Herculus.Config where

import Herculus.Prelude
import Data.Map as Map
import Halogen as H
import Halogen.HTML as HH
import Herculus.Config.Column as Column
import Data.Map (Map)
import Halogen.Component.ChildPath (cp1, type (\/), type (<\/>))
import Herculus.Monad (Herc)
import Herculus.Utils (Options)
import Lib.Api.Schema.Column (Column)
import Lib.Custom (ColumnTag, Id, ProjectTag)
import Lib.Model.Table (Table)

data Query a
  = Update Input a
  | EditColumn (Id ColumnTag) a
  | Close' a

type Input =
  { cols :: Map (Id ColumnTag) Column
  , tables :: Options (Id Table)
  , projectId :: Id ProjectTag
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

type Child =
  Column.Query <\/>
  Const Void

type Slot =
  Id ColumnTag \/
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
          , projectId: st.input.projectId
          , tables: st.input.tables
          }
        handler o = Just $ H.action case o of
         Column.Close -> Close'
      in
        HH.slot' cp1 colId Column.comp input handler

eval :: Query ~> H.ParentDSL State Query Child Slot Output Herc
eval = case _ of

  Update i next -> do
    modify _{ input = i }
    pure next

  EditColumn c next -> do
    modify _{ view = VColumn c }
    pure next

  Close' next -> do
    modify _{ view = VClosed }
    H.raise Close
    pure next
