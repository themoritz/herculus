module Herculus.DataCell where

import Herculus.Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Herculus.Monad (Herc)
import Lib.Api.Schema.Column (DataCol(..))
import Lib.Model.Cell (CellContent, Value)

data Query a
  = Update Input a
  | SetValue' Value a

type Input =
  { content :: CellContent
  , dataCol :: DataCol
  }

data Output = SetValue Value

type State =
  { input :: Input
  }

type Child =
  Const Void

type Slot =
  Void

comp :: H.Component HH.HTML Query Input Output Herc
comp = H.parentComponent
  { initialState:
    { input: _
    }
  , receiver: const Nothing
  , render
  , eval
  }

render :: State -> H.ParentHTML Query Child Slot Herc
render st = HH.text "Cell"

eval :: Query ~> H.ParentDSL State Query Child Slot Output Herc
eval = case _ of

  Update input next -> do
    modify _{ input = input }
    pure next

  SetValue' val next -> do
    H.raise $ SetValue val
    pure next
