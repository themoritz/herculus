module Herculus.Grid where

import Herculus.Prelude
import CSS as CSS
import Data.Map as Map
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Data.Array (length)
import Data.Generic (gCompare, gEq)
import Data.Int (toNumber)
import Data.Map (Map)
import Herculus.Monad (Herc)
import Herculus.Project.Data (Coords(..))
import Herculus.Utils (cldiv_)
import Lib.Api.Schema.Column (Column(..))
import Lib.Api.Schema.Project (Command, Project(..))
import Lib.Custom (ColumnTag, Id(..), ProjectTag)
import Lib.Model (Entity(..))
import Lib.Model.Cell (CellContent)
import Lib.Model.Row (Row(..))
import Lib.Model.Table (Table(..))

data Query a
  = Update a

type Input =
  { cells :: Map Coords CellContent
  , cols :: Array Column
  , rows :: Array (Tuple (Id Row) Row)
  , tableId :: Id Table
  , projectId :: Id ProjectTag
  }

type Output = Command

type State =
  { input :: Input
  }

type Child =
  Const Void

data Slot
  = Row (Id Row)
  | Column (Id ColumnTag)
  | Cell (Id ColumnTag) (Id Row)

derive instance genericSlot :: Generic Slot
instance eqSlot :: Eq Slot where eq = gEq
instance ordSlot :: Ord Slot where compare = gCompare

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
render st = cldiv_ "grid"
  ([ posDiv 0 0 delRowWidth headHeight
    [ cldiv_ "grid__origin" [] ]
  -- Add row
  , posDiv 0 (length st.input.rows * cellHeight + headHeight)
           delRowWidth addRowHeight
    [ cldiv_ "grid__row-new"
      [ HH.text "new row"
      ]
    ]
  -- Add col
  , posDiv (length st.input.cols * cellWidth + delRowWidth) 0
           addColWidth headHeight
    [ cldiv_ "grid__column-new"
      [ HH.text "new col"
      ]
    ]
  ])

  where

  headHeight = 54
  cellHeight = 37
  addRowHeight = 37
  delRowWidth = 37
  cellWidth = 230
  addColWidth = 37

  posDiv :: forall p i. Int -> Int -> Int -> Int -> Array (HH.HTML p i) -> HH.HTML p i
  posDiv left top width height content = HH.div
    [ HP.class_ (H.ClassName "grid__generic-cell")
    , HC.style do
        CSS.left   $ CSS.px $ toNumber left
        CSS.top    $ CSS.px $ toNumber top
        CSS.width  $ CSS.px $ toNumber width
        CSS.height $ CSS.px $ toNumber height
    ]
    content

eval :: Query ~> H.ParentDSL State Query Child Slot Output Herc
eval = case _ of

  Update next ->
    pure next
