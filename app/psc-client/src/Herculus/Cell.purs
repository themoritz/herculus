module Herculus.Cell where

import Herculus.Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Herculus.Monad (Herc)

data Query a
  = Update a

type State = Unit

type Child =
  Const Void

type Slot =
  Void

comp :: H.Component HH.HTML Query Unit Void Herc
comp = H.parentComponent
  { initialState: const unit
  , receiver: const Nothing
  , render
  , eval
  }

render :: State -> H.ParentHTML Query Child Slot Herc
render st = HH.text "Cell"

eval :: Query ~> H.ParentDSL State Query Child Slot Void Herc
eval = case _ of

  Update next ->
    pure next
