module Herculus.Row where

import Herculus.Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Herculus.Monad (Herc)

data Query a
  = Update a

type State = Unit

comp :: H.Component HH.HTML Query Unit Void Herc
comp = H.component
  { initialState: const unit
  , receiver: const Nothing
  , render
  , eval
  }

render :: State -> H.ComponentHTML Query
render st = HH.text "Row"

eval :: Query ~> H.ComponentDSL State Query Void Herc
eval = case _ of

  Update next ->
    pure next
