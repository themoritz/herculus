module Herculus.Row where

import Herculus.Prelude
import Halogen as H
import Halogen.HTML as HH
import Herculus.Monad (Herc)
import Herculus.Utils (faButton_)

data Query a
  = ClickDelete a

type State = Unit

data Output = Delete

comp :: H.Component HH.HTML Query Unit Output Herc
comp = H.component
  { initialState: const unit
  , receiver: const Nothing
  , render
  , eval
  }

render :: State -> H.ComponentHTML Query
render st = faButton_ "minus-circle" ClickDelete

eval :: Query ~> H.ComponentDSL State Query Output Herc
eval = case _ of

  ClickDelete next -> do
    H.raise Delete
    pure next
