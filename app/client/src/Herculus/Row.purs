module Herculus.Row where

import Herculus.Prelude
import Halogen as H
import Halogen.HTML as HH
import Herculus.Monad (Herc)
import Herculus.Utils (cldiv_, faButton_)

data Query a
  = Delete' a

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
render st = cldiv_ "center"
  [ faButton_ "minus-circle fa-lg" Delete'
  ]

eval :: Query ~> H.ComponentDSL State Query Output Herc
eval = case _ of

  Delete' next -> do
    H.raise Delete
    pure next
