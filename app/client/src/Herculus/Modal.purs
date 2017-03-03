module Herculus.Modal where

import Herculus.Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Herculus.Monad (Herc)
import Herculus.Utils (cldiv_, faIcon_)

data Query v a
  = Update (Input v) a
  | Open a
  | ClickAction v a

type Action v =
  { icon :: String
  , label :: String
  , value :: v
  }

type Input v =
  { title :: String
  , text :: String
  , actions :: Array (Action v)
  }

type State v =
  { input :: Input v
  , open :: Boolean
  }

comp :: forall v. H.Component HH.HTML (Query v) (Input v) v Herc
comp = H.component
  { initialState: { input: _, open: false }
  , render
  , eval
  , receiver: Just <<< H.action <<< Update
  }

render :: forall v. State v -> H.ComponentHTML (Query v)
render st = case st.open of
  false -> HH.text ""
  true -> cldiv_ "modal__container"
    [ cldiv_ "modal"
      [ cldiv_ "h3 m0 p1 lightgray bold bg-anthrazit"
        [ HH.text st.input.title ]
      , cldiv_ "p1"
        [ HH.text st.input.text ]
      , cldiv_ "bg-lightgray right-align p1"
        (map renderAction st.input.actions)
      ]
    ]

  where

  renderAction action = HH.button
    [ HE.onClick $ HE.input_ $ ClickAction action.value
    , HP.class_ $ H.ClassName "button ml1"
    ]
    [ faIcon_ (action.icon <> " mr1")
    , HH.text action.label
    ]

eval :: forall v. Query v ~> H.ComponentDSL (State v) (Query v) v Herc
eval = case _ of

  Update input next -> do
    modify _{ input = input }
    pure next

  Open next -> do
    modify _ { open = true }
    pure next

  ClickAction v next -> do
    modify _ { open = false }
    H.raise v
    pure next
