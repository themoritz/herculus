module Herculus.PopupMenu where

import Herculus.Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Control.Monad.Aff.AVar (AVAR)
import DOM.HTML.Types (HTMLElement)
import Halogen.Query.HalogenM (halt)
import Herculus.Monad (Herc)
import Herculus.Utils (cldiv_, faIcon_)

data Query v a
  = Update (Input v) a
  | Toggle a
  | Open a
  | Close a
  | Select v a
  | HandleClickOutside (H.SubscribeStatus -> a)

data Position
  = BelowLeft
  | BelowRight

type Entry v =
  { icon :: String
  , label :: String
  , value :: v
  }

type Input v =
  { position :: Position
  , entries :: Array (Entry v)
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

  where

  render :: State v -> H.ComponentHTML (Query v)
  render st = HH.div
    [ HP.ref (H.RefLabel "container") ]
    [ case st.open of
        true -> cldiv_ ("popup-menu  " <> position)
          (map menuItem st.input.entries)
        false -> HH.text ""
    ]

    where

    position = case st.input.position of
      BelowLeft -> "popup-menu--below-left"
      BelowRight -> "popup-menu--below-right"

    menuItem :: Entry v -> H.ComponentHTML (Query v)
    menuItem e = HH.button
      [ HP.class_ (H.ClassName "popup-menu__item")
      , HE.onClick $ HE.input_ (Select e.value)
      ]
      [ cldiv_ "popup-menu__item-icon"
        [ faIcon_ e.icon
        ]
      , cldiv_ "popup-menu__item-label"
        [ HH.text e.label ]
      ]

  eval :: Query v ~> H.ComponentDSL (State v) (Query v) v Herc

  eval (Update input next) = do
    modify _{ input = input }
    pure next

  eval (Toggle next) = do
    { open } <- get
    setOpen (not open)
    pure next

  eval (Open next) = do
    setOpen true
    pure next

  eval (Close next) = do
    setOpen false
    pure next

  eval (Select v next) = do
    setOpen false
    H.raise v
    pure next

  eval (HandleClickOutside reply) = do
    setOpen false
    pure $ reply H.Done

  setOpen :: Boolean -> H.ComponentDSL (State v) (Query v) v Herc Unit
  setOpen open = do
    modify _{ open = open }
    H.getHTMLElementRef (H.RefLabel "container") >>= case _ of
      Nothing -> halt "PopupMenu: Could not find container."
      Just el -> case open of
        true -> 
          H.subscribe $ H.eventSource_
            (attachClickOutside el)
            (H.request HandleClickOutside)
        false ->
          liftEff (detachClickOutside el)

foreign import attachClickOutside
  :: forall eff
   . HTMLElement
  -> Eff (avar :: AVAR | eff) Unit
  -> Eff (avar :: AVAR | eff) Unit

foreign import detachClickOutside
  :: forall eff
   . HTMLElement
  -> Eff eff Unit
