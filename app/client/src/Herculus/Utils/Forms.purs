module Herculus.Utils.Forms where

import Herculus.Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Events as HE
import DOM.Event.KeyboardEvent (KeyboardEvent, code)


keyEnter :: KeyboardEvent -> Boolean
keyEnter e = code e == "Enter"

renderSubmit :: forall f. H.Action f -> H.ComponentHTML f
renderSubmit onClick = HH.tr_
  [ HH.td
    [ HP.class_ (H.ClassName "right-align py1")
    , HP.colSpan 2
    ]
    [ HH.button
      [ HE.onClick (HE.input_ onClick)
      , HP.class_ (HH.ClassName "button")
      ]
      [ HH.text "Submit" ]
    ]
  ]

renderRow
  :: forall f
   . String
  -> String
  -> Boolean
  -> String
  -> (String -> Maybe (f Unit))
  -> f Unit
  -> H.ComponentHTML f
renderRow label name password value onInput onEnter = HH.tr_
  [ HH.td
    [ HP.class_ (HH.ClassName "py1 auth-form__cell") ]
    [ HH.label
      [ HP.for name ]
      [ HH.text label ]
    ]
  , HH.td
    [ HP.class_ (HH.ClassName "py1 auth-form__cell") ]
    [ HH.input
      [ HP.class_ (H.ClassName "input")
      , HP.type_ (if password then HP.InputPassword else HP.InputText)
      , HP.value value
      , HP.name name
      , HE.onValueInput onInput
      , HE.onKeyDown \e -> if keyEnter e
          then Just onEnter
          else Nothing
      ]
    ]
  ]
