module Herculus.Utils.Templates where

import Herculus.Prelude
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Herculus.Utils (cldiv_)

app
  :: forall p i
   . Array (HH.HTML p i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
  -> HH.HTML p i
app navigation headerElements content = cldiv_ "container"
  [ appHeader navigation headerElements
  , cldiv_ "content-body"
    [ content
    ]
  ]

plainApp :: forall p i. HH.HTML p i -> HH.HTML p i
plainApp = app [] []

appHeader
  :: forall p i
   . Array (HH.HTML p i)
  -> Array (HH.HTML p i)
  -> HH.HTML p i
appHeader navigation elements = cldiv_ "menubar"
  ([ cldiv_ "title"
    [ cldiv_ "title__logo"
      [ HH.img
        [ HP.src "img/herculus.svg"
        ]
      ]
    , cldiv_ "title__text" [ HH.text "Herculus" ]
    , cldiv_ "title__beta" [ HH.text "beta" ]
    ]
  , cldiv_ "navigation" navigation
  ] <> elements)
