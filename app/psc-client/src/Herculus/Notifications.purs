module Herculus.Notifications where

import Herculus.Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Data.Array (deleteAt, snoc)
import Herculus.Monad (Herc)
import Herculus.Notifications.Types (Config, Kind(..))
import Herculus.Utils (cldiv_, clspan_, faIcon_, mkIndexed)

data Query a
  = Push Config a
  | Dismiss Int a

type State =
  { notifications :: Array Config
  }

notifications :: H.Component HH.HTML Query Unit Void Herc
notifications = H.component
  { initialState: const
      { notifications: []
      }
  , receiver: const Nothing
  , render
  , eval
  }

  where

  render :: State -> H.ComponentHTML Query
  render st = HH.div_ (map renderOne $ mkIndexed st.notifications)

    where

    renderOne :: Tuple Int Config -> H.ComponentHTML Query
    renderOne (Tuple i cfg) =
      let
        cls = case cfg.kind of
          Info -> "info"
          Success -> "success"
          Warn -> "warning"
          Error -> "error"
        icon = case cfg.kind of
          Info -> "exclamation-circle"
          Success -> "check-circle-o"
          Warn -> "exclamation-triangle"
          Error -> "times-circle-o"
        title = case cfg.kind of
          Info -> "Information"
          Success -> "Success"
          Warn -> "Warning"
          Error -> "Error"
      in
        cldiv_ ("message" <> cls)
        [ cldiv_ "header"
          [ cldiv_ "title"
            [ clspan_ "symbol"
              [ faIcon_ icon
              ]
            , HH.text title
            ]
          , cldiv_ "button"
            [ HH.button
              [ HP.class_ (H.ClassName "pure")
              , HE.onClick (HE.input_ (Dismiss i))
              ]
              [ faIcon_ "time" ]
            ]
          , cldiv_ "content"
            [ HH.text cfg.message
            ]
          ]
        ]

  eval :: Query ~> H.ComponentDSL State Query Void Herc
  eval (Push config next) = do
    modify \st -> st
      { notifications = snoc st.notifications config
      }
    pure next

  eval (Dismiss i next) = do
    modify \st -> st
      { notifications = case deleteAt i st.notifications of
          Nothing -> st.notifications
          Just new -> new
      }
    pure next
