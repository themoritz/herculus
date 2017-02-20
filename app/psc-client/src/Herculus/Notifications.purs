module Herculus.Notifications where

import Herculus.Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Data.Array (deleteAt, snoc)
import Herculus.Monad (Herc)
import Herculus.Notifications.Types (Config, Kind(..))
import Herculus.Utils (cldiv, cldiv_, faIcon_, mkIndexed)

data Query a
  = Push Config a
  | Dismiss Int a

type State =
  { notifications :: Array Config
  }

comp :: H.Component HH.HTML Query Unit Void Herc
comp = H.component
  { initialState: const
      { notifications: []
      }
  , receiver: const Nothing
  , render
  , eval
  }

  where

  render :: State -> H.ComponentHTML Query
  render st = cldiv_
    "notifications"
    (map renderOne $ mkIndexed st.notifications)

    where

    renderOne :: Tuple Int Config -> H.ComponentHTML Query
    renderOne (Tuple i cfg) =
      let
        cls = case cfg.kind of
          Info -> "bg-blueg"
          Success -> "bg-green"
          Warn -> "bg-orange"
          Error -> "bg-red"
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
        detail msg =
          [ cldiv_ "font-smaller mt2"
            [ HH.text msg
            ]
          ]
      in
        cldiv_ ("p2 mb2 notification " <> cls)
        ([ cldiv_ "clearfix"
          [ cldiv_ "left bold font-larger"
            [ faIcon_ $ icon <> " mr2"
            , HH.text title
            ]
          , cldiv "right align-top right-align notification__close"
            [ HE.onClick (HE.input_ (Dismiss i)) ]
            [ faIcon_ "times" ]
          ]
        , cldiv_ "mt2"
          [ HH.text cfg.message
          ]
        ] <> maybe [] detail cfg.detail)

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
