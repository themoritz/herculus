module Herculus.Config.Common where

import Herculus.Prelude
import CSS as CSS
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Control.Monad.Aff (delay)
import Control.Monad.Eff.Ref (Ref, newRef, readRef, writeRef)
import Control.Monad.Fork (fork)
import Data.Array (null)
import Data.Int (toNumber)
import Data.Time.Duration (Milliseconds(..))
import Herculus.Monad (Herc)
import Herculus.Utils (cldiv, cldiv_, faIcon_)
import Lib.Compiler.Error (Error(..))

withDelay
  :: forall s f g p o
   . Int
  -> H.HalogenM { delayRef :: Maybe (Ref Boolean) | s } f g p o Herc Unit
  -> H.HalogenM { delayRef :: Maybe (Ref Boolean) | s } f g p o Herc Unit
withDelay duration action = do
  { delayRef } <- get
  case delayRef of
    Nothing -> pure unit
    Just r -> liftEff $ writeRef r false
  newRef <- liftEff $ newRef true
  _ <- fork do
    liftAff $ delay (Milliseconds $ toNumber duration)
    case delayRef of
      Nothing -> action
      Just r -> do
        active <- liftEff $ readRef newRef
        if active then action else pure unit
  modify _{ delayRef = Just newRef }

section :: forall p i. String -> HH.HTML p i -> HH.HTML p i -> HH.HTML p i
section icon head body = cldiv_ "flex items-start config__section"
  [ cldiv_ "pt1 pb1 pl1"
    [ faIcon_ $ icon <> " fa-lg fa-fw lightgray"
    ]
  , cldiv_ "flex-auto"
    [ cldiv "p1 border-box"
      [ HC.style do
          CSS.minHeight (CSS.px 24.0)
      ]
      [ head
      ]
    , cldiv_ "px1 pb1"
      [ body
      ]
    ]
  ]

errorList :: forall p i. Array Error -> HH.HTML p i
errorList errors = cldiv_ "" $
  if null errors
  then [ errorItem true "All fine!" ]
  else map (\(Error e) -> errorItem false e.errMsg) errors

  where
  errorItem ok text = cldiv_ "flex"
    [ cldiv_ ""
      [ faIcon_ $ if ok
                  then "check-circle fa-fw green"
                  else "exclamation-circle fa-fw red"
      ]
    , cldiv_ "flex-auto gray pl1"
      [ HH.text text ]
    ]
