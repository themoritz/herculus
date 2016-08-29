module Views.Combinators where

import Data.Monoid

import GHCJS.Types (JSString)

import React.Flux

faButton_ :: JSString -> handler -> ReactElementM handler ()
faButton_ icon h = button_
  [ "className" $= "pure"
  , onClick $ \_ _ -> h
  ] $ faIcon_ $ icon <> " fa-fw fa-lg"
