module Views.Combinators where

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text

import GHCJS.Types (JSString)

import React.Flux

import Helper (keyENTER)

faButton_ :: JSString -> handler -> ReactElementM handler ()
faButton_ !icon !h = button_
  [ "className" $= "pure"
  , onClick $ \_ _ -> h
  ] $ faIcon_ $ icon <> " fa-fw fa-lg"

clspan_ :: JSString -> ReactElementM handler a -> ReactElementM handler a
clspan_ !cl = span_ ["className" &= cl]

inputNew_ :: Text -> (Text -> [SomeStoreAction]) -> ReactElementM eh ()
inputNew_ !p !cb = view (inputNew p) cb mempty

inputNew :: Text -> ReactView (Text -> [SomeStoreAction])
inputNew p = defineStatefulView "inputNew" ("" :: Text) $ \curText cb ->
  input_
    [ "placeholder" &= p
    , "value" &= curText
    , onChange $ \evt _ -> ([], Just $ target evt "value")
    , onKeyDown $ \_ evt curState ->
        if keyENTER evt && not (Text.null curState)
           then (cb curState, Just "")
           else ([], Nothing)
    ]
