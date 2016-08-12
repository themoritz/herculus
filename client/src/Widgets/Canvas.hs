module Widgets.Canvas
  where

import Reflex.Dom
import qualified Data.Map as Map
import Data.Text (pack)
import Data.Monoid

data Rectangle = Rectangle
  { rectLeft :: Int
  , rectTop :: Int
  , rectWidth :: Int
  , rectHeight :: Int
  }

rectangleDyn :: MonadWidget t m => Dynamic t Rectangle -> m a -> m a
rectangleDyn dynRect child = do
  let attrs = ffor dynRect $ \(Rectangle left top width height) ->
        Map.fromList
          [ ("style", "left: " <> (pack . show) left <>
                       "px; top: " <> (pack . show) top <>
                       "px; width: " <> (pack . show) width <>
                       "px; height: " <> (pack . show) height <>
                       "px")
          , ("class", "element")
          ]
  elDynAttr "div" attrs child

rectangle :: MonadWidget t m => Rectangle -> m a -> m a
rectangle rect child = rectangleDyn (constDyn rect) child
