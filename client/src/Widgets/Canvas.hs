module Widgets.Canvas
  where

import Reflex.Dom
import qualified Data.Map as Map

data Rectangle = Rectangle
  { rectLeft :: Int
  , rectTop :: Int
  , rectWidth :: Int
  , rectHeight :: Int
  }

rectangleDyn :: MonadWidget t m => Dynamic t Rectangle -> m a -> m a
rectangleDyn dynRect child = do
  attrs <- forDyn dynRect $ \(Rectangle left top width height) ->
    Map.fromList
      [ ("style", "left: " ++ show left ++
                   "px; top: " ++ show top ++
                   "px; width: " ++ show width ++
                   "px; height: " ++ show height ++
                   "px")
      , ("class", "element")
      ]
  elDynAttr "div" attrs child

rectangle :: MonadWidget t m => Rectangle -> m a -> m a
rectangle rect child = rectangleDyn (constDyn rect) child
