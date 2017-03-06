module Herculus.Grid.Geometry where

import Herculus.Prelude

data Direction
  = DirLeft
  | DirRight
  | DirUp
  | DirDown

type Point =
  { x :: Int
  , y :: Int
  }

type Rect =
  { start :: Point
  , end :: Point
  }

singletonRect :: Point -> Rect
singletonRect pt = { start: pt, end: pt }

upperLeft :: Rect -> Point
upperLeft r =
  { x: min r.start.x r.end.x
  , y: min r.start.y r.end.y
  }

lowerRight :: Rect -> Point
lowerRight r =
  { x: max r.start.x r.end.x
  , y: max r.start.y r.end.y
  }

pointInRect :: Point -> Rect -> Boolean
pointInRect pt r = inX && inY
  where
    inX = r.start.x <= pt.x && pt.x <= r.end.x ||
          r.start.x >= pt.x && pt.x >= r.end.x
    inY = r.start.y <= pt.y && pt.y <= r.end.y ||
          r.start.y >= pt.y && pt.y >= r.end.y

pointEq :: Point -> Point -> Boolean
pointEq pt1 pt2 = pt1.x == pt2.x && pt1.y == pt2.y

--------------------------------------------------------------------------------

headHeight :: Int
headHeight = 54

rowHeight :: Int
rowHeight = 37

addRowHeight :: Int
addRowHeight = 37

gutterWidth :: Int
gutterWidth = 37
