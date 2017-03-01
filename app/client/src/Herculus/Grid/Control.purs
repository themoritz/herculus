module Herculus.Grid.Control where

import Herculus.Prelude
import CSS as CSS
import Data.Map as Map
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import CSS (width)
import DOM.Event.Event (preventDefault)
import DOM.Event.Types (MouseEvent, mouseEventToEvent)
import Data.Array ((!!))
import Data.Int (round, toNumber)
import Data.Traversable (Accum, mapAccumL)
import Herculus.Monad (Herc)
import Herculus.Utils (cldiv, cldiv_, mkIndexed)
import Herculus.Utils.Drag (DragEvent(..), dragEventSource)

data Query a
  = Update Input a
  | ResizeStart Index MouseEvent a
  | Resizing Index DragEvent a

type Input =
  { cols :: Array Int
  }

data Output
  = ResizeColumn Index Int
  | ReorderColumn Index Index

type State =
  { cols :: Array Int
  , resizing :: Maybe { ix :: Index, size :: Int }
  }

comp :: H.Component HH.HTML Query Input Output Herc
comp = H.component
  { initialState: \input ->
      { cols: input.cols
      , resizing: Nothing
      }
  , receiver: Just <<< H.action <<< Update
  , render
  , eval
  }

render :: State -> H.ComponentHTML Query
render st = HH.div
  [ HP.classes $
    [ H.ClassName "absolute left-0 top-0 right-0 bottom-0"
    ] <> case st.resizing of
           Just _ ->  [ H.ClassName "cursor-col-resize"]
           Nothing -> [ H.ClassName "no-pointer-events" ]
  ]
  (mapAccumL column 37 (mkIndexed st.cols)).value

  where

  column :: Int -> Tuple Index Int -> Accum Int (H.ComponentHTML Query)
  column left (Tuple ix width) =
    { accum: left + width
    , value: case st.resizing of
        Just val | val.ix == ix ->
          cldiv "absolute bg-grid-blue"
          [ HC.style do
              CSS.width  $ CSS.px $ toNumber 1
              CSS.left   $ CSS.px $ toNumber (left + val.size - 1)
              CSS.top    $ CSS.px $ toNumber 0
              CSS.bottom $ CSS.px $ toNumber 0
          ]
          [
          ]
        _ ->
          cldiv "absolute grid-resizer cursor-col-resize"
          [ HC.style do
              CSS.width  $ CSS.px $ toNumber 3
              CSS.left   $ CSS.px $ toNumber (left + width - 3)
              CSS.top    $ CSS.px $ toNumber 0
              CSS.height $ CSS.px $ toNumber 54
          , HE.onMouseDown $ HE.input \ev -> ResizeStart ix ev
          ]
          [ ]
    }

eval :: Query ~> H.ComponentDSL State Query Output Herc
eval = case _ of

  Update input next -> do
    modify _{ cols = input.cols }
    pure next

  ResizeStart ix ev next -> do
    { cols } <- get
    for_ (cols !! ix) \size -> do
      liftEff $ preventDefault $ mouseEventToEvent ev
      H.subscribe $ dragEventSource ev \drag -> Just $ Resizing ix drag H.Listening
      modify _ { resizing = Just { ix, size } }
    pure next

  Resizing ix drag next -> do
    { cols, resizing } <- get
    for_ (cols !! ix) \oldSize ->
      case drag of
        Move _ d -> do
          let newSize = max 50 (oldSize + round d.offsetX)
          modify _ { resizing = Just { ix, size: newSize }}
        Done _ -> for_ resizing \val -> do
          modify _ { resizing = Nothing }
          H.raise $ ResizeColumn ix val.size
    pure next
    
