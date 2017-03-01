module Herculus.Grid.Control where

import Herculus.Prelude
import CSS as CSS
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import DOM.Event.Event (preventDefault)
import DOM.Event.Types (MouseEvent, mouseEventToEvent)
import Data.Array ((!!), toUnfoldable)
import Data.Int (round, toNumber)
import Data.List (List(..))
import Data.Traversable (Accum, mapAccumL)
import Herculus.Monad (Herc)
import Herculus.Utils (cldiv, faIcon_, mkIndexed)
import Herculus.Utils.Drag (DragEvent(..), dragEventSource, mouseEventToPageCoord)

data Query a
  = Update Input a
  | ResizeStart Index MouseEvent a
  | Resizing Index DragEvent a
  | ReorderStart Index Int MouseEvent a
  | Reordering Index Int DragEvent a

type Input =
  { cols :: Array Int
  }

data Output
  = ResizeColumn Index Int
  | ReorderColumn Index Index

data Action
  = Idle
  | Resize { ix :: Index, size :: Int }
  | Reorder { ix :: Index, left :: Int, target :: Index }

type State =
  { cols :: Array Int
  , action :: Action
  }

comp :: H.Component HH.HTML Query Input Output Herc
comp = H.component
  { initialState: \input ->
      { cols: input.cols
      , action: Idle
      }
  , receiver: Just <<< H.action <<< Update
  , render
  , eval
  }

render :: State -> H.ComponentHTML Query
render st = HH.div
  [ HP.classes $
    [ H.ClassName "absolute left-0 top-0 right-0 bottom-0"
    ] <> case st.action of
           Idle      -> [ H.ClassName "no-pointer-events" ]
           Resize _  -> [ H.ClassName "cursor-col-resize"]
           Reorder _ -> [ H.ClassName "cursor-grabbing"]
  ]
  (join (mapAccumL column 37 (mkIndexed st.cols)).value)

  where

  column :: Int -> Tuple Index Int -> Accum Int (Array (H.ComponentHTML Query))
  column left (Tuple ix width) =
    { accum: left + width
    , value:
      [ case st.action of
          Resize val | val.ix == ix ->
            cldiv "absolute bg-grid-blue"
            [ HC.style do
                CSS.width  $ CSS.px $ toNumber 1
                CSS.left   $ CSS.px $ toNumber (left + val.size - 1)
                CSS.top    $ CSS.px $ toNumber 0
                CSS.bottom $ CSS.px $ toNumber 0
            ]
            [ ]
          _ ->
            cldiv "absolute grid-resizer pointer-events cursor-col-resize"
            [ HC.style do
                CSS.width  $ CSS.px $ toNumber 3
                CSS.left   $ CSS.px $ toNumber (left + width - 3)
                CSS.top    $ CSS.px $ toNumber 0
                CSS.height $ CSS.px $ toNumber 54
            , HE.onMouseDown $ HE.input \ev -> ResizeStart ix ev
            ]
            [ ]
      , case st.action of
          Reorder val | val.ix == ix ->
            cldiv "absolute overlay-gray"
            [ HC.style do
                CSS.width  $ CSS.px $ toNumber width
                CSS.left   $ CSS.px $ toNumber val.left
                CSS.top    $ CSS.px $ toNumber 0
                CSS.bottom $ CSS.px $ toNumber 0
            ]
            [ ]
          _ -> HH.text ""
      , case st.action of
          Reorder val | val.target == ix ->
            cldiv "absolute bg-gray"
            [ HC.style do
                CSS.width  $ CSS.px $ toNumber 3
                CSS.left   $ CSS.px $ toNumber (left - 1)
                CSS.top    $ CSS.px $ toNumber 0
                CSS.bottom $ CSS.px $ toNumber 0
            ]
            [ ]
          _ -> HH.text ""
      , cldiv ("absolute pointer-events " <> case st.action of
                                               Reorder _ -> "cursor-grabbing"
                                               _         -> "cursor-grab")
        [ HC.style do
            CSS.width  $ CSS.px $ toNumber 11
            CSS.left   $ CSS.px $ toNumber (left + width - 16)
            CSS.top    $ CSS.px $ toNumber 24
            CSS.height $ CSS.px $ toNumber 13
        , HE.onMouseDown $ HE.input \ev -> ReorderStart ix left ev
        ]
        [ faIcon_ "bars font-smaller gray"
        ]
      ]
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
      H.subscribe $ dragEventSource ev \drag ->
        Just $ Resizing ix drag H.Listening
      modify _ { action = Resize { ix, size } }
    pure next

  Resizing ix drag next -> do
    { cols, action } <- get
    for_ (cols !! ix) \oldSize ->
      case drag of
        Move _ d -> do
          let newSize = max 50 (oldSize + round d.offsetX)
          modify _ { action = Resize { ix, size: newSize } }
        Done _ -> case action of
          Resize val -> do
            modify _ { action = Idle }
            H.raise $ ResizeColumn ix val.size
          _ -> pure unit
    pure next
    
  ReorderStart ix left ev next -> do
    { cols } <- get
    liftEff $ preventDefault $ mouseEventToEvent ev
    H.subscribe $ dragEventSource ev \drag ->
      Just $ Reordering ix left drag H.Listening
    modify _ { action = Reorder
               { ix
               , left
               , target: getReorderTarget cols ev
               }
             }
    pure next

  Reordering ix left drag next -> do
    { action, cols } <- get
    case drag of
      Move ev d -> do
        let newLeft = left + round d.offsetX
        modify _ { action = Reorder
                   { ix
                   , left: newLeft
                   , target: getReorderTarget cols ev
                   }
                 }
      Done _ -> case action of
        Reorder val -> do
          modify _ { action = Idle }
        _ -> pure unit
    pure next

getReorderTarget :: Array Int -> MouseEvent -> Index
getReorderTarget cols ev =
  let
    x = round (mouseEventToPageCoord ev).pageX
    go :: Index -> Int -> List Int -> Index
    go ix left = case _ of
      Nil -> ix
      Cons width tail ->
        if x < left + width / 2
        then ix
        else go (ix + 1) (left + width) tail
  in
    go 0 37 (toUnfoldable cols)
    
