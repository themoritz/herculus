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
import DOM.HTML.HTMLElement (getBoundingClientRect)
import Data.Array ((!!), toUnfoldable)
import Data.Int (round, toNumber)
import Data.List (List(..))
import Data.Traversable (Accum, mapAccumL)
import Herculus.Monad (Herc)
import Herculus.Utils (cldiv, faIcon_, mkIndexed)
import Herculus.Utils.Drag (DragEvent(..), dragEventSource, mouseEventToPageCoord)
import Herculus.Utils.Ordering (Relative(..), RelativeTo(..), getRelativeTarget, reorder)
import Lib.Custom (ColumnTag, Id)

data Query a
  = Update Input a
  | ResizeStart Index MouseEvent a
  | Resizing Index DragEvent a
  | ReorderStart Index Int MouseEvent a
  | Reordering Index Int DragEvent a

type Input =
  { cols :: Array (Tuple (Id ColumnTag) Int)
  }

data Output
  = ResizeColumn (Id ColumnTag) Int
  | ReorderColumns (Array (Id ColumnTag))

data Action
  = Idle
  | Resize { ix :: Index, size :: Int }
  | Reorder { ix :: Index, left :: Int, target :: RelativeTo Index }

type State =
  { cols :: Array (Tuple (Id ColumnTag) Int)
  , action :: Action
  }

containerRef :: H.RefLabel
containerRef = H.RefLabel "control-ref"

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
  , HP.ref containerRef
  ]
  (join (mapAccumL column 37 (mkIndexed st.cols)).value)

  where

  column :: Int -> Tuple Index (Tuple (Id ColumnTag) Int)
         -> Accum Int (Array (H.ComponentHTML Query))
  column left (Tuple ix (Tuple _ width)) =
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
            cldiv "absolute overlay-gray z2"
            [ HC.style do
                CSS.width  $ CSS.px $ toNumber width
                CSS.left   $ CSS.px $ toNumber val.left
                CSS.top    $ CSS.px $ toNumber 0
                CSS.bottom $ CSS.px $ toNumber 0
            ]
            [ ]
          _ -> HH.text ""
      , case st.action of
          Reorder val | getRelativeTarget val.target == ix ->
            cldiv "absolute bg-gray"
            [ HC.style do
                let x = case val.target of
                          RelativeTo rel _ -> case rel of
                            Before -> left
                            After -> left + width
                CSS.width  $ CSS.px $ toNumber 3
                CSS.left   $ CSS.px $ toNumber (x - 1)
                CSS.top    $ CSS.px $ toNumber 0
                CSS.bottom $ CSS.px $ toNumber 0
            ]
            [ ]
          _ -> HH.text ""
      , cldiv ("absolute pointer-events " <> case st.action of
                                               Reorder _ -> "cursor-grabbing"
                                               _         -> "cursor-grab")
        [ HC.style do
            CSS.width  $ CSS.px $ toNumber 16
            CSS.left   $ CSS.px $ toNumber (left + 6)
            CSS.top    $ CSS.px $ toNumber 7
            CSS.height $ CSS.px $ toNumber 14
        , HE.onMouseDown $ HE.input \ev -> ReorderStart ix left ev
        ]
        [ faIcon_ "reorder fa-rotate-90 lightgray"
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
    for_ (cols !! ix) \(Tuple _ size) -> do
      liftEff $ preventDefault $ mouseEventToEvent ev
      H.subscribe $ dragEventSource ev \drag ->
        Just $ Resizing ix drag H.Listening
      modify _ { action = Resize { ix, size } }
    pure next

  Resizing ix drag next -> do
    { cols, action } <- get
    for_ (cols !! ix) \(Tuple colId oldSize) ->
      case drag of
        Move _ d -> do
          let newSize = max 50 (oldSize + round d.offsetX)
          modify _ { action = Resize { ix, size: newSize } }
        Done _ -> case action of
          Resize val -> do
            modify _ { action = Idle }
            H.raise $ ResizeColumn colId val.size
          _ -> pure unit
    pure next
    
  ReorderStart ix left ev next -> do
    { cols } <- get
    liftEff $ preventDefault $ mouseEventToEvent ev
    H.subscribe $ dragEventSource ev \drag ->
      Just $ Reordering ix left drag H.Listening
    target <- getReorderTarget (map snd cols) ev
    modify _ { action = Reorder { ix, left, target } }
    pure next

  Reordering ix left drag next -> do
    { action, cols } <- get
    case drag of
      Move ev d -> do
        let newLeft = left + round d.offsetX
        target <- getReorderTarget (map snd cols) ev
        modify _ { action = Reorder
                   { ix
                   , left: newLeft
                   , target
                   }
                 }
      Done _ -> case action of
        Reorder val -> do
          modify _ { action = Idle }
          H.raise $ ReorderColumns $ map fst (reorder ix val.target cols)
        _ -> pure unit
    pure next

getReorderTarget :: Array Int -> MouseEvent -> H.ComponentDSL State Query Output Herc (RelativeTo Index)
getReorderTarget cols ev = do
  -- Note: This doesn't work when the grid does not start at x=0 relative to
  -- the viewport.
  scroll <- H.getHTMLElementRef containerRef >>= case _ of
    Nothing -> pure 0.0
    Just el -> _.left <$> (liftEff $ getBoundingClientRect el)
  let
    x = round $ (mouseEventToPageCoord ev).pageX - scroll
    go :: Index -> Int -> List Int -> RelativeTo Index
    go ix left = case _ of
      Nil -> RelativeTo After (ix - 1)
      Cons width tail ->
        if x < left + width / 2
        then RelativeTo Before ix
        else go (ix + 1) (left + width) tail
  pure $ go 0 37 (toUnfoldable cols)
    
