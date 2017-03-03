module Herculus.Grid.Control where

import Herculus.Prelude
import CSS as CSS
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Control.Monad.Eff.Console (logShow)
import DOM.Event.Event (preventDefault)
import DOM.Event.KeyboardEvent (code, key, shiftKey)
import DOM.Event.Types (KeyboardEvent, MouseEvent, keyboardEventToEvent, mouseEventToEvent)
import DOM.HTML.HTMLElement (focus, getBoundingClientRect)
import Data.Array (length, toUnfoldable, (!!))
import Data.Int (round, toNumber)
import Data.List (List(..))
import Data.String as Str
import Data.Traversable (mapAccumL)
import Herculus.Grid.Geometry (gutterWidth, headHeight, rowHeight)
import Herculus.Monad (Herc)
import Herculus.Utils (cldiv, conditionalClasses, faIcon_, mkIndexed)
import Herculus.Utils.Drag (DragEvent(..), dragEventSource, mouseEventToPageCoord)
import Herculus.Utils.Ordering (Relative(..), RelativeTo(..), getRelativeTarget, reorder)
import Lib.Custom (ColumnTag, Id)
import Lib.Model.Row (Row)

data Query a
  = Initialize a
  | Update Input a
  | ResizeStart Index MouseEvent a
  | Resizing Index DragEvent a
  | ReorderStart Index Int MouseEvent a
  | Reordering Index Int DragEvent a
  | SelectStart Point MouseEvent a
  | SelectOver Point a
  | SelectEnd a
  | EditCell a
  | KeyDown KeyboardEvent a

type Input =
  { cols :: Array (Tuple (Id ColumnTag) Int)
  , rows :: Array (Id Row)
  }

data Output
  = ResizeColumn (Id ColumnTag) Int
  | ReorderColumns (Array (Id ColumnTag))

type Point =
  { x :: Index
  , y :: Index
  }

type Rect =
  { start :: Point
  , end :: Point
  }

singletonRect :: Point -> Rect
singletonRect pt = { start: pt, end: pt }

pointInRect :: Point -> Rect -> Boolean
pointInRect pt r = inX && inY
  where
    inX = r.start.x <= pt.x && pt.x <= r.end.x ||
          r.start.x >= pt.x && pt.x >= r.end.x
    inY = r.start.y <= pt.y && pt.y <= r.end.y ||
          r.start.y >= pt.y && pt.y >= r.end.y

isBorderTop :: Point -> Rect -> Boolean
isBorderTop pt r = pointInRect pt r && onTop
  where
    onTop = r.start.y <= r.end.y && pt.y == r.start.y ||
            r.start.y >= r.end.y && pt.y == r.end.y

isBorderBottom :: Point -> Rect -> Boolean
isBorderBottom pt r = pointInRect pt r && onBottom
  where
    onBottom = r.start.y <= r.end.y && pt.y == r.end.y ||
               r.start.y >= r.end.y && pt.y == r.start.y

isBorderLeft :: Point -> Rect -> Boolean
isBorderLeft pt r = pointInRect pt r && onLeft
  where
    onLeft = r.start.x <= r.end.x && pt.x == r.start.x ||
             r.start.x >= r.end.x && pt.x == r.end.x

isBorderRight :: Point -> Rect -> Boolean
isBorderRight pt r = pointInRect pt r && onRight
  where
    onRight = r.start.x <= r.end.x && pt.x == r.end.x ||
              r.start.x >= r.end.x && pt.x == r.start.x

pointEq :: Point -> Point -> Boolean
pointEq pt1 pt2 = pt1.x == pt2.x && pt1.y == pt2.y

data Action
  = Idle
  | Resize { ix :: Index, size :: Int }
  | Reorder { ix :: Index, left :: Int, target :: RelativeTo Index }
  | Select

type State =
  { input :: Input
  , action :: Action
  , selection :: Rect
  }

containerRef :: H.RefLabel
containerRef = H.RefLabel "control-ref"

selectedCellRef :: H.RefLabel
selectedCellRef = H.RefLabel "selected-cell"

comp :: H.Component HH.HTML Query Input Output Herc
comp = H.lifecycleComponent
  { initialState:
      { input: _
      , action: Idle
      , selection:
        { start: { x: 0, y: 0 }
        , end:   { x: 0, y: 0 }
        }
      }
  , receiver: Just <<< H.action <<< Update
  , render
  , eval
  , initializer: Just (H.action Initialize)
  , finalizer: Nothing
  }

render :: State -> H.ComponentHTML Query
render st = HH.div
  [ HP.classes $
    [ H.ClassName "absolute left-0 top-0 right-0 bottom-0"
    ] <> case st.action of
           Idle      -> [ H.ClassName "no-pointer-events" ]
           Resize _  -> [ H.ClassName "cursor-col-resize"]
           Reorder _ -> [ H.ClassName "cursor-grabbing"]
           Select    -> [ ]
  , HP.ref containerRef
  ]
  (join (map column indexedCols) <> cells)

  where

  indexedCols = mkIndexed st.input.cols
  indexedRows = mkIndexed st.input.rows

  colOffsets = mkOffsets gutterWidth $ map snd st.input.cols
  colOffset i = fromMaybe 0 $ colOffsets !! i
  rowOffsets = mkOffsets headHeight $ map (const rowHeight) st.input.rows
  rowOffset i = fromMaybe 0 $ rowOffsets !! i

  cells :: Array (H.ComponentHTML Query)
  cells = do
    Tuple x (Tuple _ width) <- indexedCols
    Tuple y _ <- indexedRows
    pure $ cell { x, y } (colOffset x) (rowOffset y) width rowHeight

  cell :: Point -> Int -> Int -> Int -> Int -> H.ComponentHTML Query
  cell pt left top width height =
    let
      selecting = case st.action of
        Select -> true
        _      -> false
    in HH.div (
    [ conditionalClasses
      [ { cls: "absolute"
        , on: true
        }
      , { cls: "border-box"
        , on: true
        }
      , { cls: "pointer-events"
        , on: true
        }
      , { cls: "overlay-blue"
        , on: pointInRect pt st.selection &&
              not (pointEq st.selection.start st.selection.end)
        }
      , { cls: "selection--border-top"
        , on: not selecting && isBorderTop pt st.selection
        }
      , { cls: "selection--border-bottom"
        , on: not selecting && isBorderBottom pt st.selection
        }
      , { cls: "selection--border-left"
        , on: not selecting && isBorderLeft pt st.selection
        }
      , { cls: "selection--border-right"
        , on: not selecting && isBorderRight pt st.selection
        }
      , { cls: "selection--start"
        , on: pointEq pt st.selection.start
        }
      ]
    , HC.style do
        let
          extendLeft = pointEq pt st.selection.start ||
                       isBorderLeft pt st.selection
          extendTop  = pointEq pt st.selection.start ||
                       isBorderTop pt st.selection
          left'   = if extendLeft then left - 1 else left
          width'  = if extendLeft then width + 1 else width
          top'    = if extendTop then top - 1 else top
          height' = if extendTop then height + 1 else height
          
        CSS.left   $ CSS.px $ toNumber left'
        CSS.top    $ CSS.px $ toNumber top'
        CSS.width  $ CSS.px $ toNumber width'
        CSS.height $ CSS.px $ toNumber height'
    , HE.onMouseDown $ HE.input $ SelectStart pt
    , HE.onMouseUp $ HE.input_ SelectEnd
    , HE.onDoubleClick $ HE.input_ EditCell
    , HE.onMouseEnter $ HE.input_ $ SelectOver pt
    ] <> if pointEq pt st.selection.start
         then [ HP.ref selectedCellRef
              , HP.tabIndex (-1)
              , HE.onKeyDown $ HE.input KeyDown
              ]
         else [ ])
    [ ]

  column :: Tuple Index (Tuple (Id ColumnTag) Int)
         -> Array (H.ComponentHTML Query)
  column (Tuple ix (Tuple _ width)) =
    let left = colOffset ix in
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
              CSS.height $ CSS.px $ toNumber headHeight
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

eval :: Query ~> H.ComponentDSL State Query Output Herc
eval = case _ of

  Initialize next -> do
    focusCell
    pure next

  Update input next -> do
    modify _{ input = input }
    pure next

  ResizeStart ix ev next -> do
    { input } <- get
    for_ (input.cols !! ix) \(Tuple _ size) -> do
      liftEff $ preventDefault $ mouseEventToEvent ev
      H.subscribe $ dragEventSource ev \drag ->
        Just $ Resizing ix drag H.Listening
      modify _ { action = Resize { ix, size } }
    pure next

  Resizing ix drag next -> do
    { input, action } <- get
    for_ (input.cols !! ix) \(Tuple colId oldSize) ->
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
    { input } <- get
    liftEff $ preventDefault $ mouseEventToEvent ev
    H.subscribe $ dragEventSource ev \drag ->
      Just $ Reordering ix left drag H.Listening
    target <- getReorderTarget (map snd input.cols) ev
    modify _ { action = Reorder { ix, left, target } }
    pure next

  Reordering ix left drag next -> do
    { action, input } <- get
    case drag of
      Move ev d -> do
        let newLeft = left + round d.offsetX
        target <- getReorderTarget (map snd input.cols) ev
        modify _ { action = Reorder
                   { ix
                   , left: newLeft
                   , target
                   }
                 }
      Done _ -> case action of
        Reorder val -> do
          modify _ { action = Idle }
          H.raise $ ReorderColumns $ map fst (reorder ix val.target input.cols)
        _ -> pure unit
    pure next

  SelectStart point ev next -> do
    liftEff $ preventDefault $ mouseEventToEvent ev
    modify _
      { selection = singletonRect point
      , action = Select
      }
    focusCell
    pure next

  SelectOver point next -> do
    { action} <- get
    case action of
      Select -> modify _ { selection { end = point } }
      _ -> pure unit
    pure next

  SelectEnd next -> do
    modify _ { action = Idle }
    pure next

  EditCell next -> do
    editCell Nothing
    pure next

  KeyDown ev next -> do
    st <- get
    liftEff $ preventDefault $ keyboardEventToEvent ev
    let
      modifySelection f = modify \st -> st { selection = f st.selection }
      moveLeft = modifySelection \r -> singletonRect $ r.start
        { x = max 0 (r.start.x - 1) }
      moveRight = modifySelection \r -> singletonRect $ r.start
        { x = min (length st.input.cols - 1) (r.start.x + 1) }
      moveUp = modifySelection \r -> singletonRect $ r.start
        { y = max 0 (r.start.y - 1) }
      moveDown = modifySelection \r -> singletonRect $ r.start
        { y = min (length st.input.rows - 1) (r.start.y + 1) }
    case code ev of
      "ArrowLeft" -> moveLeft
      "ArrowRight" -> moveRight
      "ArrowUp" -> moveUp
      "ArrowDown" -> moveDown
      "Tab" | shiftKey ev -> moveLeft
      "Tab" -> moveRight
      "Enter" -> editCell Nothing
      _ ->
        let char = key ev in
        unless (Str.length char > 1) $ editCell (Just char)
    focusCell
    pure next

editCell :: Maybe String -> H.ComponentDSL State Query Output Herc Unit
editCell mChar = do
  modify \st -> st { selection =
                     { start: st.selection.start
                     , end: st.selection.start
                     }
                   }
  liftEff $ logShow mChar

focusCell :: H.ComponentDSL State Query Output Herc Unit
focusCell = H.getHTMLElementRef selectedCellRef >>= case _ of
  Nothing -> pure unit
  Just el -> liftEff $ focus el

getReorderTarget
  :: Array Int -> MouseEvent
  -> H.ComponentDSL State Query Output Herc (RelativeTo Index)
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
  pure $ go 0 gutterWidth (toUnfoldable cols)

mkOffsets :: forall a. Semiring a => a -> Array a -> Array a
mkOffsets init arr = (mapAccumL go init arr).value
  where go offset w =
          { accum: offset + w
          , value: offset
          }
