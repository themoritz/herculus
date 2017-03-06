module Herculus.Grid.Control where

import Herculus.Prelude
import CSS as CSS
import Data.String as Str
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import DOM (DOM)
import DOM.Event.Event (currentTarget, preventDefault)
import DOM.Event.KeyboardEvent (code, key, shiftKey)
import DOM.Event.MouseEvent (clientX, clientY)
import DOM.Event.Types (KeyboardEvent, MouseEvent, keyboardEventToEvent, mouseEventToEvent)
import DOM.HTML.HTMLElement (getBoundingClientRect)
import Data.Array (catMaybes, length, toUnfoldable, (!!))
import Data.Int (round, toNumber)
import Data.List (List(..))
import Data.Traversable (mapAccumL)
import Herculus.Grid.Geometry (Direction(..), Point, Rect, gutterWidth, headHeight, lowerRight, pointEq, rowHeight, singletonRect, upperLeft)
import Herculus.Monad (Herc)
import Herculus.Project.Data (Coords(..))
import Herculus.Utils (cldiv, conditionalClasses, faIcon_, focusElement, mkIndexed)
import Herculus.Utils.Drag (DragEvent(..), dragEventSource, mouseEventToPageCoord)
import Herculus.Utils.Ordering (Relative(..), RelativeTo(..), getRelativeTarget, reorder)
import Lib.Custom (ColumnTag, Id)
import Lib.Model.Row (Row)
import Unsafe.Coerce (unsafeCoerce)

data Query a
  = Initialize a
  | Update Input a
  -- External
  | Focus (Maybe Direction) a
  | MouseDown MouseEvent a
  | MouseMove MouseEvent a
  | MouseUp MouseEvent a
  -- Internal
  | ResizeStart Index MouseEvent a
  | Resizing Index DragEvent a
  | ReorderStart Index Int MouseEvent a
  | Reordering Index Int DragEvent a
  | EditCell' a
  | KeyDown KeyboardEvent a

type Input =
  { cols :: Array (Tuple (Id ColumnTag) Int)
  , rows :: Array (Id Row)
  }

data Output
  = ResizeColumn (Id ColumnTag) Int
  | ReorderColumns (Array (Id ColumnTag))
  | EditCell Coords (Maybe String)

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
           Resize _  -> [ H.ClassName "cursor-col-resize" ]
           Reorder _ -> [ H.ClassName "cursor-grabbing" ]
           Select    -> [ H.ClassName "no-pointer-events" ]
  , HP.ref containerRef
  ]
  (join (map column indexedCols) <> catMaybes selection)

  where

  indexedCols = mkIndexed st.input.cols
  indexedRows = mkIndexed st.input.rows

  colWidths = map snd st.input.cols
  colWidthAt i = fromMaybe 0 $ colWidths !! i
  colOffsets = mkOffsets gutterWidth colWidths
  colOffsetAt i = fromMaybe 0 $ colOffsets !! i

  rowHeights = map (const rowHeight) st.input.rows
  rowHeightAt i = fromMaybe 0 $ rowHeights !! i
  rowOffsets = mkOffsets headHeight rowHeights
  rowOffsetAt i = fromMaybe 0 $ rowOffsets !! i

  selecting = case st.action of
    Select -> true
    _      -> false

  selection =
    [ if pointEq st.selection.start st.selection.end
      then Nothing
      else Just $ HH.div
        [ conditionalClasses
          [ { cls: "absolute"
            , on: true
            }
          , { cls: "overlay-blue"
            , on: true
            }
          , { cls: "selection--border"
            , on: not selecting
            }
          ]
        , HC.style do
            let
              ul = upperLeft st.selection
              lr = lowerRight st.selection
              left = colOffsetAt ul.x
              top = rowOffsetAt ul.y
              width = colOffsetAt lr.x - left + colWidthAt lr.x
              height = rowOffsetAt lr.y - top + rowHeightAt lr.y
            CSS.left   $ CSS.px $ toNumber (left - 1)
            CSS.top    $ CSS.px $ toNumber (top - 1)
            CSS.width  $ CSS.px $ toNumber (width + 1)
            CSS.height $ CSS.px $ toNumber (height + 1)
        ]
        [ ]
    , Just $ cldiv "absolute selection--start"
        [ HC.style do
            let
              left = colOffsetAt st.selection.start.x
              top = rowOffsetAt st.selection.start.y
              width = colWidthAt st.selection.start.x
              height = rowHeightAt st.selection.start.y
            CSS.left   $ CSS.px $ toNumber (left - 1)
            CSS.top    $ CSS.px $ toNumber (top - 1)
            CSS.width  $ CSS.px $ toNumber (width + 1)
            CSS.height $ CSS.px $ toNumber (height + 1)
        , HP.ref selectedCellRef
        , HP.tabIndex (-1)
        , HE.onKeyDown $ HE.input KeyDown
        ]
        [ ]
    ]

  column :: Tuple Index (Tuple (Id ColumnTag) Int)
         -> Array (H.ComponentHTML Query)
  column (Tuple ix (Tuple _ width)) =
    let left = colOffsetAt ix in
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
    focusElement selectedCellRef
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

  Focus mDir next -> do
    focusElement selectedCellRef
    for_ mDir move
    pure next

  MouseDown ev next -> do
    pt <- liftEff $ getMouseCoords ev
    liftEff $ preventDefault $ mouseEventToEvent ev
    mouseEventIndices pt >>= case _ of
      Nothing -> pure unit
      Just point -> do
        modify _
          { selection = singletonRect point
          , action = Select
          }
    pure next

  MouseMove ev next -> do
    pt <- liftEff $ getMouseCoords ev
    { action } <- get
    point <- mouseEventIndices' pt
    case action of
      Select -> modify _ { selection { end = point } }
      _ -> pure unit
    pure next

  MouseUp _ next -> do
    { action } <- get
    case action of
      Select -> modify _ { action = Idle }
      _ -> pure unit
    focusElement selectedCellRef
    pure next

  EditCell' next -> do
    editCell Nothing
    pure next

  KeyDown ev next -> do
    st <- get
    liftEff $ preventDefault $ keyboardEventToEvent ev
    case code ev of
      "ArrowLeft"         -> move DirLeft
      "ArrowRight"        -> move DirRight
      "ArrowUp"           -> move DirUp
      "ArrowDown"         -> move DirDown
      "Tab" | shiftKey ev -> move DirLeft
      "Tab"               -> move DirRight
      "Enter"             -> editCell Nothing
      _ ->
        let char = key ev in
        unless (Str.length char > 1) $ editCell (Just char)
    focusElement selectedCellRef
    pure next

move :: forall f o. Direction -> H.ComponentDSL State f o Herc Unit
move dir = do
  st <- get
  let
    modifySelection f = modify \st' -> st' { selection = f st'.selection }
  case dir of
    DirLeft -> modifySelection \r -> singletonRect $ r.start
      { x = max 0 (r.start.x - 1) }
    DirRight -> modifySelection \r -> singletonRect $ r.start
      { x = min (length st.input.cols - 1) (r.start.x + 1) }
    DirUp -> modifySelection \r -> singletonRect $ r.start
      { y = max 0 (r.start.y - 1) }
    DirDown -> modifySelection \r -> singletonRect $ r.start
      { y = min (length st.input.rows - 1) (r.start.y + 1) }

editCell :: Maybe String -> H.ComponentDSL State Query Output Herc Unit
editCell mChar = do
  start <- gets _.selection.start
  modify _ { selection = { start , end: start } }
  st <- get
  let mCoords = do
        colId <- fst <$> st.input.cols !! start.x
        rowId <- st.input.rows !! start.y
        pure $ Coords colId rowId
  for_ mCoords \coords -> H.raise $ EditCell coords mChar

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

mouseEventIndices
  :: Point
  -> H.ComponentDSL State Query Output Herc (Maybe Point)
mouseEventIndices pt = do
  cols <- map snd <$> gets _.input.cols
  rows <- map (const rowHeight) <$> gets _.input.rows
  pure $
    case getIndex gutterWidth cols pt.x
       , getIndex headHeight rows pt.y of
      Found x, Found y -> Just { x, y }
      _, _             -> Nothing

mouseEventIndices'
  :: Point
  -> H.ComponentDSL State Query Output Herc Point
mouseEventIndices' pt = do
  cols <- map snd <$> gets _.input.cols
  rows <- map (const rowHeight) <$> gets _.input.rows
  pure { x: getIndex' gutterWidth cols pt.x
       , y: getIndex' headHeight rows pt.y
       }

data IndexResult
  = TooSmall
  | TooLarge
  | Found Index

getIndex :: Int -> Array Int -> Int -> IndexResult
getIndex init arr x = go init 0 (toUnfoldable arr)
  where
    go offset ix = case _ of
      Nil -> TooLarge
      Cons w tail ->
        if x < offset
        then TooSmall
        else if x <= offset + w
             then Found ix
             else go (offset + w) (ix + 1) tail

getIndex' :: Int -> Array Int -> Int -> Index
getIndex' init arr x = case getIndex init arr x of
  TooSmall -> 0
  TooLarge -> length arr - 1
  Found ix -> ix

mkOffsets :: forall a. Semiring a => a -> Array a -> Array a
mkOffsets init arr = res.value
  where
    res = mapAccumL go init arr
    go offset w =
          { accum: offset + w
          , value: offset
          }

getMouseCoords :: forall eff. MouseEvent -> Eff (dom :: DOM | eff) Point
getMouseCoords ev = do
  rect <- liftEff $
          getBoundingClientRect $
          unsafeCoerce $
          currentTarget $
          mouseEventToEvent ev
  pure
    { x: clientX ev - round rect.left
    , y: clientY ev - round rect.top
    }

