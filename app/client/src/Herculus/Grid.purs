module Herculus.Grid where

import Herculus.Prelude
import CSS as CSS
import Data.Map as Map
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Elements.Keyed as HK
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Herculus.Column as Col
import Herculus.DataCell as DataCell
import Herculus.Grid.Control as Control
import Herculus.PopupMenu as Popup
import Herculus.ReportCell as ReportCell
import Herculus.Row as Row
import DOM.Event.ClipboardEvent (ClipboardEvent, clipboardData)
import DOM.Event.MouseEvent (MouseEvent)
import DOM.HTML.Event.DataTransfer (getData, setData)
import Data.Array (drop, length, singleton, zipWith, (!!))
import Data.Foldable (maximum)
import Data.Int (toNumber)
import Data.Map (Map)
import Data.MediaType.Common (textPlain)
import Halogen.Component.ChildPath (type (<\/>), type (\/), cp1, cp2, cp3, cp4, cp5, cp6)
import Herculus.DataCell (defaultValue)
import Herculus.Grid.CSV (parseCSV, showCSV)
import Herculus.Grid.Geometry (Direction, Point, addRowHeight, gutterWidth, headHeight, rowHeight)
import Herculus.Monad (Herc)
import Herculus.Project.Data (Coords(..), RowCache)
import Herculus.Utils (Options, cldiv_, faButton_, mkIndexed)
import Herculus.Utils.Ordering (orderMap)
import Lib.Api.Schema.Column (Column, ColumnKind(ColumnReport, ColumnData), columnId, columnKind, dataColIsDerived, dataColType)
import Lib.Api.Schema.Project (Command(..))
import Lib.Custom (ColumnTag, Id, ProjectTag)
import Lib.Model.Cell (CellContent(..), Value)
import Lib.Model.Column (IsDerived(..))
import Lib.Model.Row (Row)
import Lib.Model.Table (Table)

data Query a
  = Update Input a
  | SendCommand Command a
  | AddRow a
  | AddCol ColType a
  | TogglePopup a
  | ResizeColumn' (Id ColumnTag) Int a
  | ReorderColumns' (Array (Id ColumnTag)) a
  | EditCell Coords (Maybe String) a
  | Cut ClipboardEvent Control.CellSubset a
  | Copy ClipboardEvent Control.CellSubset a
  | Paste ClipboardEvent Point a
  | SaveValue (Id ColumnTag) (Id Row) Value (Maybe Direction) a
  | YieldFocus a
  | MouseDown MouseEvent a
  | MouseMove MouseEvent a
  | MouseUp MouseEvent a
  | DoubleClick MouseEvent a
  | OpenColumnConfig' (Id ColumnTag) a

type Input =
  { cells :: Map Coords CellContent
  , cols :: Map (Id ColumnTag) Column
  , rows :: Array (Tuple (Id Row) Row)
  , tables :: Options (Id Table)
  , rowCache :: RowCache
  , tableId :: Id Table
  , projectId :: Id ProjectTag
  , colSizes :: Map (Id ColumnTag) Int
  , colOrder :: Array (Id ColumnTag)
  }

data Output
  = Commands (Array Command)
  | ResizeColumn (Id ColumnTag) Int
  | ReorderColumns (Array (Id ColumnTag))
  | OpenColumnConfig (Id ColumnTag)

type State =
  { input :: Input
  }

data ColType
  = ReportCol
  | DataCol

type Child =
  Row.Query <\/>
  Col.Query <\/>
  DataCell.Query <\/>
  ReportCell.Query <\/>
  Popup.Query ColType <\/>
  Control.Query <\/>
  Const Void

type Slot =
  Id Row \/
  Id ColumnTag \/
  Coords \/
  Coords \/
  Unit \/
  Unit \/
  Unit

comp :: H.Component HH.HTML Query Input Output Herc
comp = H.parentComponent
  { initialState:
      { input: _
      }
  , receiver: Just <<< H.action <<< Update
  , render
  , eval
  }

popupEntries :: Array (Popup.Entry ColType)
popupEntries =
  [ { icon: "columns"
    , label: "New data column"
    , value: DataCol
    }
  , { icon: "file-text"
    , label: "New report column"
    , value: ReportCol
    }
  ]

render :: State -> H.ParentHTML Query Child Slot Herc
render st = HK.div
  [ HE.onMouseDown $ HE.input MouseDown
  , HE.onMouseMove $ HE.input MouseMove
  , HE.onMouseUp $ HE.input MouseUp
  , HE.onDoubleClick $ HE.input DoubleClick
  , HP.class_ (H.ClassName "absolute left-0 top-0 right-0 bottom-0 overflow-scroll")
  ] $
  [ Tuple "origin" $
    posDiv 0 0 gutterWidth headHeight
    [ cldiv_ "grid-cell" [] ]
  -- Add row
  , Tuple "add-row" $
    posDiv 0 (length st.input.rows * rowHeight + headHeight)
           gutterWidth addRowHeight
    [ cldiv_ "center p1"
      [ faButton_ "plus-circle" AddRow
      ]
    ]
  -- Add col
  , Tuple "add-col" $
    posDiv (colLeft (length orderedCols)) 0
           addColWidth headHeight
    [ cldiv_ "center p1"
      [ faButton_ "plus-circle" TogglePopup
      , HH.slot' cp5 unit Popup.comp
                 { entries: popupEntries
                 , position: Popup.BelowRight
                 }
                 (Just <<< H.action <<< AddCol)
      ]
    ]
  ] <> rows <> cols <> cells <>
  [ Tuple "control" $
    HH.slot' cp6 unit Control.comp
             { cols: colSizes
             , rows: map fst st.input.rows
             }
             case _ of
               Control.ResizeColumn colId width ->
                 Just $ H.action $ ResizeColumn' colId width
               Control.ReorderColumns orders ->
                 Just $ H.action $ ReorderColumns' orders
               Control.EditCell coords mChar ->
                 Just $ H.action $ EditCell coords mChar
               Control.Cut ev subset ->
                 Just $ H.action $ Cut ev subset
               Control.Copy ev subset ->
                 Just $ H.action $ Copy ev subset
               Control.Paste ev start ->
                 Just $ H.action $ Paste ev start
  ]

  where

  orderedCols = orderMap st.input.colOrder st.input.cols
  colSizes = orderedCols <#> \(Tuple colId _) ->
    Tuple colId (fromMaybe 230 $ Map.lookup colId st.input.colSizes)

  irows = mkIndexed st.input.rows
  icols = mkIndexed orderedCols

  rows = irows <#> \(Tuple y (Tuple rowId row)) ->
    Tuple (show rowId) $
    posDiv 0 (y * rowHeight + headHeight)
           gutterWidth rowHeight
    [ cldiv_ "grid-cell p1"
      [ HH.slot' cp1 rowId Row.comp unit \Row.Delete ->
          Just $ H.action $ SendCommand $ CmdRowDelete rowId
      ]
    ]

  cols = icols <#> \(Tuple x (Tuple _ col)) ->
    let colId = col ^. columnId in
    Tuple (show colId) $
    posDiv (colLeft x) 0
           (colWidth x) headHeight
    [ cldiv_ "grid-cell p1"
      [ let
          input =
            { column: col
            , tables: st.input.tables
            }
          handler o = Just $ H.action case o of
            Col.SetName name -> SendCommand $ CmdColumnSetName colId name
            Col.OpenConfig -> OpenColumnConfig' colId
        in
          HH.slot' cp2 colId Col.comp input handler
      ]
    ]

  cells = do
    Tuple x (Tuple _ col) <- icols
    Tuple y (Tuple rowId row) <- irows
    let
      colId = col ^. columnId
      coords = Coords (col ^. columnId) rowId
    pure $
      Tuple (show colId <> show rowId) $
      posDiv (colLeft x) (y * rowHeight + headHeight)
             (colWidth x) rowHeight
      [ cldiv_ "grid-cell p1"
        [ case col ^. columnKind of
            ColumnData dataCol ->
              case Map.lookup coords st.input.cells of
                Nothing      -> HH.text "cell not found!"
                Just content ->
                  let
                    input =
                      { content
                      , dataCol
                      , rowCache: st.input.rowCache
                      }
                    handler = case _ of
                      DataCell.SaveValue val mDir ->
                        Just $ H.action $ SaveValue colId rowId val mDir
                      DataCell.YieldFocus ->
                        Just $ H.action $ YieldFocus
                  in
                    HH.slot' cp3 coords DataCell.comp input handler
            ColumnReport reportCol ->
              HH.slot' cp4 coords ReportCell.comp { reportCol, coords } absurd
        ]
      ]

  colWidth ix = case colSizes !! ix of
    Nothing -> 230
    Just (Tuple _ size) -> size
  colLeft ix = if ix == 0
               then addColWidth
               else colLeft (ix - 1) + colWidth (ix - 1)
  addColWidth = 37

  posDiv :: forall p i. Int -> Int -> Int -> Int -> Array (HH.HTML p i) -> HH.HTML p i
  posDiv left top width height content = HH.div
    [ HP.class_ (H.ClassName "absolute")
    , HC.style do
        CSS.left   $ CSS.px $ toNumber left
        CSS.top    $ CSS.px $ toNumber top
        CSS.width  $ CSS.px $ toNumber width
        CSS.height $ CSS.px $ toNumber height
    ]
    content

eval :: Query ~> H.ParentDSL State Query Child Slot Output Herc
eval = case _ of

  Update input next -> do
    modify _{ input = input }
    pure next

  SendCommand cmd next -> do
    H.raise $ Commands [cmd]
    pure next

  AddRow next -> do
    st <- H.get
    H.raise $ Commands [CmdRowCreate st.input.tableId]
    pure next

  AddCol colType next -> do
    st <- H.get
    H.raise $ Commands $ singleton $ case colType of
      DataCol -> CmdDataColCreate st.input.tableId
      ReportCol -> CmdReportColCreate st.input.tableId
    pure next

  TogglePopup next -> do
    H.query' cp5 unit (H.action Popup.Toggle)
    pure next

  ResizeColumn' colId width next -> do
    H.raise $ ResizeColumn colId width
    pure next

  ReorderColumns' order next -> do
    H.raise $ ReorderColumns order
    pure next

  EditCell coords mChar next -> do
    H.query' cp3 coords $ H.action $ DataCell.StartEdit mChar
    pure next

  Cut ev subset@(Tuple subsetCols subsetRows) next -> do
    writeClipboard subset ev
    cols <- gets _.input.cols

    H.raise $ Commands do
      c <- subsetCols
      case Map.lookup c cols of
        Nothing -> []
        Just col -> case col ^. columnKind of
          ColumnReport _ -> []
          ColumnData dataCol -> case dataCol ^. dataColIsDerived of
            Derived -> []
            NotDerived -> do
              r <- subsetRows
              let val = defaultValue (dataCol ^. dataColType)
              [CmdCellSet c r val]
    pure next

  Copy ev subset next -> do
    writeClipboard subset ev
    pure next

  Paste ev start next -> do
    { input } <- get
    dat <- liftEff $ getData textPlain (clipboardData ev)
    let
      orderedCols = orderMap input.colOrder input.cols
      availableCols = drop start.x orderedCols
      availableRows = drop start.y input.rows
      types = availableCols <#> \(Tuple _ col) -> case col ^. columnKind of
        ColumnReport _ -> Nothing
        ColumnData dataCol -> case dataCol ^. dataColIsDerived of
          Derived -> Nothing
          NotDerived -> Just (dataCol ^. dataColType)
      csv = parseCSV '\t' types dat
      cmds = join $ join $ zipWith goRow availableRows csv
        where
        goRow (Tuple r _) csvRow = zipWith goCell availableCols csvRow
          where
          goCell (Tuple c _) mVal = case mVal of
            Just val -> [CmdCellSet c r val]
            Nothing -> []
      pastedRows = max 0 $ min (length availableRows) (length csv) - 1
      pastedCols = max 0 $ min (length availableCols) maxWidth - 1
        where maxWidth = fromMaybe 0 $ maximum $ map length csv
    H.query' cp6 unit $ H.action $ Control.SetSelection
      { start
      , end:
        { x: start.x + pastedCols
        , y: start.y + pastedRows
        }
      }
    H.raise $ Commands cmds
    pure next

  SaveValue colId rowId val mDir next -> do
    H.query' cp6 unit $ H.action $ Control.Focus mDir
    H.raise $ Commands [CmdCellSet colId rowId val]
    pure next

  YieldFocus next -> do
    H.query' cp6 unit $ H.action $ Control.Focus Nothing
    pure next

  MouseDown ev next -> do
    H.query' cp6 unit $ H.action $ Control.MouseDown ev
    pure next

  MouseMove ev next -> do
    H.query' cp6 unit $ H.action $ Control.MouseMove ev
    pure next

  MouseUp ev next -> do
    H.query' cp6 unit $ H.action $ Control.MouseUp ev
    pure next

  DoubleClick ev next -> do
    H.query' cp6 unit $ H.action $ Control.DoubleClick ev
    pure next

  OpenColumnConfig' c next -> do
    H.raise $ OpenColumnConfig c
    pure next

writeClipboard
  :: Control.CellSubset
  -> ClipboardEvent
  -> H.ParentDSL State Query Child Slot Output Herc Unit
writeClipboard (Tuple subsetCols subsetRows) ev = do
  cells <- gets _.input.cells
  let
    csv = showCSV '\t' $ subsetRows <#> \r ->
      subsetCols <#> \c -> case Map.lookup (Coords c r) cells of
        Nothing -> Nothing
        Just content -> case content of
          CellError _ -> Nothing
          CellValue val -> Just val
  liftEff $ setData textPlain csv (clipboardData ev)
