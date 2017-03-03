module Herculus.Grid where

import Herculus.Prelude
import CSS as CSS
import Data.Map as Map
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Properties as HP
import Herculus.Column as Col
import Herculus.DataCell as DataCell
import Herculus.Grid.Control as Control
import Herculus.PopupMenu as Popup
import Herculus.ReportCell as ReportCell
import Herculus.Row as Row
import Data.Array (length, (!!))
import Data.Int (toNumber)
import Data.Map (Map)
import Halogen.Component.ChildPath (type (<\/>), type (\/), cp1, cp2, cp3, cp4, cp5, cp6)
import Herculus.Grid.Geometry (addRowHeight, gutterWidth, headHeight, rowHeight)
import Herculus.Monad (Herc)
import Herculus.Project.Data (Coords(..), RowCache)
import Herculus.Utils (Options, cldiv_, faButton_, mkIndexed)
import Herculus.Utils.Ordering (orderMap)
import Lib.Api.Schema.Column (Column, ColumnKind(ColumnReport, ColumnData), columnId, columnKind)
import Lib.Api.Schema.Project (Command(CmdReportColCreate, CmdDataColCreate, CmdRowCreate, CmdCellSet, CmdDataColUpdate, CmdReportColUpdate, CmdColumnDelete, CmdColumnSetName, CmdRowDelete))
import Lib.Custom (ColumnTag, Id, ProjectTag)
import Lib.Model.Cell (CellContent)
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
  = Command Command
  | ResizeColumn (Id ColumnTag) Int
  | ReorderColumns (Array (Id ColumnTag))

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
render st = cldiv_ "absolute left-0 top-0 right-0 bottom-0 overflow-scroll" $
  [ posDiv 0 0 gutterWidth headHeight
    [ cldiv_ "grid-cell" [] ]
  -- Add row
  , posDiv 0 (length st.input.rows * rowHeight + headHeight)
           gutterWidth addRowHeight
    [ cldiv_ "center p1"
      [ faButton_ "plus-circle" AddRow
      ]
    ]
  -- Add col
  , posDiv (colLeft (length orderedCols)) 0
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
  [ HH.slot' cp6 unit Control.comp
             { cols: colSizes
             , rows: map fst st.input.rows
             }
             case _ of
               Control.ResizeColumn colId width ->
                 Just $ H.action $ ResizeColumn' colId width
               Control.ReorderColumns orders ->
                 Just $ H.action $ ReorderColumns' orders
  ]

  where

  orderedCols = orderMap st.input.colOrder st.input.cols
  colSizes = orderedCols <#> \(Tuple colId _) ->
    Tuple colId (fromMaybe 230 $ Map.lookup colId st.input.colSizes)

  irows = mkIndexed st.input.rows
  icols = mkIndexed orderedCols

  rows = irows <#> \(Tuple y (Tuple rowId row)) ->
    posDiv 0 (y * rowHeight + headHeight)
           gutterWidth rowHeight
    [ cldiv_ "grid-cell p1"
      [ HH.slot' cp1 rowId Row.comp unit \Row.Delete ->
          Just $ H.action $ SendCommand $ CmdRowDelete rowId
      ]
    ]

  cols = icols <#> \(Tuple x (Tuple _ col)) ->
    posDiv (colLeft x) 0
           (colWidth x) headHeight
    [ cldiv_ "grid-cell p1"
      [ let
          colId = col ^. columnId
          input =
            { column: col
            , tables: st.input.tables
            }
          handler o = Just $ H.action $ SendCommand case o of
            Col.SetName name -> CmdColumnSetName colId name
            Col.Delete -> CmdColumnDelete colId
            Col.SaveReportCol t f l -> CmdReportColUpdate colId t f l
            Col.SaveDataCol dt d f -> CmdDataColUpdate colId dt d f
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
                    handler (DataCell.SetValue val) =
                      Just $ H.action $ SendCommand $ CmdCellSet colId rowId val
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
    H.raise $ Command cmd
    pure next

  AddRow next -> do
    st <- H.get
    H.raise $ Command $ CmdRowCreate st.input.tableId
    pure next

  AddCol colType next -> do
    st <- H.get
    H.raise $ Command $ case colType of
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
