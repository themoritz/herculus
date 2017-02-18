module Herculus.DataCell where

import Herculus.Prelude
import Data.Map as Map
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Herculus.DatePicker as Date
import Herculus.EditBox as EditBox
import Data.Array (deleteAt, length, snoc, take, updateAt)
import Data.Foldable (intercalate)
import Data.Map (Map)
import Halogen.Component.ChildPath (type (<\/>), type (\/), cp1, cp2, cp3)
import Halogen.HTML.Properties (id_, placeholder)
import Herculus.Monad (Herc)
import Herculus.Project.Data (RowCache)
import Herculus.Utils (cldiv_, clspan, clspan_, dropdown, faButton_, faIcon_, mkIndexed)
import Lib.Api.Schema.Column (ColumnKind(..), DataCol(..), columnKind, columnName, dataColIsDerived, dataColType)
import Lib.Custom (Id(..), ValNumber(..), ValTime(..), parseValNumber)
import Lib.Model.Cell (CellContent(..), Value(..))
import Lib.Model.Column (DataType(..), IsDerived(..))
import Lib.Model.Row (Row(..))
import Lib.Model.Table (Table(..))

data Query a
  = Update Input a
  | SetValue' Value a
  | ToggleExpanded a

type Input =
  { content :: CellContent
  , dataCol :: DataCol
  , rowCache :: RowCache
  }

data Output = SetValue Value

type State =
  { input :: Input
  , expanded :: Boolean
  }

type Child =
  EditBox.Query String <\/>
  EditBox.Query ValNumber <\/>
  Date.Query <\/>
  Const Void

type Slot =
  Unit \/
  Unit \/
  Unit \/
  Void

data RenderMode
  = Compact
  | Full

needsExpand :: DataType -> IsDerived -> Boolean
needsExpand dt derived = case dt, derived of
  DataBool,      _          -> false
  DataString,    _          -> false
  DataNumber,    _          -> false
  DataTime,      _          -> false
  DataRowRef _,  NotDerived -> false
  DataRowRef _,  Derived    -> true
  DataList _,    _          -> true
  DataMaybe sub, _          -> needsExpand sub derived

-- TODO: delete in favor of ajax calls to the server version of defaultContent
defaultValue :: DataType -> Value
defaultValue = case _ of
  DataBool     -> VBool false
  DataString   -> VString ""
  DataNumber   -> VNumber (ValNumber "0")
  DataTime     -> VTime (ValTime "2017-01-01T00:00:00Z")
  DataRowRef _ -> VRowRef Nothing
  DataList   _ -> VList []
  DataMaybe  _ -> VMaybe Nothing

comp :: H.Component HH.HTML Query Input Output Herc
comp = H.parentComponent
  { initialState:
    { input: _
    , expanded: false
    }
  , receiver: Just <<< H.action <<< Update
  , render
  , eval
  }

render :: State -> H.ParentHTML Query Child Slot Herc
render st = case st.input.content of
  CellError msg -> clspan "error"
    [ HP.title msg ]
    [ HH.text "Error" ]
  CellValue val ->
    let
      dt = st.input.dataCol ^. dataColType
      needsEx = needsExpand dt derived
      inline = value (if needsEx then Compact else Full) dt val SetValue'
    in
      case needsEx of
        true -> cldiv_ "compactWrapper"
          [ cldiv_ "compact" [ inline ]
          , cldiv_ "expand"
            [ faButton_ ("expand " <> if st.expanded then "open" else "")
                        ToggleExpanded
            , case st.expanded of
                true -> cldiv_ "expanded"
                  [ cldiv_ "body"
                    [ value Full dt val SetValue'
                    ]
                  ]
                false -> HH.div_ []
            ]
          ]
        false -> inline

  where

  derived = st.input.dataCol ^. dataColIsDerived

  value
    :: RenderMode -> DataType -> Value -> (Value -> H.Action Query)
    -> H.ParentHTML Query Child Slot Herc
  value mode dt val cb = case derived of
    Derived -> showValue mode dt val
    NotDerived -> editValue mode dt val cb

  editValue
    :: RenderMode -> DataType -> Value -> (Value -> H.Action Query)
    -> H.ParentHTML Query Child Slot Herc
  editValue mode dt val cb = case val of
    VBool b -> editBool b (cb <<< VBool)
    VString s -> editString s (cb <<< VString)
    VNumber n -> editNumber n (cb <<< VNumber)
    VTime t -> editTime t (cb <<< VTime)
    VRowRef mr -> case dt of
      DataRowRef t -> editRowRef mode t mr (cb <<< VRowRef)
      _            -> HH.div_ []
    VList vs -> case dt of
      DataList sub -> editList mode sub vs (cb <<< VList)
      _            -> HH.div_ []
    VMaybe v -> case dt of
      DataMaybe sub -> editMaybe mode sub v (cb <<< VMaybe)
      _             -> HH.div_ []

  showValue
    :: RenderMode -> DataType -> Value
    -> H.ParentHTML Query Child Slot Herc
  showValue mode dt val = case val of
    VBool b -> showBool b
    VString s -> showString s
    VNumber n -> showNumber n
    VTime t -> showTime t
    VRowRef mr -> case dt of
      DataRowRef t -> showRowRef mode t mr
      _            -> HH.div_ []
    VList vs -> case dt of
      DataList sub -> showList mode sub vs
      _            -> HH.div_ []
    VMaybe v -> case dt of
      DataMaybe sub -> showMaybe mode sub v
      _             -> HH.div_ []

  editBool
    :: Boolean -> (Boolean -> H.Action Query)
    -> H.ParentHTML Query Child Slot Herc
  editBool val cb = cldiv_ "bool plain"
    [ HH.input
      [ HP.type_ HP.InputCheckbox
      , HP.checked val
      , HE.onChecked (Just <<< H.action <<< cb)
      ]
    ]

  showBool val = HH.text (if val then "True" else "False")

  editString
    :: String -> (String -> H.Action Query)
    -> H.ParentHTML Query Child Slot Herc
  editString val cb =
    HH.slot' cp1 unit EditBox.comp
             { value: val
             , placeholder: ""
             , className: "string"
             , show: id
             , validate: Just
             }
             (Just <<< H.action <<< cb)

  showString val = HH.text val

  editNumber
    :: ValNumber -> (ValNumber -> H.Action Query)
    -> H.ParentHTML Query Child Slot Herc
  editNumber val cb =
    HH.slot' cp2 unit EditBox.comp
             { value: val
             , placeholder: ""
             , className: "number"
             , show: \(ValNumber str) -> str
             , validate: parseValNumber
             }
             (Just <<< H.action <<< cb)

  showNumber (ValNumber str) = HH.text str

  editTime
    :: ValTime -> (ValTime -> H.Action Query)
    -> H.ParentHTML Query Child Slot Herc
  editTime val@(ValTime str) cb =
    HH.slot' cp3 unit Date.comp { date: val }
             \(Date.DateChanged val') ->
               Just (H.action (cb val'))

  showTime (ValTime str) = HH.text str

  editRowRef
    :: RenderMode -> Id Table -> Maybe (Id Row) -> (Maybe (Id Row) -> H.Action Query)
    -> H.ParentHTML Query Child Slot Herc
  editRowRef mode t val cb = 
    let
      cls = if isNothing val then "invalid" else ""
      options = Map.toUnfoldable (rows t) <#> \(Tuple r row) ->
        { value: Just r
        , label: showPairs row
        }
      defaultOption = if isNothing val
        then [ { value: Nothing, label: "" } ]
        else []
    in
      dropdown cls (defaultOption <> options) val cb

  showRowRef mode t val = case mode of
    Compact -> HH.text case val of
      Nothing -> "<no row chosen>"
      Just r -> case Map.lookup r (rows t) of
        Nothing -> ""
        Just row -> showPairs row
    Full -> case val of
      Nothing -> HH.text "Impossible: invalid row in derived cell"
      Just r -> case Map.lookup r (rows t) of
        Nothing -> HH.div_ []
        Just row -> HH.div_ $ Map.toUnfoldable row <#> \(Tuple _ tuple) -> case tuple of
          Tuple c content -> cldiv_ "field"
            [ cldiv_ "key"
              [ HH.text (c ^. columnName) ]
            , cldiv_ "content"
              [ case content, c ^. columnKind of
                  CellValue v, ColumnData dat ->
                    showValue mode (dat ^. dataColType) v
                  CellError _, _ ->
                    clspan_ "error" [ HH.text "Error" ]
                  _, _ ->
                    HH.div_ []
              ]
            ]

  showPairs = intercalate ", " <<<
              map showCol <<<
              Map.values
    where
    showCol (Tuple col cont) =
      col ^. columnName <> ": " <> showContent cont
    showContent = case _ of
      CellError e -> ""
      CellValue v -> showVal v
    showVal = case _ of
      VBool b -> if b then "True" else "False"
      VNumber (ValNumber str) -> str
      VString str -> str
      VTime (ValTime str) -> str
      VRowRef _ -> "Row .."
      VList _ -> "[..]"
      VMaybe _ -> "Maybe .."

  rows t = fromMaybe Map.empty $ Map.lookup t st.input.rowCache

  editList
    :: RenderMode -> DataType -> Array Value -> (Array Value -> H.Action Query)
    -> H.ParentHTML Query Child Slot Herc
  editList mode subDt vals cb = cldiv_ "list"
    case mode of
      Compact -> compactList mode subDt vals
      Full ->
        elements <>
        [ cldiv_ "new"
          [ HH.button
            [ HE.onClick $ HE.input_ $ cb (snoc vals (defaultValue subDt)) ]
            [ faIcon_ "plus-circle" ]
          ]
        ]
    where
    elements = mkIndexed vals <#> \(Tuple i v) ->
      cldiv_ "element-wrapper"
      [ cldiv_ "element input"
        [ cldiv_ "delete"
          [ HH.button
            [ HE.onClick $ HE.input_ $ cb (listDel i vals) ]
            [ faIcon_ "minus-circle" ]
          ]
        , cldiv_ "content"
          [ editValue mode subDt v (\nv -> cb (listMod i nv vals))
          ]
        ]
      ]
    listMod i x xs = fromMaybe xs $ updateAt i x xs
    listDel i xs   = fromMaybe xs $ deleteAt i xs

  showList mode subDt vals = cldiv_ "list"
    case mode of
      Compact ->
        compactList mode subDt vals
      Full -> vals <#> \v ->
        cldiv_ "element-wrapper" 
        [ cldiv_ "element"
          [ cldiv_ "content"
            [ showValue mode subDt v
            ]
          ]
        ]

  compactList mode subDt vals =
    [ clspan_ "info"
      [ HH.text $ show $ length vals ]
    ] <> (
      take 4 vals <#> \v ->
        cldiv_ "element"
        [ showValue mode subDt v
        ]
    )

  editMaybe
    :: RenderMode -> DataType -> Maybe Value -> (Maybe Value -> H.Action Query)
    -> H.ParentHTML Query Child Slot Herc
  editMaybe mode subDt mVal cb = case mVal of
    Nothing ->
      cldiv_ "maybe nothing"
      [ HH.button
        [ HP.class_ (H.ClassName "button--pure")
        , HE.onClick $ HE.input_ $ cb (Just (defaultValue subDt)) ]
        [ faIcon_ "plus-circle" ]
      , cldiv_ "content"
        [ HH.text "Nothing" ]
      ]
    Just val ->
      cldiv_ "maybe just"
      [ HH.button
        [ HP.class_ (H.ClassName "button--pure")
        , HE.onClick $ HE.input_ $ cb Nothing ]
        [ faIcon_ "minus-circle" ]
      , cldiv_ "content"
        [ editValue mode subDt val (cb <<< Just)
        ]
      ]

  showMaybe mode subDt mVal = case mVal of
    Nothing  -> HH.text "Nothing"
    Just val -> showValue mode subDt val

eval :: Query ~> H.ParentDSL State Query Child Slot Output Herc
eval = case _ of

  Update input next -> do
    modify _{ input = input }
    pure next

  SetValue' val next -> do
    H.raise $ SetValue val
    pure next

  ToggleExpanded next -> do
    modify \st -> st { expanded = not st.expanded }
    pure next
