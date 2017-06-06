module Herculus.DataCell where

import Herculus.Prelude
import Data.Map as Map
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Herculus.DatePicker as Date
import Herculus.EditBox as EditBox
import DOM.Event.Event (stopPropagation)
import DOM.Event.MouseEvent (MouseEvent, mouseEventToEvent)
import Data.Array (deleteAt, length, snoc, take, zip)
import Data.Exists (Exists, mkExists, runExists)
import Data.Foldable (intercalate, maximum, minimumBy)
import Data.Generic (gCompare, gEq)
import Data.Lens (Setter', _2, _Just, element, traversed)
import Data.List (head)
import Data.Map (Map)
import Halogen.Component.ChildPath (type (<\/>), type (\/), cp1, cp2, cp3, cp4)
import Herculus.EditBox (SaveKey(..))
import Herculus.Grid.Geometry (Direction(..))
import Herculus.Monad (Herc)
import Herculus.Project.Data (RowCache)
import Herculus.Utils (bLens, cldiv, cldiv_, clspan, clspan_, dropdown, faButton_, faIcon_, mkIndexed)
import Lib.Api.Schema.Column (ColumnKind(ColumnData), DataCol, columnKind, columnName, dataColIsDerived, dataColType)
import Lib.Api.Schema.Compiler (Type(TypeRecord, TypeTable, TypeApp, TypeConstructor, TypeVar), TyconInfo, tyconParams, tyconValueConstrs)
import Lib.Custom (Id, ValNumber(ValNumber), ValTime(ValTime), parseInteger, parseValNumber)
import Lib.Model.Cell (CellContent(..), Value(..), _VBool, _VData, _VInteger, _VList, _VMaybe, _VNumber, _VRecord, _VRowRef, _VString, _VTime)
import Lib.Model.Column (DataType(..), IsDerived(..))
import Lib.Model.Row (Row)
import Lib.Model.Table (Table)
import Partial.Unsafe (unsafePartial)

type Path a = Setter' Value a

data ValSetterF a = ValSetterF (Path a) a
type ValSetter = Exists ValSetterF

setValueAction :: forall a. Path a -> Maybe Direction -> a -> H.Action Query
setValueAction path mDir val =
  SetValue (mkExists $ ValSetterF path val) mDir

setValue :: forall a. Path a -> Maybe Direction -> a -> Maybe (Query Unit)
setValue path mDir val = Just $ H.action $ setValueAction path mDir val

data Query a
  = Update Input a
  -- External
  | StartEdit (Maybe String) a
  | CancelEdit a
  -- Internal
  | SetValue ValSetter (Maybe Direction) a
  | ToggleExpanded a
  | StopPropagation MouseEvent a

type Input =
  { content :: CellContent
  , dataCol :: DataCol
  , types :: Map String TyconInfo
  , rowCache :: RowCache
  }

data Output
  = SaveValue Value (Maybe Direction)
  | YieldFocus

--------------------------------------------------------------------------------

type State =
  { input :: Input
  , expanded :: Boolean
  }

type Child =
  EditBox.Query String <\/>
  EditBox.Query ValNumber <\/>
  Date.Query <\/>
  EditBox.Query Int <\/>
  Const Void

data SlotPath
  = SlotRoot
  | SlotSub SlotPath
  | SlotItem Int SlotPath

ifRootElse :: forall a. SlotPath -> a -> a -> a
ifRootElse (SlotSub SlotRoot) t _ = t
ifRootElse _                  _ e = e

derive instance genericSlotPath :: Generic SlotPath
instance eqSlotPath :: Eq SlotPath where eq = gEq
instance ordSlotPath :: Ord SlotPath where compare = gCompare

type Slot =
  SlotPath \/
  SlotPath \/
  SlotPath \/
  SlotPath \/
  Void

--------------------------------------------------------------------------------

data RenderMode
  = Compact
  | Full

instConstructors
  :: Array DataType -> TyconInfo
  -> Array (Tuple String (Array DataType))
instConstructors dataTypes tci =
  map goConstr (tci ^. tyconValueConstrs)
  where
  goConstr (Tuple c args) = Tuple c (map (unsafePartial goType) args)
  goType :: Partial => Type -> DataType
  goType = case _ of
    TypeVar v -> resolveParam v
    TypeConstructor c -> DataAlgebraic c []
    TypeApp f arg -> case goType f of
      DataAlgebraic c f' -> DataAlgebraic c (snoc f' $ goType arg)
    TypeTable t -> DataTable t
    TypeRecord r -> DataRecord $ map (\(Tuple f t) -> Tuple f (goType t)) r
  resolveParam :: Partial => String -> DataType
  resolveParam p = fromJust $ Map.lookup p paramMap
  paramMap = Map.fromFoldable $ zip (tci ^. tyconParams) dataTypes

hasSimpleVcons :: TyconInfo -> Boolean
hasSimpleVcons info =
  let cs = info ^. tyconValueConstrs in
  case maximum (map (length <<< snd) cs) of
    Just i | i > 0 -> false
    _              -> true

needsExpand
  :: (String -> TyconInfo) -> DataType -> IsDerived -> Boolean
needsExpand resolveTycon dt derived = case dt, derived of
  DataAlgebraic "Boolean" [],  _ -> false
  DataAlgebraic "String" [],   _ -> false
  DataAlgebraic "Number" [],   _ -> false
  DataAlgebraic "Integer" [],  _ -> false
  DataAlgebraic "DateTime" [], _ -> false
  DataAlgebraic "List" [_],    _ -> true
  DataAlgebraic "Record" [_],  _ -> true
  DataAlgebraic "Maybe" [sub], _ -> needsExpand resolveTycon sub derived
  DataAlgebraic c _, _ -> not $ hasSimpleVcons (resolveTycon c)
  DataRecord _, _ -> true
  DataTable _,  NotDerived -> false
  DataTable _,  Derived    -> true

defaultValue
  :: (Id Table -> Maybe (Id Row))
  -> (String -> TyconInfo)
  -> DataType -> Value
defaultValue getRow resolveTycon = defaultValue' 10
  where
  -- Max recursion depth to avoid constructing infinite values
  defaultValue' :: Int -> DataType -> Value
  defaultValue' i dt' = if i == 0 then VUndefined else case dt' of
    DataAlgebraic "Boolean" [] ->
      VBool false
    DataAlgebraic "String" [] ->
      VString ""
    DataAlgebraic "Number" [] ->
      VNumber (ValNumber "0")
    DataAlgebraic "Integer" [] ->
      VInteger 0
    DataAlgebraic "DateTime" [] ->
      VTime (ValTime "2017-01-01T00:00:00Z")
    DataAlgebraic "Row" [sub] ->
      defaultValue' (i-1) sub
    DataAlgebraic "Record" [sub] ->
      defaultValue' (i-1) sub
    DataAlgebraic "List" [_] ->
      VList []
    DataAlgebraic "Maybe" [_] ->
      VMaybe Nothing
    DataAlgebraic tycon args ->
      let
        cs = instConstructors args (resolveTycon tycon)
        compare' (Tuple _ as) (Tuple _ as') = compare (length as) (length as')
      in
        case minimumBy compare' cs of
          Nothing -> VUndefined
          Just (Tuple vcon vargs) ->
            VData vcon (map (defaultValue' (i-1)) vargs)
    DataRecord dts ->
      VRecord $ dts <#> \(Tuple f dt) -> Tuple f (defaultValue' (i-1) dt)
    DataTable t -> VRowRef $ getRow t

shortVal :: Value -> String
shortVal = case _ of
  VUndefined -> "<undefined>"
  VBool b -> if b then "True" else "False"
  VNumber (ValNumber str) -> str
  VInteger i -> show i
  VString str -> str
  VTime (ValTime str) -> str
  VRowRef _ -> "Row .."
  VData c _ -> c <> " .."
  VRecord _ -> "Record .."
  VList _ -> "[..]"
  VMaybe _ -> "Maybe .."

--------------------------------------------------------------------------------

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
  CellError msg -> clspan "font-smaller italic red"
    [ HP.title msg ]
    [ HH.text "Error" ]
  CellValue val ->
    let
      dt = st.input.dataCol ^. dataColType
      needsEx = needsExpand resolveTycon dt derived
      inline = value (if needsEx then Compact else Full) dt val
    in
      case needsEx of
        true -> cldiv_ "flex"
          [ cldiv_ "flex-auto cell-compact" [ inline ]
          , HH.div
            [ HE.onMouseDown $ HE.input StopPropagation ]
            [ faButton_ ("expand cell__expand-button fa-lg " <>
                         if st.expanded then "cell__expand-button--open" else ""
                        )
                        ToggleExpanded
            , case st.expanded of
                true -> cldiv_ "relative"
                  [ cldiv_ "cell-expanded"
                    [ cldiv_ "cell-expanded__body p1"
                      [ value Full dt val
                      ]
                    ]
                  ]
                false -> HH.div_ []
            ]
          ]
        false -> inline

  where

  resolveTycon :: String -> TyconInfo
  resolveTycon c = unsafePartial $ fromJust $ Map.lookup c st.input.types

  getOneRow :: Id Table -> Maybe (Id Row)
  getOneRow t = do
    table <- Map.lookup t st.input.rowCache
    head $ Map.keys table

  derived = st.input.dataCol ^. dataColIsDerived

  value
    :: RenderMode -> DataType -> Value
    -> H.ParentHTML Query Child Slot Herc
  value mode dt val = case derived of
    Derived -> showValue mode dt val
    NotDerived -> editValue SlotRoot mode dt val id

  editValue
    :: SlotPath -> RenderMode -> DataType -> Value -> Path Value
    -> H.ParentHTML Query Child Slot Herc
  editValue slot mode dt val path = case val of
    VUndefined -> HH.text "<undefined>"
    VBool b -> editBool b (path <<< _VBool)
    VString s -> editString (SlotSub slot) s (path <<< _VString)
    VNumber n -> editNumber (SlotSub slot) n (path <<< _VNumber)
    VInteger i -> editInteger (SlotSub slot) i (path <<< _VInteger)
    VTime t -> editTime (SlotSub slot) t (path <<< _VTime)
    VRowRef mr -> case dt of
      DataAlgebraic "Row" [DataTable t] ->
        editRowRef mode t mr (path <<< _VRowRef)
      _ ->
        HH.div_ []
    VList vs -> case dt of
      DataAlgebraic "List" [sub] ->
        editList (SlotSub slot) mode sub vs (path <<< _VList)
      _ ->
        HH.div_ []
    VMaybe v -> case dt of
      DataAlgebraic "Maybe" [sub] ->
        editMaybe (SlotSub slot) mode sub v (path <<< _VMaybe)
      _ ->
        HH.div_ []
    VData vcon vargs -> case dt of
      DataAlgebraic tycon tyargs ->
        editAlgebraic (SlotSub slot) mode tycon tyargs vcon vargs
                      (path <<< _VData)
      _ ->
        HH.div_ []
    VRecord vrec -> case dt of
      DataAlgebraic "Record" [DataRecord trec] ->
        editRecord (SlotSub slot) mode trec vrec (path <<< _VRecord)
      _ ->
        HH.div_ []

  showValue
    :: RenderMode -> DataType -> Value
    -> H.ParentHTML Query Child Slot Herc
  showValue mode dt val = case val of
    VUndefined -> HH.text "<undefined>"
    VBool b -> showBool b
    VString s -> showString s
    VNumber n -> showNumber n
    VInteger i -> showInteger i
    VTime t -> showTime t
    VRowRef mr -> case dt of
      DataAlgebraic "Row" [DataTable t] ->
        showRowRef mode t mr
      _ ->
        HH.div_ []
    VList vs -> case dt of
      DataAlgebraic "List" [sub] ->
        showList mode sub vs
      _ ->
        HH.div_ []
    VMaybe v -> case dt of
      DataAlgebraic "Maybe" [sub] ->
        showMaybe mode sub v
      _ ->
        HH.div_ []
    VData vcon vargs -> case dt of
      DataAlgebraic tycon tyargs ->
        showAlgebraic mode tycon tyargs vcon vargs
      _ ->
        HH.div_ []
    VRecord vrec -> case dt of
      DataAlgebraic "Record" [DataRecord trec] ->
        showRecord mode trec vrec
      _ ->
        HH.div_ []

  editBool
    :: Boolean -> Path Boolean
    -> H.ParentHTML Query Child Slot Herc
  editBool val path = cldiv_ "cell-plain"
    [ HH.input
      [ HP.type_ HP.InputCheckbox
      , HP.checked val
      , HE.onChecked (setValue path Nothing)
      ]
    ]

  showBool val = cldiv_ "cell-plain"
    [ HH.text (if val then "True" else "False") ]

  editBoxHandler
    :: forall v. SlotPath -> Path v -> EditBox.Output v -> Maybe (Query Unit)
  editBoxHandler slot path e = ifRootElse slot rootH nonRootH
    where
      rootH = case e of
        EditBox.Save v mKey -> setValue path (keyToDir <$> mKey) v
        EditBox.Cancel      -> Just $ H.action CancelEdit
      nonRootH = case e of
        EditBox.Save v _ -> setValue path Nothing v
        EditBox.Cancel   -> Nothing

  editString
    :: SlotPath -> String -> Path String
    -> H.ParentHTML Query Child Slot Herc
  editString slot val path =
    HH.slot' cp1 slot EditBox.comp
             { value: val
             , placeholder: ""
             , className: ifRootElse slot "plaincell" "editbox"
             , inputClassName: ifRootElse slot "plaincell__input"
                                               "editbox__input"
             , invalidClassName: ifRootElse slot "red"
                                                 "editbox__input--invalid"
             , show: id
             , validate: Just
             , clickable: ifRootElse slot false true
             }
             (editBoxHandler slot path)

  showString val = cldiv_ "cell-plain cell-string"
    [ HH.text val ]

  editNumber
    :: SlotPath -> ValNumber -> Path ValNumber
    -> H.ParentHTML Query Child Slot Herc
  editNumber slot val path =
    HH.slot' cp2 slot EditBox.comp
             { value: val
             , placeholder: ""
             , className: "right-align full-height " <>
                          ifRootElse slot "plaincell" "editbox"
             , inputClassName: ifRootElse slot "plaincell__input"
                                               "editbox__input"
             , invalidClassName: ifRootElse slot "red"
                                                 "editbox__input--invalid"
             , show: \(ValNumber str) -> str
             , validate: parseValNumber
             , clickable: ifRootElse slot false true
             }
             (editBoxHandler slot path)

  showNumber (ValNumber str) = cldiv_ "cell-plain right-align"
    [ HH.text str ]

  editInteger
    :: SlotPath -> Int -> Path Int
    -> H.ParentHTML Query Child Slot Herc
  editInteger slot val path =
    HH.slot' cp4 slot EditBox.comp
             { value: val
             , placeholder: ""
             , className: "right-align full-height " <>
                          ifRootElse slot "plaincell" "editbox"
             , inputClassName: ifRootElse slot "plaincell__input"
                                               "editbox__input"
             , invalidClassName: ifRootElse slot "red"
                                                 "editbox__input--invalid"
             , show: show
             , validate: parseInteger
             , clickable: ifRootElse slot false true
             }
             (editBoxHandler slot path)

  showInteger i = cldiv_ "cell-plain right-align"
    [ HH.text (show i) ]

  editTime
    :: SlotPath -> ValTime -> Path ValTime
    -> H.ParentHTML Query Child Slot Herc
  editTime slot val@(ValTime str) path = HH.div
    [ HE.onDoubleClick $ HE.input StopPropagation
    , HE.onMouseDown $ HE.input StopPropagation
    ]
    [ HH.slot' cp3 slot Date.comp { date: val }
               case _ of
                 Date.DateChanged val' -> setValue path Nothing val'
                 Date.Closed -> Just $ H.action CancelEdit
    ]

  showTime (ValTime str) = cldiv_ "cell-plain"
    [ HH.text str ]

  editRowRef
    :: RenderMode -> Id Table -> Maybe (Id Row) -> Path (Maybe (Id Row))
    -> H.ParentHTML Query Child Slot Herc
  editRowRef mode t val path =  cldiv "cell-plain"
    [ HE.onMouseDown $ HE.input StopPropagation ]
    let
      options = Map.toUnfoldable (rows t) <#> \(Tuple r row) ->
        { value: Just r
        , label: showPairs row
        }
      defaultOption = if isNothing val
        then [ { value: Nothing, label: "" } ]
        else []
    in
      [ dropdown "select select--width100" (defaultOption <> options) val
                 (setValueAction path Nothing)
      ]

  showRowRef mode t val = case mode of
    Compact -> cldiv_ "cell-plain"
      [ HH.text case val of
          Nothing -> "<no row chosen>"
          Just r -> case Map.lookup r (rows t) of
            Nothing -> ""
            Just row -> showPairs row
      ]
    Full -> case val of
      Nothing -> HH.text "Impossible: invalid row in derived cell"
      Just r -> case Map.lookup r (rows t) of
        Nothing -> HH.div_ []
        Just row -> cldiv_ "cell-table" $ Map.toUnfoldable row <#> \(Tuple _ tuple) -> case tuple of
          Tuple c content -> cldiv_ "cell-table__row"
            [ cldiv_ "table-cell bg-white bold p1"
              [ HH.text (c ^. columnName) ]
            , cldiv_ "table-cell bg-white p1"
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
      CellValue v -> shortVal v

  rows t = fromMaybe Map.empty $ Map.lookup t st.input.rowCache

  editList
    :: SlotPath -> RenderMode -> DataType -> Array Value -> Path (Array Value)
    -> H.ParentHTML Query Child Slot Herc
  editList slot mode subDt vals path = case mode of
      Compact -> compactList mode subDt vals
      Full -> cldiv_ "cell-list" $
        elements <>
        [ cldiv_ "cell-list__new p1"
          [ HH.button
            [ HP.class_ (H.ClassName "cell-button")
            , HE.onClick \_ ->
                setValue path Nothing
                         (snoc vals (defaultValue getOneRow resolveTycon subDt))
            ]
            [ faIcon_ "plus-circle" ]
          ]
        ]
    where
    elements = mkIndexed vals <#> \(Tuple i v) ->
      cldiv_ "cell-list__element flex mb1"
      [ cldiv_ "cell-list__element-delete p1"
        [ HH.button
          [ HP.class_ (H.ClassName "cell-button")
          , HE.onClick \_ -> setValue path Nothing (listDel i vals) ]
          [ faIcon_ "minus-circle" ]
        ]
      , cldiv_ "flex-auto p1"
        [ editValue (SlotItem i slot) mode subDt v
                    (path <<< element i traversed)
        ]
      ]
    listDel i xs   = fromMaybe xs $ deleteAt i xs

  showList mode subDt vals = case mode of
    Compact ->
      compactList mode subDt vals
    Full -> cldiv_ "cell-list" $ vals <#> \v ->
      cldiv_ "cell-list__element mb1 p1"
      [ showValue mode subDt v
      ]

  compactList mode subDt vals = cldiv_ "" $
    [ clspan_ "font-smaller gray align-middle"
      [ HH.text $ show (length vals) <> " items" ]
    ] <> (
      take 4 vals <#> \v ->
        cldiv_ "cell-compact-list__element align-middle ml1"
        [ showValue mode subDt v
        ]
    )

  editMaybe
    :: SlotPath -> RenderMode -> DataType -> Maybe Value -> Path (Maybe Value)
    -> H.ParentHTML Query Child Slot Herc
  editMaybe slotPrefix mode subDt mVal path =
    cldiv_ "flex items-center" case mVal of
      Nothing ->
        [ HH.button
          [ HP.class_ (H.ClassName "cell-button")
          , HE.onClick \_ ->
              setValue path Nothing
                       (Just (defaultValue getOneRow resolveTycon subDt))
          ]
          [ faIcon_ "plus-circle" ]
        , cldiv_ "cell-maybe--nothing flex-auto"
          [ HH.text "Nothing" ]
        ]
      Just val ->
        [ HH.button
          [ HP.class_ (H.ClassName "cell-button")
          , HE.onClick \_ -> setValue path Nothing Nothing
          ]
          [ faIcon_ "minus-circle" ]
        , cldiv_ "flex-auto"
          [ editValue slotPrefix mode subDt val (path <<< _Just)
          ]
        ]

  showMaybe mode subDt mVal = case mVal of
    Nothing  -> cldiv_ "cell-maybe--nothing" [ HH.text "Nothing" ]
    Just val -> showValue mode subDt val

  prepareAlgebraic tycon tyargs vcon vargs =
    let
      vcons = instConstructors tyargs (resolveTycon tycon)
      vconsMap = Map.fromFoldable vcons
      getArgDts c = unsafePartial $ fromJust $ Map.lookup c vconsMap
    in
      { arguments: mkIndexed $ zip (getArgDts vcon) vargs
      , mkDefaultArgs: \c ->
          map (defaultValue getOneRow resolveTycon) (getArgDts c)
      , options: vcons <#> \(Tuple c _) -> { value: c, label: c }
      }

  editAlgebraic
    :: SlotPath -> RenderMode
    -> String -> Array DataType
    -> String -> Array Value
    -> Path { a :: String, b :: Array Value }
    -> H.ParentHTML Query Child Slot Herc
  editAlgebraic slotPrefix mode tycon tyargs vcon vargs path = case mode of
    Compact ->
      compactAlgebraic mode tycon tyargs vcon vargs
    Full ->
      let
        prep = prepareAlgebraic tycon tyargs vcon vargs
        argument (Tuple i (Tuple dt v)) =
          cldiv_ "cell-table__row"
          [ cldiv_ "table-cell bg-white p1"
            [ editValue (SlotItem i slotPrefix) mode dt v
                        (path <<< bLens <<< element i traversed)
            ]
          ]
        selVcon = dropdown "select" prep.options vcon \c ->
          setValueAction path Nothing
            { a: c
            , b: if c == vcon then vargs else prep.mkDefaultArgs c
            }
      in if hasSimpleVcons (resolveTycon tycon)
      then
        HH.div
        [ HE.onMouseDown $ HE.input StopPropagation ]
        [ selVcon ]
      else
        cldiv "cell-table"
        [ HE.onMouseDown $ HE.input StopPropagation ] $
        [ cldiv_ "cell-table__row"
          [ cldiv_ "table-cell bg-white p1"
            [ selVcon
            ]
          ]
        ] <> map argument prep.arguments

  showAlgebraic
    :: RenderMode
    -> String -> Array DataType
    -> String -> Array Value
    -> H.ParentHTML Query Child Slot Herc
  showAlgebraic mode tycon tyargs vcon vargs = case mode of
    Compact ->
      compactAlgebraic mode tycon tyargs vcon vargs
    Full ->
      let
        prep = prepareAlgebraic tycon tyargs vcon vargs
        argument (Tuple i (Tuple dt v)) =
          cldiv_ "cell-table__row"
          [ cldiv_ "table-cell bg-white p1"
            [ showValue mode dt v ]
          ]
      in
        cldiv_ "cell-table" $
        [ cldiv_ "cell-table__row"
          [ cldiv_ "table-cell bg-white p1 bold"
            [ HH.text vcon ]
          ]
        ] <> map argument prep.arguments

  compactAlgebraic mode tycon tyargs vcon vargs =
    let
      prep = prepareAlgebraic tycon tyargs vcon vargs
      argument (Tuple i (Tuple dt v)) = shortVal v
    in
      HH.text $ vcon <> " " <> intercalate " " (map argument prep.arguments)

  editRecord
    :: SlotPath -> RenderMode
    -> Array (Tuple String DataType)
    -> Array (Tuple String Value)
    -> Path (Array (Tuple String Value))
    -> H.ParentHTML Query Child Slot Herc
  editRecord slotPrefix mode trec vrec path = case mode of
    Compact ->
      compactRecord vrec
    Full ->
      let
        fields = mkIndexed $ zip trec vrec
        field (Tuple i (Tuple (Tuple f dt) (Tuple _ v))) =
          cldiv_ "cell-table__row"
          [ cldiv_ "table-cell col-4 bg-white p1" [ HH.text f ]
          , cldiv_ "table-cell col-8 bg-white p1"
            [ editValue (SlotItem i slotPrefix) mode dt v
                        (path <<< element i traversed <<< _2)
            ]
          ] 
      in
        cldiv_ "cell-table" $ map field fields

  showRecord
    :: RenderMode
    -> Array (Tuple String DataType)
    -> Array (Tuple String Value)
    -> H.ParentHTML Query Child Slot Herc
  showRecord mode trec vrec = case mode of
    Compact ->
      compactRecord vrec
    Full ->
      let
        field (Tuple (Tuple f dt) (Tuple _ v)) =
          cldiv_ "cell-table__row"
          [ cldiv_ "table-cell col-4 bg-white p1" [ HH.text f ]
          , cldiv_ "table-cell col-8 bg-white p1"
            [ showValue mode dt v ]
          ]
      in
        cldiv_ "cell-table" $ map field $ zip trec vrec

  compactRecord vrec =
    HH.text $ intercalate ", " $ map field vrec
    where
    field (Tuple f v) = f <> ": " <> shortVal v

keyToDir :: SaveKey -> Direction
keyToDir = case _ of
  Enter -> DirDown
  Tab -> DirRight

eval :: Query ~> H.ParentDSL State Query Child Slot Output Herc
eval = case _ of

  Update input next -> do
    modify _{ input = input }
    pure next

  StartEdit mChar next -> do
    dataCol <- gets _.input.dataCol
    _ <- case dataCol ^. dataColType of
      DataAlgebraic "Number" [] ->
        H.query' cp2 (SlotSub SlotRoot) $
        H.action $ EditBox.StartEdit mChar
      DataAlgebraic "Integer" [] ->
        H.query' cp4 (SlotSub SlotRoot) $
        H.action $ EditBox.StartEdit mChar
      DataAlgebraic "String" [] ->
        H.query' cp1 (SlotSub SlotRoot) $
        H.action $ EditBox.StartEdit mChar
      DataAlgebraic "DateTime" [] ->
        H.query' cp3 (SlotSub SlotRoot) $
        H.action Date.Open
      _          -> pure Nothing
    pure next

  CancelEdit next -> do
    H.raise YieldFocus
    pure next

  SetValue setter mDir next -> do
    gets _.input.content >>= case _ of
      CellError _ -> pure unit
      CellValue oldVal ->
        let
          go :: forall a. ValSetterF a -> Value -> Value
          go (ValSetterF path val) = path .~ val
        in
          H.raise $ SaveValue (runExists go setter oldVal) mDir
    pure next

  StopPropagation ev next -> do
    liftEff $ stopPropagation $ mouseEventToEvent ev
    pure next

  ToggleExpanded next -> do
    modify \st -> st { expanded = not st.expanded }
    pure next
