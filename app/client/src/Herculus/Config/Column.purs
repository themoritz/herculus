module Herculus.Config.Column where

import Herculus.Prelude
import CSS as CSS
import Data.Map as Map
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Herculus.Ace as Ace
import Herculus.EditBox as Edit
import Herculus.Config.Common.Header as Header
import Control.Monad.Eff.Ref (Ref)
import Data.Array (cons, deleteAt, find, head, length, snoc, zip)
import Data.Exists (Exists, mkExists, runExists)
import Data.Foldable (minimumBy)
import Data.Lens (_1, _2, element, traversed, (^?))
import Data.Lens.Types (Setter')
import Data.Map (Map)
import Halogen.Component.ChildPath (type (<\/>), type (\/), cp1, cp2, cp3)
import Herculus.Config.Column.Types (filterTypes, subTypes)
import Herculus.Config.Common (withDelay, section, errorList)
import Herculus.Monad (Herc, withApi)
import Herculus.Utils (Options, bLens, cldiv_, dropdown, faButton_, mkIndexed)
import Lib.Api.Rest (postProjectLintDataColByColumnId, postProjectLintReportColByColumnId, postProjectRunCommandsByProjectId)
import Lib.Api.Schema.Column (Column, ColumnKind(..), CompileStatus(StatusError, StatusNone, StatusOk), DataCol, ReportCol, _ColumnData, _ColumnReport, columnId, columnKind, columnName, columnTableId, dataColCompileStatus, dataColIsDerived, dataColSourceCode, dataColType, reportColCompileStatus, reportColFormat, reportColLanguage, reportColTemplate)
import Lib.Api.Schema.Compiler (Kind(KindRecord, KindTable, KindType), TyconInfo, tyconKind)
import Lib.Api.Schema.Project (Command(..))
import Lib.Compiler.Error (Error)
import Lib.Custom (Id(..), ProjectTag)
import Lib.Model.Column (DataType(..), IsDerived(..), ReportFormat(..), ReportLanguage(..), _DataAlgebraic, _DataRecord, _DataTable)
import Lib.Model.Table (Table)
import Partial.Unsafe (unsafePartial)

type Path a = Setter' DataType a

data DtSetterF a = DtSetterF (Path a) a
type DtSetter = Exists DtSetterF

setDataTypeAction :: forall a. Path a -> a -> H.Action Query
setDataTypeAction path val =
  SetDataType (mkExists $ DtSetterF path val)

setDataType :: forall a. Path a -> a -> Maybe (Query Unit)
setDataType path val = Just $ H.action $ setDataTypeAction path val

data Query a
  = Initialize a
  | Update Input a
  | Reset a
  | Save a
  | SetName String a
  | SetReportLang (Maybe ReportLanguage) a
  | SetReportFormat ReportFormat a
  | SetReportTemplate String a
  | SetFormula String a
  | SetIsDerived IsDerived a
  | SetDataType DtSetter a
  | Close' a
  | Delete' a

type Input =
  { column :: Column
  , types :: Map String TyconInfo
  , tables :: Options (Id Table)
  , projectId :: Id ProjectTag
  }

type State =
  { input :: Input
  , tmp :: Tmp
  , errors :: Array Error
  , delayRef :: Maybe (Ref Boolean)
  }

type Tmp =
  { dataType       :: Maybe DataType
  , isDerived      :: Maybe IsDerived
  , formula        :: Maybe String
  , reportLanguage :: Maybe (Maybe ReportLanguage)
  , reportFormat   :: Maybe ReportFormat
  , reportTemplate :: Maybe String
  }

emptyTmp :: Tmp
emptyTmp =
  { dataType       : Nothing
  , isDerived      : Nothing
  , formula        : Nothing
  , reportLanguage : Nothing
  , reportFormat   : Nothing
  , reportTemplate : Nothing
  }

unsavedChanges :: State -> Boolean
unsavedChanges st =
  isJust st.tmp.dataType ||
  isJust st.tmp.isDerived ||
  (case st.tmp.formula,
        st.input.column ^? columnKind <<< _ColumnData <<< dataColSourceCode of
    Just f, Just f' | f /= f' -> true
    _, _ -> false) ||
  isJust st.tmp.reportLanguage ||
  isJust st.tmp.reportFormat ||
  (case st.tmp.reportTemplate,
        st.input.column ^? columnKind <<< _ColumnReport <<< reportColTemplate of
    Just t, Just t' | t /= t' -> true
    _, _ -> false)

type Child =
  Ace.Query <\/>
  Header.Query <\/>
  Edit.Query String <\/>
  Const Void

type SlotPath = Array Int

type Slot =
  Unit \/
  Unit \/
  SlotPath \/
  Unit

data Output
  = Close

--------------------------------------------------------------------------------
  
getDataType :: DataCol -> State -> DataType
getDataType dat st = fromMaybe (dat ^. dataColType) st.tmp.dataType
  
getIsDerived :: DataCol -> State -> IsDerived
getIsDerived dat st = fromMaybe (dat ^. dataColIsDerived) st.tmp.isDerived
  
getFormula :: DataCol -> State -> String
getFormula dat st = fromMaybe (dat ^. dataColSourceCode) st.tmp.formula

getReportLanguage :: ReportCol -> State -> Maybe ReportLanguage
getReportLanguage rep st = fromMaybe (rep ^. reportColLanguage) st.tmp.reportLanguage

getReportFormat :: ReportCol -> State -> ReportFormat
getReportFormat rep st = fromMaybe (rep ^. reportColFormat) st.tmp.reportFormat

getReportTemplate :: ReportCol -> State -> String
getReportTemplate rep st = fromMaybe (rep ^. reportColTemplate) st.tmp.reportTemplate

reportLangs :: Options (Maybe ReportLanguage)
reportLangs =
  [ { value: Nothing                    , label: "Plaintext" }
  , { value: Just ReportLanguageMarkdown, label: "Markdown"  }
  , { value: Just ReportLanguageLatex   , label: "Latex"     }
  , { value: Just ReportLanguageHTML    , label: "HTML"      }
  ]

reportFormats :: Options ReportFormat
reportFormats =
  [ { value: ReportFormatPlain , label: "Plaintext" }
  , { value: ReportFormatPDF   , label: "PDF"       }
  , { value: ReportFormatHTML  , label: "HTML"      }
  ]

--------------------------------------------------------------------------------

getColumnErrors :: Column -> Array Error
getColumnErrors c = case c ^. columnKind of
  ColumnData dat -> case dat ^. dataColIsDerived of
    Derived -> statusToErrors (dat ^. dataColCompileStatus)
    NotDerived -> []
  ColumnReport rep -> statusToErrors (rep ^. reportColCompileStatus)

statusToErrors :: CompileStatus -> Array Error
statusToErrors = case _ of
  StatusOk -> []
  StatusNone -> []
  StatusError e -> e

--------------------------------------------------------------------------------

comp :: H.Component HH.HTML Query Input Output Herc
comp = H.lifecycleParentComponent
  { initialState:
    { input: _
    , tmp: emptyTmp
    , errors: []
    , delayRef: Nothing
    }
  , render
  , eval
  , receiver: Just <<< H.action <<< Update
  , initializer: Just $ H.action Initialize
  , finalizer: Nothing
  }

render :: State -> H.ParentHTML Query Child Slot Herc
render st = HH.div_
  case st.input.column ^. columnKind of
    ColumnData dat -> dataColConf dat
    ColumnReport rep -> reportColConf rep

  where

  header = HH.slot' cp2 unit Header.comp
    { name: st.input.column ^. columnName
    , unsaved: unsavedChanges st
    , subTitle: HH.div_
        [ HH.text $ "Column on table "
        , HH.b_
          [ HH.text $ maybe "???" _.label
              (find (\t -> t.value == st.input.column ^. columnTableId)
                    st.input.tables)
          ]
        ]
    }
    \o -> Just $ H.action $ case o of
      Header.SetName n -> SetName n
      Header.Close -> Close'
      Header.Reset -> Reset
      Header.Delete -> Delete'
      Header.Save -> Save

  dataColConf dat =
    [ header
    , let
        head = cldiv_ "bold"
          [ HH.text "Column Type" ]
        body = cldiv_ ""
          [ selDataType [] KindType (getDataType dat st) id
          ]
      in
        section "cube" head body
    , let
        head = cldiv_ "bold"
          [ HH.text "Column Formula" ]
        body = cldiv_ ""
          [ cldiv_ ""
            [ HH.input
              [ HP.type_ HP.InputCheckbox
              , HP.checked $ case getIsDerived dat st of
                  Derived -> true
                  NotDerived -> false
              , HE.onChecked \on ->
                  Just $ H.action $ SetIsDerived $
                  if on then Derived else NotDerived
              ]
            , HH.span_
              [ HH.text "Calculate content with formula ("
              , HH.a
                [ HP.href "/doc/formulas/#the-hexl-language"
                , HP.target "_blank"
                ]
                [ HH.text "Help" ]
              , HH.text "):"
              ]
            ]
          , cldiv_ ("my1 column-config__editor" <> case getIsDerived dat st of
                      Derived -> ""
                      NotDerived -> " column-config__editor--disabled"
                   )
            [ HH.slot' cp1 unit Ace.comp
                       { mode: "ace/mode/haskell"
                       }
                       \(Ace.TextChanged f) -> Just $ H.action $ SetFormula f
            ]
          , errorList st.errors
          ]
      in
        section "calculator" head body
    ]

  getFitting :: Kind -> Array (Tuple String (Array Kind))
  getFitting goal = filterTypes goal $ st.input.types

  getOneFitting :: Kind -> Tuple String (Array Kind)
  getOneFitting goal =
    unsafePartial $ fromJust $ minimumBy comp' $ getFitting goal
    where
    comp' (Tuple _ ks) (Tuple _ ks') = compare (length ks) (length ks')

  selTable
    :: Id Table -> Path (Id Table)
    -> H.ParentHTML Query Child Slot Herc
  selTable tableId path =
    let
      options = cons { value: Id "", label: "" } st.input.tables
    in
      dropdown "select" options tableId (setDataTypeAction path)

  selRecord
    :: SlotPath -> Array (Tuple String DataType)
    -> Path (Array (Tuple String DataType))
    -> H.ParentHTML Query Child Slot Herc
  selRecord slot record path = HH.div_
    [ cldiv_ "flex flex-wrap" $ join $ map field $ mkIndexed record
    , faButton_ "plus-circle"
      (setDataTypeAction path $ record `snoc` Tuple "x" (DataAlgebraic "Number" []))
    ]
    where
    field (Tuple i (Tuple f dt)) =
      [ cldiv_ "col-3 flex"
        [ faButton_ "minus-circle"
          (setDataTypeAction path $ unsafePartial $ fromJust $ deleteAt i record)
        , HH.slot' cp3 (slot `snoc` i) Edit.comp
                 { value: f
                 , placeholder: ""
                 , className: "flex-auto editbox"
                 , inputClassName: "editbox__input"
                 , invalidClassName: "editbox__input--invalid"
                 , show: id
                 , validate: case _ of
                     "" -> Nothing 
                     f' -> Just f'
                 , clickable: true
                 }
                 case _ of
                   Edit.Save v _ ->
                     setDataType (path <<< element i traversed <<< _1) v
                   Edit.Cancel ->
                     Nothing
        ]
      , cldiv_ "pl1 col-9"
        [ selDataType (slot `snoc` i) KindType dt
                      (path <<< element i traversed <<< _2)
        ]
      ]

  selAlgebraic
    :: SlotPath -> Kind -> String -> Array DataType
    -> Path { a :: String, b :: Array DataType }
    -> H.ParentHTML Query Child Slot Herc
  selAlgebraic slot kindGoal constructor args path =
    let
      mkDefaultArgs :: Kind -> String -> Array DataType
      mkDefaultArgs goal = map goGoal <<< getArgGoals goal
        where
        goGoal = case _ of
          KindTable ->
            DataTable (unsafePartial $ fromJust $ head st.input.tables).value
          KindRecord ->
            DataRecord []
          goal' ->
            let c' = fst (getOneFitting goal') in
            DataAlgebraic c' (mkDefaultArgs goal' c')

      options = map (\(Tuple name _) -> { value: name, label: name })
                    (getFitting kindGoal)

      getArgGoals :: Kind -> String -> Array Kind
      getArgGoals goal c = fromMaybe [] do
        c' <- Map.lookup c st.input.types
        subTypes goal (c' ^. tyconKind)

      argument (Tuple i (Tuple argGoal dt)) =
        selDataType (slot `snoc` i) argGoal dt
                    (path <<< bLens <<< element i traversed)
    in 
    HH.span_
    [ dropdown "select" options constructor \constr ->
        setDataTypeAction path
          { a: constr
          , b: if constr == constructor then args else mkDefaultArgs kindGoal constr
          }
    , cldiv_ "flex"
      [ HH.div
        [ HC.style (CSS.width $ CSS.px 20.0)
        ] []
      , cldiv_ "flex-auto" $
        map argument $ mkIndexed $ zip (getArgGoals kindGoal constructor) args
      ]
    ]

  selDataType
    :: SlotPath -> Kind -> DataType -> Path DataType
    -> H.ParentHTML Query Child Slot Herc
  selDataType slot kindGoal dt path = case kindGoal, dt of
    KindTable, DataTable tableId ->
      selTable tableId (path <<< _DataTable)
    KindRecord, DataRecord record ->
      selRecord slot record (path <<< _DataRecord)
    goal, DataAlgebraic constr args ->
      selAlgebraic slot goal constr args (path <<< _DataAlgebraic)
    _, _ -> HH.text "Invalid kind/dt combination"

  reportColConf rep =
    [ header
    , let
        head = cldiv_ "bold"
          [ HH.text "Input Language" ]
        body =
          dropdown "select" reportLangs
                   (getReportLanguage rep st) SetReportLang
      in
        section "file-code-o" head body
    , let
        head = cldiv_ "bold"
          [ HH.text "Output Format" ]
        body = cldiv_ ""
          [ dropdown "select" reportFormats
              (getReportFormat rep st) SetReportFormat
          ]
      in
        section "file-pdf-o" head body
    , let
        head = cldiv_ "bold"
          [ HH.text "Report Template"
          , HH.text " ("
          , HH.a
            [ HP.href "/doc/formulas/#report-templates"
            , HP.target "_blank"
            ]
            [ HH.text "Help" ]
          , HH.text ")"
          ]
        body = cldiv_ ""
          [ cldiv_ "column-config__editor my1"
            [ HH.slot' cp1 unit Ace.comp
                       { mode: case getReportLanguage rep st of
                           Nothing -> "ace/mode/text"
                           Just ReportLanguageHTML -> "ace/mode/html"
                           Just ReportLanguageLatex -> "ace/mode/latex"
                           Just ReportLanguageMarkdown -> "ace/mode/markdown"
                       }
                       \(Ace.TextChanged t) -> Just (H.action $ SetReportTemplate t)
            ]
          , errorList st.errors
          ]
      in
        section "file-text-o" head body
    ]

eval :: Query ~> H.ParentDSL State Query Child Slot Output Herc
eval = case _ of

  Initialize next -> do
    updateAce
    pure next
    
  Update input next -> do
    modify _
      { input = input
      , errors = getColumnErrors input.column
      }
    updateAce
    pure next

  Reset next -> do
    modify _{ tmp = emptyTmp
            }
    updateAce
    lintFormula
    lintTemplate
    pure next

  Save next -> do
    st <- H.get
    let
      c = st.input.column ^. columnId
      cmd = case st.input.column ^. columnKind of
        ColumnData col -> CmdDataColUpdate c
          (getDataType col st)
          (getIsDerived col st)
          (getFormula col st)
        ColumnReport rep -> CmdReportColUpdate c
          (getReportTemplate rep st)
          (getReportFormat rep st)
          (getReportLanguage rep st)
      sendCmd = postProjectRunCommandsByProjectId [cmd] st.input.projectId
    withApi sendCmd \_ ->
      modify _{ tmp = emptyTmp
              }
    pure next

  SetName name next -> do
    st <- H.get
    when (name /= st.input.column ^. columnName) $ do
      let
        cmd = CmdColumnSetName (st.input.column ^. columnId) name
        sendCmd = postProjectRunCommandsByProjectId [cmd] st.input.projectId
      withApi sendCmd \_ -> pure unit
    pure next

  SetReportLang lang next -> do
    modify _{ tmp { reportLanguage = Just lang } }
    pure next

  SetReportFormat format next -> do
    modify _{ tmp { reportFormat = Just format } }
    pure next

  SetReportTemplate template next -> do
    modify _{ tmp { reportTemplate = Just template } }
    withDelay 700 lintTemplate
    pure next

  SetFormula formula next -> do
    modify _{ tmp { formula = Just formula } }
    withDelay 700 lintFormula
    pure next

  SetIsDerived isDerived next -> do
    modify _{ tmp { isDerived = Just isDerived } }
    case isDerived of
      Derived -> lintFormula
      NotDerived -> pure unit
    pure next

  SetDataType setter next -> do
    st <- get
    col <- gets _.input.column
    case col ^. columnKind of
      ColumnData dataCol -> do
        let
          old = getDataType dataCol st
          go :: forall a. DtSetterF a -> DataType -> DataType
          go (DtSetterF path val) = path .~ val
        modify _{ tmp { dataType = Just $ runExists go setter old } }
      ColumnReport _ -> pure unit
    lintFormula
    pure next

  Close' next -> do
    H.raise Close
    pure next

  Delete' next -> do
    st <- get
    let
      cmd = CmdColumnDelete (st.input.column ^. columnId)
      call = postProjectRunCommandsByProjectId [cmd] st.input.projectId
    withApi call \_ -> H.raise Close
    pure next

updateAce :: H.ParentDSL State Query Child Slot Output Herc Unit
updateAce = do
  st <- get
  _ <- H.query' cp1 unit $ H.action $ Ace.SetText $
    case st.input.column ^. columnKind of
      ColumnReport repCol -> getReportTemplate repCol st
      ColumnData dataCol -> getFormula dataCol st
  pure unit

lintTemplate :: H.ParentDSL State Query Child Slot Output Herc Unit
lintTemplate = do
  st <- get
  let col = st.input.column
  case col ^. columnKind of
    ColumnReport repCol -> do
      let
        template = getReportTemplate repCol st
        call = postProjectLintReportColByColumnId template (col ^. columnId)
      withApi call \errs -> do
        _ <- H.query' cp1 unit $ H.action $ Ace.SetAnnotations errs
        modify _{ errors = errs }
        pure unit
    ColumnData _ -> pure unit

lintFormula :: H.ParentDSL State Query Child Slot Output Herc Unit
lintFormula = do
  st <- get
  let col = st.input.column
  case col ^. columnKind of
    ColumnData dataCol -> case getIsDerived dataCol st of
      Derived -> do
        let
          dt = getDataType dataCol st
          formula = getFormula dataCol st
          call = postProjectLintDataColByColumnId
                     (Tuple dt formula) (col ^. columnId)
        withApi call \errs -> do
          _ <- H.query' cp1 unit $ H.action $ Ace.SetAnnotations errs
          modify _{ errors = errs }
          pure unit
      NotDerived -> pure unit
    ColumnReport _ -> pure unit

