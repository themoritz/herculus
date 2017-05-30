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
import Control.Monad.Aff (delay)
import Control.Monad.Eff.Ref (Ref, newRef, readRef, writeRef)
import Control.Monad.Fork (fork)
import Data.Array (cons, find, null, updateAt, zip)
import Data.Int (toNumber)
import Data.Lens (view, (^?))
import Data.Map (Map)
import Data.Time.Duration (Milliseconds(..))
import Halogen.Component.ChildPath (cp1, cp2, type (\/), type (<\/>))
import Herculus.Config.Column.Types (filterTypes, subTypes)
import Herculus.Monad (Herc, withApi)
import Herculus.Utils (Options, clbutton_, cldiv, cldiv_, dropdown, faIcon_, mkIndexed)
import Lib.Api.Rest (postProjectLintDataColByColumnId, postProjectLintReportColByColumnId, postProjectRunCommandsByProjectId)
import Lib.Api.Schema.Column (Column, ColumnKind(ColumnReport, ColumnData), CompileStatus(StatusError, StatusNone, StatusOk), DataCol, ReportCol, _ColumnData, _ColumnReport, columnId, columnKind, columnName, columnTableId, dataColCompileStatus, dataColIsDerived, dataColSourceCode, dataColType, reportColCompileStatus, reportColFormat, reportColLanguage, reportColTemplate)
import Lib.Api.Schema.Compiler (Kind(..), TyconInfo(..), tyconKind)
import Lib.Api.Schema.Project (Command(..))
import Lib.Compiler.Error (Error(..))
import Lib.Custom (Id(..), ProjectTag)
import Lib.Model.Column (DataType(..), IsDerived(..), ReportFormat(..), ReportLanguage(..))
import Lib.Model.Table (Table)
import Partial.Unsafe (unsafePartial)

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
  | SetDataType DataType a
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
  Edit.Query String <\/>
  Const Void

type Slot =
  Unit \/
  Unit \/
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

  header =
    let
      head = cldiv_ "flex"
        [ cldiv_ "flex-auto"
          [ HH.slot' cp2 unit Edit.comp
                   { value: st.input.column ^. columnName
                   , placeholder: "Name..."
                   , className: "bold editbox"
                   , inputClassName: "editbox__input"
                   , invalidClassName: "editbox__input--invalid"
                   , show: id
                   , validate: Just
                   , clickable: true
                   }
                   case _ of
                     Edit.Save v _ -> Just $ H.action $ SetName v
                     Edit.Cancel -> Nothing
          ]
        , cldiv_ ""
          [ clbutton_ "button--pure" Close'
            [ faIcon_ "close fa-lg fa-fw" ]
          ]
        ]
      unsaved = unsavedChanges st
      disabled = if unsaved then "" else " button--disabled"
      body = cldiv_ "flex items-end"
        [ cldiv_ "font-smaller flex-auto"
          [ HH.text $ "Column on table "
          , HH.b_
            [ HH.text $ maybe "???" _.label
                (find (\t -> t.value == st.input.column ^. columnTableId)
                      st.input.tables)
            ]
          ]
        , cldiv_ "font-smaller"
          [ clbutton_ "button bold mr1" Delete'
            [ faIcon_ "close red mr1"
            , HH.text "Delete"
            ]
          , HH.button
            [ HP.class_ (H.ClassName $ "button bold mr1" <> disabled)
            , HE.onClick (if unsaved then HE.input_ Reset else const Nothing)
            , HP.disabled (not unsaved)
            ]
            [ faIcon_ "undo gray mr1"
            , HH.text "Reset"
            ]
          , clbutton_ "button bold" Save
            [ faIcon_ $ "check mr1 " <> if unsaved then "green" else "gray"
            , HH.text "Save"
            ]
          ]
        ]
    in
      section "wrench" head body 

  dataColConf dat =
    [ header
    , let
        head = cldiv_ "bold"
          [ HH.text "Column Type" ]
        body = cldiv_ ""
          [ selDataType KindType (getDataType dat st) SetDataType ]
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
          , errorBar
          ]
      in
        section "calculator" head body
    ]

  selTable
    :: forall p. Id Table -> (Id Table -> H.Action Query)
    -> HH.HTML p (Query Unit)
  selTable tableId cb =
    let
      options = cons { value: Id "", label: "" } st.input.tables
    in
      dropdown "select" options tableId cb

  selRecord
    :: forall p. Array (Tuple String DataType)
    -> (Array (Tuple String DataType) -> H.Action Query)
    -> HH.HTML p (Query Unit)
  selRecord record cb =
    HH.text "Record select"

  selAlgebraic
    :: forall p. Kind -> String -> Array DataType
    -> (String -> Array DataType -> H.Action Query)
    -> HH.HTML p (Query Unit)
  selAlgebraic kindGoal constructor args cb =
    let
      fitting :: Array (Tuple String (Array Kind))
      fitting = filterTypes kindGoal $ map (view tyconKind) st.input.types
      options = map (\(Tuple name _) -> { value: name, label: name }) fitting
      argGoals = unsafePartial $ fromJust do
        c <- Map.lookup constructor st.input.types
        subTypes kindGoal (c ^. tyconKind)
      argument (Tuple i (Tuple argGoal dt)) =
        selDataType argGoal dt \dt' ->
          cb constructor $ unsafePartial $ fromJust $ updateAt i dt' args
    in 
    HH.span_
    [ dropdown "select" options constructor \constr -> cb constr args
    , cldiv_ "flex"
      [ HH.div
        [ HC.style (CSS.width $ CSS.px 20.0)
        ] []
      , cldiv_ "flex-auto" $
        map argument $ mkIndexed $ zip argGoals args
      ]
    ]

  selDataType
    :: forall p. Kind -> DataType -> (DataType -> H.Action Query)
    -> HH.HTML p (Query Unit)
  selDataType kindGoal dt cb = case kindGoal, dt of
    KindTable, DataTable tableId ->
      selTable tableId (cb <<< DataTable)
    KindRecord, DataRecord record ->
      selRecord record (cb <<< DataRecord)
    goal, DataAlgebraic constr args ->
      selAlgebraic goal constr args \c as -> cb (DataAlgebraic c as)
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
          , errorBar
          ]
      in
        section "file-text-o" head body
    ]

  errorItem ok text = cldiv_ "flex"
    [ cldiv_ ""
      [ faIcon_ $ if ok
                  then "check-circle fa-fw green"
                  else "exclamation-circle fa-fw red"
      ]
    , cldiv_ "flex-auto gray pl1"
      [ HH.text text ]
    ]

  errorBar = cldiv_ "" $
    if null st.errors
    then [ errorItem true "All fine!" ]
    else map (\(Error e) -> errorItem false e.errMsg) st.errors

section :: forall p i. String -> HH.HTML p i -> HH.HTML p i -> HH.HTML p i
section icon head body = cldiv_ "flex items-start config__section"
  [ cldiv_ "pt1 pb1 pl1"
    [ faIcon_ $ icon <> " fa-lg fa-fw lightgray"
    ]
  , cldiv_ "flex-auto"
    [ cldiv "p1 border-box"
      [ HC.style do
          CSS.minHeight (CSS.px 24.0)
      ]
      [ head
      ]
    , cldiv_ "px1 pb1"
      [ body
      ]
    ]
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

  SetDataType dt next -> do
    modify _{ tmp { dataType = Just dt } }
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

withDelay
  :: Int
  -> H.ParentDSL State Query Child Slot Output Herc Unit
  -> H.ParentDSL State Query Child Slot Output Herc Unit
withDelay duration action = do
  { delayRef } <- get
  case delayRef of
    Nothing -> pure unit
    Just r -> liftEff $ writeRef r false
  newRef <- liftEff $ newRef true
  _ <- fork do
    liftAff $ delay (Milliseconds $ toNumber duration)
    case delayRef of
      Nothing -> action
      Just r -> do
        active <- liftEff $ readRef newRef
        if active then action else pure unit
  modify _{ delayRef = Just newRef }

