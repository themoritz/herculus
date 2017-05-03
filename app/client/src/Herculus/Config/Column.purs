module Herculus.Config.Column where

import Herculus.Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Herculus.Ace as Ace
import Data.Array (cons, null)
import Halogen.Component.ChildPath (cp1, type (\/), type (<\/>))
import Herculus.Monad (Herc, withApi)
import Herculus.Utils (Options, clbutton_, cldiv_, clspan, dropdown, faIcon_)
import Lib.Api.Rest (postProjectLintDataColByColumnId, postProjectLintReportColByColumnId, postProjectRunCommandsByProjectId)
import Lib.Api.Schema.Column (Column, ColumnKind(ColumnReport, ColumnData), CompileStatus(StatusError, StatusNone, StatusOk), DataCol, ReportCol, columnId, columnKind, columnName, dataColCompileStatus, dataColIsDerived, dataColSourceCode, dataColType, reportColCompileStatus, reportColFormat, reportColLanguage, reportColTemplate)
import Lib.Api.Schema.Project (Command(..))
import Lib.Compiler.Error (Error(..))
import Lib.Custom (Id(..), ProjectTag)
import Lib.Model.Column (DataType(..), IsDerived(..), ReportFormat(..), ReportLanguage(..))
import Lib.Model.Table (Table)

data Query a
  = Initialize a
  | Update Input a
  | Reset a
  | Save a
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
  , tables :: Options (Id Table)
  , projectId :: Id ProjectTag
  }

type State =
  { input :: Input
  , tmp :: Tmp
  , errors :: Array Error
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

type Child =
  Ace.Query <\/>
  Const Void

type Slot =
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

data Branch
  = BBool
  | BString
  | BNumber
  | BTime
  | BRowRef
  | BList
  | BMaybe

derive instance genericBranch :: Generic Branch

toBranch :: DataType -> Branch
toBranch = case _ of
  DataBool     -> BBool
  DataString   -> BString
  DataNumber   -> BNumber
  DataTime     -> BTime
  DataRowRef _ -> BRowRef
  DataList   _ -> BList
  DataMaybe  _ -> BMaybe

branches :: Options Branch
branches =
  [ { value: BBool  , label: "Bool"   }
  , { value: BString, label: "String" }
  , { value: BNumber, label: "Number" }
  , { value: BTime  , label: "Time"   }
  , { value: BRowRef, label: "Row"    }
  , { value: BList  , label: "List"   }
  , { value: BMaybe , label: "Maybe"  }
  ]

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

subType :: DataType -> Maybe DataType
subType (DataList  a) = Just a
subType (DataMaybe a) = Just a
subType _             = Nothing

recordTableId :: DataType -> Maybe (Id Table)
recordTableId (DataRowRef tId) = Just tId
recordTableId _                = Nothing

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
    }
  , render
  , eval
  , receiver: Just <<< H.action <<< Update
  , initializer: Just $ H.action Initialize
  , finalizer: Nothing
  }

render :: State -> H.ParentHTML Query Child Slot Herc
render st = HH.div_
  [ cldiv_ "p1"
    [ HH.text $ "Edit Column "
    , HH.b_
      [ HH.text $ st.input.column ^. columnName ]
    ]
  , confFooter
  , case st.input.column ^. columnKind of
      ColumnData dat -> dataColConf dat
      ColumnReport rep -> reportColConf rep
  , statusBar
  ]

  where

  dataColConf dat = HH.div_
    [ cldiv_ "p1"
      [ HH.text "Column Datatype"
      , HH.div_
        [ selBranch (getDataType dat st) SetDataType ]
      ]
    , cldiv_ ""
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
        [ HH.text "Use formula ("
        , HH.a
          [ HP.href "/doc/formulas/#the-hexl-language"
          , HP.target "_blank"
          ]
          [ HH.text "Help" ]
        , HH.text ")"
        ]
      ]
    , cldiv_ ("pb1" <> case getIsDerived dat st of
                Derived -> ""
                NotDerived -> " column-config__editor--disabled"
             )
      [ HH.slot' cp1 unit Ace.comp
                 { mode: "ace/mode/haskell"
                 }
                 \(Ace.TextChanged f) -> Just $ H.action $ SetFormula f
      ]
    ]

  selBranch
    :: forall p. DataType -> (DataType -> H.Action Query)
    -> HH.HTML p (Query Unit)
  selBranch value cb = HH.span_
    [ dropdown "select" branches (toBranch value) case _ of
        BBool   -> cb DataBool
        BString -> cb DataString
        BNumber -> cb DataNumber
        BTime   -> cb DataTime
        BMaybe  -> cb (DataMaybe DataNumber)
        BList   -> cb (DataList DataNumber)
        BRowRef -> cb (DataRowRef (Id ""))
    , HH.text " "
    , case value of
        DataMaybe sub -> selBranch sub (cb <<< DataMaybe)
        DataList sub  -> selBranch sub (cb <<< DataList)
        DataRowRef tableId ->
          let
            options = cons { value: Id "", label: "" } st.input.tables
          in
            dropdown "select" options tableId (cb <<< DataRowRef)
        _ -> HH.text ""
    ]

  reportColConf rep = HH.div_
    [ cldiv_ "table col-12"
      [ cldiv_ "table-cell col-6 p1"
        [ HH.text "Input language ("
        , HH.a
          [ HP.href "/doc/formulas/#report-templates"
          , HP.target "_blank"
          ]
          [ HH.text "Help" ]
        , HH.text ")"
        , dropdown "select" reportLangs
                   (getReportLanguage rep st) SetReportLang
        ]
      , cldiv_ "table-cell col-1 p1 align-bottom center"
        [ faIcon_ "long-arrow-right"
        ]
      , cldiv_ "table-cell col-5 p1"
        [ HH.text "Output format"
        , dropdown "select" reportFormats
                   (getReportFormat rep st) SetReportFormat
        ]
      ]
    , cldiv_ "px1"
      [ HH.text "Template"
      ]
    , cldiv_ "pb1"
      [ HH.slot' cp1 unit Ace.comp
                 { mode: case getReportLanguage rep st of
                     Nothing -> "ace/mode/text"
                     Just ReportLanguageHTML -> "ace/mode/html"
                     Just ReportLanguageLatex -> "ace/mode/latex"
                     Just ReportLanguageMarkdown -> "ace/mode/markdown"
                 }
                 \(Ace.TextChanged t) -> Just (H.action $ SetReportTemplate t)
      ]
    ]

  confFooter = cldiv_ "clearfix p1 bg-lightgray"
    [ cldiv_ "left"
      [ clspan "link font-smaller"
        [ HE.onClick (HE.input_ Close') ]
        [ HH.text "Close" ]
      , HH.text " "
      , clspan "link font-smaller"
        [ HE.onClick (HE.input_ Reset) ]
        [ HH.text "Reset" ]
      ]
    , cldiv_ "right"
      [ clbutton_ "button bold mr1" Delete'
        [ faIcon_ "close red mr1"
        , HH.text "Delete column"
        ]
      , clbutton_ "button bold" Save
        [ faIcon_ "check green mr1"
        , HH.text "Save"
        ]
      ]
    ]

  statusBar = if null st.errors
    then cldiv_ "bg-lightgreen m0 p1"
      [ HH.text "No errors found." ]
    else HH.div_ $
      st.errors <#> \(Error e) ->
        cldiv_ "bg-lightred m0 p1"
        [ HH.text e.errMsg ]

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

  SetReportLang lang next -> do
    modify _{ tmp { reportLanguage = Just lang } }
    pure next

  SetReportFormat format next -> do
    modify _{ tmp { reportFormat = Just format } }
    pure next

  SetReportTemplate template next -> do
    modify _{ tmp { reportTemplate = Just template } }
    lintTemplate
    pure next

  SetFormula formula next -> do
    modify _{ tmp { formula = Just formula } }
    lintFormula
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
    ColumnData dataCol -> do
      let
        dt = getDataType dataCol st
        formula = getFormula dataCol st
        call = postProjectLintDataColByColumnId
                   (Tuple dt formula) (col ^. columnId)
      withApi call \errs -> do
        _ <- H.query' cp1 unit $ H.action $ Ace.SetAnnotations errs
        modify _{ errors = errs }
        pure unit
    ColumnReport _ -> pure unit
