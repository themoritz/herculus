module Herculus.Column where

import Herculus.Prelude
import CSS as CSS
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML.CSS as HC
import Herculus.Ace as Ace
import Herculus.EditBox as Edit
import Data.Array (cons, find)
import Halogen.Component.ChildPath (cp1, cp2, type (\/), type (<\/>))
import Herculus.Monad (Herc)
import Herculus.Utils (Options, clbutton_, cldiv, cldiv_, clspan, clspan_, dropdown, faIcon_)
import Lib.Api.Schema.Column (Column, ColumnKind(ColumnReport, ColumnData), CompileStatus(StatusError, StatusNone, StatusOk), DataCol, ReportCol, columnKind, columnName, dataColCompileStatus, dataColIsDerived, dataColSourceCode, dataColType, reportColCompileStatus, reportColFormat, reportColLanguage, reportColTemplate)
import Lib.Custom (Id(..))
import Lib.Model.Column (DataType(..), IsDerived(..), ReportFormat(..), ReportLanguage(..))
import Lib.Model.Table (Table)

data Query a
  = Update Input a
  | SetName' String a
  | ConfigOpen a
  | ConfigCancel a
  | ConfigSave a
  | SetReportLang (Maybe ReportLanguage) a
  | SetReportFormat ReportFormat a
  | SetReportTemplate String a
  | SetFormula String a
  | SetIsDerived IsDerived a
  | SetDataType DataType a
  | Delete' a

type Input =
  { column :: Column
  , tables :: Options (Id Table)
  }

data Output
  = SetName String
  | Delete
  | SaveReportCol String ReportFormat (Maybe ReportLanguage)
  | SaveDataCol DataType IsDerived String

type State =
  { input :: Input
  , open :: Boolean
  , tmp :: Tmp
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

type Child =
  Edit.Query String <\/>
  Ace.Query <\/>
  Const Void

type Slot =
  Unit \/
  Unit \/
  Unit

comp :: H.Component HH.HTML Query Input Output Herc
comp = H.parentComponent
  { initialState:
    { input: _
    , open: false
    , tmp: emptyTmp
    }
  , receiver: Just <<< H.action <<< Update
  , render
  , eval
  }

render :: State -> H.ParentHTML Query Child Slot Herc
render st = cldiv_ "flex items-center"
  [ cldiv_ "flex-auto"
    [ HH.div
      [ HC.style do
          CSS.marginLeft $ CSS.px 18.0
      ]
      [ HH.slot' cp1 unit Edit.comp
               { value: st.input.column ^. columnName
               , placeholder: "Name..."
               , className: "bold"
               , show: id
               , validate: Just
               }
               (Just <<< H.action <<< SetName')
      ]
    , columnInfo
    ]
  , columnConfig
  ]

  where

  columnInfo = cldiv_ "column-info font-smaller gray"
    case st.input.column ^. columnKind of
      ColumnData dat ->
        [ case dat ^. dataColIsDerived of
            Derived    -> faIcon_ "superscript mr1"
            NotDerived -> faIcon_ "i-cursor mr1"
        , HH.span_
          [ HH.text $ dataTypeInfo (dat ^. dataColType)
          ]
        ]
      ColumnReport rep ->
        [ faIcon_ "file-text-o mr1"
        , let
            lang = case rep ^. reportColLanguage of
              Nothing                     -> "Plaintext"
              Just ReportLanguageMarkdown -> "Markdown"
              Just ReportLanguageLatex    -> "Latex"
              Just ReportLanguageHTML     -> "HTML"
          in
            HH.text ("Report " <> lang <> " ")
        , faIcon_ "long-arrow-right mr1"
        , let
            format = case rep ^. reportColFormat of
              ReportFormatPlain -> "Plaintext"
              ReportFormatPDF   -> "PDF"
              ReportFormatHTML  -> "HTML"
          in
            HH.text format
        ]
        

  dataTypeInfo = case _ of
    DataBool     -> "Bool"
    DataString   -> "String"
    DataNumber   -> "Number"
    DataTime     -> "Time"
    DataRowRef t -> "Row from " <>
                    maybe "missing table" _.label (find (\o -> o.value == t) st.input.tables)
    DataList   d -> "List (" <> dataTypeInfo d <> ")"
    DataMaybe  d -> "Maybe (" <> dataTypeInfo d <> ")"

  columnConfig = cldiv_ ""
    [ HH.button
      [ HP.classes
        [ H.ClassName "button--pure"
        , H.ClassName "pr1"
        , H.ClassName (if isJust getError then "red" else "")
        ]
      , HE.onClick (HE.input_ ConfigOpen)
      ]
      [ faIcon_ "gear fa-2x" ]
    , cldiv_ "relative" case st.open of
        false -> []
        true ->
          [ case st.input.column ^. columnKind of
              ColumnData dat -> dataColConf dat
              ColumnReport rep -> reportColConf rep
          ]
    ]

  getError = case st.input.column ^. columnKind of
    ColumnData dat -> case dat ^. dataColIsDerived of
      Derived -> getErrorStatus (dat ^. dataColCompileStatus)
      NotDerived -> Nothing
    ColumnReport rep -> getErrorStatus (rep ^. reportColCompileStatus)

  getErrorStatus = case _ of
    StatusOk -> Nothing
    StatusNone -> Nothing
    StatusError e -> Just e

  dataColConf dat = cldiv_ "column-popup" (
    [ cldiv_ "column-popup__wrapper"
      [ cldiv_ "table col-12 bg-white"
        [ cldiv_ "table-cell col-2 p1 column-popup__datatype"
          [ selBranch (getDataType dat st) SetDataType
          ]
        , cldiv_ "table-cell col-10 p1"
          [ HH.div_
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
          , cldiv_ ("column-popup__editor " <>
                    case getIsDerived dat st of
                      Derived -> ""
                      NotDerived -> "column-popup__editor--disabled"
                   )
            [ HH.slot' cp2 unit Ace.comp
                       { value: getFormula dat st
                       , mode: "ace/mode/haskell"
                       }
                       \(Ace.TextChanged f) -> Just $ H.action $ SetFormula f
            ]
          ]
        ]
      ]
    ] <> confFooter
    )

  selBranch :: forall p. DataType -> (DataType -> H.Action Query) -> HH.HTML p (Query Unit)
  selBranch value cb = cldiv_ "selBranch"
    [ dropdown "select" branches (toBranch value) case _ of
        BBool   -> cb DataBool
        BString -> cb DataString
        BNumber -> cb DataNumber
        BTime   -> cb DataTime
        BMaybe  -> cb (DataMaybe DataNumber)
        BList   -> cb (DataList DataNumber)
        BRowRef -> cb (DataRowRef (Id ""))
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

  reportColConf rep = cldiv_ "column-popup" (
    [ cldiv_ "column-popup__wrapper"
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
      , cldiv_ "column-popup__editor mb1 mx1"
        [ HH.slot' cp2 unit Ace.comp
                   { value: getReportTemplate rep st
                   , mode: case getReportLanguage rep st of
                       Nothing -> "ace/mode/text"
                       Just ReportLanguageHTML -> "ace/mode/html"
                       Just ReportLanguageLatex -> "ace/mode/latex"
                       Just ReportLanguageMarkdown -> "ace/mode/markdown"
                   }
                   \(Ace.TextChanged t) -> Just (H.action $ SetReportTemplate t)
        ]
      ]
    ] <> confFooter
    )

  confFooter =
    [ case getError of
        Just e -> cldiv_ "bg-lightred m0 p1 font-smaller"
          [ clspan_ "red bold"
            [ HH.text "Error"
            ]
          , HH.div_
            [ HH.text e
            ]
          ]
        Nothing -> HH.text ""
    , cldiv_ "clearfix p1 bg-lightgray"
      [ cldiv_ "left"
        [ clspan "link font-smaller"
          [ HE.onClick (HE.input_ ConfigCancel) ]
          [ HH.text "Cancel" ]
        ]
      , cldiv_ "right"
        [ clbutton_ "button bold mr1" Delete'
          [ faIcon_ "close red mr1"
          , HH.text "Delete column"
          ]
        , clbutton_ "button bold" ConfigSave
          [ faIcon_ "check green mr1"
          , HH.text "Save"
          ]
        ]
      ]
    ]

eval :: Query ~> H.ParentDSL State Query Child Slot Output Herc
eval = case _ of

  Update input next -> do
    modify _{ input = input }
    pure next

  SetName' name next -> do
    H.raise $ SetName name
    pure next

  ConfigOpen next -> do
    modify _{ open = true }
    pure next

  ConfigCancel next -> do
    modify _
      { open = false
      , tmp = emptyTmp
      }
    pure next

  ConfigSave next -> do
    st <- H.get
    modify _
      { open = false
      , tmp = emptyTmp
      }
    case st.input.column ^. columnKind of
      ColumnData col ->
        H.raise $ SaveDataCol (getDataType col st)
                              (getIsDerived col st)
                              (getFormula col st)
      ColumnReport rep ->
        H.raise $ SaveReportCol (getReportTemplate rep st)
                                (getReportFormat rep st)
                                (getReportLanguage rep st)
    pure next

  SetReportLang lang next -> do
    modify _{ tmp { reportLanguage = Just lang } }
    pure next

  SetReportFormat format next -> do
    modify _{ tmp { reportFormat = Just format } }
    pure next

  SetReportTemplate template next -> do
    modify _{ tmp { reportTemplate = Just template } }
    pure next

  SetFormula formula next -> do
    modify _{ tmp { formula = Just formula } }
    pure next

  SetIsDerived isDerived next -> do
    modify _{ tmp { isDerived = Just isDerived } }
    pure next

  SetDataType dt next -> do
    modify _{ tmp { dataType = Just dt } }
    pure next

  Delete' next -> do
    H.raise Delete
    pure next
