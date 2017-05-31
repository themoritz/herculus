module Herculus.Column where

import Herculus.Prelude
import CSS as CSS
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Data.Array (find, intercalate, null)
import Herculus.Monad (Herc)
import Herculus.Utils (Options, cldiv, cldiv_, faIcon_)
import Lib.Api.Schema.Column (Column, ColumnKind(ColumnReport, ColumnData), CompileStatus(StatusError, StatusNone, StatusOk), columnKind, columnName, dataColCompileStatus, dataColIsDerived, dataColType, reportColCompileStatus, reportColFormat, reportColLanguage)
import Lib.Custom (Id)
import Lib.Model.Column (DataType(..), IsDerived(..), ReportFormat(..), ReportLanguage(..))
import Lib.Model.Table (Table)

data Query a
  = Update Input a
  | OpenConfig' a

type Input =
  { column :: Column
  , tables :: Options (Id Table)
  }

data Output
  = OpenConfig

type State =
  { input :: Input
  }

type Child =
  Const Void

type Slot =
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

render :: State -> H.ParentHTML Query Child Slot Herc
render st = cldiv_ ""
  [ cldiv "bold"
    [ HC.style do
        CSS.marginLeft $ CSS.px 20.0
        CSS.minHeight $ CSS.px 24.0
    ]
    [ HH.text $ st.input.column ^. columnName
    ]
  , columnInfo
  ]

  where

  columnInfo = cldiv_ "column-info font-smaller gray" $
    [ HH.button
      [ HE.onClick $ HE.input_ OpenConfig'
      , HP.class_ (H.ClassName "button--pure mr1")
      ]
      [ faIcon_ $ "gear fa-lg" <> (if hasErrors then " red" else "") ]
    ] <>
    case st.input.column ^. columnKind of
      ColumnData dat ->
        [ case dat ^. dataColIsDerived of
            Derived    -> faIcon_ "superscript mr1"
            NotDerived -> faIcon_ "pencil-square-o mr1"
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
            HH.text (lang <> " ")
        , faIcon_ "long-arrow-right mr1"
        , let
            format = case rep ^. reportColFormat of
              ReportFormatPlain -> "Plaintext"
              ReportFormatPDF   -> "PDF"
              ReportFormatHTML  -> "HTML"
          in
            HH.text format
        ]

  dataTypeInfo :: DataType -> String
  dataTypeInfo dt = (renderDataType dt).text

  renderDataType :: DataType -> { text :: String, needsParens :: Boolean }
  renderDataType = case _ of
    DataAlgebraic c args ->
      { text:
          let
            goArg arg =
              let sub = renderDataType arg in
              if sub.needsParens then " (" <> sub.text <> ")"
                                 else " " <> sub.text
          in
            c <> intercalate "" (map goArg args)
      , needsParens: not $ null args
      }
    DataTable t ->
      { text:
          "#" <> (maybe "<missing table>"
                  _.label (find (\o -> o.value == t) st.input.tables))
      , needsParens: false
      }
    DataRecord r ->
      { text:
          let
            goField (Tuple field dt) = field <> " : " <> (renderDataType dt).text
          in
            "{ " <> intercalate ", " (map goField r) <> " }"
      , needsParens: false
      }

  hasErrors = hasColumnErrors st.input.column

eval :: Query ~> H.ParentDSL State Query Child Slot Output Herc
eval = case _ of

  Update input next -> do
    modify _{ input = input}
    pure next

  OpenConfig' next -> do
    H.raise OpenConfig
    pure next

hasColumnErrors :: Column -> Boolean
hasColumnErrors c = case c ^. columnKind of
  ColumnData dat -> case dat ^. dataColIsDerived of
    Derived -> errorStatus (dat ^. dataColCompileStatus)
    NotDerived -> false
  ColumnReport rep -> errorStatus (rep ^. reportColCompileStatus)

errorStatus :: CompileStatus -> Boolean
errorStatus = case _ of
  StatusOk -> false
  StatusNone -> false
  StatusError _ -> true
