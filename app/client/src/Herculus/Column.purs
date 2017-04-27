module Herculus.Column where

import Herculus.Prelude
import CSS as CSS
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.CSS as HC
import Halogen.HTML.Events as HE
import Halogen.Component.ChildPath (cp1, type (\/), type (<\/>))
import Data.Array (find)
import Herculus.EditBox as Edit
import Herculus.Monad (Herc)
import Herculus.Utils (Options, cldiv_, faIcon_)
import Lib.Api.Schema.Column (Column, ColumnKind(ColumnReport, ColumnData), CompileStatus(StatusError, StatusNone, StatusOk), columnKind, columnName, dataColCompileStatus, dataColIsDerived, dataColType, reportColCompileStatus, reportColFormat, reportColLanguage)
import Lib.Custom (Id)
import Lib.Model.Column (DataType(..), IsDerived(..), ReportFormat(..), ReportLanguage(..))
import Lib.Model.Table (Table)

data Query a
  = Update Input a
  | SetName' String a
  | OpenConfig' a

type Input =
  { column :: Column
  , tables :: Options (Id Table)
  }

data Output
  = SetName String
  | OpenConfig

type State =
  { input :: Input
  }

type Child =
  Edit.Query String <\/>
  Const Void

type Slot =
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
               , className: "bold editbox"
               , inputClassName: "editbox__input"
               , invalidClassName: "editbox__input--invalid"
               , show: id
               , validate: Just
               , clickable: true
               }
               case _ of
                 Edit.Save v _ -> Just $ H.action $ SetName' v
                 Edit.Cancel -> Nothing
      ]
    , columnInfo
    ]
  , HH.div_
    [ HH.button
      [ HE.onClick $ HE.input_ OpenConfig'
      , HP.class_ (H.ClassName "button--pure")
      ]
      [ faIcon_ $ "gear fa-2x" <> (if hasErrors then " red" else "") ]
    ]
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
                    maybe "missing table"
                          _.label (find (\o -> o.value == t) st.input.tables)
    DataList   d -> "List (" <> dataTypeInfo d <> ")"
    DataMaybe  d -> "Maybe (" <> dataTypeInfo d <> ")"

  hasErrors = hasColumnErrors st.input.column

eval :: Query ~> H.ParentDSL State Query Child Slot Output Herc
eval = case _ of

  Update input next -> do
    modify _{ input = input}
    pure next

  OpenConfig' next -> do
    H.raise OpenConfig
    pure next

  SetName' name next -> do
    oldName <- gets \st -> st.input.column ^. columnName
    when (name /= oldName) $ H.raise $ SetName name
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
