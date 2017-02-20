module Herculus.ReportCell where

import Herculus.Prelude
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Herculus.Monad (Herc, getApiUrl, getAuthToken)
import Herculus.Project.Data (Coords(..))
import Herculus.Utils (clspan_, faIcon_)
import Lib.Api.Schema.Column (CompileStatus(StatusOk, StatusError, StatusNone), ReportCol, reportColCompileStatus, reportColFormat)
import Lib.Custom (Id(..))
import Lib.Model.Column (ReportFormat(..))

data Query a
  = Initialize a
  | Update Input a

type Input =
  { reportCol :: ReportCol
  , coords :: Coords
  }

type State =
  { input :: Input
  , getReportPath :: Maybe (String -> String)
  }

comp :: H.Component HH.HTML Query Input Void Herc
comp = H.lifecycleComponent
  { initialState:
    { input: _
    , getReportPath: Nothing
    }
  , receiver: Just <<< H.action <<< Update
  , initializer: Just (H.action Initialize)
  , finalizer: Nothing
  , render
  , eval
  }

render :: State -> H.ComponentHTML Query
render st = case st.input.reportCol ^. reportColCompileStatus of
  StatusNone -> clspan_ "error"
    [ HH.text "No template saved" ]
  StatusError _ -> clspan_ "error"
    [ HH.text "Error" ]
  StatusOk -> case st.getReportPath of
    Nothing -> HH.text ""
    Just mkPath -> case st.input.reportCol ^. reportColFormat of
      ReportFormatPlain ->
        HH.a
        [ HP.href (mkPath "getReportPlain")
        , HP.target "_blank"
        ]
        [ faIcon_ "file-text-o fa-lg" ]
      ReportFormatPDF ->
        HH.a
        [ HP.href (mkPath "getReportPDF")
        , HP.target "_blank"
        ]
        [ faIcon_ "file-pdf-o fa-lg" ]
      ReportFormatHTML ->
        HH.a
        [ HP.href (mkPath "getReportHTML")
        , HP.target "_blank"
        ]
        [ faIcon_ "file-code-o fa-lg" ]

eval :: Query ~> H.ComponentDSL State Query Void Herc
eval = case _ of

  Initialize next -> do
    input <- H.gets _.input
    authToken <- fromMaybe "" <$> getAuthToken
    apiUrl <- getApiUrl
    case input.coords of
      Coords (Id colId) (Id rowId) -> modify _
        { getReportPath = Just \which ->
            apiUrl <> "cell/" <> which <>
            "/" <> colId <>
            "/" <> rowId <>
            "?sessionKey=" <> authToken
        }
    pure next

  Update input next -> do
    modify _{ input = input }
    pure next
