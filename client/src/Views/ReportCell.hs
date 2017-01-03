module Views.ReportCell
  ( ReportCellProps (..)
  , reportCell_
  ) where

import           Control.Lens      hiding (view)

import           Data.Monoid       ((<>))
import           Data.Text         (Text)
import qualified Data.Text         as Text
import           React.Flux        (ReactElementM, ReactView, a_, cldiv_,
                                    defineView, faIcon_, view, ($=), (&=))
import           Web.HttpApiData   (toUrlPiece)

import           Lib.Api.Rest      (sessionParamStr)
import           Lib.Model.Auth    (SessionKey)
import           Lib.Model.Column
import           Lib.Model.Row
import           Lib.Types

import           Views.Combinators (clspan_)

data ReportCellProps = ReportCellProps
  { reportCellSKey      :: !SessionKey
  , reportCellColId     :: !(Id Column)
  , reportCellRecId     :: !(Id Row)
  , reportCellColReport :: !ReportCol
  }

reportCell_ :: ReportCellProps -> ReactElementM eh ()
reportCell_ !p = view reportCell p mempty

reportCell :: ReactView ReportCellProps
reportCell = defineView "reportCell" $ \ReportCellProps{..} -> cldiv_ "reportCell" $
  case reportCellColReport ^. reportColCompiledTemplate of
    CompileResultNone -> clspan_ "error" "No template saved"
    CompileResultError _ -> clspan_ "error" "Error"
    CompileResultOk _ -> case reportCellColReport ^. reportColFormat of
      ReportFormatPlain ->
        a_ [ "href" &= getPlain reportCellSKey reportCellColId reportCellRecId
          , "target" $= "_blank"
          ] $ faIcon_ "file-text-o fa-lg"
      ReportFormatPDF ->
        a_ [ "href" &= getPDF reportCellSKey reportCellColId reportCellRecId
          , "target" $= "_blank"
          ] $ faIcon_ "file-pdf-o fa-lg"
      ReportFormatHTML ->
        a_ [ "href" &= getHTML reportCellSKey reportCellColId reportCellRecId
          , "target" $= "_blank"
          ] $ faIcon_ "file-code-o fa-lg"

getPlain :: SessionKey -> Id Column -> Id Row -> Text
getPlain = getReportPath "getReportPlain"

getPDF :: SessionKey -> Id Column -> Id Row -> Text
getPDF = getReportPath "getReportPDF"

getHTML :: SessionKey -> Id Column -> Id Row -> Text
getHTML = getReportPath "getReportHTML"

getReportPath :: Text -> SessionKey -> Id Column -> Id Row -> Text
getReportPath directory sKey columnId recordId =
  "/api/cell/" <> directory <>
  "/" <> (Text.pack . show) columnId <>
  "/" <> (Text.pack . show) recordId <>
  "?" <> sessionParamStr <> "=" <> toUrlPiece sKey
