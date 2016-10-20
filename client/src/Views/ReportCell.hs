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

import           Lib.Model.Auth    (SessionKey)
import           Lib.Model.Column
import           Lib.Model.Record
import           Lib.Types

import           Views.Combinators (clspan_)

data ReportCellProps = ReportCellProps
  { sKey                :: !(Maybe SessionKey)
  , reportCellColId     :: !(Id Column)
  , reportCellRecId     :: !(Id Record)
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
        a_ [ "href" &= getPlain sKey reportCellColId reportCellRecId
          , "target" $= "_blank"
          ] $ faIcon_ "file-text-o fa-lg"
      ReportFormatPDF ->
        a_ [ "href" &= getPDF sKey reportCellColId reportCellRecId
          , "target" $= "_blank"
          ] $ faIcon_ "file-pdf-o fa-lg"
      ReportFormatHTML ->
        a_ [ "href" &= getHTML sKey reportCellColId reportCellRecId
          , "target" $= "_blank"
          ] $ faIcon_ "file-code-o fa-lg"

getPlain :: Maybe SessionKey -> Id Column -> Id Record -> Text
getPlain = getReportPath "getReportPlain"

getPDF :: Maybe SessionKey -> Id Column -> Id Record -> Text
getPDF = getReportPath "getReportPDF"

getHTML :: Maybe SessionKey -> Id Column -> Id Record -> Text
getHTML = getReportPath "getReportHTML"

getReportPath :: Text -> Maybe SessionKey -> Id Column -> Id Record -> Text
getReportPath folder sKey columnId recordId =
  "/cell/" <> folder <>
  "?sessionKey=" <> toUrlPiece sKey <>
  "&columnId=" <> (Text.pack . show) columnId <>
  "&recordId=" <> (Text.pack . show) recordId
