module Views.ReportCell
  ( ReportCellProps (..)
  , reportCell_
  ) where

import           Control.Lens        hiding (view)

import           Data.Proxy
import           Data.Text           (Text, pack)

import           Servant.Utils.Links

import           React.Flux

import           Lib.Api.Rest
import           Lib.Model.Column
import           Lib.Model.Record
import           Lib.Types

import           Views.Combinators

data ReportCellProps = ReportCellProps
  { reportCellColId     :: !(Id Column)
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
        a_ [ "href" &= getPlain reportCellColId reportCellRecId
           , "target" $= "_blank"
           ] $ faIcon_ "file-text-o fa-lg"
      ReportFormatPDF ->
        a_ [ "href" &= getPDF reportCellColId reportCellRecId
           , "target" $= "_blank"
           ] $ faIcon_ "file-pdf-o fa-lg"
      ReportFormatHTML ->
        a_ [ "href" &= getHTML reportCellColId reportCellRecId
           , "target" $= "_blank"
           ] $ faIcon_ "file-code-o fa-lg"

api :: Proxy Routes
api = Proxy

getPlain :: Id Column -> Id Record -> Text
getPlain c r = toPath $ safeLink api (Proxy :: Proxy CellGetReportPlain) c r

getPDF :: Id Column -> Id Record -> Text
getPDF c r = toPath $ safeLink api (Proxy :: Proxy CellGetReportPDF) c r

getHTML :: Id Column -> Id Record -> Text
getHTML c r = toPath $ safeLink api (Proxy :: Proxy CellGetReportHTML) c r

toPath :: URI -> Text
toPath = pack . ('/':) . uriPath
