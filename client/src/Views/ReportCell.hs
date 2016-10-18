module Views.ReportCell
  ( ReportCellProps (..)
  , reportCell_
  ) where

import           Control.Lens        hiding (view)

import           Data.Proxy
import           Data.Text           (Text, pack)

import           Servant.Utils.Links (URI, safeLink, uriPath)

import           React.Flux

import           Lib.Api.Rest
import           Lib.Model.Auth      (SessionKey)
import           Lib.Model.Column
import           Lib.Model.Record
import           Lib.Types

import           Store               (Action (GetReportFormatHTML, GetReportFormatPDF, GetReportFormatPlain),
                                      dispatch, session, stateSessionKey, store)

import           Views.Combinators   (clspan_, faButton_)

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
        faButton_ "file-plain-o" $ dispatch $ GetReportFormatPDF reportCellColId reportCellRecId
      ReportFormatPDF ->
        faButton_ "file-pdf-o" $ dispatch $ GetReportFormatPDF reportCellColId reportCellRecId
      ReportFormatHTML ->
        faButton_ "file-text-o" $ dispatch $ GetReportFormatHTML reportCellColId reportCellRecId

api :: Proxy Routes
api = Proxy

toPath :: URI -> Text
toPath = pack . ('/':) . uriPath
