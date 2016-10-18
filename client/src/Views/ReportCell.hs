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
import           Lib.Model.Auth      (SessionKey)
import           Lib.Model.Column
import           Lib.Model.Record
import           Lib.Types

import           Store               (session, stateSessionKey, store)

import           Views.Combinators

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

api :: Proxy Routes
api = Proxy

getPlain :: Maybe SessionKey -> Id Column -> Id Record -> Text
getPlain sKey c r = toPath $ safeLink api (Proxy :: Proxy CellGetReportPlain) (session sKey) c r

getPDF :: Maybe SessionKey -> Id Column -> Id Record -> Text
getPDF sKey c r = toPath $ safeLink api (Proxy :: Proxy CellGetReportPDF) (session sKey) c r

getHTML :: Maybe SessionKey -> Id Column -> Id Record -> Text
getHTML sKey c r = toPath $ safeLink api (Proxy :: Proxy CellGetReportHTML) (session sKey) c r

toPath :: URI -> Text
toPath = pack . ('/':) . uriPath
