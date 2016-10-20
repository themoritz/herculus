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

import           Store               (dispatch, session, stateSessionKey, store)

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
getPlain sKey columnId recordId = case sKey of
  Just sKey -> pack $ getReportPath "getReportPlain" sKey columnId recordId
  Nothing   -> pack "#"

getPDF :: Maybe SessionKey -> Id Column -> Id Record -> Text
getPDF sKey columnId recordId = case sKey of
  Just sKey -> pack $ getReportPath "getReportPDF" sKey columnId recordId
  Nothing   -> pack "#"

getHTML :: Maybe SessionKey -> Id Column -> Id Record -> Text
getHTML sKey columnId recordId = case sKey of
  Just sKey -> pack $ getReportPath "getReportHTML" sKey columnId recordId
  Nothing   -> pack "#"

getReportPath :: String -> SessionKey -> Id Column -> Id Record -> String
getReportPath folder sKey columnId recordId =
  "/cell/" ++ folder ++
  "?sessionKey=" ++ show sKey ++
  "&columnId=" ++ show columnId ++
  "&recordId=" ++ show recordId
