{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
-- |

module Action.Column where

import           Control.DeepSeq  (NFData)
import           Data.Text        (Text)
import           GHC.Generics     (Generic)
import           Lib.Model.Column (DataType, IsDerived, ReportFormat,
                                   ReportLanguage)

data Action
  = SetTmpDataType         DataType
  | UnsetTmpDataType
  | SetTmpIsFormula        IsDerived
  | UnsetTmpIsFormula
  | SetTmpFormula          Text
  | UnsetTmpFormula
  | SetVisibility          Bool
  | SetTmpReportLang       (Maybe ReportLanguage)
  | UnsetTmpReportLang
  | SetTmpReportFormat     ReportFormat
  | UnsetTmpReportFormat
  | SetTmpReportTemplate   Text
  | UnsetTmpReportTemplate

  | Rename Text
  -- Data column
  | DataColUpdate (DataType, IsDerived, Text)
  -- Report column
  | ReportColUpdate (Text, ReportFormat, Maybe ReportLanguage)
  deriving (NFData, Generic)
