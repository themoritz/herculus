{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
-- |

module Views.Column.ConfigDialog where

import           Control.DeepSeq   (NFData)
import           Control.Lens
import           Data.Map          (Map)
import qualified Data.Map          as Map
import           Data.Proxy        (Proxy (..))
import           Data.Text         (Text)
import           GHC.Generics      (Generic)

import qualified Lib.Api.Rest      as Api
import           Lib.Model         (Entity (..))
import           Lib.Model.Column  (Column, DataType, IsDerived, ReportFormat,
                                    ReportLanguage)
import           Lib.Model.Project (Project)
import           Lib.Model.Table   (Table)
import           Lib.Types         (Id)
import           React.Flux        (ReactStore, StoreData (..), mkStore)

type TableCache = Map (Id Table) Text

data State = State
  { _stDialogs    :: Map (Id Column) DialogState
  , _stTableCache :: TableCache
  }

data DialogState = DialogState
  { _stTmpDataType       :: Maybe DataType
  , _stTmpIsFormula      :: Maybe IsDerived
  , _stTmpFormula        :: Maybe Text
  , _stVisible           :: Bool
  , _stTmpReportLanguage :: Maybe (Maybe ReportLanguage)
  , _stTmpReportFormat   :: Maybe ReportFormat
  , _stTmpReportTemplate :: Maybe Text
  }

-- helper lens
-- atDialog :: Applicative f
--          => Id Column
--          -> (Dialog.State -> f Dialog.State)
atDialog i = stDialogs . at i . _Just

store :: ReactStore State
store = mkStore $ State Map.empty

data Action = Action (Id Column) DialogAction

data DialogAction
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
  -- table cache
  | GetTableCache          (Id Project)
  | SetTableCache          TableCache
  deriving (NFData, Generic)

instance StoreData State where
  type StoreAction State = Action
  transform (Action i action) st =
    case action of
      SetTmpDataType dt ->
        pure $ State $ st & atDialog i . stTmpDataType .~ Just dt
      UnsetTmpDataType ->
        pure $ State $ st & atDialog i . stTmpDataType .~ Nothing
      SetTmpIsFormula it ->
        pure $ State $ st & atDialog i . stTmpIsFormula .~ Just it
      UnsetTmpIsFormula ->
        pure $ State $ st & atDialog i . stTmpIsFormula .~ Nothing
      SetTmpFormula s ->
        pure $ State $ st & atDialog i . stTmpFormula .~ Just s
      UnsetTmpFormula ->
        pure $ State $ st & atDialog i . stTmpFormula .~ Nothing
      SetVisibility b ->
        pure $ State $ st & atDialog i . stVisible .~ b
      SetTmpReportLang lang ->
        pure $ State $ st & atDialog i . stTmpReportLanguage .~ Just lang
      UnsetTmpReportLang ->
        pure $ State $ st & atDialog i . stTmpReportLanguage .~ Nothing
      SetTmpReportFormat format ->
        pure $ State $ st & atDialog i . stTmpReportFormat .~ Just format
      UnsetTmpReportFormat ->
        pure $ State $ st & atDialog i . stTmpReportFormat .~ Nothing
      SetTmpReportTemplate templ ->
        pure $ State $ st & atDialog i . stTmpReportTemplate .~ Just templ
      UnsetTmpReportTemplate ->
        pure $ State $ st & atDialog i . stTmpReportTemplate .~ Nothing
      GetTableCache projectId sessionKey -> do
          when (Map.null $ st ^. stTableCache) $
            request api (Proxy :: Proxy Api.TableList)
                    (session sessionKey)
                    projectId $
                    mkCallback $
                    \tables -> [SetTableCache $ toTableMap tables]
          pure st
        where
          toTableMap = Map.fromList . map entityToPair
          entityToPair (Entity tableId table) = (tableId, table ^. tableName)
      SetTableCache m ->
       pure $ st & stTableCache .~ m

makeLenses ''State
makeLenses ''DialogState
