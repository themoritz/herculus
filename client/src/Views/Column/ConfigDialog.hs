{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
-- |

module Views.Column.ConfigDialog where

import           Control.DeepSeq           (NFData)
import           Control.Lens
import           Data.Map                  (Map)
import qualified Data.Map                  as Map
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           GHC.Generics              (Generic)
import           React.Flux                (ReactStore, SomeStoreAction (..),
                                            StoreData (..), mkStore)
import           React.Flux.Addons.Servant (HandleResponse)

import qualified Action                    as MainAction
import           Lib.Model.Column          (Column, DataType, IsDerived,
                                            ReportFormat, ReportLanguage)
import           Lib.Types                 (Id)
import qualified Store                     as MainStore
import qualified Store.Message             as Message

data State = State
  { _stDialogs    :: Map (Id Column) DialogState
  }

data DialogState = DialogState
  { _stTmpDataType       :: Maybe DataType
  , _stTmpIsFormula      :: Maybe IsDerived
  , _stTmpFormula        :: Maybe Text
  , _stVisible           :: Bool
  , _stTmpReportLanguage :: Maybe (Maybe ReportLanguage)
  , _stTmpReportFormat   :: Maybe ReportFormat
  , _stTmpReportTemplate :: Maybe Text
  } deriving (Eq)

makeLenses ''State
makeLenses ''DialogState

mkDialog :: DialogState
mkDialog = DialogState
  { _stTmpDataType       = Nothing
  , _stTmpIsFormula      = Nothing
  , _stTmpFormula        = Nothing
  , _stVisible           = False
  , _stTmpReportLanguage = Nothing
  , _stTmpReportFormat   = Nothing
  , _stTmpReportTemplate = Nothing
  }

-- helper

-- helper lens
atDialog :: Applicative f
         => Id Column
         -> (DialogState -> f DialogState)
         -> State -> f State
atDialog i = stDialogs . at i . non mkDialog

-- | column config dialog action, concerns local column config store
mkAction :: DialogAction -> SomeStoreAction
mkAction = SomeStoreAction store . Action

dispatch :: DialogAction -> [SomeStoreAction]
dispatch a = [mkAction a]

mkCallback :: (a -> [DialogAction]) -> HandleResponse a
mkCallback cbSuccess = pure . \case
  Left (401, e) -> MainStore.dispatch $ MainAction.MessageAction $ Message.SetWarning $
                    "Unauthorized. Are you logged in?" <>
                    " (401) " <> Text.pack e
  Left (n, e)   -> MainStore.dispatch $ MainAction.MessageAction $ Message.SetError $
                    " (" <> (Text.pack . show) n <> ") " <> Text.pack e
  Right x       -> mkAction <$> cbSuccess x

store :: ReactStore State
store = mkStore State
  { _stDialogs    = Map.empty
  }

data Action = Action DialogAction
  deriving (NFData, Generic)

data DialogAction
  = SetTmpDataType         (Id Column) DataType
  | UnsetTmpDataType       (Id Column)
  | SetTmpIsFormula        (Id Column) IsDerived
  | UnsetTmpIsFormula      (Id Column)
  | SetTmpFormula          (Id Column) Text
  | UnsetTmpFormula        (Id Column)
  | SetVisibility          (Id Column) Bool
  | SetTmpReportLang       (Id Column) (Maybe ReportLanguage)
  | UnsetTmpReportLang     (Id Column)
  | SetTmpReportFormat     (Id Column) ReportFormat
  | UnsetTmpReportFormat   (Id Column)
  | SetTmpReportTemplate   (Id Column) Text
  | UnsetTmpReportTemplate (Id Column)
  deriving (NFData, Generic)

instance StoreData State where
  type StoreAction State = Action
  transform (Action action) st =
    case action of
      SetTmpDataType i dt ->
        pure $ st & atDialog i . stTmpDataType .~ Just dt
      UnsetTmpDataType i ->
        pure $ st & atDialog i . stTmpDataType .~ Nothing
      SetTmpIsFormula i it ->
        pure $ st & atDialog i . stTmpIsFormula .~ Just it
      UnsetTmpIsFormula i ->
        pure $ st & atDialog i . stTmpIsFormula .~ Nothing
      SetTmpFormula i s ->
        pure $ st & atDialog i . stTmpFormula .~ Just s
      UnsetTmpFormula i ->
        pure $ st & atDialog i . stTmpFormula .~ Nothing
      SetVisibility i b ->
        pure $ st & atDialog i . stVisible .~ b
      SetTmpReportLang i lang ->
        pure $ st & atDialog i . stTmpReportLanguage .~ Just lang
      UnsetTmpReportLang i ->
        pure $ st & atDialog i . stTmpReportLanguage .~ Nothing
      SetTmpReportFormat i format ->
        pure $ st & atDialog i . stTmpReportFormat .~ Just format
      UnsetTmpReportFormat i ->
        pure $ st & atDialog i . stTmpReportFormat .~ Nothing
      SetTmpReportTemplate i templ ->
        pure $ st & atDialog i . stTmpReportTemplate .~ Just templ
      UnsetTmpReportTemplate i ->
        pure $ st & atDialog i . stTmpReportTemplate .~ Nothing
