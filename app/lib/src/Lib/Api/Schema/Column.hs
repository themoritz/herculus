{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
-- |

module Lib.Api.Schema.Column where

import           Lib.Prelude

import           Control.Lens       (makeLenses, makePrisms)

import           Data.Aeson         (FromJSON, ToJSON)
import           Data.Text          (Text)

import           Lib.Compiler.Error
import           Lib.Model
import qualified Lib.Model.Column   as M
import qualified Lib.Model.Common   as M
import qualified Lib.Model.Table    as M
import           Lib.Types

data CompileStatus
  = StatusOk
  | StatusNone
  | StatusError [Error]
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data ReportCol = ReportCol
  { _reportColTemplate      :: Text
  , _reportColCompileStatus :: CompileStatus
  , _reportColLanguage      :: Maybe M.ReportLanguage
  , _reportColFormat        :: M.ReportFormat
  } deriving (Eq, Generic, ToJSON, FromJSON, Show)

makeLenses ''ReportCol

--------------------------------------------------------------------------------

data DataCol = DataCol
  { _dataColType          :: M.DataType
  , _dataColIsDerived     :: M.IsDerived
  , _dataColSourceCode    :: Text
  , _dataColCompileStatus :: CompileStatus
  } deriving (Eq, Generic, ToJSON, FromJSON, Show)

makeLenses ''DataCol

--------------------------------------------------------------------------------

data ColumnKind
  = ColumnReport ReportCol
  | ColumnData DataCol
  deriving (Eq, Show, ToJSON, FromJSON, Generic)

makePrisms ''ColumnKind

--------------------------------------------------------------------------------

data Column = Column
  { _columnId      :: Id M.Column
  , _columnTableId :: Id M.Table
  , _columnName    :: Text
  , _columnKind    :: ColumnKind
  } deriving (Generic, ToJSON, FromJSON, Show)

makeLenses ''Column

columnFromEntity :: Entity M.Column -> Column
columnFromEntity (Entity i (M.Column t n k)) = Column i t n (goKind k)
  where
    goKind (M.ColumnReport r) = ColumnReport (goReport r)
    goKind (M.ColumnData d)   = ColumnData (goData d)

    goReport (M.ReportCol t' c l f) = ReportCol t' (goStatus c) l f

    goData (M.DataCol t' d s c) = DataCol t' d s (goStatus c)

    goStatus (M.CompileResultOk _)    = StatusOk
    goStatus (M.CompileResultNone)    = StatusNone
    goStatus (M.CompileResultError e) = StatusError e

