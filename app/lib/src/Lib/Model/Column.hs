{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

module Lib.Model.Column where

import           Lib.Prelude

import           Control.Lens                 (makeLenses)

import           Control.Lens
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Aeson.Bson
import           Data.Bson                    (Val, (=:))
import qualified Data.Bson                    as Bson
import           Data.Text                    (Text, pack)

import           Lib.Compiler.Core
import           Lib.Compiler.Error
import           Lib.Model.Class
import           Lib.Model.Dependencies.Types
import           Lib.Model.Table
import           Lib.Template.Core
import           Lib.Types

data DataType
  = DataAlgebraic Text [DataType]
  | DataTable (Id Table)
  | DataRecord [(Text, DataType)]
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

getTypeDependencies :: DataType -> TypeDependencies
getTypeDependencies = \case
  DataAlgebraic _ args ->
    mconcat $ map getTypeDependencies args
  DataTable t  ->
    singleRowRef t
  DataRecord fields ->
    mconcat $ map (getTypeDependencies . snd) fields

--------------------------------------------------------------------------------

data CompileResult a
  = CompileResultOk a
  | CompileResultNone
  | CompileResultError [Error]
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

type DataCompileResult = CompileResult Expr
type ReportCompileResult = CompileResult [TplChunk]

data IsDerived
  = Derived
  | NotDerived
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON)

data ReportLanguage
  = ReportLanguageMarkdown
  | ReportLanguageLatex
  | ReportLanguageHTML
  deriving (Eq, Ord, Generic, ToJSON, FromJSON, Read, Show)

data ReportFormat
  = ReportFormatPlain
  | ReportFormatPDF
  | ReportFormatHTML
  deriving (Eq, Ord, Generic, ToJSON, FromJSON, Read, Show)

--------------------------------------------------------------------------------

data ReportCol = ReportCol
  { _reportColTemplate         :: Text
  , _reportColCompiledTemplate :: ReportCompileResult
  , _reportColLanguage         :: Maybe ReportLanguage
  , _reportColFormat           :: ReportFormat
  } deriving (Eq, Generic, ToJSON, FromJSON, Show)

makeLenses ''ReportCol

--------------------------------------------------------------------------------

data DataCol = DataCol
  { _dataColType          :: DataType
  , _dataColIsDerived     :: IsDerived
  , _dataColSourceCode    :: Text
  , _dataColCompileResult :: DataCompileResult
  } deriving (Eq, Generic, ToJSON, FromJSON, Show)

makeLenses ''DataCol

--------------------------------------------------------------------------------

data ColumnKind
  = ColumnReport ReportCol
  | ColumnData DataCol
  deriving (Eq, Show, ToJSON, FromJSON, Generic)

makePrisms ''ColumnKind

instance ToBSON ColumnKind
instance FromBSON ColumnKind

instance Val ColumnKind where
  val = toValue
  cast' = decodeValue

--------------------------------------------------------------------------------

data Column = Column
  { _columnTableId :: Id Table
  , _columnName    :: Text
  , _columnKind    :: ColumnKind
  } deriving (Eq, Generic, ToJSON, FromJSON, Show)

makeLenses ''Column

instance Model Column where
  collectionName = const "columns"

instance ToDocument Column where
  toDocument (Column t name kind) =
    [ "tableId" =: toObjectId t
    , "name"    =: name
    , "kind"    =: toValue kind
    ]

instance FromDocument Column where
  parseDocument doc = do
    t       <- Bson.lookup "tableId" doc
    name    <- Bson.lookup "name" doc
    kindVal <- Bson.lookup "kind" doc
    case eitherDecodeValue kindVal of
      Right kind ->  pure $ Column (fromObjectId t) name kind
      Left msg   -> Left $ pack msg

-- Util ------------------------------------------------------------------------

emptyDataColType :: DataType
emptyDataColType = DataAlgebraic "Number" []

emptyReportCol :: Id Table -> Column
emptyReportCol i = Column
  { _columnTableId = i
  , _columnName = ""
  , _columnKind = ColumnReport ReportCol
    { _reportColTemplate         = ""
    , _reportColCompiledTemplate = CompileResultNone
    , _reportColLanguage         = Just ReportLanguageMarkdown
    , _reportColFormat           = ReportFormatPDF
    }
  }

emptyDataCol :: Id Table -> Column
emptyDataCol i = Column
  { _columnTableId = i
  , _columnName    = ""
  , _columnKind    = ColumnData DataCol
    { _dataColType          = emptyDataColType
    , _dataColIsDerived     = NotDerived
    , _dataColSourceCode    = ""
    , _dataColCompileResult = CompileResultNone
    }
  }

getColumnError :: Column -> Maybe [Error]
getColumnError col = case col ^. columnKind of
  ColumnData dat ->
    case (dat ^. dataColIsDerived, dat ^. dataColCompileResult) of
      (Derived, CompileResultError msg) -> Just msg
      _                                 -> Nothing
  ColumnReport rep ->
    case rep ^. reportColCompiledTemplate of
      CompileResultError msg -> Just msg
      _                      -> Nothing
