{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Lib.Model.Column where

import           Control.DeepSeq

import           Control.Lens.Lens
import           Control.Lens.Prism
import           Data.Aeson         (FromJSON (..), ToJSON (..))
import           Data.Aeson.Bson
import           Data.Bson          (Val, (=:))
import qualified Data.Bson          as Bson
import           Data.Text          (Text, pack)

import           GHC.Generics

import           Lib.Compiler.Types
import           Lib.Model.Class
import           Lib.Model.Table
import           Lib.Template.Types
import           Lib.Types

data DataType
  = DataBool
  | DataString
  | DataNumber
  | DataTime
  | DataRecord (Id Table)
  | DataList DataType
  | DataMaybe DataType
  deriving (Eq, Ord, Show, Read, Generic, NFData)

instance ToJSON DataType
instance FromJSON DataType

instance ToBSON DataType
instance FromBSON DataType

getReference :: DataType -> Maybe (Id Table)
getReference = \case
  DataBool      -> Nothing
  DataString    -> Nothing
  DataNumber    -> Nothing
  DataTime      -> Nothing
  DataRecord t  -> Just t
  DataList sub  -> getReference sub
  DataMaybe sub -> getReference sub

data Column = Column
  { _columnTableId :: Id Table
  , _columnName    :: Text
  , _columnKind    :: ColumnKind
  } deriving (Eq, Generic, NFData, Show)

columnTableId :: Lens' Column (Id Table)
columnTableId = lens _columnTableId (\c i -> c { _columnTableId = i})

columnName :: Lens' Column Text
columnName = lens _columnName (\c n -> c { _columnName = n})

columnKind :: Lens' Column ColumnKind
columnKind = lens _columnKind (\c k -> c { _columnKind = k})

instance Model Column where
  collectionName = const "columns"

instance ToJSON Column
instance FromJSON Column

instance ToDocument Column where
  toDocument (Column t name kind) =
    [ "tableId" =: toObjectId t
    , "name" =: name
    , "kind" =: toValue kind
    ]

instance FromDocument Column where
  parseDocument doc = do
    t <- Bson.lookup "tableId" doc
    name <- Bson.lookup "name" doc
    kindVal <- Bson.lookup "kind" doc
    case eitherDecodeValue kindVal of
      Right kind ->  pure $ Column (fromObjectId t) name kind
      Left msg -> Left $ pack msg

data ColumnKind
  = ColumnReport ReportCol
  | ColumnData DataCol
  deriving (Eq, Show, NFData, Generic)
  -- Future: | ColumnAction Action

_ColumnReport :: Prism' ColumnKind ReportCol
_ColumnReport = prism' ColumnReport $ \case
  ColumnReport rep -> Just rep
  ColumnData   _   -> Nothing

_ColumnData :: Prism' ColumnKind DataCol
_ColumnData = prism' ColumnData $ \case
  ColumnReport _   -> Nothing
  ColumnData   dat -> Just dat

instance ToJSON ColumnKind
instance FromJSON ColumnKind

instance ToBSON ColumnKind
instance FromBSON ColumnKind

instance Val ColumnKind where
  val = toValue
  cast' = decodeValue

data ReportCol = ReportCol
  { _reportColTemplate         :: Text
  , _reportColCompiledTemplate :: ReportCompileResult
  , _reportColLanguage         :: Maybe ReportLanguage
  , _reportColFormat           :: ReportFormat
  } deriving (Eq, Generic, NFData, Show)

reportColTemplate :: Lens' ReportCol Text
reportColTemplate = lens _reportColTemplate (\r t -> r { _reportColTemplate = t})

reportColCompiledTemplate :: Lens' ReportCol (CompileResult TTemplate)
reportColCompiledTemplate = lens _reportColCompiledTemplate (\r ct -> r { _reportColCompiledTemplate = ct})

reportColLanguage :: Lens' ReportCol (Maybe ReportLanguage)
reportColLanguage = lens _reportColLanguage (\r l -> r { _reportColLanguage = l})

reportColFormat :: Lens' ReportCol ReportFormat
reportColFormat = lens _reportColFormat (\r f -> r { _reportColFormat = f})

instance ToJSON ReportCol
instance FromJSON ReportCol

data ReportLanguage
  = ReportLanguageMarkdown
  | ReportLanguageLatex
  | ReportLanguageHTML
  deriving (Eq, Ord, Generic, NFData, Read, Show)

instance ToJSON ReportLanguage
instance FromJSON ReportLanguage

data ReportFormat
  = ReportFormatPlain
  | ReportFormatPDF
  | ReportFormatHTML
  deriving (Eq, Ord, Generic, NFData, Read, Show)

instance ToJSON ReportFormat
instance FromJSON ReportFormat

data DataCol = DataCol
  { _dataColType          :: DataType
  , _dataColIsDerived     :: IsDerived
  , _dataColSourceCode    :: Text
  , _dataColCompileResult :: DataCompileResult
  } deriving (Eq, Generic, NFData, Show)

dataColType :: Lens' DataCol DataType
dataColType = lens _dataColType (\d dt -> d { _dataColType = dt })

dataColIsDerived :: Lens' DataCol IsDerived
dataColIsDerived = lens _dataColIsDerived (\d iD -> d { _dataColIsDerived = iD })

dataColSourceCode :: Lens' DataCol Text
dataColSourceCode = lens _dataColSourceCode (\d s -> d { _dataColSourceCode = s })

dataColCompileResult :: Lens' DataCol (CompileResult TExpr)
dataColCompileResult = lens _dataColCompileResult (\d cr -> d { _dataColCompileResult = cr })

instance ToJSON DataCol
instance FromJSON DataCol

data CompileResult a
  = CompileResultOk a
  | CompileResultNone
  | CompileResultError Text
  deriving (Eq, Show, Generic, NFData)

type DataCompileResult = CompileResult TExpr
type ReportCompileResult = CompileResult TTemplate

instance ToJSON a => ToJSON (CompileResult a)
instance FromJSON a => FromJSON (CompileResult a)

instance ToBSON a => ToBSON (CompileResult a)
instance FromBSON a => FromBSON (CompileResult a)

data IsDerived
  = Derived
  | NotDerived
  deriving (Eq, Ord, Show, Read, Generic, NFData)

instance ToJSON IsDerived
instance FromJSON IsDerived

instance ToBSON IsDerived
instance FromBSON IsDerived

instance Val IsDerived where
  val = toValue
  cast' = decodeValue
