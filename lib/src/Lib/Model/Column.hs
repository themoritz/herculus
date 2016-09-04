{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib.Model.Column where

import           Control.DeepSeq

import           Control.Lens.Prism
import           Control.Lens (makeLenses)
import           Data.Aeson                   (FromJSON (..), ToJSON (..))
import           Data.Aeson.Bson
import           Data.Bson                    (Val, (=:))
import qualified Data.Bson                    as Bson
import           Data.Monoid                  ((<>))
import           Data.Text                    (Text, pack)

import           GHC.Generics

import           Lib.Compiler.Types
import           Lib.Model.Class
import           Lib.Model.Dependencies.Types
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
  } deriving (Eq, Generic, NFData)

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
  { reportColTemplate         :: Text
  , reportColCompiledTemplate :: CompileResult TTemplate
  , reportColLanguage         :: ReportLanguage
  , reportColFormat           :: ReportFormat
  } deriving (Eq, Generic, NFData, Show)

instance ToJSON ReportCol
instance FromJSON ReportCol

data ReportLanguage
  = ReportLanguagePlain
  | ReportLanguageMarkdown
  | ReportLanguageLatex
  | ReportLanguageHTML
  deriving (Eq, Generic, NFData, Show)

instance ToJSON ReportLanguage
instance FromJSON ReportLanguage

data ReportFormat
  = ReportFormatPlain
  | ReportFormatPDF
  | ReportFormatHTML
  | ReportFormatMarkdown
  deriving (Eq, Generic, NFData, Show)

instance ToJSON ReportFormat
instance FromJSON ReportFormat

data DataCol = DataCol
  { dataColType          :: DataType
  , dataColIsDerived     :: IsDerived
  , dataColSourceCode    :: Text
  , dataColCompileResult :: CompileResult TExpr
  } deriving (Eq, Generic, NFData, Show)

instance ToJSON DataCol
instance FromJSON DataCol

data CompileResult a
  = CompileResultOk a
  | CompileResultNone
  | CompileResultError Text
  deriving (Eq, Show, Generic, NFData)

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

--

collectDependencies :: TExpr -> [(Id Column, DependencyType)]
collectDependencies = go
  where go e' = case e' of
          TLam _ body       -> go body
          TApp f e          -> go f <> go e
          TLet _ e body     -> go e <> go body
          TIf c t e         -> go c <> go t <> go e
          TVar _            -> []
          TLit _            -> []
          TBinop _ l r      -> go l <> go r
          TPrjRecord e _    -> go e
          TColumnRef c      -> [(c, OneToOne)]
          TWholeColumnRef c -> [(c, OneToAll)]
          TTableRef _ cs    -> map (,OneToAll) cs

makeLenses ''Column
