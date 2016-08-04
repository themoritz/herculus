{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Lib.Model.Column where

import           Data.Aeson                   (FromJSON (..), ToJSON (..),
                                               Value (..), object, (.:), (.=))
import           Data.Aeson.Bson
import           Data.Bson                    (Val, (=:))
import qualified Data.Bson                    as Bson
import qualified Data.ByteString.Base64       as Base64
import           Data.Monoid                  ((<>))
import           Data.Serialize
import qualified Data.ByteString as BS
import           Data.Text                    (Text, pack)
import           Data.Text.Encoding

import           GHC.Generics

import           Lib.Model.Class
import           Lib.Model.Dependencies.Types
import           Lib.Model.Types
import           Lib.Types

data Column = Column
  { columnTableId      :: Id Table
  , columnName         :: Text
  , columnDataType     :: DataType
  , columnInputType    :: InputType
  , columnSourceCode   :: Text
  , columnCompiledCode :: CompiledCode
  } deriving (Generic)

instance Model Column where
  collectionName = const "columns"

instance ToJSON Column
instance FromJSON Column

instance ToDocument Column where
  toDocument (Column t name typ inputType source compiled) =
    [ "tableId" =: toObjectId t
    , "name" =: name
    , "type" =: toValue typ
    , "inputType" =: toValue inputType
    , "sourceCode" =: source
    , "compiledCode" =: compiled
    ]

instance FromDocument Column where
  parseDocument doc = do
    t <- Bson.lookup "tableId" doc
    name <- Bson.lookup "name" doc
    typVal <- Bson.lookup "type" doc
    inpTypeVal <- Bson.lookup "inputType" doc
    source <- Bson.lookup "sourceCode" doc
    compiled <- Bson.lookup "compiledCode" doc
    case eitherDecodeValue typVal of
      Right typ -> case eitherDecodeValue inpTypeVal of
        Right inpTyp -> pure $ Column (fromObjectId t) name typ inpTyp source compiled
        Left msg -> Left $ pack msg
      Left msg -> Left $ pack msg

data CompiledCode
  = CompiledCode ATExpr
  | CompiledCodeNone
  | CompiledCodeError Text
  deriving (Show)

instance ToJSON CompiledCode where
  toJSON (CompiledCode atexpr) = object
    [ "tag" .= ("CompiledCode" :: String)
    , "content" .= BS.unpack (Base64.encode $ encode atexpr)
    ]
  toJSON CompiledCodeNone = object
    [ "tag" .= ("CompiledCodeNone" :: String)
    ]
  toJSON (CompiledCodeError e) = object
    [ "tag" .= ("CompiledCodeError" :: String)
    , "content" .= e
    ]

instance FromJSON CompiledCode where
  parseJSON (Object v) = do
    (tag :: String) <- v .: "tag"
    case tag of
      "CompiledCode" -> do
        text <- v .: "content"
        case Base64.decode (BS.pack text) >>= decode of
          Left msg -> fail $ "could not decode atexpr" <> msg
          Right atexpr -> pure $ CompiledCode atexpr
      "CompiledCodeNone" -> pure $ CompiledCodeNone
      "CompiledCodeError" -> CompiledCodeError <$> v .: "content"
      _ -> fail "unknown tag"

instance Eq CompiledCode where
  CompiledCode a == CompiledCode b = encode a == encode b
  CompiledCodeNone == CompiledCodeNone = True
  CompiledCodeError a == CompiledCodeError b = a == b
  _ == _ = False

instance Val CompiledCode where
  val (CompiledCode atexpr) = Bson.Doc
    [ "tag" =: ("CompiledCode" :: Text)
    , "content" =: Bson.Bin (Bson.Binary $ encode atexpr)
    ]
  val CompiledCodeNone = Bson.Doc
    [ "tag" =: ("CompiledCodeNone" :: Text) ]
  val (CompiledCodeError msg) = Bson.Doc
    [ "tag" =: ("CompiledCodeError" :: Text)
    , "content" =: msg
    ]
  cast' (Bson.Doc doc) = do
    (tag :: String) <- Bson.lookup "tag" doc
    case tag of
      "CompiledCode" -> do
        (Bson.Bin (Bson.Binary bs)) <- Bson.lookup "content" doc
        case decode bs of
          Left msg -> fail $ "could not decode atexpr" <> msg
          Right atexpr -> pure $ CompiledCode atexpr
      "CompiledCodeError" -> CompiledCodeError <$> Bson.lookup "content" doc
      "CompiledCodeNone" -> pure $ CompiledCodeNone

data InputType
  = ColumnInput
  | ColumnDerived
  deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON InputType
instance FromJSON InputType

instance ToBSON InputType
instance FromBSON InputType

instance Val InputType where
  val = toValue
  cast' = decodeValue

--

data TExpr a where
  TExprLitString :: Text -> TExpr Text
  TExprLitNumber :: Number -> TExpr Number

  TExprColumnRefString :: Id Column -> TExpr Text
  TExprColumnRefNumber :: Id Column -> TExpr Number

  TExprColumnRefStrings :: Id Column -> TExpr [Text]
  TExprColumnRefNumbers :: Id Column -> TExpr [Number]

  TExprStringAppend :: TExpr Text -> TExpr Text -> TExpr Text
  TExprNumberAdd :: TExpr Number -> TExpr Number -> TExpr Number

  TExprSum :: TExpr [Number] -> TExpr Number

deriving instance Show (TExpr a)

data ATExpr = forall a. MakeValue a => TExpr a ::: TType a

deriving instance Show ATExpr

instance Serialize ATExpr where
  put (texpr ::: _) = put' texpr
    where
      put' :: Putter (TExpr a)
      put' e' = case e' of
        TExprLitString t        -> putWord8 0 *> put (Utf8Text t)
        TExprLitNumber n        -> putWord8 1 *> put n
        TExprColumnRefString c  -> putWord8 2 *> put c
        TExprColumnRefNumber c  -> putWord8 3 *> put c
        TExprColumnRefStrings c -> putWord8 4 *> put c
        TExprColumnRefNumbers c -> putWord8 5 *> put c
        TExprStringAppend l r   -> putWord8 6 *> put' l *> put' r
        TExprNumberAdd l r      -> putWord8 7 *> put' l *> put' r
        TExprSum s              -> putWord8 8 *> put' s

  get = getWord8 >>= \case
    0 -> do
      t <- get
      pure $ TExprLitString (unUtf8Text t) ::: TypeString
    1 -> do
      n <- get
      pure $ TExprLitNumber n ::: TypeNumber
    2 -> do
      c <- get
      pure $ TExprColumnRefString c ::: TypeString
    3 -> do
      c <- get
      pure $ TExprColumnRefNumber c ::: TypeNumber
    4 -> do
      c <- get
      pure $ TExprColumnRefStrings c ::: TypeStringList
    5 -> do
      c <- get
      pure $ TExprColumnRefNumbers c ::: TypeNumberList
    6 -> do
      (l ::: TypeString) <- get; (r ::: TypeString) <- get
      pure $ TExprStringAppend l r ::: TypeString
    7 -> do
      (l ::: TypeNumber) <- get; (r ::: TypeNumber) <- get
      pure $ TExprNumberAdd l r ::: TypeNumber
    8 -> do
      (s ::: TypeNumberList) <- get
      pure $ TExprSum s ::: TypeNumber

collectDependencies :: ATExpr -> [(Id Column, DependencyType)]
collectDependencies (texpr ::: _) = collectDependencies' texpr
  where
    collectDependencies' :: TExpr a -> [(Id Column, DependencyType)]
    collectDependencies' expr = case expr of
      TExprLitString _ -> []
      TExprLitNumber _ -> []
      TExprColumnRefString c -> [(c, OneToOne)]
      TExprColumnRefNumber c -> [(c, OneToOne)]
      TExprColumnRefStrings c -> [(c, OneToAll)]
      TExprColumnRefNumbers c -> [(c, OneToAll)]
      TExprStringAppend l r -> collectDependencies' l <> collectDependencies' r
      TExprNumberAdd l r -> collectDependencies' l <> collectDependencies' r
      TExprSum sub -> collectDependencies' sub
