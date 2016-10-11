{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


module Lib.Model.Auth
( PwHash
, LoginData (..)
, LoginResponse (..)
) where

import           Control.DeepSeq      (NFData)
import           Control.Lens         (makeLenses)
import           Data.ByteString      (ByteString)
import           Data.Text            (Text)
import           Lib.Types            (Id, fromObjectId, toObjectId)

import           Data.Aeson           (FromJSON, ToJSON)
import           Data.Bson            ((=:))
import qualified Data.Bson            as Bson

import           GHC.Generics         (Generic)

import qualified Data.Text.Encoding   as Text

import           Lib.Model.Class      (FromDocument (..), Model (..),
                                       ToDocument (..))

import           Crypto.PasswordStore (makePassword)
import           Data.Time.Clock      (UTCTime)



-- Login

mkPwHash :: Text -> IO PwHash
mkPwHash txt =
  PwHash <$> makePassword (Text.encodeUtf8 txt) 17

data LoginData = LoginData { ldUserName :: Text, ldPassword :: Text  } -- OAuth
  deriving (Generic, FromJSON, ToJSON)

type SessionKey = Text

data LoginResponse = LoginSuccess SessionKey | LoginFailed Text
  deriving (Generic, FromJSON, ToJSON)

-- User

data User = User
  { _userName   :: Text
  , _userPwHash :: PwHash
  } deriving (Generic, NFData)


instance Model User where
  collectionName = const "users"


instance ToDocument User where
  toDocument User
   { _userName = name
    , _userPwHash = pwHash
    } =
      [ "name" =: name
      , "pwHash" =: pwHash
      ]

instance FromDocument User where
  parseDocument doc = User <$> Bson.lookup "name" doc <*> Bson.lookup "pwHash" doc

newtype PwHash = PwHash {unPwHash :: ByteString} deriving (Generic, Show, Eq, NFData)

fromPwHashToText :: PwHash -> Text
fromPwHashToText =
  Text.decodeUtf8 . unPwHash

fromTextToPwHash :: Text -> PwHash
fromTextToPwHash =
  PwHash . Text.encodeUtf8


instance Bson.Val PwHash where
  val pwHash = Bson.String $ fromPwHashToText pwHash
  cast' (Bson.String txt) = Just $ fromTextToPwHash txt
  cast' _ = Nothing

-- Session

data Session = Session
  { _sessionUserId  :: Id User
  , _sessionKey     :: SessionKey
  , _sessionExpDate :: UTCTime
  } deriving (Generic, NFData)

instance Model Session where
  collectionName = const "sessions"

instance ToDocument Session where
  toDocument Session
   { _sessionUserId = userId
    , _sessionKey = sessionKey
    , _sessionExpDate = sessionExpDate
    } =
      [ "userId" =: toObjectId userId
      , "sessionKey" =: sessionKey
      , "sessionExpDate" =: sessionExpDate
      ]

instance FromDocument Session where
  parseDocument doc = Session
    <$> (fromObjectId <$> Bson.lookup "userId" doc)
    <*> Bson.lookup "sessionKey" doc
    <*> Bson.lookup "sessionExpDate" doc


makeLenses ''User

makeLenses ''Session
