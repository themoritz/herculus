{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


module Lib.Model.Auth
  ( PwHash
  , LoginData (..)
  , LoginResponse (..)
  , SignupData (..)
  , SignupResponse (..)
  , mkPwHash
  , mkSession
  , prolongSession
  , SessionKey
  , Session
  , sessionExpDate
  , sessionKey
  , sessionUserId
  , User (..)
  , userName
  , userPwHash
  , verifyPassword
  ) where

import           Control.DeepSeq        (NFData)
import           Control.Lens           (makeLenses)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Crypto.PasswordStore   (makePassword)
import qualified Crypto.PasswordStore
import           Data.Aeson             (FromJSON, ToJSON)
import           Data.Bson              ((=:))
import qualified Data.Bson              as Bson
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Base64 as Base64
import           Data.Text              (Text)
import qualified Data.Text.Encoding     as Text
import           Data.Time.Clock        (addUTCTime)
import           GHC.Generics           (Generic)
import           Lib.Types              (Id, fromObjectId, toObjectId)
import           System.Entropy         (getEntropy)

import           Lib.Model.Class        (FromDocument (..), Model (..),
                                         ToDocument (..))
import           Lib.Types              (Time, addSeconds)

-- Login

 -- TODO: OAuth
data LoginData = LoginData
  { ldUserName :: Text
  , ldPassword :: Text
  } deriving (Generic, FromJSON, ToJSON)

type SessionKey = Text

data LoginResponse
  = LoginSuccess SessionKey
  | LoginFailed Text
  deriving (Generic, FromJSON, ToJSON)

data SignupData = SignupData
  { suUserName :: Text
  , suPassword :: Text
  } deriving (Generic, FromJSON, ToJSON)

data SignupResponse
  = SignupSuccess SessionKey
  | SignupFailed Text
  deriving (Generic, FromJSON, ToJSON)

-- TODO: a "user" object that is available to auth protected handlers
--       the `User` below is not quite it, because it also contains the password
--       Maybe a different ADT. It's just `Id User` for now

mkPwHash :: MonadIO m => Text -> m PwHash
mkPwHash txt =
  liftIO $ PwHash <$> makePassword (Text.encodeUtf8 txt) 17

verifyPassword :: Text -> PwHash -> Bool
verifyPassword str (PwHash bs2) =
  Crypto.PasswordStore.verifyPassword (Text.encodeUtf8 str) bs2

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
  , _sessionExpDate :: Time
  } deriving (Generic, NFData)

mkSession :: MonadIO m => Id User -> Time -> m Session
mkSession userId created = do
  key <- liftIO $ Text.decodeUtf8 . Base64.encode <$> getEntropy 32
  -- session expiry in seconds
  pure $ Session userId key (addSeconds 600 created)

prolongSession :: Session -> Session
prolongSession session@Session{ _sessionExpDate = expiry } =
  session { _sessionExpDate = addSeconds 600 expiry }

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
