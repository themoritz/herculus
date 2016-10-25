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
  , SessionKey
  , Session (..)
  , sessionExpDate
  , sessionKey
  , sessionUserId
  , User (..)
  , UserInfo (..)
  , uiUserId
  , uiUserName
  , uiSessionKey
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
import           Data.Text              (Text)
import qualified Data.Text.Encoding     as Text
import           GHC.Generics           (Generic)

import           Lib.Model.Class        (FromDocument (..), Model (..),
                                         ToDocument (..))
import           Lib.Types              (Id, Time, fromObjectId, toObjectId)
import           Lib.Util.Base64        (Base64, toBase64Unsafe, unBase64)


-- Login

 -- TODO: OAuth
data LoginData = LoginData
  { ldUserName :: Text
  , ldPassword :: Text
  } deriving (Generic, FromJSON, ToJSON, NFData)

type SessionKey = Base64

data LoginResponse
  = LoginSuccess UserInfo
  | LoginFailed Text
  deriving (Generic, FromJSON, ToJSON)

data SignupData = SignupData
  { suUserName :: Text
  , suPassword :: Text
  } deriving (Generic, FromJSON, ToJSON)

data SignupResponse
  = SignupSuccess UserInfo
  | SignupFailed Text
  deriving (Generic, FromJSON, ToJSON)

mkPwHash :: MonadIO m => Text -> m PwHash
mkPwHash txt = liftIO $ toBase64Unsafe <$> makePassword (Text.encodeUtf8 txt) 17

verifyPassword :: Text -> PwHash -> Bool
verifyPassword str pwHash =
  Crypto.PasswordStore.verifyPassword (Text.encodeUtf8 str) (unBase64 pwHash)

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

type PwHash = Base64

-- a "user" object that is available to auth protected handlers
data UserInfo = UserInfo
  { _uiUserId     :: Id User
  , _uiUserName   :: Text
  , _uiSessionKey :: SessionKey
  } deriving (Generic, FromJSON, ToJSON, NFData)

-- Session

data Session = Session
  { _sessionUserId  :: Id User
  , _sessionKey     :: SessionKey
  , _sessionExpDate :: Time
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
makeLenses ''UserInfo
makeLenses ''Session
