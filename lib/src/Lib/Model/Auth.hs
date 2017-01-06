{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Lib.Model.Auth
  ( PwHash
  , LoginData (..)
  , LoginResponse (..)
  , SignupData (..)
  , SignupResponse (..)
  , GetUserInfoResponse (..)
  , ChangePwdData (..)
  , ChangePwdResponse (..)
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
  , userIntention
  , verifyPassword
  ) where

import           Control.DeepSeq        (NFData)
import           Control.Lens           (Lens', lens)
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
import           Lib.Util.Base64        (Base64, Base64Url, toBase64Unsafe,
                                         unBase64)


-- Login

 -- TODO: OAuth
data LoginData = LoginData
  { ldUserName :: Text
  , ldPassword :: Text
  } deriving (Generic, FromJSON, ToJSON, NFData)

type SessionKey = Base64Url

data LoginResponse
  = LoginSuccess UserInfo
  | LoginFailed Text
  deriving (Generic, FromJSON, ToJSON, NFData)

data SignupData = SignupData
  { suUserName  :: Text
  , suPassword  :: Text
  , suIntention :: Text
  } deriving (Generic, FromJSON, ToJSON, NFData)

data SignupResponse
  = SignupSuccess UserInfo
  | SignupFailed Text
  deriving (Generic, FromJSON, ToJSON, NFData)

data GetUserInfoResponse
  = GetUserInfoSuccess UserInfo
  | GetUserInfoFailed Text
  deriving (Generic, FromJSON, ToJSON, NFData)

mkPwHash :: MonadIO m => Text -> m PwHash
mkPwHash txt = liftIO $ toBase64Unsafe <$> makePassword (Text.encodeUtf8 txt) 17

verifyPassword :: Text -> PwHash -> Bool
verifyPassword str pwHash =
  Crypto.PasswordStore.verifyPassword (Text.encodeUtf8 str) (unBase64 pwHash)

-- User

data User = User
  { _userName      :: Text
  , _userPwHash    :: PwHash
  , _userIntention :: Text
  } deriving (Generic, NFData)

userName :: Lens' User Text
userName = lens _userName (\s a -> s { _userName = a })

userPwHash :: Lens' User PwHash
userPwHash = lens _userPwHash (\s a -> s { _userPwHash = a })

userIntention :: Lens' User Text
userIntention = lens _userIntention (\s a -> s { _userIntention = a })

instance Model User where
  collectionName = const "users"

instance ToDocument User where
  toDocument User
    { _userName = name
    , _userPwHash = pwHash
    , _userIntention = intention
    } =
      [ "name" =: name
      , "pwHash" =: pwHash
      , "intention" =: intention
      ]

instance FromDocument User where
  parseDocument doc =
    User <$> Bson.lookup "name" doc
         <*> Bson.lookup "pwHash" doc
         <*> Bson.lookup "intention" doc

type PwHash = Base64

-- a "user" object that is available to auth protected handlers
data UserInfo = UserInfo
  { _uiUserId     :: Id User
  , _uiUserName   :: Text
  , _uiSessionKey :: SessionKey
  } deriving (Generic, FromJSON, ToJSON, NFData)

uiUserId :: Lens' UserInfo (Id User)
uiUserId = lens _uiUserId (\s a -> s { _uiUserId = a })

uiUserName :: Lens' UserInfo Text
uiUserName = lens _uiUserName (\s a -> s { _uiUserName = a })

uiSessionKey :: Lens' UserInfo SessionKey
uiSessionKey = lens _uiSessionKey (\s a -> s { _uiSessionKey = a })

-- Session

data Session = Session
  { _sessionUserId  :: Id User
  , _sessionKey     :: SessionKey
  , _sessionExpDate :: Time
  } deriving (Generic, NFData)

sessionUserId :: Lens' Session (Id User)
sessionUserId = lens _sessionUserId (\s a -> s { _sessionUserId = a })

sessionKey :: Lens' Session SessionKey
sessionKey = lens _sessionKey (\s a -> s { _sessionKey = a })

sessionExpDate :: Lens' Session Time
sessionExpDate = lens _sessionExpDate (\s a -> s { _sessionExpDate = a })

instance Model Session where
  collectionName = const "sessions"

instance ToDocument Session where
  toDocument Session{..} =
    [ "userId"         =: toObjectId _sessionUserId
    , "sessionKey"     =: _sessionKey
    , "sessionExpDate" =: _sessionExpDate
    ]

instance FromDocument Session where
  parseDocument doc = Session
    <$> (fromObjectId <$> Bson.lookup "userId" doc)
    <*> Bson.lookup "sessionKey" doc
    <*> Bson.lookup "sessionExpDate" doc

-- change password

data ChangePwdData = ChangePwdData
  { cpdOldPassword :: Text
  , cpdNewPassword :: Text
  } deriving (Generic, FromJSON, ToJSON, NFData)

data ChangePwdResponse
  = ChangePwdSuccess
  | ChangePwdFailure Text
  deriving (Generic, FromJSON, ToJSON, NFData)
