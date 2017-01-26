{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Lib.Model.Auth where

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
import           Data.Time.Clock        (UTCTime)

import           GHC.Generics           (Generic)
import qualified Text.Email.Validate    as Email

import           Lib.Model.Class        (FromDocument (..), Model (..),
                                         ToDocument (..))
import           Lib.Types              (Id, Time, fromObjectId, toObjectId)
import           Lib.Util.Base64        (Base64, Base64Url, toBase64Unsafe,
                                         unBase64)

newtype Email = Email { unEmail :: Text }
  deriving (Generic, FromJSON, ToJSON, NFData, Eq, Show)

instance Bson.Val Email where
  val = Bson.val . unEmail
  cast' = fmap Email . Bson.cast'

-- Login -----------------------------------------------------------------------

data LoginData = LoginData
  { ldEmail    :: Email
  , ldPassword :: Text
  } deriving (Generic, FromJSON, ToJSON, NFData, Show)

type SessionKey = Base64Url

data LoginResponse
  = LoginSuccess UserInfo
  | LoginFailed Text
  deriving (Generic, FromJSON, ToJSON, NFData)

-- Sign up ---------------------------------------------------------------------

data SignupData = SignupData
  { suUserName  :: Text
  , suEmail     :: Email
  , suPassword  :: Text
  , suIntention :: Text
  } deriving (Generic, FromJSON, ToJSON, NFData, Show)

data SignupResponse
  = SignupSuccess UserInfo
  | SignupFailed Text
  deriving (Generic, FromJSON, ToJSON, NFData)

data GetUserInfoResponse
  = GetUserInfoSuccess UserInfo
  | GetUserInfoFailed Text
  deriving (Generic, FromJSON, ToJSON, NFData, Show)

mkPwHash :: MonadIO m => Text -> m PwHash
mkPwHash txt = liftIO $ toBase64Unsafe <$> makePassword (Text.encodeUtf8 txt) 17

verifyPassword :: Text -> PwHash -> Bool
verifyPassword str pwHash =
  Crypto.PasswordStore.verifyPassword (Text.encodeUtf8 str) (unBase64 pwHash)

verifyEmail :: Email -> Bool
verifyEmail = Email.isValid . Text.encodeUtf8 . unEmail

-- User model ------------------------------------------------------------------

data User = User
  { _userName       :: Text
  , _userEmail      :: Email
  , _userPwHash     :: PwHash
  , _userSignupDate :: UTCTime
  , _userIntention  :: Text
  } deriving (Generic, NFData, Show)

userName :: Lens' User Text
userName = lens _userName (\s a -> s { _userName = a })

userEmail :: Lens' User Email
userEmail = lens _userEmail (\s a -> s { _userEmail = a })

userPwHash :: Lens' User PwHash
userPwHash = lens _userPwHash (\s a -> s { _userPwHash = a })

userSignupDate :: Lens' User UTCTime
userSignupDate = lens _userSignupDate (\s a -> s { _userSignupDate = a })

userIntention :: Lens' User Text
userIntention = lens _userIntention (\s a -> s { _userIntention = a })

instance Model User where
  collectionName = const "users"

instance ToDocument User where
  toDocument User{..} =
    [ "name"       =: _userName
    , "email"      =: _userEmail
    , "pwHash"     =: _userPwHash
    , "signupDate" =: _userSignupDate
    , "intention"  =: _userIntention
    ]

instance FromDocument User where
  parseDocument doc =
    User <$> Bson.lookup "name" doc
         <*> Bson.lookup "email" doc
         <*> Bson.lookup "pwHash" doc
         <*> Bson.lookup "signupDate" doc
         <*> Bson.lookup "intention" doc

type PwHash = Base64

-- | A "user" object that is available to auth protected handlers
data UserInfo = UserInfo
  { _uiUserId     :: Id User
  , _uiUserName   :: Text
  , _uiUserEmail  :: Email
  , _uiSessionKey :: SessionKey
  } deriving (Generic, FromJSON, ToJSON, NFData, Show)

uiUserId :: Lens' UserInfo (Id User)
uiUserId = lens _uiUserId (\s a -> s { _uiUserId = a })

uiUserName :: Lens' UserInfo Text
uiUserName = lens _uiUserName (\s a -> s { _uiUserName = a })

uiUserEmail :: Lens' UserInfo Email
uiUserEmail = lens _uiUserEmail (\s a -> s { _uiUserEmail = a })

uiSessionKey :: Lens' UserInfo SessionKey
uiSessionKey = lens _uiSessionKey (\s a -> s { _uiSessionKey = a })

-- Session model ---------------------------------------------------------------

data Session = Session
  { _sessionUserId  :: Id User
  , _sessionKey     :: SessionKey
  , _sessionExpDate :: Time
  } deriving (Generic, NFData, Show)

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

-- Change password -------------------------------------------------------------

data ChangePwdData = ChangePwdData
  { cpdOldPassword :: Text
  , cpdNewPassword :: Text
  } deriving (Generic, FromJSON, ToJSON, NFData, Show)

data ChangePwdResponse
  = ChangePwdSuccess
  | ChangePwdFailure Text
  deriving (Generic, FromJSON, ToJSON, NFData)
