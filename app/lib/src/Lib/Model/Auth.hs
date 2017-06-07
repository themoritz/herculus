{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Lib.Model.Auth where

import           Lib.Prelude

import           Control.Lens           (makeLenses)
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
import           Lib.Utils.Base64       (Base64, Base64Url, toBase64Unsafe,
                                         unBase64)

newtype Email = Email { unEmail :: Text }
  deriving (Generic, FromJSON, ToJSON, Eq, Show)

instance Bson.Val Email where
  val = Bson.val . unEmail
  cast' = fmap Email . Bson.cast'

--------------------------------------------------------------------------------

type PwHash = Base64
type SessionKey = Base64Url

-- Sign up ---------------------------------------------------------------------

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
  } deriving (Generic, Show)

makeLenses ''User

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

-- Session model ---------------------------------------------------------------

data Session = Session
  { _sessionUserId  :: Id User
  , _sessionKey     :: SessionKey
  , _sessionExpDate :: Time
  } deriving (Generic, Show)

makeLenses ''Session

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
