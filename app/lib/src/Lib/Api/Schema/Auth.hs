{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
-- |

module Lib.Api.Schema.Auth where

import           Lib.Prelude

import           Control.Lens   (makeLenses)

import           Data.Aeson     (FromJSON, ToJSON)
import           Data.Text      (Text)

import qualified Lib.Model.Auth as M
import           Lib.Types

--------------------------------------------------------------------------------

data LoginData = LoginData
  { ldEmail    :: M.Email
  , ldPassword :: Text
  } deriving (Generic, FromJSON, ToJSON, Show)

data LoginResponse
  = LoginSuccess UserInfo
  | LoginFailed Text
  deriving (Generic, FromJSON, ToJSON)

--------------------------------------------------------------------------------

data SignupData = SignupData
  { suUserName  :: Text
  , suEmail     :: M.Email
  , suPassword  :: Text
  , suIntention :: Text
  } deriving (Generic, FromJSON, ToJSON, Show)

data SignupResponse
  = SignupSuccess UserInfo
  | SignupFailed Text
  deriving (Generic, FromJSON, ToJSON)

--------------------------------------------------------------------------------

data GetUserInfoResponse
  = GetUserInfoSuccess UserInfo
  | GetUserInfoFailed Text
  deriving (Generic, FromJSON, ToJSON, Show)

-- | A "user" object that is available to auth protected handlers
data UserInfo = UserInfo
  { _uiUserId     :: Id M.User
  , _uiUserName   :: Text
  , _uiUserEmail  :: M.Email
  , _uiSessionKey :: M.SessionKey
  } deriving (Generic, FromJSON, ToJSON, Show)

makeLenses ''UserInfo

--------------------------------------------------------------------------------

data ChangePwdData = ChangePwdData
  { cpdOldPassword :: Text
  , cpdNewPassword :: Text
  } deriving (Generic, FromJSON, ToJSON, Show)

data ChangePwdResponse
  = ChangePwdSuccess
  | ChangePwdFailure Text
  deriving (Generic, FromJSON, ToJSON)
