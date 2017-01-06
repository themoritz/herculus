{-# LANGUAGE TemplateHaskell #-}
-- |

module Store.Message
  ( module Action.Message
  , module Store.Message
  ) where

import           Control.Lens
import           Data.Text      (Text)

import           Action.Message (Action (..))

data Type
  = Info
  | Success
  | Warning
  | Error
  deriving (Show)

-- in future we maybe want a list of messages
-- instead of one message at a time

type State = Maybe Message

data Message = Message
  { _stContent :: Text
  , _stType    :: Type
  } deriving Show

makeLenses ''Message

runAction :: Action -> State
runAction = \case
  SetInfo    str -> Just $ Message str Info
  SetSuccess str -> Just $ Message str Success
  SetWarning str -> Just $ Message str Warning
  SetError   str -> Just $ Message str Error
  Unset          -> Nothing
