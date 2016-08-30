{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Views.Common where

import           Control.Lens hiding (view)
import           Control.DeepSeq     (NFData)
import           Data.Maybe (fromMaybe)
import Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as Text
import           GHC.Generics        (Generic)

import React.Flux
import React.Flux.Internal (toJSString, JSString)

data EditBoxProps = EditBoxProps
  { editBoxValue       :: Text
  , editBoxPlaceholder :: Text
  , editBoxClassName   :: Text
  , editBoxOnSave      :: Text -> [SomeStoreAction]
  }

data EditBoxState = EditBoxState
  { _ebsValue :: Maybe Text
  , _ebsIsEditing :: Bool
  } deriving (Generic, NFData)

makeLenses ''EditBoxProps
makeLenses ''EditBoxState

emptyEditBox :: EditBoxState
emptyEditBox = EditBoxState Nothing False

editBox_ :: EditBoxProps -> ReactElementM eh ()
editBox_ props = view editBox props mempty

editBox :: ReactView EditBoxProps
editBox = defineStatefulView "editBox" emptyEditBox $ \state EditBoxProps{..} -> do
  let value = state ^. ebsValue ?: editBoxValue
  cldiv_ ("editBox " <> textToJS editBoxClassName) $ case state ^. ebsIsEditing of
    True  -> input_
      [ "placeholder" &= editBoxPlaceholder
      , "value"       &= value
      , "autoFocus"   &= True
      , onChange $ \evt st -> let v = Just $ target evt "value"
                              in  ([], Just $ st & ebsValue .~ v)
      , onKeyDown $ \_ evt _ ->
          let v = state ^. ebsValue ?: ""
          in  if keyCode evt == 13 && not (Text.null v) -- 13 = Enter
                then (editBoxOnSave v, Nothing)
                else ([], Nothing)
      -- the keyUp event, unlike keydown, is consistently handled among browsers
      , onKeyUp $ \_ evt st -> case keyCode evt of
          27 -> ([], Just $ st & ebsIsEditing .~ False
                               & ebsValue     .~ Just editBoxValue)
          _  -> ([], Nothing)
      , onBlur  $ \_ _ st  -> ([], Just $ st & ebsIsEditing .~ False
                                             & ebsValue     .~ Just editBoxValue)
      ]
    False -> div_
      [ classNames [("placeholder", value == "")]
      , onClick $ \_ _ st -> ([], Just $ st & ebsIsEditing .~ True)
      ] $ if value == "" then elemText editBoxPlaceholder else elemText value

textToJS :: Text -> JSString
textToJS = toJSString . Text.unpack

(?:) :: Maybe a -> a -> a
(?:) = flip fromMaybe

infixl 3 ?:
