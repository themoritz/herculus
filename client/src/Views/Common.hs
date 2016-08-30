{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Views.Common where

import           Control.Lens hiding (view)
import           Control.DeepSeq     (NFData)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as Text
import           GHC.Generics        (Generic)

import React.Flux
import React.Flux.Internal (toJSString, JSString)

data EditBoxProps = EditBoxProps
  { _ebpValue :: Text
  , _ebpOnSave :: (Text -> [SomeStoreAction])
  }

data EditBoxState = EditBoxState
  { _ebsValue :: Maybe Text
  , _ebsIsEditing :: Bool
  } deriving (Generic, NFData)

makeLenses ''EditBoxProps
makeLenses ''EditBoxState

emptyEditBox :: EditBoxState
emptyEditBox = EditBoxState Nothing False

editBox_ :: Text -> EditBoxProps -> ReactElementM eh ()
editBox_ name props = view (editBox name) props mempty

editBox :: Text -> ReactView EditBoxProps
editBox name = defineStatefulView (textToJS name) emptyEditBox $ \state props -> do
  let value = state ^. ebsValue ?: props ^. ebpValue
  div_ [ "class" &= ("editBox" :: Text) ] $ case state ^. ebsIsEditing of
    True  -> input_
      [ "placeholder" &= name
      , "value"       &= value
      , "autoFocus"   &= True
      , onChange $ \evt st -> let v = Just $ target evt "value"
                              in  ([], Just $ st & ebsValue .~ v)
      , onKeyDown $ \_ evt _ ->
          let v = state ^. ebsValue ?: ""
          in  if keyCode evt == 13 && not (Text.null v) -- 13 = Enter
                then (props ^. ebpOnSave $ v, Nothing)
                else ([], Nothing)
      -- the keyUp event, unlike keydown, is consistently handled among browsers
      , onKeyUp $ \_ evt st -> case keyCode evt of
          27 -> ([], Just $ st & ebsIsEditing .~ False
                               & ebsValue     .~ Just (props ^. ebpValue))
          _  -> ([], Nothing)
      , onBlur  $ \_ _ st  -> ([], Just $ st & ebsIsEditing .~ False
                                             & ebsValue     .~ Just (props ^. ebpValue))
      ]
    False -> div_
      [ onClick $ \_ _ st -> ([], Just $ st & ebsIsEditing .~ True)
      ] $ elemText value

textToJS :: Text -> JSString
textToJS = toJSString . Text.unpack

(?:) :: Maybe a -> a -> a
(?:) = flip fromMaybe

infixl 3 ?:
