{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Views.Common
  ( EditBoxProps(..)
  , editBox_
  ) where

import           Control.Applicative ((<|>))
import           Control.DeepSeq     (NFData)
import           Control.Lens        hiding (view)

import           Data.Maybe          (fromMaybe, isJust)
import           Data.Text           (Text)
import           Data.Typeable       (Typeable)

import           GHC.Generics        (Generic)

import           React.Flux

data EditBoxProps a = EditBoxProps
  { editBoxValue       :: a
  , editBoxPlaceholder :: Text
  , editBoxClassName   :: Text
  , editBoxShow        :: a -> Text
  , editBoxValidator   :: Text -> Maybe a
  , editBoxOnSave      :: a -> [SomeStoreAction]
  }

data EditBoxState a = EditBoxState
  { _ebsValue       :: Maybe a
  , _ebsInvalidText :: Maybe Text
  , _ebsIsEditing   :: Bool
  } deriving (Generic, NFData)

makeLenses ''EditBoxProps
makeLenses ''EditBoxState

emptyEditBox :: EditBoxState a
emptyEditBox = EditBoxState Nothing Nothing False

editBox_ :: (NFData a, Typeable a) => EditBoxProps a -> ReactElementM eh ()
editBox_ props = view editBox props mempty

editBox :: (NFData a, Typeable a) => ReactView (EditBoxProps a)
editBox = defineStatefulView "editBox" emptyEditBox $
  \state EditBoxProps{..} -> do
    let text = editBoxShow <$> state ^. ebsValue
           <|> state ^. ebsInvalidText
            ?: editBoxShow editBoxValue
    div_
      [ classNames
          [ ("editBox", True)
          , (editBoxClassName, True)
          , ("invalid", isJust $ state ^. ebsInvalidText)
          ]
      ] $ case state ^. ebsIsEditing of
        True -> input_
          [ "placeholder" &= editBoxPlaceholder
          , "value"       &= text
          , "autoFocus"   &= True
          , onChange $ \evt st ->
              let v = target evt "value"
              in  case editBoxValidator v of
                    Nothing -> ([], Just $ st & ebsValue .~ Nothing
                                              & ebsInvalidText .~ Just v)
                    Just a ->  ([], Just $ st & ebsValue .~ Just a
                                              & ebsInvalidText .~ Nothing)
          , onKeyDown $ \_ evt _ ->
              case (keyCode evt, state ^. ebsValue) of
                (13, Just a) -> (editBoxOnSave a, Nothing)
                _            -> ([],              Nothing)
          -- the keyUp event, unlike keydown, is consistently handled among browsers
          , onKeyUp $ \_ evt _ -> case keyCode evt of
              27 -> ([], Just emptyEditBox)
              _  -> ([], Nothing)
          , onBlur  $ \_ _ _  -> ([], Just emptyEditBox)
          ]
        False -> div_
          [ classNames [("placeholder", text == "")]
          , onClick $ \_ _ st -> ([], Just $ st & ebsIsEditing .~ True)
          ] $ if text == "" then elemText editBoxPlaceholder else elemText text

(?:) :: Maybe a -> a -> a
(?:) = flip fromMaybe

infixl 3 ?:
