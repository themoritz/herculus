{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Widgets.Cell
  where

import Control.Monad

import Data.Text (Text, pack)
import Data.Monoid
import Data.Maybe

import Reflex.Dom hiding (Value)

import Lib.Types
import Lib.Model.Types
import Lib.Model.Cell
import Lib.Model.Column

import Misc

data CellConfig t = CellConfig
  { _cellConfig_content :: Dynamic t CellContent
  , _cellConfig_column :: Column
  }

cell :: MonadWidget t m
     => Id Column -> Id Record
     -> CellConfig t -> m (Event t Value)
cell colId recId (CellConfig content column) = el "div" $
  case columnInputType column of
    ColumnInput ->
      switchEvent $ dynWidget content $ \c -> do
        case columnDataType column of
          DataString -> do
            let val = case c of
                  CellValue (VString v) -> v
                  _                         -> ""
            inp <- cellInput val
            pure $ (VString <$> inp)
          DataNumber -> do
            let val = case c of
                  CellValue (VNumber v) -> v
                  _                         -> 0
            inp <- cellInput val
            pure $ (VNumber <$> inp)

    ColumnDerived -> do
      void $ dynWidget content $ \case
        CellNothing ->
          text "Nothing"
        CellEvalError msg ->
          text $ "Error: " <> msg
        CellValue val -> case columnDataType column of
            DataString ->
              cellResult $ fromMaybe "" (extractValue val :: Maybe Text)
            DataNumber ->
              cellResult $ fromMaybe 0 (extractValue val :: Maybe Number)
      pure never


class CellInput a where
  cellInput :: MonadWidget t m => a -> m (Event t a)

class CellResult a where
  cellResult :: MonadWidget t m => a -> m ()

instance CellInput Text where
  cellInput val = do
    new <- _textInput_value <$> textInput def
             { _textInputConfig_initialValue = val
             }
    set <- button "Set"
    pure $ tagPromptlyDyn new set

instance CellResult Text where
  cellResult = text

instance CellInput Number where
  cellInput :: forall t m. MonadWidget t m => Number -> m (Event t Number)
  cellInput val = mdo
    mNew <- (fmap parseValue . _textInput_value) <$> textInput (def :: TextInputConfig t)
             { _textInputConfig_initialValue = pack $ show val
             , _textInputConfig_attributes = attrs
             }
    let attrs = ffor mNew $ \case
          Nothing -> "style" =: "border-color: red"
          Just _  -> "style" =: "border-color: auto"
    set <- button "Set"
    pure $ fmapMaybe id $ tagPromptlyDyn mNew set

instance CellResult Number where
  cellResult = text . pack . show
