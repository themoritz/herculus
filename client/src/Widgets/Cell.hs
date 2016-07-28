module Widgets.Cell
  where

import Control.Monad

import Data.Text (Text, pack, unpack)

import Reflex.Dom hiding (Value)

import Api.Rest (loader, api)
import qualified Api.Rest as Api
import Lib.Types
import Lib.Model.Types
import Lib.Api.Rest

data CellConfig t = CellConfig
  { _cellConfig_setValue :: Event t Value
  , _cellConfig_initialValue :: Value
  }

cell :: MonadWidget t m
     => Id Table -> Id Column -> Id Record
     -> CellConfig t -> m (Event t Value)
cell tblId colId recId (CellConfig setValue initialValue) = el "div" $ do
  value <- (fmap (Value . pack) . current . _textInput_value) <$> textInput
             def { _textInputConfig_setValue = (unpack . unValue) <$> setValue
                 , _textInputConfig_initialValue = (unpack . unValue) initialValue
                 }
  let cellSetArg = (\v -> Right $ Cell v $ Aspects tblId colId recId) <$> value
  trigger <- button "Set"
  _ <- loader (Api.cellSet api cellSetArg) trigger
  pure $ tag value trigger
