module Widgets.Cell
  where

import Control.Monad

import Data.Text (Text, pack, unpack)

import Reflex.Dom hiding (Value)

import Api.Rest (loader, api)
import qualified Api.Rest as Api
import Lib
import Lib.Api.Rest

data CellConfig t = CellConfig
  { _cellConfig_setValue :: Event t Value
  , _cellConfig_initialValue :: Value
  }

cell :: MonadWidget t m
     => Id Table -> Id Column -> Id Record
     -> CellConfig t -> m ()
cell tblId colId recId (CellConfig setValue initialValue) = el "div" $ do
  value <- (fmap (Value . pack) . current . _textInput_value) <$> textInput
             def { _textInputConfig_setValue = (unpack . unValue) <$> setValue
                 , _textInputConfig_initialValue = (unpack . unValue) initialValue
                 }
  let cellSetArg = (Right . CellSet tblId colId recId) <$> value
  trigger <- button "Set"
  void $ loader (Api.cellSet api cellSetArg) trigger
