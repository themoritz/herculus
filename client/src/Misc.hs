{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

module Misc where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text, pack)

import Reflex.Dom

-- Lists

listWithKeyNoHold :: forall t k v m a. (Ord k, MonadWidget t m)
                  => Dynamic t (Map k v)
                  -> (k -> v -> Event t v -> m a)
                  -> m (Dynamic t (Map k a))
listWithKeyNoHold vals mkChild = do
  postBuild <- getPostBuild
  rec sentVals :: Dynamic t (Map k v) <- foldDyn (flip applyMap) Map.empty changeVals
      let changeVals :: Event t (Map k (Maybe v))
          changeVals = attachWith diffMapNoEq (current sentVals) $ leftmost
                         [ updated vals
                         , tag (current vals) postBuild
                         ]
  listWithKeyShallowDiff Map.empty changeVals mkChild

listWithKeyNoHoldEvent :: forall t k v m a. (Ord k, MonadWidget t m)
                       => Dynamic t (Map k v)
                       -> (k -> v -> Event t v -> m (Event t a))
                       -> m (Event t (k, a))
listWithKeyNoHoldEvent vals mkChild = do
  dynMap <- listWithKeyNoHold vals mkChild
  dynEvent <- mapDyn (leftmost . map (\(k, e) -> (\ex -> (k, ex)) <$> e) . Map.toList) dynMap
  pure $ switchPromptlyDyn dynEvent

listWithKeyEvent :: forall t k v m a. (Ord k, MonadWidget t m)
                 => Dynamic t (Map k v)
                 -> (k -> Dynamic t v -> m (Event t a))
                 -> m (Event t (k, a))
listWithKeyEvent vals mkChild = do
  dynMap <- listWithKey vals mkChild
  dynEvent <- mapDyn (leftmost . map (\(k, e) -> (\ex -> (k, ex)) <$> e) . Map.toList) dynMap
  pure $ switchPromptlyDyn dynEvent

textInputT :: MonadWidget t m => TextInputConfig t -> m (Dynamic t Text)
textInputT config = do
  inp <- textInput config
  mapDyn pack $ _textInput_value inp

dynWidget :: MonadWidget t m => Dynamic t a -> (a -> m b) -> m (Event t b)
dynWidget d f = mapDyn f d >>= dyn

switchEvent :: MonadWidget t m => m (Event t (Event t a)) -> m (Event t a)
switchEvent action = do
  ee <- action
  de <- holdDyn never ee
  pure $ switchPromptlyDyn de
