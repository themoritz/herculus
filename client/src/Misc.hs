{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}

module Misc where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text, pack)
import Data.These
import Data.Align

import Reflex.Dom

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

dynMapEvents :: (Ord k, MonadWidget t m) => Dynamic t (Map k (Event t a)) -> m (Event t (k, a))
dynMapEvents dynMap = do
  dynEvent <- mapDyn (leftmost . map (\(k, e) -> (\ex -> (k, ex)) <$> e) . Map.toList) dynMap
  pure $ switchPromptlyDyn dynEvent

--

textInputT :: MonadWidget t m => TextInputConfig t -> m (Dynamic t Text)
textInputT config = do
  inp <- textInput config
  mapDyn pack $ _textInput_value inp

--

dynWidget :: MonadWidget t m => Dynamic t a -> (a -> m b) -> m (Event t b)
dynWidget d f = mapDyn f d >>= dyn

switchEvent :: MonadWidget t m => m (Event t (Event t a)) -> m (Event t a)
switchEvent action = do
  ee <- action
  de <- holdDyn never ee
  pure $ switchPromptlyDyn de

--

diffMap :: (Ord k, Eq v) => Map k v -> Map k v -> Map k (Maybe v)
diffMap olds news = flip Map.mapMaybe (align olds news) $ \case
  This _ -> Just Nothing
  These old new -> if old == new then Nothing else Just (Just new)
  That new -> Just $ Just new

listWithKeyEq :: forall t k v m a. (Ord k, Eq v, MonadWidget t m)
              => Dynamic t (Map k v) -> (k -> Dynamic t v -> m a)
              -> m (Dynamic t (Map k a))
listWithKeyEq vals mkChild = do
  postBuild <- getPostBuild
  rec sentVals :: Dynamic t (Map k v) <- foldDyn (flip applyMap) Map.empty changeVals
      let changeVals :: Event t (Map k (Maybe v))
          changeVals = attachWith diffMap (current sentVals) $ leftmost
                         [ updated vals
                         , tag (current vals) postBuild
                         ]
  listWithKeyShallowDiff Map.empty changeVals $ \k v0 dv -> do
    mkChild k =<< holdDyn v0 dv
