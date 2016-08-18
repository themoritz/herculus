{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE LambdaCase #-}

module Misc where

import Control.Monad (join)

import Data.Map (Map)
import qualified Data.Map as Map

import Reflex.Dom

dynMapEvents :: (Ord k, Reflex t) => Dynamic t (Map k (Event t a)) -> Event t (k, a)
dynMapEvents = dynMapEventsWith id

dynMapEventsWith :: (Ord k, Reflex t)
                 => (a -> Event t b) -> Dynamic t (Map k a)
                 -> Event t (k, b)
dynMapEventsWith f dynMap = switchPromptlyDyn $
  ffor dynMap (leftmost . map (\(k, a) -> (\ex -> (k, ex)) <$> f a) . Map.toList)

--

dynWidget :: MonadWidget t m => Dynamic t a -> (a -> m b) -> m (Event t b)
dynWidget d f = dyn $ f <$> d

switchEvent :: MonadWidget t m => m (Event t (Event t a)) -> m (Event t a)
switchEvent = switchEventWith id

switchEventWith :: MonadWidget t m => (a -> Event t b) -> m (Event t a) -> m (Event t b)
switchEventWith f action = do
  e <- action
  switchPromptly never (f <$> e)

switcherDyn :: MonadWidget t m => Dynamic t a -> Event t (Dynamic t a) -> m (Dynamic t a)
switcherDyn d ed = join <$> holdDyn d ed

--

listWithKeyEq :: forall t k v m a. (Ord k, Eq v, MonadWidget t m)
              => Dynamic t (Map k v) -> (k -> Dynamic t v -> m a)
              -> m (Dynamic t (Map k a))
listWithKeyEq vals mkChild = do
  postBuild <- getPostBuild
  rec sentVals :: Dynamic t (Map k v) <- foldDyn applyMap Map.empty changeVals
      let changeVals :: Event t (Map k (Maybe v))
          changeVals = attachWith diffMap (current sentVals) $ leftmost
                         [ updated vals
                         , tag (current vals) postBuild
                         ]
  listWithKeyShallowDiff Map.empty changeVals $ \k v0 dv -> do
    mkChild k =<< holdDyn v0 dv
