{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}

module Misc where

import Data.Map (Map)
import qualified Data.Map as Map

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
