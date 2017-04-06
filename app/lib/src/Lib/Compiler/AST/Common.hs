{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeOperators     #-}
-- |

module Lib.Compiler.AST.Common where

import           Lib.Prelude

import           Data.Functor.Foldable

import           Control.Comonad.Cofree

import           Lib.Compiler.AST.Position


type WithSpan f = Cofree f Span

--------------------------------------------------------------------------------

stripAnn :: Functor f => Cofree f a -> Fix f
stripAnn (_ :< e) = Fix $ map stripAnn e

liftAlg :: Functor f => (f a -> a) -> f (Cofree g a) -> a
liftAlg alg = alg . map (\(a :< _) -> a)

mapAlg :: (f ~> g) -> (g a -> a) -> f a -> a
mapAlg nat alg = alg . nat

-- | Change the base functor of a fixed point
hoistFix :: Functor f => (f ~> g) -> Fix f -> Fix g
hoistFix nat (Fix f) = Fix $ nat $ map (hoistFix nat) f
