{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}
-- |

module Lib.Prelude
  ( module Exports
  , (:+:)(..)
  , coproduct
  , unsafeLeft
  , unsafeRight
  , type (~>)
  ) where

import           Protolude as Exports hiding ((:*:), (:+:), Fixity, Infix, Type,
                                       TypeError, reduce)

data (f :+: g) a
  = InjL (f a)
  | InjR (g a)

infixr 5 :+:

coproduct :: (f a -> b) -> (g a -> b) -> (f :+: g) a -> b
coproduct x _ (InjL f) = x f
coproduct _ y (InjR g) = y g

unsafeLeft :: (f :+: g) a -> (f a -> b) -> b
unsafeLeft (InjL f) x = x f

unsafeRight :: (f :+: g) a -> (g a -> b) -> b
unsafeRight (InjR g) y = y g

type f ~> g = forall a. f a -> g a
