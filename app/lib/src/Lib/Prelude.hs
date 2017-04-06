{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
-- |

module Lib.Prelude
  ( module Exports
  , id
  --
  , (:+:)(..)
  , coproduct
  , type (~>)
  , (:<:)(..)
  , unsafePrj
  --
  , cataM
  --
  , mapLeft
  ) where

import           Protolude as Exports hiding ((:*:), (:+:), Fixity, Infix, Type,
                                       TypeError, reduce, Constraint)

import           Data.Functor.Foldable

import           Data.Maybe (fromJust)

--------------------------------------------------------------------------------

data (f :+: g) a
  = InjL (f a)
  | InjR (g a)

infixr 5 :+:

coproduct :: (f a -> b) -> (g a -> b) -> (f :+: g) a -> b
coproduct fun _ (InjL f) = fun f
coproduct _ fun (InjR g) = fun g

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f = coproduct (InjL . fmap f) (InjR . fmap f)

type f ~> g = forall a. f a -> g a

class f :<: g where
  inj :: f a -> g a
  prj :: g a -> Maybe (f a)

instance f :<: (f :+: g) where
  inj = InjL
  prj (InjL f) = Just f
  prj _        = Nothing

instance {-# OVERLAPPABLE #-} h :<: g => h :<: (f :+: g) where
  inj = InjR . inj
  prj (InjR g) = prj g
  prj _        = Nothing

instance {-# OVERLAPS #-} f :<: f where
  inj = id
  prj = Just . id

unsafePrj :: f :<: g => g a -> f a
unsafePrj = fromJust . prj

--------------------------------------------------------------------------------

cataM
  :: (Recursive t, Monad m, Traversable (Base t))
  => (Base t a -> m a) -> t -> m a
cataM f = go where
  go t = f =<< traverse go (project t)

--------------------------------------------------------------------------------

mapLeft :: (a -> a') -> Either a b -> Either a' b
mapLeft f (Left a) = Left (f a)
mapLeft _ (Right b) = Right b
