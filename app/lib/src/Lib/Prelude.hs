{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveTraversable      #-}
-- |

module Lib.Prelude
  ( module Exports
  , id
  --
  , (:+:)(..)
  , coproduct
  , type (~>)
  , (:<:)(..)
  , injFix
  , unsafePrj
  , unsafePrjFix
  --
  , cataM
  , paraM
  --
  , mapLeft
  , hoistError
  ) where

import           Protolude as Exports hiding ((:*:), (:+:), Fixity, Infix, Type,
                                       TypeError, reduce, Constraint)

import           Data.Functor.Foldable

import           Data.Maybe (fromJust)

--------------------------------------------------------------------------------

data (f :+: g) a
  = InjL (f a)
  | InjR (g a)
  deriving (Functor, Foldable, Traversable)

infixr 5 :+:

coproduct :: (f a -> b) -> (g a -> b) -> (f :+: g) a -> b
coproduct fun _ (InjL f) = fun f
coproduct _ fun (InjR g) = fun g

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

injFix :: (Functor f, f :<: g) => Fix f -> Fix g
injFix = Fix . inj . fmap injFix . unfix

unsafePrj :: f :<: g => g a -> f a
unsafePrj = fromJust . prj

unsafePrjFix :: (Functor g, f :<: g) => Fix g -> Fix f
unsafePrjFix = Fix . unsafePrj . fmap unsafePrjFix . unfix

--------------------------------------------------------------------------------

cataM
  :: (Recursive t, Monad m, Traversable (Base t))
  => (Base t a -> m a) -> t -> m a
cataM alg = go where
  go t = alg =<< traverse go (project t)

paraM
  :: (Recursive t, Monad m, Traversable (Base t))
  => (Base t (t, a) -> m a) -> t -> m a
paraM alg = go where
  go t = alg =<< traverse (fmap (t,) . go) (project t)

--------------------------------------------------------------------------------

mapLeft :: (a -> a') -> Either a b -> Either a' b
mapLeft f (Left a) = Left (f a)
mapLeft _ (Right b) = Right b

hoistError :: MonadError e m => Either e a -> m a
hoistError = either throwError pure
