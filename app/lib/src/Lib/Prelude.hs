{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveTraversable     #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeOperators         #-}
-- |

module Lib.Prelude
  ( module Exports
  , id
  , (***), (&&&)
  , (#)
  , (<#>)
  , ExceptT(..)
  --
  , Fix(Fix)
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
  --
  , mapMaybeM
  ) where

import           Prelude                    (id)

import           Control.Arrow              ((&&&), (***))

import           Protolude                  as Exports hiding ((:*:), (:+:),
                                                        Constraint, Fixity,
                                                        Infix, Type, TypeError,
                                                        reduce)
import           Protolude.Error            as Exports

import           Data.Functor.Foldable
import           Data.Fix                   (Fix(Fix), unFix)

import           Data.Maybe                 (fromJust)

--------------------------------------------------------------------------------

(#) :: a -> (a -> b) -> b
(#) a f = f a

infixl 1 #

(<#>) :: Functor f => f a -> (a -> b) -> f b
(<#>) m f = map f m

infixl 1 <#>

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

instance {-# OVERLAPPABLE #-} (h :<: g) => h :<: (f :+: g) where
  inj = InjR . inj
  prj (InjR g) = prj g
  prj _        = Nothing

instance {-# OVERLAPS #-} f :<: f where
  inj = id
  prj = Just . id

injFix :: (Functor f, f :<: g) => Fix f -> Fix g
injFix = Fix . inj . fmap injFix . unFix

unsafePrj :: (f :<: g) => g a -> f a
unsafePrj = fromJust . prj

unsafePrjFix :: (Functor g, f :<: g) => Fix g -> Fix f
unsafePrjFix = Fix . unsafePrj . fmap unsafePrjFix . unFix

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
mapLeft f (Left a)  = Left (f a)
mapLeft _ (Right b) = Right b

hoistError :: MonadError e m => Either e a -> m a
hoistError = either throwError pure

--------------------------------------------------------------------------------

mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f = \case
  [] -> pure []
  a:as -> f a >>= \case
    Nothing -> mapMaybeM f as
    Just b -> (b:) <$> mapMaybeM f as
