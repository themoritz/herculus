{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}
-- |

module React.Flux.Addons.Free
  ( FreeFlux
  , FreeFluxAction
  , freeFluxDispatch
  , freeFluxTransform
  , ajax
  , halt
  ) where

import           Control.DeepSeq
import           Control.Monad.State

import           GHC.Generics

import           React.Flux
import           React.Flux.Addons.Servant


data FluxF s next
  = ModifyState (s -> (next, s))
  | Halt
  | forall a. LiftIO (IO a) (a -> next)
  | forall a. NFData a => Ajax (HandleResponse a -> IO ())
                               (Either (Int, String) a -> next)

deriving instance Functor (FluxF s)

-- TODO: Is this correct?
instance NFData (FluxF s next) where
  rnf (ModifyState f) = f `seq` ()
  rnf Halt            =  ()
  rnf (LiftIO m f)    = m `seq` f `seq` ()
  rnf (Ajax go f)     = go `seq` f `seq` ()

data FreeFlux s a
  = Free (FluxF s (FreeFlux s a))
  | Pure a
  deriving (Generic, NFData, Functor)

liftF :: FluxF s a -> FreeFlux s a
liftF = Free . fmap Pure

instance Applicative (FreeFlux s) where
  pure = Pure
  Pure f <*> Pure a = Pure (f a)
  Pure f <*> Free a = Free (fmap f <$> a)
  Free f <*> ma = Free ((<*> ma) <$> f)

instance Monad (FreeFlux s) where
  Free x >>= f = Free (fmap (>>= f) x)
  Pure a >>= f = f a

instance MonadState s (FreeFlux s) where
  state = liftF . ModifyState

instance MonadIO (FreeFlux s) where
  liftIO m = liftF $ LiftIO m id

halt :: FreeFlux s a
halt = liftF Halt

ajax :: NFData a => (HandleResponse a -> IO ())
                 -> FreeFlux s (Either (Int, String) a)
ajax go = liftF $ Ajax go id

data FreeFluxAction s ac
  = forall a. NFData a => Continue (FreeFlux s a)
  | Enter ac

instance NFData action => NFData (FreeFluxAction s action) where
  rnf (Continue m) = rnf m
  rnf (Enter a)    = rnf a

freeFluxDispatch :: ac -> FreeFluxAction s ac
freeFluxDispatch = Enter

freeFluxTransform :: ( NFData a, NFData ac, StoreData s
                     , StoreAction s ~ FreeFluxAction s ac )
                  => ReactStore s -> (ac -> FreeFlux s a)
                  -> FreeFluxAction s ac -> s -> IO s
freeFluxTransform store handler fluxAction st = case fluxAction of
  Continue m   -> runFreeFlux store st m
  Enter action -> runFreeFlux store st (handler action)

runFreeFlux :: forall a s ac
             . ( NFData a, NFData ac, StoreData s
               , StoreAction s ~ FreeFluxAction s ac )
            => ReactStore s -> s
            -> FreeFlux s a
            -> IO s
runFreeFlux store st m' = execStateT (interpret m') st

  where
    interpret :: NFData b => FreeFlux s b -> StateT s IO ()
    interpret m = case m of

      Free (ModifyState f) -> do
        old <- get
        let (next, new) = f old
        put new
        interpret next

      Free Halt -> pure ()

      Free (LiftIO action next) -> do
        a <- liftIO action
        interpret (next a)

      Free (Ajax go next) -> do
        liftIO $ go $ \result ->
          pure [SomeStoreAction store $ Continue (next result)]
        pure ()

      Pure _ -> pure ()

