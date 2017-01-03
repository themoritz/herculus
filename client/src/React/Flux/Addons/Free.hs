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
  , async
  , ajax
  ) where

import           Control.Concurrent.Lifted
import           Control.DeepSeq
import           Control.Monad             (void)
import           Control.Monad.State

import           GHC.Generics

import           React.Flux
import           React.Flux.Addons.Servant


data FluxF s next
  = ModifyState (s -> (next, s))
  | forall a. NFData a => Async (FreeFlux s a)
                                (a -> next)
  | forall a. LiftIO (IO a) (a -> next)
  | forall a. NFData a => Ajax (HandleResponse a -> IO ())
                               (Either (Int, String) a -> next)

deriving instance Functor (FluxF s)

instance NFData (FluxF s next) where
  rnf (ModifyState f) = f `seq` ()
  rnf (Async m f)     = rnf m `seq` f `seq` ()
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

async :: NFData a => FreeFlux s a -> FreeFlux s a
async m = liftF $ Async m id

ajax :: NFData a => (HandleResponse a -> IO ())
                 -> FreeFlux s (Either (Int, String) a)
ajax go = liftF $ Ajax go id

data FreeFluxAction s ac
  = forall a. NFData a => Continue (MVar a) (FreeFlux s a)
  | Enter ac

instance NFData action => NFData (FreeFluxAction s action) where
  rnf (Continue ref m) = ref `seq` rnf m
  rnf (Enter a)        = rnf a

freeFluxDispatch :: ac -> FreeFluxAction s ac
freeFluxDispatch = Enter

freeFluxTransform :: ( NFData a, NFData ac, StoreData s
                     , StoreAction s ~ FreeFluxAction s ac )
                  => ReactStore s -> (ac -> FreeFlux s a)
                  -> FreeFluxAction s ac -> s -> IO s
freeFluxTransform store handler fluxAction st = case fluxAction of

  Continue ref m -> do
    (downRef, st') <- runFreeFlux store st m
    -- We need to do this in another thread since this transform needs to
    -- finish before the next action is run in `runFreeFlux`.
    void $ fork $ do
      a <- takeMVar downRef
      putMVar ref a
    pure st'

  Enter action -> snd <$> runFreeFlux store st (handler action)

runFreeFlux :: forall a s ac
             . ( NFData a, NFData ac, StoreData s
               , StoreAction s ~ FreeFluxAction s ac )
            => ReactStore s -> s
            -> FreeFlux s a
            -> IO (MVar a, s)
runFreeFlux store st m' = runStateT (interpret m') st

  where
    interpret :: NFData b => FreeFlux s b -> StateT s IO (MVar b)
    interpret m = case m of

      Free (ModifyState f) -> do
        old <- get
        let (next, new) = f old
        put new
        ref <- newEmptyMVar
        void $ fork $ liftIO $ alterStore store $ Continue ref next
        pure ref

      Free (Async f next) -> do
        ref <- newEmptyMVar
        void $ fork $ do
          aRef <- interpret f
          a <- takeMVar aRef
          liftIO $ alterStore store $ Continue ref (next a)
        pure ref

      Free (LiftIO action next) -> do
        a <- liftIO action
        interpret (next a)

      Free (Ajax go next) -> do
        ref <- newEmptyMVar
        liftIO $ go $ \result ->
          pure [SomeStoreAction store $ Continue ref (next result)]
        pure ref

      Pure a -> newMVar a

