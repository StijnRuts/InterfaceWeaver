{-# LANGUAGE DataKinds #-}

module Data.Events where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import qualified Control.Monad
import qualified Data.Foldable as Foldable
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Union (Member, Union (..), inject, project)

newtype Events a = Events ((a -> IO ()) -> IO ())

source :: IO (Events a, a -> IO ())
source = do
  listenersRef <- newIORef []
  let events = Events $ \callback -> do
        atomicModifyIORef' listenersRef $ \callbacks -> (callback : callbacks, ())
  let push a = do
        callbacks <- readIORef listenersRef
        mapM_ (\callback -> callback a) callbacks
  return (events, push)

sink :: (a -> IO ()) -> Events a -> IO ()
sink f (Events register) = do
  register f

keepAlive :: IO ()
keepAlive = forever $ threadDelay maxBound

instance Functor Events where
  fmap :: (a -> b) -> Events a -> Events b
  fmap f (Events registerA) = Events $ \callbackB -> do
    registerA $ \a -> callbackB $ f a

instance Semigroup (Events a) where
  (<>) :: Events a -> Events a -> Events a
  Events register1 <> Events register2 = Events $ \callback -> do
    register1 callback
    register2 callback

instance Monoid (Events a) where
  mempty :: Events a
  mempty = Events $ \_ -> return ()

flatten :: Events [a] -> Events a
flatten (Events register) = Events $ \callback -> do
  register $ \as -> Foldable.forM_ as callback

filterPredicate :: (a -> Bool) -> Events a -> Events a
filterPredicate predicate (Events register) = Events $ \callback -> do
  register $ \a -> Control.Monad.when (predicate a) $ callback a

filterMap :: (a -> Maybe b) -> Events a -> Events b
filterMap f (Events registerA) = Events $ \callbackB -> do
  registerA $ \a -> Foldable.forM_ (f a) callbackB

relax :: Events a -> Events (Union '[a])
relax events = inject <$> events

specialize :: (Member a u) => Events (Union u) -> Events a
specialize = filterMap project

