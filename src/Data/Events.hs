{-# LANGUAGE DataKinds #-}

module Data.Events where

import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.List as List
import Data.Maybe (maybeToList)
import Data.Union (Member, Union (..), inject, project)

newtype Events a = Events ((a -> IO ()) -> IO ())

-- Sourcing and sinking events

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

-- Transforming Events

bindEvent :: (a -> IO [b]) -> Events a -> Events b
bindEvent f (Events register) =
  Events $ \callback ->
    register $ \a -> do
      bs <- f a
      mapM_ callback bs

instance Functor Events where
  fmap :: (a -> b) -> Events a -> Events b
  fmap f = bindEvent $ pure . List.singleton . f

instance Semigroup (Events a) where
  (<>) :: Events a -> Events a -> Events a
  Events register1 <> Events register2 = Events $ \callback -> do
    register1 callback
    register2 callback

instance Monoid (Events a) where
  mempty :: Events a
  mempty = Events $ \_ -> return ()

flatten :: Events [a] -> Events a
flatten = bindEvent pure

filterPredicate :: (a -> Bool) -> Events a -> Events a
filterPredicate predicate = bindEvent $ \a -> if predicate a then pure [a] else pure []

filterMap :: (a -> Maybe b) -> Events a -> Events b
filterMap f = bindEvent $ pure . maybeToList . f

-- Events of Union types

relax :: (Member a u) => Events a -> Events (Union u)
relax events = inject <$> events

specialize :: (Member a u) => Events (Union u) -> Events a
specialize = filterMap project

relaxF :: (Member a u, Member b v) => (Events a -> Events b) -> Events (Union u) -> Events (Union v)
relaxF f = relax . f . specialize

specializeF :: (Member a u, Member b v) => (Events (Union u) -> Events (Union v)) -> Events a -> Events b
specializeF f = specialize . f . relax

