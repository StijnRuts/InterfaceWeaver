{-# LANGUAGE DataKinds #-}

module Data.Events where

import Control.Exception (evaluate)
import Control.Monad.State (State, runState)
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.List as List
import Data.Maybe (maybeToList)
import Data.Union (Member, Union (..), inject, project)
import InterfaceWeaver.App
import System.Directory (doesFileExist)

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

-- State-based Events

withStateIO :: IO s -> (s -> IO ()) -> ((a, s) -> (b, s)) -> App (Events a -> Events b)
withStateIO load save f = do
  ref <- run $ newIORef =<< load
  onShutdown $ readIORef ref >>= save
  return $ bindEvent $ toIO ref
  where
    toIO ref a = do
      s <- readIORef ref
      let (b, s') = f (a, s)
      writeIORef ref s'
      return [b]

withState :: s -> ((a, s) -> (b, s)) -> App (Events a -> Events b)
withState initial = withStateIO (return initial) (\_ -> return ())

withPersistentState :: (Read s, Show s) => FilePath -> s -> ((a, s) -> (b, s)) -> App (Events a -> Events b)
withPersistentState path initial = withStateIO load save
  where
    load = do
      exists <- doesFileExist path
      if exists
        then evaluate . read =<< readFile path
        else return initial
    save s = writeFile path $ show s


withStateM :: s -> (a -> State s b) -> App (Events a -> Events b)
withStateM initial f = withState initial $ \(a, s) -> runState (f a) s

withPersistentStateM :: (Read s, Show s) => FilePath -> s -> (a -> State s b) -> App (Events a -> Events b)
withPersistentStateM path initial f = withPersistentState path initial $ \(a, s) -> runState (f a) s

removeRepeats :: (Eq a) => App (Events a -> Events a)
removeRepeats = (flatten .) <$> withState Nothing update
  where
    update (a, Nothing) = ([a], Just a)
    update (a, Just prev) = if a /= prev then ([a], Just a) else ([], Just a)

