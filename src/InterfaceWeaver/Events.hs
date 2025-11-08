module InterfaceWeaver.Events where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import qualified Control.Monad
import qualified Data.Foldable
import Data.IORef (atomicModifyIORef', newIORef, readIORef)

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

sink :: Events a -> (a -> IO ()) -> IO ()
sink (Events register) = register

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

