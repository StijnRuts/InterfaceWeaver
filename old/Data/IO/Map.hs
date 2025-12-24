module Data.IO.Map where

import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map (Map)
import qualified Data.Map as Map

newtype IOMap k a = IOMap (IORef (Map k a))

new :: IO (IOMap k a)
new = IOMap <$> newIORef Map.empty

add :: (Ord k) => IOMap k a -> k -> a -> IO ()
add (IOMap ref) k a = atomicModifyIORef' ref $ \old -> (Map.insert k a old, ())

get :: IOMap k a -> IO (Map k a)
get (IOMap ref) = readIORef ref

delete :: (Ord k) => k -> IOMap k a -> IO (Maybe a)
delete k (IOMap ref) = atomicModifyIORef' ref $ \old -> (Map.delete k old, Map.lookup k old)
