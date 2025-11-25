module Data.IOSeq where

import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq

newtype IOSeq a = IOSeq (IORef (Seq a))

new :: IO (IOSeq a)
new = IOSeq <$> newIORef Seq.empty

add :: IOSeq a -> a -> IO ()
add (IOSeq ref) a = atomicModifyIORef' ref $ \old -> (old |> a, ())

get :: IOSeq a -> IO (Seq a)
get (IOSeq ref) = readIORef ref

empty :: IOSeq a -> IO (Seq a)
empty (IOSeq ref) = atomicModifyIORef' ref (Seq.empty,)
