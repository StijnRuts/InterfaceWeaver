{-# LANGUAGE GADTs #-}

module Main2 (main) where

import Control.Category (Category, (>>>))
import qualified Control.Category as C
import Control.Concurrent (threadDelay)
import Control.Monad (forever, void)
import Control.Monad.Free
import Data.Char (toUpper)
import Data.Void

--

data Channel m i o
  = Output o (Channel m i o)
  | Input (i -> Channel m i o)
  | forall a. LiftM (m a) (a -> Channel m i o)
  | forall a b. LiftM2 (m a) (m b) (a -> b -> Channel m i o)

type Producer m o = Channel m Void o

type Consumer m i = Channel m i Void

data Graph m i o
  = Embed (Channel m i o)
  | forall x. Sequential (Graph m i x) (Graph m x o)
  | forall a b s. Parallel (Merge a b s o) (Graph m i a) (Graph m i b)

data Merge a b s o = Merge
  { initialState :: s,
    getState :: o -> s,
    inputA :: s -> a -> o,
    inputB :: s -> b -> o
  }

statelessMerge :: (a -> o) -> (b -> o) -> Merge a b () o
statelessMerge fa fb =
  Merge
    { initialState = (),
      getState = const (),
      inputA = const fa,
      inputB = const fb
    }

sumMerge :: Merge a b () (Either a b)
sumMerge = statelessMerge Left Right

appendMerge :: Merge a a () a
appendMerge = statelessMerge id id

productMerge :: a -> b -> Merge a b (a, b) (a, b)
productMerge initA initB =
  Merge
    { initialState = (initA, initB),
      getState = id,
      inputA = flip (,) . snd,
      inputB = (,) . fst
    }

monoidMerge :: (Monoid a, Monoid b) => Merge a b (a, b) (a, b)
monoidMerge = productMerge mempty mempty

--

runChannel :: (Monad m) => Channel m Void Void -> m ()
runChannel (Output _ _) = error "This should not happen" -- because of Void
runChannel (Input _) = error "This should not happen" -- because of Void
runChannel (LiftM ma f) = runChannel . f =<< ma
runChannel (LiftM2 ma mb f) = (mb >>=) . (runChannel .) . f =<< ma

flattenGraph :: Graph m i o -> Channel m i o
flattenGraph (Embed channel) = channel
flattenGraph (Sequential graph1 graph2) = sequential (flattenGraph graph1) (flattenGraph graph2)
flattenGraph (Parallel merge graph1 graph2) = parallel merge (flattenGraph graph1) (flattenGraph graph2)

sequential :: Channel m i x -> Channel m x o -> Channel m i o
sequential (Output x next) (Input f) = sequential next (f x)
sequential (Input f) right = Input (\i -> sequential (f i) right)
sequential left (Output o next) = Output o $ sequential left next
sequential (LiftM ma f) right = LiftM ma (\a -> sequential (f a) right)
sequential left (LiftM ma f) = LiftM ma (\a -> sequential left (f a))
sequential (LiftM2 ma mb f) right = LiftM2 ma mb (\a b -> sequential (f a b) right)
sequential left (LiftM2 ma mb f) = LiftM2 ma mb (\a b -> sequential left (f a b))

parallel :: Merge a b s o -> Channel m i a -> Channel m i b -> Channel m i o
parallel (Merge {initialState, getState, inputA, inputB}) = parallel' initialState
  where
    parallel' s (Input f1) (Input f2) = Input (\i -> parallel' s (f1 i) (f2 i))
    parallel' s (Output a left) (Output b right) =
      let oa = inputA s a
          ob = inputB (getState oa) b
       in Output oa $ Output ob $ parallel' (getState ob) left right
    parallel' s (Output a next) right =
      let o = inputA s a
       in Output o $ parallel' (getState o) next right
    parallel' s left (Output b next) =
      let o = inputB s b
       in Output o $ parallel' (getState o) left next
    parallel' s (LiftM ma fa) (LiftM mb fb) = LiftM2 ma mb (\a b -> parallel' s (fa a) (fb b))
    parallel' s (LiftM ma fa) right = LiftM ma (\a -> parallel' s (fa a) right)
    parallel' s left (LiftM mb fb) = LiftM mb (\b -> parallel' s left (fb b))
    parallel' s (LiftM2 ma mb f) right = LiftM2 ma mb (\a b -> parallel' s (f a b) right)
    parallel' s left (LiftM2 ma mb f) = LiftM2 ma mb (\a b -> parallel' s left (f a b))

--

producer :: Producer IO Int
producer = go fibs
  where
    go [] = error "This should not happen" -- because there are infinite Fibonacci numbers
    go (n : rest) = Output n $ LiftM (threadDelay 1000000) (\() -> go rest)
    fibs = 0 : 1 : zipWith (+) fibs (drop 1 fibs)

double :: Channel m Int Int
double = Input (\i -> Output (2 * i) double)

showStr :: (Show s) => Channel m s String
showStr = Input (\i -> Output (show i) showStr)

consumer :: Consumer IO String
consumer = Input (\s -> LiftM (putStrLn s) (\() -> consumer))

--

main :: IO ()
main =
  runChannel $
    flattenGraph $
      Sequential (Embed producer) $
        Sequential (Embed double) $
          Sequential
            (Embed showStr)
            (Embed consumer)

