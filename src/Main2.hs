{-# LANGUAGE GADTs #-}

module Main2 (main) where

import Control.Category (Category, (>>>))
import qualified Control.Category as C
import Control.Concurrent (threadDelay)
import Control.Monad (forever, void)
import Control.Monad.Free
import Data.Char (toUpper)
import Data.Functor (($>), (<&>))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Void
import System.Random

--

data Channel m i o
  = Output o (Channel m i o)
  | Input (i -> Channel m i o)
  | Actions (ActionsList m i o)
  | forall x. Sequential (Channel m i x) (Channel m x o)
  | forall s a b. Parallel (Merge s a b o) (Channel m i o) (Channel m i o)

type Producer m o = Channel m Void o

type Consumer m i = Channel m i Void

data ActionsList m i o
  = Single (m (Channel m i o))
  | forall i1 o1 i2 o2. Chain
      (Channel m i1 o1 -> Channel m i2 o2 -> Channel m i o)
      (ActionsList m i1 o1)
      (ActionsList m i2 o2)

runActionsList :: ActionsList m i o -> m (Channel m i o)
runActionsList (Single mChannel) = mChannel
runActionsList (Chain merge left right) = merge <$> runActionsList left <*> runActionsList right

data Merge s a b o = Merge
  { initialState :: s,
    getState :: o -> s,
    inputA :: s -> a -> o,
    inputB :: s -> b -> o
  }

statelessMerge :: (a -> o) -> (b -> o) -> Merge () a b o
statelessMerge fa fb =
  Merge
    { initialState = (),
      getState = const (),
      inputA = const fa,
      inputB = const fb
    }

sumMerge :: Merge () a b (Either a b)
sumMerge = statelessMerge Left Right

appendMerge :: Merge () a a a
appendMerge = statelessMerge id id

productMerge :: a -> b -> Merge (a, b) a b (a, b)
productMerge initA initB =
  Merge
    { initialState = (initA, initB),
      getState = id,
      inputA = flip (,) . snd,
      inputB = (,) . fst
    }

monoidMerge :: (Monoid a, Monoid b) => Merge (a, b) a b (a, b)
monoidMerge = productMerge mempty mempty

--

runChannel :: (Monad m) => Channel m Void Void -> m ()
runChannel (Output _ _) = error "This should not happen" -- because of Void
runChannel (Input _) = error "This should not happen" -- because of Void
runChannel (Actions actions) = runChannel =<< runActionsList actions
runChannel (Sequential left right) = runChannel $ sequential left right
runChannel (Parallel merge left right) = runChannel $ parallel merge left right

sequential :: Channel m i x -> Channel m x o -> Channel m i o
sequential (Output x next) (Input f) = sequential next (f x)
sequential (Input f) right = Input (\i -> sequential (f i) right)
sequential left (Output o next) = Output o $ sequential left next
sequential (Actions left) (Actions right) = Actions $ Chain sequential left right
sequential (Actions left) right = _
sequential left (Actions right) = _

parallel :: Merge s a b o -> Channel m i a -> Channel m i b -> Channel m i o
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
    parallel' s (Actions left) (Actions right) = Actions $ Chain (parallel' s) left right
    parallel' s (Actions left) right = _
    parallel' s left (Actions right) = _

--

fibProducer :: Producer IO Int
fibProducer = go fibs
  where
    go [] = error "This should not happen" -- because there are infinite Fibonacci numbers
    go (n : rest) = Output n $ Actions $ Single (threadDelay 1000000 $> go rest)
    fibs = 0 : 1 : zipWith (+) fibs (drop 1 fibs)

randomProducer :: Producer IO Int
randomProducer = Actions $ Single (threadDelay 1000000 >> randomIO <&> \n -> Output n randomProducer)

double :: Channel m Int Int
double = Input (\i -> Output (2 * i) double)


printer :: (Show s) => Consumer IO s
printer = Input (\s -> Actions $ Single (print s $> printer))

--

main :: IO ()
main = runChannel $ Sequential randomProducer printer
