{-# LANGUAGE GADTs #-}

module Main (main) where

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

data FlattenedChannel m i o
  = FlatOutput o (Channel m i o)
  | FlatInput (i -> Channel m i o)
  | FlatAction (m (Channel m i o))

data Channel m i o
  = Output o (Channel m i o)
  | Input (i -> Channel m i o)
  | Action (m (Channel m i o))
  | forall x. Sequential (Channel m i x) (Channel m x o)
  | forall s a b. Parallel (Merge s a b o) (Channel m i o) (Channel m i o)

data Merge s a b o = Merge
  { initialState :: s,
    getState :: o -> s,
    inputA :: s -> a -> o,
    inputB :: s -> b -> o
  }

type Producer m o = Channel m Void o

type Consumer m i = Channel m i Void

type Final m = FlattenedChannel m Void Void

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

runFinal :: (Monad m) => Final m -> m ()
runFinal (FlatOutput _ _) = error "This should not happen" -- because of Void
runFinal (FlatInput _) = error "This should not happen" -- because of Void
runFinal (FlatAction mChannel) = runFinal =<< flatten =<< mChannel

flatten :: (Monad m) => Channel m i o -> m (FlattenedChannel m i o)
flatten (Output o channel) = FlatOutput o channel
flatten (Input f) = FlatInput f
flatten (Action action) = FlatAction action
flatten (Sequential left right) = flatten =<< sequential left right
flatten (Parallel merge left right) = flatten =<< parallel merge left right

sequential :: (Monad m) => Channel m i x -> Channel m x o -> m (Channel m i o)
sequential (Output x next) (Input f) = sequential next (f x)
sequential (Input f) right = return $ Input (\i -> Sequential (f i) right)
sequential left (Output o next) = return $ Output o $ Sequential left next
sequential (Action lAction) (Action rAction) = do
  left <- lAction
  right <- rAction
  sequential left right
sequential (Action lAction) right = do
  left <- lAction
  sequential left right
sequential left (Action rAction) = do
  right <- rAction
  sequential left right

parallel :: (Monad m) => Merge s a b o -> Channel m i a -> Channel m i b -> m (Channel m i o)
parallel (Merge {initialState, getState, inputA, inputB}) = parallel' initialState
  where
    parallel' :: (Monad m) => s -> Channel m i a -> Channel m i b -> m (Channel m i o)
    parallel' s (Input f1) (Input f2) =
      let merge' = Merge {initialState = s, getState, inputA, inputB}
       in return $ Input (\i -> Parallel merge' (f1 i) (f2 i))
    parallel' s (Output a left) (Output b right) =
      let oa = inputA s a
          ob = inputB (getState oa) b
       in Output oa . Output ob <$> parallel' (getState ob) left right
    parallel' s (Output a next) right =
      let o = inputA s a
       in Output o <$> parallel' (getState o) next right
    parallel' s left (Output b next) =
      let o = inputB s b
       in Output o <$> parallel' (getState o) left next
    parallel' s (Action lAction) (Action rAction) = do
      left <- lAction
      right <- rAction
      parallel' s left right
    parallel' s (Action lAction) right = do
      left <- lAction
      parallel' s left right
    parallel' s left (Action rAction) = do
      right <- rAction
      parallel' s left right

--

fibProducer :: Producer IO Int
fibProducer = go fibs
  where
    go [] = error "This should not happen" -- because there are infinite Fibonacci numbers
    go (n : rest) = Output n $ Action $ threadDelay 1000000 $> go rest
    fibs = 0 : 1 : zipWith (+) fibs (drop 1 fibs)

randomProducer :: Producer IO Int
randomProducer = Action $ threadDelay 1000000 >> randomIO <&> \n -> Output n randomProducer

double :: Channel m Int Int
double = Input (\i -> Output (2 * i) double)


printer :: (Show s) => Consumer IO s
printer = Input (\s -> Action $ print s $> printer)

main :: IO ()
main = runFinal $ flatten $ Sequential randomProducer printer
