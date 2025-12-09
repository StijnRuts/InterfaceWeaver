{-# LANGUAGE GADTs #-}

module Main (main) where

-- import Control.Category (Category, (>>>))
-- import qualified Control.Category as C
import Control.Concurrent (threadDelay)
-- import Control.Monad (forever, void)
-- import Control.Monad.Free
import Data.Functor (($>), (<&>))
import Data.Void
import System.Random (randomIO)

--

data Channel m i o
  = Output o (m (Channel m i o))
  | Input (i -> m (Channel m i o))
  | Action (m (Channel m i o))

data Graph m i o
  = Embed (Channel m i o)
  | forall x. Sequential (Graph m i x) (Graph m x o)
  | Parallel (Graph m i o) (Graph m i o)

type Producer m o = Channel m Void o

type Consumer m i = Channel m i Void

type Final m = Channel m Void Void

type Program m = Graph m Void Void

{-
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
-}

--

runFinal :: (Monad m) => Final m -> m ()
runFinal (Output _ _) = error "This should not happen" -- because of Void
runFinal (Input _) = error "This should not happen" -- because of Void
runFinal (Action mChannel) = runFinal =<< mChannel

flatten :: (Monad m) => Graph m i o -> m (Channel m i o)
flatten (Embed channel) = return channel
flatten (Sequential left right) = do
  l <- flatten left
  r <- flatten right
  sequential l r
flatten (Parallel left right) = do
  l <- flatten left
  r <- flatten right
  parallel l r

sequential :: (Monad m) => Channel m i x -> Channel m x o -> m (Channel m i o)
sequential (Output x next) (Input f) = do
  left <- next
  right <- f x
  sequential left right
sequential (Input f) right =
  return $
    Input
      ( \i -> do
          left <- f i
          sequential left right
      )
sequential left (Output o next) = return $ Output o $ do
  right <- next
  sequential left right
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

parallel :: (Monad m) => Channel m i o -> Channel m i o -> m (Channel m i o)
parallel (Input f1) (Input f2) =
  return $
    Input
      ( \i -> do
          left <- f1 i
          right <- f2 i
          parallel left right
      )
parallel (Output o1 lNext) (Output o2 rNext) = do
  left <- lNext
  right <- rNext
  return $ Output o1 $ return $ Output o2 $ parallel left right
parallel (Output o next) right = do
  left <- next
  return $ Output o $ parallel left right
parallel left (Output o next) = do
  right <- next
  return $ Output o $ parallel left right
parallel (Action lAction) (Action rAction) = do
  left <- lAction
  right <- rAction
  parallel left right
parallel (Action lAction) right = do
  left <- lAction
  parallel left right
parallel left (Action rAction) = do
  right <- rAction
  parallel left right

--

fibProducer :: Producer IO Int
fibProducer = go fibs
  where
    go [] = error "This should not happen" -- because there are infinite Fibonacci numbers
    go (n : rest) = Output n $ return $ Action $ threadDelay 1500000 $> go rest
    fibs = 0 : 1 : zipWith (+) fibs (drop 1 fibs)

randomProducer :: Producer IO Int
randomProducer = Action $ threadDelay 1000000 >> randomIO <&> \n -> Output n (return randomProducer)

double :: (Monad m) => Channel m Int Int
double = Input (\i -> return $ Output (2 * i) (return double))


printer :: (Show s) => Consumer IO s
printer = Input (\s -> return $ Action $ print s $> printer)

program :: Program IO
program = Sequential (Sequential (Embed fibProducer) (Embed double)) (Embed printer)

main :: IO ()
main = runFinal =<< flatten program
