{-# LANGUAGE GADTs #-}

module Main (main) where

import Control.Category
import Control.Concurrent (threadDelay)
-- import Control.Monad (forever, void)
import Data.Functor (($>), (<&>))
import Data.Profunctor
import Data.Void
import System.Random (randomIO)

--

data Channel m i o
  = Output o (m (Channel m i o))
  | Input (i -> m (Channel m i o))
  | Action (m (Channel m i o))
  | Stop

instance (Functor m) => Functor (Channel m i) where
  fmap :: (o -> o') -> Channel m i o -> Channel m i o'
  fmap = rmap

instance (Functor m) => Profunctor (Channel m) where
  lmap :: (i' -> i) -> Channel m i o -> Channel m i' o
  lmap f (Output o next) = Output o $ lmap f <$> next
  lmap f (Input next) = Input (\i' -> lmap f <$> next (f i'))
  lmap f (Action next) = Action $ lmap f <$> next
  lmap _ Stop = Stop

  rmap :: (o -> o') -> Channel m i o -> Channel m i o'
  rmap f (Output o next) = Output (f o) $ rmap f <$> next
  rmap f (Input next) = Input (\i -> rmap f <$> next i)
  rmap f (Action next) = Action $ rmap f <$> next
  rmap _ Stop = Stop

instance (Monad m) => Applicative (Channel m i) where
  pure :: o -> Channel m i o
  pure o = Output o $ return (pure o)

  (<*>) :: Channel m i (o -> o') -> Channel m i o -> Channel m i o'
  (Output f left) <*> (Output o right) = Output (f o) (liftA2 (<*>) left right)
  (Action left) <*> (Action right) = Action (liftA2 (<*>) left right)
  (Action left) <*> right = Action (liftA2 (<*>) left (pure right))
  left <*> (Action right) = Action (liftA2 (<*>) (pure left) right)
  (Input lNext) <*> (Input rNext) = Input (\i -> liftA2 (<*>) (lNext i) (rNext i))
  (Input lNext) <*> right = Input (\i -> liftA2 (<*>) (lNext i) (pure right))
  left <*> (Input rNext) = Input (\i -> liftA2 (<*>) (pure left) (rNext i))
  Stop <*> Stop = Stop
  Stop <*> (Output _ _) = Stop
  (Output _ _) <*> Stop = Stop

instance (Monad m) => Monad (Channel m i) where
  return :: o -> Channel m i o
  return = pure

  (>>=) :: Channel m i o -> (o -> Channel m i o') -> Channel m i o'
  (Output o lNext) >>= f = cont (f o)
    where
      cont (Output o' rNext) = Output o' (cont <$> rNext)
      cont (Input rNext) = Input (\i -> cont <$> rNext i)
      cont (Action rNext) = Action (cont <$> rNext)
      cont Stop = Action (fmap (>>= f) lNext)
  (Input next) >>= f = Input (\i -> fmap (>>= f) (next i))
  (Action next) >>= f = Action (fmap (>>= f) next)
  Stop >>= _ = Stop

--

data Graph m i o
  = Embed (Channel m i o)
  | forall x. Sequential (Graph m i x) (Graph m x o)
  | Parallel (Graph m i o) (Graph m i o)

instance (Functor m) => Functor (Graph m i) where
  fmap :: (o -> o') -> Graph m i o -> Graph m i o'
  fmap = rmap

instance (Functor m) => Profunctor (Graph m) where
  lmap :: (i' -> i) -> Graph m i o -> Graph m i' o
  lmap f (Embed channel) = Embed (lmap f channel)
  lmap f (Sequential left right) = Sequential (lmap f left) right
  lmap f (Parallel left right) = Parallel (lmap f left) (lmap f right)

  rmap :: (o -> o') -> Graph m i o -> Graph m i o'
  rmap f (Embed channel) = Embed (rmap f channel)
  rmap f (Sequential left right) = Sequential left (rmap f right)
  rmap f (Parallel left right) = Parallel (rmap f left) (rmap f right)

instance Semigroup (Graph m i o) where
  (<>) :: Graph m i o -> Graph m i o -> Graph m i o
  (<>) = Parallel

instance Monoid (Graph m i o) where
  mempty :: Graph m i o
  mempty = Embed Stop

instance (Monad m) => Category (Graph m) where
  id :: Graph m a a
  id = Embed $ id'
    where
      id' = Input (\a -> return $ Output a (return id'))

  (.) :: Graph m x o -> Graph m i x -> Graph m i o
  (.) = flip Sequential

{-
-}

--

type Producer m o = Channel m Void o

type Consumer m i = Channel m i Void

type Final m = Channel m Void Void

type Program m = Graph m Void Void

--

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
runFinal Stop = return ()

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
parallel left (Output o next) = {- HLint ignore -} do
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
