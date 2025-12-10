{-# LANGUAGE GADTs #-}

module Main (main) where

import Control.Category (Category)
import qualified Control.Category as C
import Control.Concurrent (threadDelay)
import Control.Monad (forever, when)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Free
import Data.Functor (($>), (<&>))
import Data.Profunctor
import Data.Void
import System.Random (randomRIO)
import Witherable

--

data ChannelF i o next
  = Output o next
  | Input (i -> next)
  deriving (Functor)

type ChannelT i o m a = FreeT (ChannelF i o) m a

output :: (Monad m) => o -> ChannelT i o m ()
output o = liftF (Output o ())

input :: (Monad m) => ChannelT i o m i
input = liftF (Input id)

--

fiveProducer :: ChannelT Void Int IO ()
fiveProducer = do
  output 1
  lift $ threadDelay 1000000
  output 2
  lift $ threadDelay 1000000
  output 3
  lift $ threadDelay 1000000
  output 4
  lift $ threadDelay 1000000
  output 5

fibProducer :: ChannelT Void Int IO ()
fibProducer = go 0 1
  where
    go a b = do
      output a
      lift $ threadDelay 1000000
      go b (a + b)

randomProducer :: ChannelT Void Int IO ()
randomProducer = forever $ do
  n <- lift $ randomRIO (0, 999)
  output n
  lift $ threadDelay 1000000

transform :: (Monad m) => (a -> b) -> ChannelT a b m ()
transform f = forever $ do
  a <- input
  output (f a)

matching :: (Monad m) => (a -> Bool) -> ChannelT a a m ()
matching predicate = forever $ do
  a <- input
  when (predicate a) (output a)

printer :: (Show s) => ChannelT s Void IO ()
printer = forever $ do
  s <- input
  lift $ print s

--

data Graph m a i o
  = Embed (ChannelT i o m a)
  | forall x. Sequential (Graph m a i x) (Graph m a x o)
  | Parallel (Graph m a i o) (Graph m a i o)

instance Semigroup (Graph m a i o) where
  (<>) :: Graph m a i o -> Graph m a i o -> Graph m a i o
  (<>) = Parallel

instance (Monad m) => Category (Graph m a) where
  id :: Graph m a x x
  id = Embed $ forever $ input >>= output
  (.) :: Graph m a x o -> Graph m a i x -> Graph m a i o
  (.) = flip Sequential

instance (Monad m) => Functor (Graph m a i) where
  fmap :: (o -> o') -> Graph m a i o -> Graph m a i o'
  fmap = rmap

instance (Monad m) => Profunctor (Graph m a) where
  dimap :: (i' -> i) -> (o -> o') -> Graph m a i o -> Graph m a i' o'
  dimap fi fo (Sequential left right) = Sequential (lmap fi left) (rmap fo right)
  dimap fi fo (Parallel left right) = Parallel (dimap fi fo left) (dimap fi fo right)
  dimap fi fo (Embed channel) = Embed (transFreeT (dimapChan fi fo) channel)
    where
      dimapChan :: (i' -> i) -> (o -> o') -> ChannelF i o next -> ChannelF i' o' next
      dimapChan _ fo (Output o next) = Output (fo o) next
      dimapChan fi _ (Input next) = Input (next . fi)

instance (Monad m) => Filterable (Graph m a i) where
  mapMaybe :: (o -> Maybe o') -> Graph m a i o -> Graph m a i o'
  mapMaybe p (Sequential left right) = Sequential left (mapMaybe p right)
  mapMaybe p (Parallel left right) = Parallel (mapMaybe p left) (mapMaybe p right)
  mapMaybe p (Embed channel) = Embed (mapMaybeChanT p channel)
    where
      mapMaybeChanT :: (o -> Maybe o') -> ChannelT i o m a -> ChannelT i o' m a
      mapMaybeChanT p chan = FreeT $ do
        step <- runFreeT chan
        case step of
          Pure a ->  pure $ Pure a
          Free (Input next) -> pure $ Free $ Input $ \i -> mapMaybeChanT p (next i)
          Free (Output o next) ->
            case p o of
              Just o' -> pure $ Free $ Output o' $ mapMaybeChanT p next
              Nothing -> runFreeT $ mapMaybeChanT p next

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

program :: Program IO
program = Sequential (Sequential (Embed fibProducer) (Embed double)) (Embed printer)

main :: IO ()
main = runFinal =<< flatten program
-}
