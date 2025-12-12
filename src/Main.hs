{-# LANGUAGE PatternSynonyms #-}

module Main (main) where

import Control.Category (Category, (>>>))
import qualified Control.Category as C
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Free
import Data.Profunctor
import Data.Void
import System.Random (randomRIO)
import Witherable

-- Type definitions

data ChannelF i o next
  = Input (i -> next)
  | Output o next
  deriving (Functor)

type ChannelT i o m a = FreeT (ChannelF i o) m a

newtype Channel m a i o = Channel (ChannelT i o m a)

type Producer m a o = Channel m a Void o

type IOProducer o = Producer IO () o

type Consumer m a i = Channel m a i Void

type IOConsumer i = Consumer IO () i

type Program m a = Channel m a Void Void

type IOProgram = Program IO ()

input :: (Monad m) => ChannelT i o m i
input = liftF $ Input id

output :: (Monad m) => o -> ChannelT i o m ()
output o = liftF $ Output o ()

-- Internal synonyms for shorter code

pattern InputF next = Free (Input next)

pattern OutputF o next = Free (Output o next)

input' = pure . Free . Input

output' = pure . Free . Output

pure' = pure . Pure

onChan f (Channel chan) = Channel $ f chan

onChan2 f (Channel chan1) (Channel chan2) = Channel $ f chan1 chan2

runChanT run chanT = FreeT $ bind run (runFreeT chanT)

runChanT2 run chanT1 chanT2 = FreeT $ liftA2 run (runFreeT chanT1) (runFreeT chanT2)

-- TODO concurency

-- Typeclass instances

instance Semigroup (Channel m a i o) where
  (<>) :: Channel m a i o -> Channel m a i o -> Channel m a i o
  lChan <> rChan = onChan2 (runChanT2 parallel) lChan rChan
    where
      parallel (InputF f1) (InputF f2) = input' $ \i -> runChanT2 parallel (f1 i) (f2 i)
      parallel (OutputF o1 next1) (OutputF o2 next2) = output' o1 $ output' o2 $ runChanT2 parallel next1 next2
      parallel (OutputF o next) right = output' o $ runChanT2 parallel next right
      parallel left (OutputF o next) = output' o $ runChanT2 parallel left next
      parallel left (Pure _) = left
      parallel (Pure _) right = right

instance (Monad m) => Category (Channel m a) where
  id :: Channel m a x x
  id = Channel $ forever $ input >>= output
  (.) :: Channel m a x o -> Channel m a i x -> Channel m a i o
  oChan . iChan = onChan2 (runChanT2 sequential) iChan oChan
    where
      sequential (OutputF x next) (InputF f) = runChanT2 sequential next (f x)
      sequential left (OutputF o next) = output' o $ runChanT2 sequential left next
      sequential (InputF f) right = input' $ \i -> runChanT2 sequential (f i) right
      sequential (Pure _) (Pure a) = pure' a
      sequential (Pure a) (InputF _) = pure' a
      sequential (OutputF _ _) (Pure a) = pure' a

instance (Monad m) => Functor (Channel m a i) where
  fmap :: (o -> o') -> Channel m a i o -> Channel m a i o'
  fmap = rmap

instance (Monad m) => Profunctor (Channel m a) where
  dimap :: (i' -> i) -> (o -> o') -> Channel m a i o -> Channel m a i' o'
  dimap fi fo (Channel chan) = Channel $ transFreeT go chan
    where
      go (Input next) = Input (next . fi)
      go (Output o next) = Output (fo o) next

instance (Monad m) => Filterable (Channel m a i) where
  mapMaybe :: (o -> Maybe o') -> Channel m a i o -> Channel m a i o'
  mapMaybe p = onChan (runChanT go)
    where
      go (Pure a) = pure' a
      go (InputF next) = input' $ runChanT go next
      go (OutputF o next) = case p o of
        Just o' -> output' o' $ runChanT go next
        Nothing -> runChanT go next

{-
-- https://hackage.haskell.org/package/base-4.21.0.0/docs/Control-Arrow.html

(^>>) :: Arrow a => (b -> c) -> a c d -> a b d
(>>^) :: Arrow a => a b c -> (c -> d) -> a b d

(>|>) :: Filterable f => f a -> (a -> Bool) -> f a
(>|>) = flip filter

(>>|) :: Filterable f => f a -> (a -> Maybe b) -> f b
(>>|) = flip mapMaybe

https://hackage-content.haskell.org/package/profunctors-5.6.3/docs/Data-Profunctor-Choice.html
https://hackage-content.haskell.org/package/profunctors-5.6.3/docs/Data-Profunctor-Strong.html
-}

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

-- Runners

runChannel :: (Monad m) => m i -> (o -> m ()) -> Channel m a i o -> m a
runChannel get put (Channel chan) = iterT go chan
  where
    go (Input next) = get >>= next
    go (Output o next) = put o >> next

runProgram :: (Monad m) => Program m a -> m a
runProgram = runChannel (error "Can't input Void") (error "Can't output Void")

-- Main program

fiveProducer :: IOProducer Int
fiveProducer = Channel $ do
  output 1
  lift $ threadDelay 1000000
  output 2
  lift $ threadDelay 1000000
  output 3
  lift $ threadDelay 1000000
  output 4
  lift $ threadDelay 1000000
  output 5

fibProducer :: IOProducer Int
fibProducer = Channel $ go 0 1
  where
    go a b = do
      output a
      lift $ threadDelay 1000000
      go b (a + b)

randomProducer :: IOProducer Int
randomProducer = Channel $ forever $ do
  n <- lift $ randomRIO (0, 999)
  output n
  lift $ threadDelay 1000000

printer :: (Show s) => IOConsumer s
printer = Channel $ forever $ do
  s <- input
  lift $ print s

runner :: IOConsumer (IO ())
runner = Channel $ forever $ lift =<< input

main :: IO ()
main = runProgram $ fibProducer >>> printer
