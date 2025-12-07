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
  | Input (i -> (Channel m i o))
  | forall a. LiftM (m a) (a -> Channel m i o)

type Producer m o = Channel m Void o
type Consumer m i = Channel m i Void

data Graph m i o
  = Embed (Channel m i o) -- Graph m i o
  | forall x. Sequential (Graph m i x) (Graph m x o) -- Graph m i o
  -- | forall a b. Parallel o (a -> o -> o) (b -> o -> o) (Graph m i a) (Graph m i b) -- Graph m i o
  -- APPEND :: (a -> _ -> a, _ -> a -> a)
  -- SUM :: (a -> _ -> Left a, b -> _ -> Right b)
  -- PRODUCT :: (a -> (_,b) -> (a,b), b -> (a,_) -> (a,b))

runChannel :: Monad m => Channel m Void Void -> m ()
runChannel (Output _ _) = error "This should not happen" -- because of Void
runChannel (Input _) = error "This should not happen" -- because of Void
runChannel (LiftM ma f) = runChannel . f =<< ma

flattenGraph :: Graph m i o -> Channel m i o
flattenGraph (Embed channel) = channel
flattenGraph (Sequential graph1 graph2) = sequential (flattenGraph graph1) (flattenGraph graph2)

sequential :: Channel m i x -> Channel m x o -> Channel m i o
sequential (Output x next) (Input f) = sequential next (f x)
sequential (Input f) right = Input (\i -> sequential (f i) right)
sequential left (Output o next) = Output o $ sequential left next
sequential (LiftM ma f) right = LiftM ma (\a -> sequential (f a) right)
sequential left (LiftM ma f) = LiftM ma (\a -> sequential left (f a))

--

producer :: Producer IO Int
producer = go fibs
  where
    go (n : rest) = Output n $ LiftM (threadDelay 1000000) (\() -> go rest)
    fibs = 0 : 1 : zipWith (+) fibs (drop 1 fibs)

double :: Channel m Int Int
double = Input (\i -> Output (2*i) double)

showStr :: (Show s) => Channel m s String
showStr = Input (\i -> Output (show i) showStr)

consumer :: Consumer IO String
consumer = Input (\s -> LiftM (putStrLn s) (\() -> consumer))

--

main :: IO ()
main = runChannel $ flattenGraph $
  Sequential (Embed producer) $
    Sequential (Embed double) $
      Sequential
        (Embed showStr)
        (Embed consumer)
