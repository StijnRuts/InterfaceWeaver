{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module AltMain (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan
import Control.Monad (forever)
import Data.Function ((&))
import Polysemy
import Polysemy.Input
import Polysemy.Internal.CustomErrors (FirstOrder)
import Polysemy.Output

type Producer o = forall r. (Member (Output o) r) => Sem r ()

type IOProducer o = forall r. (Members '[Output o, Embed IO] r) => Sem r ()

type Consumer i = forall r. (Member (Input i) r) => Sem r ()

type IOConsumer i = forall r. (Members '[Input i, Embed IO] r) => Sem r ()

type Pipe i o = forall r. (Members '[Input i, Output o] r) => Sem r ()

type IOPipe i o = forall r. (Members '[Input i, Output o, Embed IO] r) => Sem r ()

type Interpret from to = forall r. (Member to r) => Sem (from ': r) () -> Sem r ()

type IOInterpret from = Interpret from (Embed IO)

program :: Pipe String String
program = do
  output "What is your name?"
  name <- input
  output $ "Hello, " ++ name ++ "!"

runInputLine :: IOInterpret (Input String)
runInputLine = interpret $ \Input -> embed getLine

runOutputLine :: IOInterpret (Output String)
runOutputLine = interpret $ \(Output s) -> embed (putStrLn s)

-- main :: IO ()
-- main = runM . runOutputLine . runInputLine $ program
-- main = program & runInputLine & runOutputLine & runM
-- main = runM . runInputLine $ program & runOutputLine

fibProducer :: IOProducer Integer
fibProducer = mapM_ (\f -> output f >> embed (threadDelay 1000000)) fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (drop 1 fibs)

primeProducer :: IOProducer Integer
primeProducer = mapM_ (\p -> output p >> embed (threadDelay 5000000)) primes
  where
    primes = sieve [2 ..]
    sieve (p : xs) = p : sieve [x | x <- xs, x `mod` p /= 0]
    sieve [] = []

logger :: Pipe Integer String
logger = forever $ do
  x <- input @Integer
  output ("Value: " ++ show x)

chain ::
  (Member (Embed IO) r) =>
  Sem (Output Integer ': r) () ->
  Sem (Input Integer ': r) () ->
  Sem r ()
chain producer consumer = do
  (chan :: Chan Integer) <- embed newChan
  -- needs forkIO
  runOutputSem (embed . writeChan chan) producer
  runInputSem (embed $ readChan chan) consumer

main :: IO ()
main = runM . runOutputLine $ (fibProducer `chain` logger)

{-
a function to split a producer to multiple consumers

a function to combine multiple producers into one consumer
  - (all the same type) get whatever the latest value is
     (<>) Producer a -> Producer a -> Producer a
     append = sum id id
  - Foldable t => t (Producer a) -> Producer a
  - or, get a Product type of the latest value of each
     Producer a -> Producer b -> Producer (a, b)
     LiftA2 (,)
     liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
     product = liftA2
     requires Monoid for events that have not fired yet?
  - or, get an Sum type of the last value
     Producer a -> Producer b -> Producer (Either a b)
     either :: (a -> c) -> (b -> c) -> Either a b -> c
     liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
     the exact type (a -> c) -> (b -> c) -> f a -> f b -> f c is known as liftA2 (either)
     sum = (aToC eg Left <$> eventsA) <> (bToC eg Right <$> eventsB)

Generalize Sum and Product:
c -> ((a -> c -> c), (b -> c -> c))

(a -> _ -> Left a, b -> _ -> Right b)
(a -> (_,b) -> (a,b), b -> (a,_) -> (a,b))

sum, product (and pipe?) are (Free) Applicatives !

(a, b) -> Signal (Either a b) -> Signal (a, b)

-- Example program: independent tasks combined applicatively
program :: FreeAp Task (String, Int)
program = (,) <$> fetch "http://example.com" <*> compute 42

-- Interpreter: run tasks in parallel
runTask :: Task a -> IO a
runTask (Fetch url) = do
  putStrLn $ "Fetching from " ++ url
  pure ("<html>dummy content from " ++ url ++ "</html>")
runTask (Compute n) = do
  putStrLn $ "Computing square of " ++ show n
  pure (n * n)

-- Parallel runner using async
runParallel :: FreeAp Task a -> IO a
runParallel = runAp (async . runTask) >=> \as -> do
  results <- mapConcurrently wait as
  pure (sequenceA results)

main :: IO ()
main = do
  result <- runParallel program
  print result

If you want parallelism inside Polysemy, you can:
- Define an effect like Parallel :: [Sem r a] -> Sem r [a].
- Write an interpreter that uses async or mapConcurrently to run those sub-computations.
- Internally, you could model those sub-computations as a free applicative program if you want to guarantee independence.

data Task m a where
  Fetch   :: String -> Task m String
  Compute :: Int -> Task m Int

makeSem ''Task  -- Polysemy boilerplate

-- Free Applicative program inside Polysemy
program :: FreeAp (Task (Sem r)) (String, Int)
program = (,) <$> liftAp (Fetch "http://example.com")
              <*> liftAp (Compute 42)

-- Interpreter: run in parallel using async
runTaskParallel :: Member (Embed IO) r => Task (Sem r) a -> Sem r a
runTaskParallel (Fetch url) = embed $ download url
runTaskParallel (Compute n) = embed $ pure (n * n)

- Polysemy defines the effect system (Task).
- Free Applicative structures the program so tasks can be parallelized.
- The interpreter (runTaskParallel) decides how to execute them.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

import Polysemy
import Polysemy.Embed
import Control.Applicative.Free
import Control.Concurrent.Async

-- Define a Polysemy effect
data Task m a where
  Fetch   :: String -> Task m String
  Compute :: Int -> Task m Int
  deriving Functor

makeSem ''Task  -- generates fetch, compute smart constructors

-- Lift into Free Applicative
fetchFA :: String -> FreeAp (Task (Sem r)) String
fetchFA url = liftAp (Fetch url)

computeFA :: Int -> FreeAp (Task (Sem r)) Int
computeFA n = liftAp (Compute n)

-- Example program: independent tasks combined applicatively
program :: FreeAp (Task (Sem r)) (String, Int)
program = (,) <$> fetchFA "http://example.com" <*> computeFA 42

-- Interpreter: run tasks in parallel using async
runTaskParallel :: Member (Embed IO) r => Task (Sem r) a -> Sem r a
runTaskParallel (Fetch url) = embed $ do
  putStrLn $ "Fetching from " ++ url
  pure ("<html>dummy content from " ++ url ++ "</html>")
runTaskParallel (Compute n) = embed $ do
  putStrLn $ "Computing square of " ++ show n
  pure (n * n)

-- Fold the Free Applicative into Polysemy
runParallelFA :: Member (Embed IO) r => FreeAp (Task (Sem r)) a -> Sem r a
runParallelFA fa = do
  -- runAp collects async actions
  asyncs <- runAp (embed . async . interpretTask) fa
  results <- embed $ mapConcurrently wait asyncs
  pure (sequenceA results)
  where
    interpretTask :: Task (Sem r) a -> IO a
    interpretTask (Fetch url) = do
      putStrLn $ "Fetching from " ++ url
      pure ("<html>dummy content from " ++ url ++ "</html>")
    interpretTask (Compute n) = do
      putStrLn $ "Computing square of " ++ show n
      pure (n * n)

main :: IO ()
main = do
  result <- runM $ runParallelFA program
  print result

-}
