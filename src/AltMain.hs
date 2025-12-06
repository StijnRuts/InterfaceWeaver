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

