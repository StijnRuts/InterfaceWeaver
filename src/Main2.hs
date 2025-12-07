{-# LANGUAGE GADTs #-}

module Main2 (main) where

import Control.Category (Category, (>>>))
import qualified Control.Category as C
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.Free
import Data.Char (toUpper)
import Data.Void

--

data ChannelF i o next
  = Emit o next
  | Await (i -> next)
  deriving (Functor)

type Channel i o = Free (ChannelF i o)

type Transducer i o = Channel i o ()

type Producer o = Transducer Void o

type Consumer i = Transducer i Void

emit :: o -> Channel i o ()
emit o = liftF $ Emit o ()

await :: Channel i o i
await = liftF $ Await id

data Wire i o where
  FromTransducer :: Transducer i o -> Wire i o
  Seqential :: Wire a b -> Wire b c -> Wire a c
  Parallel :: Wire a b -> Wire a c -> Wire a (b, c)
  Merge :: Wire a b -> Wire a c -> Wire a (Either b c)

--

producer :: Producer Int
producer = go 1
  where
    go n = emit n >> go (n + 1)

double :: Transducer Int Int
double = forever $ do
  x <- await
  emit $ 2 * x

showStr :: Transducer Int String
showStr = forever $ do
  x <- await
  emit $ show x

collector :: Transducer String String
collector = forever $ do
  s <- await
  emit $ "Collected: " ++ s

--

runTransducer :: Transducer i o -> [i] -> [o]
runTransducer (Pure ()) _ = []
runTransducer (Free (Emit x next)) xs = x : runTransducer next xs
runTransducer (Free (Await next)) [] = [] -- no more input, stop
runTransducer (Free (Await next)) (x : xs) = runTransducer (next x) xs

runWire :: Wire i o -> [i] -> [o]
runWire (FromTransducer ch) is = runTransducer ch is
runWire (Seqential w1 w2) is = runWire w2 (runWire w1 is)
runWire (Parallel w1 w2) is = zip (runWire w1 is) (runWire w2 is)
runWire (Merge w1 w2) [] = []
runWire (Merge w1 w2) (i : is) = map Left (runWire w1 [i]) ++ map Right (runWire w2 [i]) ++ runWire (Merge w1 w2) is

pipeline :: Wire Void (Either Int String)
pipeline =
  Seqential
    (FromTransducer producer)
    (Merge (FromTransducer double) (FromTransducer showStr))

main :: IO ()
main = go $ runWire pipeline []
  where
    go [] = pure ()
    go (x : xs) = print x >> threadDelay 1000000 >> go xs
