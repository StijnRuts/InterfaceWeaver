{-# LANGUAGE GADTs #-}

module Main2 (main) where

import Control.Category (Category, (>>>))
import qualified Control.Category as C
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
producer = do
  emit 1
  emit 2
  emit 3

double :: Transducer Int Int
double = do
  x <- await
  emit $ 2 * x

showStr :: Transducer Int String
showStr = do
  x <- await
  emit $ show x

collector :: Transducer String String
collector = do
  s <- await
  emit $ "Collected: " ++ s

--

runTransducer :: Transducer i o -> [i] -> [o]
runTransducer (Pure ()) = const []
runTransducer (Free (Emit x next)) = let f = runTransducer next in (\xs -> x : f xs)
runTransducer (Free (Await next')) = (\(x : xs) -> let f = runTransducer (next' x) in f xs)

runWire :: Wire i o -> [i] -> [o]
runWire (FromTransducer ch) is = runTransducer ch is
runWire (Seqential w1 w2) is = runWire w2 (runWire w1 is)
runWire (Parallel w1 w2) is = zip (runWire w1 is) (runWire w2 is)
runWire (Merge w1 w2) is = map Left (runWire w1 is) ++ map Right (runWire w2 is)

pipeline :: Wire Void (Either Int String)
pipeline =
  Seqential
    (FromTransducer producer)
    (Merge (FromTransducer double) (FromTransducer showStr))

main :: IO ()
main = print $ runWire pipeline []
