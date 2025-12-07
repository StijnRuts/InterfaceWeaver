{-# LANGUAGE GADTs #-}

module Main2 (main) where

import Control.Category (Category, (>>>))
import qualified Control.Category as C
import Control.Monad.Free
import Data.Char (toUpper)

--

data ChannelF i o next
  = Emit o next
  | Await (i -> next)
  deriving (Functor)

type Channel i o = Free (ChannelF i o)

emit :: o -> Channel i o ()
emit o = liftF $ Emit o ()

await :: Channel i o i
await = liftF $ Await id

newtype Pipe i o = Pipe {runPipe :: [i] -> [o]}

instance Category Pipe where
  id = Pipe id
  (Pipe g) . (Pipe f) = Pipe (g . f)

data Wire i o where
  PipeIn :: Channel i o () -> Wire i o
  Seqential :: Wire a b -> Wire b c -> Wire a c
  Parallel :: Wire a b -> Wire a c -> Wire a (b, c)
  Merge :: Wire a b -> Wire a c -> Wire a (Either b c)

--

producer :: Channel () Int ()
producer = do
  emit 1
  emit 2
  emit 3

double :: Channel Int Int ()
double = do
  x <- await
  emit $ 2 * x

showStr :: Channel Int String ()
showStr = do
  x <- await
  emit $ show x

collector :: Channel String String ()
collector = do
  s <- await
  emit $ "Collected: " ++ s

--

runChannel :: Channel i o () -> Pipe i o
runChannel (Pure ()) = Pipe (const [])
runChannel (Free (Emit x next)) = let Pipe f = runChannel next in Pipe (\xs -> x : f xs)
runChannel (Free (Await next')) = Pipe (\(x : xs) -> let Pipe f = runChannel (next' x) in f xs)

runWire :: Wire i o -> [i] -> [o]
runWire (PipeIn ch) is = runPipe (runChannel ch) is
runWire (Seqential w1 w2) is = runWire w2 (runWire w1 is)
runWire (Parallel w1 w2) is = zip (runWire w1 is) (runWire w2 is)
runWire (Merge w1 w2) is = map Left (runWire w1 is) ++ map Right (runWire w2 is)

pipeline :: Wire () (Either Int String)
pipeline =
  Seqential
    (PipeIn producer)
    (Merge (PipeIn double) (PipeIn showStr))

main :: IO ()
main = print $ runWire pipeline []
