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

data ChannelF m i o next
  = Emit o next
  | Await (i -> next)
  | LiftM (m next)
  deriving (Functor)

type Channel m i o = Free (ChannelF m i o)

type Transducer m i o = Channel m i o ()

type Producer m o = Transducer m Void o

type Consumer m i = Transducer m i Void

emit :: (Functor m) => o -> Channel m i o ()
emit o = liftF $ Emit o ()

await :: (Functor m) => Channel m i o i
await = liftF $ Await id

liftM :: (Functor m) => m a -> Channel m i o a
liftM action = liftF $ LiftM action

data Wire m i o where
  FromTransducer :: Transducer m i o -> Wire m i o
  Seqential :: Wire m a b -> Wire m b c -> Wire m a c

-- Parallel :: Wire m a b -> Wire m a c -> Wire m a (b, c)
-- Merge :: Wire m a b -> Wire m a c -> Wire m a (Either b c)

--

{-
producer ::  Producer IO Int
producer = mapM_ (\f -> emit f >> liftM (threadDelay 1000000)) fibs
  where
    fibs = 0 : 1 : zipWith (+) fibs (drop 1 fibs)
-}

producer :: Producer IO Int
producer = do
  emit 1
  liftM $ threadDelay 1000000
  emit 2
  liftM $ threadDelay 1000000
  emit 3
  liftM $ threadDelay 1000000
  emit 4
  liftM $ threadDelay 1000000
  emit 5

double :: (Functor m) => Transducer m Int Int
double = forever $ emit . (2 *) =<< await

showStr :: (Functor m, Show s) => Transducer m s String
showStr = forever $ emit . show =<< await

collector :: Consumer IO String
collector = forever $ liftM . putStrLn =<< await

--

runChannel :: (Monad m) => Channel m i o () -> [i] -> m [o]
runChannel (Pure ()) _ = return []
runChannel (Free (Emit o next)) is = (o :) `fmap` runChannel next is
runChannel (Free (Await next)) [] = return []
runChannel (Free (Await next)) (i : is) = runChannel (next i) is
runChannel (Free (LiftM mNext)) is = flip runChannel is =<< mNext

runWire :: (Monad m) => Wire m i o -> [i] -> m [o]
runWire (FromTransducer ch) is = runChannel ch is
runWire (Seqential w1 w2) is = runWire w2 =<< runWire w1 is

pipeline :: Wire IO Void Void
pipeline =
  Seqential (FromTransducer producer) $
    Seqential (FromTransducer double) $
      Seqential
        (FromTransducer showStr)
        (FromTransducer collector)

runPipeline :: Wire IO Void Void -> IO ()
runPipeline p = void $ runWire p []

main :: IO ()
main = runPipeline pipeline
