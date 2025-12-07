module Main2 (main) where

import Control.Category (Category, (>>>))
import qualified Control.Category as C
import Control.Monad.Free
import Data.Char (toUpper)

--

data ChannelF next
  = Emit String next
  | Await (String -> next)
  deriving (Functor)

type Channel = Free ChannelF

emit :: String -> Channel ()
emit v = liftF $ Emit v ()

await :: Channel String
await = liftF $ Await id

newtype Pipe a b = Pipe {runPipe :: [a] -> [b]}

instance Category Pipe where
  id = Pipe id
  (Pipe g) . (Pipe f) = Pipe (g . f)

--

producer :: Channel ()
producer = do
  emit "hello"
  emit "world"

transducer :: Channel ()
transducer = do
  x1 <- await
  emit $ toUpper <$> x1
  x2 <- await
  emit $ toUpper <$> x2

consumer :: Channel ()
consumer = do
  x <- await
  y <- await
  emit $ "Got: " ++ x ++ " and " ++ y

--

interpret :: Channel () -> Pipe String String
interpret (Pure ()) = Pipe id
interpret (Free (Emit s next)) = let Pipe f = interpret next in Pipe (\xs -> s : f xs)
interpret (Free (Await k)) = Pipe (\(x : xs) -> let Pipe f = interpret (k x) in f xs)

pipeline :: Pipe String String
pipeline = interpret producer >>> interpret transducer >>> interpret consumer

result :: [String]
result = runPipe pipeline []

main :: IO ()
main = print result
