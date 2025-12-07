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

--

producer :: Channel String String ()
producer = do
  emit "hello"
  emit "world"

transducer :: Channel String String ()
transducer = do
  x1 <- await
  emit $ toUpper <$> x1
  x2 <- await
  emit $ toUpper <$> x2

consumer :: Channel String String ()
consumer = do
  x <- await
  y <- await
  emit $ "Got: " ++ x ++ " and " ++ y

--

interpret :: Channel i o () -> Pipe i o
interpret (Pure ()) = Pipe (const [])
interpret (Free (Emit x next)) = let Pipe f = interpret next in Pipe (\xs -> x : f xs)
interpret (Free (Await next')) = Pipe (\(x : xs) -> let Pipe f = interpret (next' x) in f xs)

pipeline :: Pipe String String
pipeline = interpret producer >>> interpret transducer >>> interpret consumer

result :: [String]
result = runPipe pipeline []

main :: IO ()
main = print result
