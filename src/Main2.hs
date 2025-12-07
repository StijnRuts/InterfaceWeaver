module Main2 (main) where

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

chain :: Channel () -> Channel () -> Channel ()
chain (Pure ()) (Pure ()) = Pure ()
chain (Free (Emit s next)) (Free (Await f)) = chain next (f s)
chain left (Free (Emit s' next')) = Free $ Emit s' $ chain left next'
chain (Free (Await _)) _ = error "Could not chain from Await"
chain (Pure _) _ = error "Left pipeline ran out"
chain _ (Pure _) = error "Right pipeline ran out"

output :: Channel () -> IO ()
output (Pure _) = pure ()
output (Free (Emit s next)) = putStrLn s >> output next
output (Free (Await _)) = error "Could not output on Await"

main :: IO ()
main = output $ producer `chain` transducer `chain` consumer
