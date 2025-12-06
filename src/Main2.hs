module Main2 (main) where

import Control.Monad.Free

--

data OutputF next = Output String next
  deriving (Functor)

type OutputProg = Free OutputF

output :: String -> OutputProg ()
output o = liftF $ Output o ()

data InputF next = Input (String -> next)
  deriving (Functor)

type InputProg = Free InputF

input :: InputProg String
input = liftF $ Input id

--

producer :: OutputProg ()
producer = do
  output "hello"
  output "world"

consumer :: InputProg [String]
consumer = do
  a <- input
  b <- input
  pure [a, b]

runTogether :: OutputProg a -> InputProg b -> (a, b)
runTogether (Pure a) (Pure b) = (a, b)
runTogether (Free (Output s next)) (Free (Input f)) = runTogether next (f s)
runTogether _ _ = error "Mismatched programs"

main :: IO ()
main = print $ runTogether producer consumer
