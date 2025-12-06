{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module AltMain (main) where

import Data.Function ((&))
import Polysemy
import Polysemy.Input
import Polysemy.Internal.CustomErrors (FirstOrder)
import Polysemy.Output

type Producer o = forall r. (Member (Output o) r) => Sem r ()

type Consumer i = forall r. (Member (Input i) r) => Sem r ()

type Pipe i o = forall r. (Members '[Input i, Output o] r) => Sem r ()

type Interpret from to = forall r. (Member to r) => Sem (from ': r) () -> Sem r ()

program :: Pipe String String
program = do
  output "What is your name?"
  name <- input
  output $ "Hello, " ++ name ++ "!"

runInputLine :: Interpret (Input String) (Embed IO)
runInputLine = interpret $ \Input -> embed getLine

runOutputLine :: Interpret (Output String) (Embed IO)
runOutputLine = interpret $ \(Output s) -> embed (putStrLn s)

main :: IO ()
-- main = runM . runOutputLine . runInputLine $ program
-- main = program & runInputLine & runOutputLine & runM
main = runM . runInputLine $ program & runOutputLine

