module Console where

import Control.Monad.Trans.Free
import Data.Profunctor

main :: IO ()
main = putStrLn "Hello"

-- TODO example, remove

data ConsoleF i o next
  = Read (i -> next)
  | Write o next
  deriving (Functor)

newtype ConsoleT m r i o = ConsoleT {unConsoleT :: FreeT (ConsoleF i o) m r}

readLine :: (Monad m) => ConsoleT m i i o
readLine = ConsoleT $ liftF $ Read id

writeLine :: (Monad m) => o -> ConsoleT m () i o
writeLine o = ConsoleT $ liftF $ Write o ()

instance (Monad m) => Profunctor (ConsoleT m r) where
  dimap :: (i' -> i) -> (o -> o') -> ConsoleT m r i o -> ConsoleT m r i' o'
  dimap fi fo consoleT = ConsoleT $ FreeT $ do
    step <- runFreeT $ unConsoleT consoleT
    case step of
      Pure r -> pure $ Pure r
      Free (Read inext) -> pure $ Free $ Read $ \i' -> unConsoleT $ dimap fi fo $ ConsoleT (inext (fi i'))
      Free (Write o next) -> pure $ Free $ Write (fo o) $ unConsoleT $ dimap fi fo (ConsoleT next)
