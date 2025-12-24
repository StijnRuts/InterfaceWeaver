module Console where

import Control.Monad.Trans.Free
import Data.Profunctor

-- TODO example, remove

data ConsoleF i o next
  = Read (i -> next)
  | Write o next
  deriving (Functor)

-- data FreeT f m a = FreeT { runFreeT :: m (FreeF f a (FreeT f m a)) }
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

instance PFunctor (ConsoleF i o) where
  pmap f g (Read k) = Read (f . k)
  pmap f g (Write o k) = Write o (g k)

newtype FreePT f m r i o
  = FreePT {runFreePT :: FreeT (f i o) m r}

instance (Functor m, Profunctor f) => Profunctor (FreePT f m r) where
  dimap fi fo (FreePT ft) = FreePT $ FreeT $ do
    step <- runFreeT ft
    pure $ case step of
      Pure r -> Pure r
      Free fx -> Free $ dimap fi fo (pmap (dimap fi fo . FreePT) fx)

newtype ConsoleT m r i o = ConsoleT {unConsoleT :: FreeT (f i o) m r}
  deriving (Profunctor) via (FreePT ConsoleF m r)
