module InterfaceWeaver.App where

import Control.Exception (bracket)
import Control.Monad (forever, unless)
import qualified Control.Monad.IO.Class
import Control.Monad.Writer (WriterT, execWriterT, lift, runWriterT, tell)

newtype LoopHook = LoopHook [IO ()]
  deriving (Semigroup, Monoid)

newtype ShutdownHook = ShutdownHook [IO ()]
  deriving (Semigroup, Monoid)

newtype App a = App (WriterT LoopHook (WriterT ShutdownHook IO) a)
  deriving (Functor, Applicative, Monad)

instance (Semigroup a) => Semigroup (App a) where
  (<>) (App x) (App y) = App $ liftA2 (<>) x y

instance (Monoid a) => Monoid (App a) where
  mempty = App $ pure mempty

onLoop :: IO () -> App ()
onLoop hook = App $ tell (LoopHook [hook])

onShutdown :: IO () -> App ()
onShutdown hook = App $ lift $ tell (ShutdownHook [hook])

liftIO :: IO a -> App a
liftIO = App . Control.Monad.IO.Class.liftIO

runApp :: App () -> IO ()
runApp (App app) =
  bracket
    (runWriterT $ execWriterT app)
    (\(_, ShutdownHook hooks) -> sequence_ hooks)
    (\(LoopHook hooks, _) -> unless (null hooks) (forever $ sequence_ hooks))
