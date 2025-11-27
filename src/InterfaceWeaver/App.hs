module InterfaceWeaver.App where

import qualified Control.Applicative
import Control.Exception (bracket)
import Control.Monad (forever, unless)
import qualified Control.Monad.IO.Class
import Control.Monad.Writer (WriterT, execWriterT, lift, runWriterT, tell)

newtype LoopHook = LoopHook [IO ()]
  deriving (Semigroup, Monoid)

newtype ShutdownHook = ShutdownHook [IO ()]
  deriving (Semigroup, Monoid)

type App a = WriterT LoopHook (WriterT ShutdownHook IO) a

instance (Semigroup a) => Semigroup (App a) where
  (<>) = liftA2 (<>)

instance (Monoid a) => Monoid (App a) where
  mempty = pure mempty

(<**>) :: (Applicative f) => f a -> f (a -> b) -> f b
(<**>) = (Control.Applicative.<**>)

infixl 1 <**> -- Change the precedence to match >>= and <&>

onLoop :: IO () -> App ()
onLoop hook = tell (LoopHook [hook])

onShutdown :: IO () -> App ()
onShutdown hook = lift $ tell (ShutdownHook [hook])

liftIO :: IO a -> App a
liftIO = Control.Monad.IO.Class.liftIO

runApp :: App () -> IO ()
runApp app =
  bracket
    (runWriterT $ execWriterT app)
    (\(_, ShutdownHook hooks) -> sequence_ hooks)
    (\(LoopHook hooks, _) -> unless (null hooks) (forever $ sequence_ hooks))
