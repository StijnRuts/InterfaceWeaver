module InterfaceWeaver.App where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (forever)
import Control.Monad.IO.Class
import Control.Monad.Writer

newtype App a = App {runApp :: WriterT (IO ()) IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadWriter (IO ()))

onShutdown :: IO () -> App ()
onShutdown = tell

run :: IO () -> App ()
run = App . liftIO

interfaceWeaver :: App () -> IO ()
interfaceWeaver app =
  bracket
    (execWriterT (runApp app))
    id
    (\_ -> forever $ threadDelay maxBound)
