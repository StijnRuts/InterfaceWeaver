module InterfaceWeaver.App where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (forever)
import Control.Monad.IO.Class
import Control.Monad.Writer

newtype App a = App {appWriter :: WriterT (IO ()) IO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadWriter (IO ()))

onShutdown :: IO () -> App ()
onShutdown = tell

run :: IO a -> App a
run = App . liftIO

runApp :: App () -> IO ()
runApp app =
  bracket
    (execWriterT (appWriter app))
    id
    (\_ -> forever $ threadDelay maxBound)

runAppTest :: App () -> IO ()
runAppTest app =
  bracket
    (execWriterT (appWriter app))
    id
    (\_ -> return ())
