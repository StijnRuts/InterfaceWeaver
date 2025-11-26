module InterfaceWeaver.App where

import qualified Control.Applicative as A
import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (forever, when)
import Control.Monad.IO.Class (MonadIO)
import qualified Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Control.Monad.Writer (MonadWriter, WriterT, execWriterT, tell)

data Environment = Production | Testing
  deriving (Eq)

newtype App a = App {getAppM :: ReaderT Environment (WriterT (IO ()) IO) a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadWriter (IO ()), MonadReader Environment)

instance (Semigroup a) => Semigroup (App a) where
  (<>) (App x) (App y) = App (liftA2 (<>) x y)

instance (Monoid a) => Monoid (App a) where
  mempty = App (pure mempty)

(<**>) :: (Applicative f) => f a -> f (a -> b) -> f b
(<**>) = (A.<**>)

infixl 1 <**> -- Change the precedence to match >>= and <&>

onShutdown :: IO () -> App ()
onShutdown = tell

getEnvironment :: App Environment
getEnvironment = ask

onEnvironment :: Environment -> App () -> App ()
onEnvironment env app = do
  environment <- getEnvironment
  when (environment == env) app

liftIO :: IO a -> App a
liftIO = Control.Monad.IO.Class.liftIO

runApp :: Environment -> App () -> IO ()
runApp env app =
  bracket
    (execWriterT (runReaderT (getAppM app) env))
    id
    (\_ -> when (env == Production) $ forever $ threadDelay maxBound)
