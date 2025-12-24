module Data.Events where

import Control.Arrow
import Control.Category (Category)
import qualified Control.Category as C
import Control.Monad (forever, void)
import Control.Monad.State (State, runState)
import Control.Timeout (TimeSpan, TimeoutM, newTimeouts, runTimeoutM)
import qualified Control.Timeout as Timeout
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as JSON
import qualified Data.Bifunctor as BF
import Data.Char
import Data.Default
import Data.IO.Seq as IOSeq
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.List as List
import Data.Maybe (fromJust, fromMaybe, maybeToList)
import Data.Profunctor
import qualified Data.Set as Set
import qualified Data.Tuple as Tuple
import Data.Union
import Data.Void
import GHC.Event (getSystemTimerManager, registerTimeout)
import InterfaceWeaver.App (App, liftIO, onShutdown)
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, doesFileExist, getXdgDirectory)
import Witherable

type Subscribe o = (o -> IO ()) -> IO ()

type Push i = i -> IO ()

-- Define a proper record type
data ChannelImpl i o = ChannelImpl
  { subscribe :: Subscribe o,
    push :: Push i
  }

newtype Channel i o = Channel (IO (ChannelImpl i o))

type Source a = Channel Void a

type Sink a = forall x. Channel x a -> IO ()

-- (&) :: a -> (a -> b) -> b
-- channel & sink :: IO ()

-- Typeclass instances

instance Category Channel where
  id :: Channel a a
  id = Channel $ do
    listeners <- IOSeq.new
    return
      ChannelImpl
        { subscribe = IOSeq.add listeners,
          push = \a -> IOSeq.get listeners >>= mapM_ ($ a)
        }

  (.) :: Channel x o -> Channel i x -> Channel i o
  Channel oChan . Channel iChan = Channel $ do
    oRec <- oChan
    iRec <- iChan
    subscribe iRec (push oRec)
    return ChannelImpl {subscribe = subscribe oRec, push = push iRec}

instance Arrow Channel where
  arr :: (i -> o) -> Channel i o
  arr f = BF.second f C.id

  (***) ::
    (Default o1, Default o2) =>
    Channel i1 o1 -> Channel i2 o2 -> Channel (i1, i2) (o1, o2)
  Channel lChan *** Channel rChan = Channel $ do
    lRec <- lChan
    rRec <- rChan
    o1Ref <- newIORef def
    o2Ref <- newIORef def
    return
      ChannelImpl
        { subscribe = \listener -> do
            subscribe lRec $ \o1 -> do
              writeIORef o1Ref o1
              o2 <- readIORef o2Ref
              listener (o1, o2)
            subscribe rRec $ \o2 -> do
              o1 <- readIORef o1Ref
              writeIORef o2Ref o2
              listener (o1, o2),
          push = \(i1, i2) -> push lRec i1 >> push rRec i2
        }

{-
instance ArrowChoice Channel where
  (+++) :: Channel i1 o1 -> Channel i2 o2 -> Channel (Either i1 i2) (Either o1 o2)

instance ArrowZero Channel where
  zeroArrow :: Channel i o

instance ArrowPlus Channel where
  (<+>) :: Channel i o -> Channel i o -> Channel i o

instance ArrowApply Channel where
  app :: Channel (Channel i o, i) o

instance ArrowLoop Channel where
  loop :: Channel (i, s) (o, s) -> Channel i o

instance Profunctor Channel where
  lmap :: (i' -> i) -> Channel i o -> Channel i' o

  rmap :: (o -> o') -> Channel i o -> Channel i o'

instance Profunctor Channel where
  dimap :: (i' -> i) -> (o -> o') -> Channel i o -> Channel i' o'

class BifunctorChannel where
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  first :: (a -> b) -> p a c -> p b c
  second :: (b -> c) -> p a b -> p a c

instance Functor (Channel i) where
  fmap :: (o -> o') -> Channel i o -> Channel i o'
  fmap = rmap

instance Applicative (Channel i) where
  pure :: o -> Channel i o

  (<*>) :: Channel i (o -> o') -> Channel i o -> Channel i o'

instance (Semigroup o) => Semigroup (Channel i o) where
  (<>) :: Channel i o -> Channel i o -> Channel i o

instance (Monoid o) => Monoid (Channel i o) where
  mempty :: Channel i o

instance Monad (Channel i) where
  (>>=) :: Channel i o -> (o -> Channel i o') -> Channel i o'

instance Filterable (Channel i) where
  mapMaybe :: (o -> Maybe o') -> Channel i o -> Channel i o'

(>||) :: (Filterable f) => f a -> (a -> Bool) -> f a
(>||) = flip Witherable.filter

(>|^) :: (Filterable f) => f a -> (a -> Maybe b) -> f b
(>|^) = flip mapMaybe

(||<) :: (Filterable f) => (a -> Bool) -> f a -> f a
(||<) = Witherable.filter

(^|<) :: (Filterable f) => (a -> Maybe b) -> f a -> f b
(^|<) = mapMaybe

-}
