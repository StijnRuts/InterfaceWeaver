{-# LANGUAGE DataKinds #-}

module Data.Events where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (State, runState)
import Control.Timeout (TimeSpan, TimeoutM, newTimeouts, runTimeoutM)
import qualified Control.Timeout as Timeout
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as JSON
import Data.Foldable (traverse_)
import Data.IO.Seq as IOSeq
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Tuple as Tuple
import Data.Union
import GHC.Event (getSystemTimerManager, registerTimeout)
import InterfaceWeaver.App (App, onShutdown)
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, doesFileExist, getXdgDirectory)

newtype Events m a = Events ((a -> m ()) -> m ())

-- Sourcing and sinking events

source :: (MonadIO m) => m (Events m a, a -> m ())
source = do
  listeners <- liftIO IOSeq.new
  let events = Events $ liftIO . IOSeq.add listeners
  let push a = liftIO (IOSeq.get listeners) >>= mapM_ ($ a)
  return (events, push)

sink :: (a -> m ()) -> Events m a -> m ()
sink listener (Events register) = register listener

-- Transforming Events

transformEvent :: ((b -> m ()) -> a -> m ()) -> Events m a -> Events m b
transformEvent f (Events register) = Events $ register . f

instance Functor (Events m) where
  fmap :: (a -> b) -> Events m a -> Events m b
  fmap f = transformEvent (. f)

instance (Applicative m) => Semigroup (Events m a) where
  (<>) :: Events m a -> Events m a -> Events m a
  Events register1 <> Events register2 = Events $ liftA2 (*>) register1 register2

instance (Applicative m) => Monoid (Events m a) where
  mempty :: Events m a
  mempty = Events $ const $ pure ()

flatten :: (Applicative m, Foldable t) => Events m (t a) -> Events m a
flatten = transformEvent traverse_

matching :: (Applicative m) => (a -> Bool) -> Events m a -> Events m a
matching predicate = transformEvent $ liftA2 when predicate

filterMap :: (Applicative m) => (a -> Maybe b) -> Events m a -> Events m b
filterMap f = flatten . fmap f

-- Parallel Events streams

partition :: (a -> Bool) -> Events m a -> Events m (Either a a)
partition predicate = fmap $ \a -> if predicate a then Right a else Left a

unpartition :: Events m (Either a a) -> Events m a
unpartition = fmap f
  where
    f (Left a) = a
    f (Right a) = a

partition' :: (Applicative m) => (a -> Bool) -> Events m a -> (Events m a, Events m a)
partition' = (split .) . partition

unpartition' :: (Applicative m) => (Events m a, Events m a) -> Events m a
unpartition' (events1, events2) = events1 <> events2

split :: (Applicative m) => Events m (Either a b) -> (Events m a, Events m b)
split events = (filterMap leftToMaybe events, filterMap rightToMaybe events)
  where
    leftToMaybe (Left l) = Just l
    leftToMaybe (Right _) = Nothing
    rightToMaybe (Left _) = Nothing
    rightToMaybe (Right r) = Just r

join :: (Applicative m) => (Events m a, Events m b) -> Events m (Either a b)
join (eventsA, eventsB) = fmap Left eventsA <> fmap Right eventsB

mapLeft :: (a -> c) -> Events m (Either a b) -> Events m (Either c b)
mapLeft f = fmap f'
  where
    f' (Left a) = Left (f a)
    f' (Right b) = Right b

mapRight :: (b -> c) -> Events m (Either a b) -> Events m (Either a c)
mapRight f = fmap f'
  where
    f' (Left a) = Left a
    f' (Right b) = Right (f b)

mapLeft' :: (Events m a -> Events m c) -> (Events m a, Events m b) -> (Events m c, Events m b)
mapLeft' f (eventsA, eventsB) = (f eventsA, eventsB)

mapRight' :: (Events m b -> Events m c) -> (Events m a, Events m b) -> (Events m a, Events m c)
mapRight' f (eventsA, eventsB) = (eventsA, f eventsB)

-- Events of Union types

relax :: (Member a u) => Events m a -> Events m (Union u)
relax events = inject <$> events

specialize :: (Applicative m) => (Member a u) => Events m (Union u) -> Events m a
specialize = filterMap project

widen :: (Subset u v) => Events m (Union u) -> Events m (Union v)
widen = fmap weaken

relaxF :: (Applicative m, Member a u, Member b v) => (Events m a -> Events m b) -> Events m (Union u) -> Events m (Union v)
relaxF f = relax . f . specialize

specializeF :: (Applicative m, Member a u, Member b v) => (Events m (Union u) -> Events m (Union v)) -> Events m a -> Events m b
specializeF f = specialize . f . relax

-- State-based Events

withStateIO :: IO s -> (s -> IO ()) -> ((a, s) -> (b, s)) -> Events a -> App (Events b)
withStateIO load save f eventsA = do
  ref <- liftIO $ newIORef =<< load
  onShutdown $ readIORef ref >>= save
  return $
    bindEvent
      (\a -> fmap List.singleton <$> atomicModifyIORef' ref $ \s -> Tuple.swap $ f (a, s))
      eventsA

withState :: s -> ((a, s) -> (b, s)) -> Events a -> App (Events b)
withState initial = withStateIO (return initial) (\_ -> return ())

withPersistentState :: (FromJSON s, ToJSON s) => FilePath -> s -> ((a, s) -> (b, s)) -> Events a -> App (Events b)
withPersistentState filename initial = withStateIO load save
  where
    load = do
      fullPath <- withBaseDir
      exists <- doesFileExist fullPath
      if exists
        then fromMaybe initial <$> JSON.decodeFileStrict fullPath
        else return initial
    save s = do
      fullPath <- withBaseDir
      JSON.encodeFile fullPath s
    withBaseDir = do
      dir <- getXdgDirectory XdgState "InputWeaver"
      createDirectoryIfMissing True dir
      return $ dir <> "/" <> filename <> ".json"

withStateM :: s -> (a -> State s b) -> Events a -> App (Events b)
withStateM initial f = withState initial $ \(a, s) -> runState (f a) s

withPersistentStateM :: (FromJSON s, ToJSON s) => FilePath -> s -> (a -> State s b) -> Events a -> App (Events b)
withPersistentStateM path initial f = withPersistentState path initial $ \(a, s) -> runState (f a) s

removeRepeats :: (Eq a) => Events a -> App (Events a)
removeRepeats = (filterMap id <$>) . withState Nothing (\(a, prev) -> (if prev == Just a then Nothing else Just a, Just a))

-- Time-based Events

data TimerUpdate t a b = TimerUpdate
  { onEvent :: a -> TimeoutM t [b],
    onTimeout :: t -> [b],
    fireOnShutdown :: t -> Bool
  }

withTimeout :: TimerUpdate t a b -> Events a -> App (Events b)
withTimeout (TimerUpdate {onEvent, onTimeout, fireOnShutdown}) (Events register) = do
  timeouts <- liftIO newTimeouts
  fireTimeoutRef <- liftIO $ newIORef Nothing

  onShutdown $ do
    fireTimeout <- fromJust <$> readIORef fireTimeoutRef
    runTimeoutM timeouts fireTimeout $
      Timeout.fire =<< Timeout.find fireOnShutdown

  return $ Events $ \callback -> do
    let fireTimeout = mapM_ callback . onTimeout
    writeIORef fireTimeoutRef $ Just fireTimeout
    register $ \a -> mapM_ callback =<< runTimeoutM timeouts fireTimeout (onEvent a)

-- Constant stream of events
every :: TimeSpan -> App (Events ())
every ts = do
  tm <- liftIO getSystemTimerManager
  return $ Events (recursive tm)
  where
    recursive tm callback = void $ registerTimeout tm ts $ do
      void $ callback ()
      recursive tm callback

-- Postpone event delivery
delay :: TimeSpan -> Events a -> App (Events a)
delay ts =
  withTimeout
    TimerUpdate
      { onEvent = \a -> Timeout.schedule ts a >> return [],
        onTimeout = (: []),
        fireOnShutdown = const True
      }

-- Delay event emission until inactivity
debounce :: (a -> a -> Bool) -> TimeSpan -> Events a -> App (Events a)
debounce f ts =
  withTimeout
    TimerUpdate
      { onEvent = \a -> do
          Timeout.clear =<< Timeout.find (f a)
          Timeout.schedule ts a
          return [],
        onTimeout = (: []),
        fireOnShutdown = const True
      }

debounceAll :: TimeSpan -> Events a -> App (Events a)
debounceAll = debounce $ \_ _ -> True

debounceByValue :: (Eq a) => TimeSpan -> Events a -> App (Events a)
debounceByValue = debounce (==)

-- Limit event frequency
throttle :: (a -> a -> Bool) -> TimeSpan -> Events a -> App (Events a)
throttle f ts =
  withTimeout
    TimerUpdate
      { onEvent = \a -> do
          throttleactive <- not . Set.null <$> Timeout.find (f a)
          if throttleactive
            then return []
            else Timeout.schedule ts a >> return [a],
        onTimeout = const [],
        fireOnShutdown = const False
      }

throttleAll :: TimeSpan -> Events a -> App (Events a)
throttleAll = throttle $ \_ _ -> True

throttleByValue :: (Eq a) => TimeSpan -> Events a -> App (Events a)
throttleByValue = throttle (==)
