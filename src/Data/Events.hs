{-# LANGUAGE DataKinds #-}

module Data.Events where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever, void)
import Control.Monad.State (State, runState)
import Control.Timeout (TimeSpan, TimeoutM, newTimeouts, runTimeoutM)
import qualified Control.Timeout as Timeout
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as JSON
import Data.IO.Seq as IOSeq
import Data.IORef (atomicModifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.List as List
import Data.Maybe (fromJust, fromMaybe, maybeToList)
import qualified Data.Set as Set
import qualified Data.Tuple as Tuple
import Data.Union
import InterfaceWeaver.App (App, liftIO, onShutdown)
import System.Directory (XdgDirectory (..), createDirectoryIfMissing, doesFileExist, getXdgDirectory)

newtype Events a = Events ((a -> IO ()) -> IO ())

-- Sourcing and sinking events

source :: IO (Events a, a -> IO ())
source = do
  listeners <- IOSeq.new
  let events = Events $ IOSeq.add listeners
  let push a = IOSeq.get listeners >>= mapM_ ($ a)
  return (events, push)

sink :: (a -> IO ()) -> Events a -> IO ()
sink f (Events register) = register f

-- Transforming Events

bindEvent :: (a -> IO [b]) -> Events a -> Events b
bindEvent f (Events register) =
  Events $ \callback ->
    register $ \a -> do
      bs <- f a
      mapM_ callback bs

instance Functor Events where
  fmap :: (a -> b) -> Events a -> Events b
  fmap f = bindEvent $ pure . List.singleton . f

instance Semigroup (Events a) where
  (<>) :: Events a -> Events a -> Events a
  Events register1 <> Events register2 = Events $ \callback -> do
    register1 callback
    register2 callback

instance Monoid (Events a) where
  mempty :: Events a
  mempty = Events $ \_ -> return ()

flatten :: Events [a] -> Events a
flatten = bindEvent pure

matching :: (a -> Bool) -> Events a -> Events a
matching predicate = bindEvent $ \a -> if predicate a then pure [a] else pure []

filterMap :: (a -> Maybe b) -> Events a -> Events b
filterMap f = bindEvent $ pure . maybeToList . f

-- Parallel Events streams

partition :: (a -> Bool) -> Events a -> Events (Either a a)
partition predicate = fmap $ \a -> if predicate a then Right a else Left a

unpartition :: Events (Either a a) -> Events a
unpartition = fmap f
  where
    f (Left a) = a
    f (Right a) = a

partition' :: (a -> Bool) -> Events a -> (Events a, Events a)
partition' = (split .) . partition

unpartition' :: (Events a, Events a) -> Events a
unpartition' (events1, events2) = events1 <> events2

split :: Events (Either a b) -> (Events a, Events b)
split events = (filterMap leftToMaybe events, filterMap rightToMaybe events)
  where
    leftToMaybe (Left l) = Just l
    leftToMaybe (Right _) = Nothing
    rightToMaybe (Left _) = Nothing
    rightToMaybe (Right r) = Just r

join :: (Events a, Events b) -> Events (Either a b)
join (eventsA, eventsB) = fmap Left eventsA <> fmap Right eventsB

mapLeft :: (a -> c) -> Events (Either a b) -> Events (Either c b)
mapLeft f = fmap f'
  where
    f' (Left a) = Left (f a)
    f' (Right b) = Right b

mapRight :: (b -> c) -> Events (Either a b) -> Events (Either a c)
mapRight f = fmap f'
  where
    f' (Left a) = Left a
    f' (Right b) = Right (f b)

mapLeft' :: (Events a -> Events c) -> (Events a, Events b) -> (Events c, Events b)
mapLeft' f (eventsA, eventsB) = (f eventsA, eventsB)

mapRight' :: (Events b -> Events c) -> (Events a, Events b) -> (Events a, Events c)
mapRight' f (eventsA, eventsB) = (eventsA, f eventsB)

-- Events of Union types

relax :: (Member a u) => Events a -> Events (Union u)
relax events = inject <$> events

specialize :: (Member a u) => Events (Union u) -> Events a
specialize = filterMap project

widen :: (Subset u v) => Events (Union u) -> Events (Union v)
widen = fmap weaken

relaxF :: (Member a u, Member b v) => (Events a -> Events b) -> Events (Union u) -> Events (Union v)
relaxF f = relax . f . specialize

specializeF :: (Member a u, Member b v) => (Events (Union u) -> Events (Union v)) -> Events a -> Events b
specializeF f = specialize . f . relax

-- State-based Events

withStateIO :: IO s -> (s -> IO ()) -> ((a, s) -> (b, s)) -> App (Events a -> Events b)
withStateIO load save f = do
  ref <- liftIO $ newIORef =<< load
  onShutdown $ readIORef ref >>= save
  return $ bindEvent $ \a ->
    fmap List.singleton <$> atomicModifyIORef' ref $ \s -> Tuple.swap $ f (a, s)

withState :: s -> ((a, s) -> (b, s)) -> App (Events a -> Events b)
withState initial = withStateIO (return initial) (\_ -> return ())

withPersistentState :: (FromJSON s, ToJSON s) => FilePath -> s -> ((a, s) -> (b, s)) -> App (Events a -> Events b)
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

withStateM :: s -> (a -> State s b) -> App (Events a -> Events b)
withStateM initial f = withState initial $ \(a, s) -> runState (f a) s

withPersistentStateM :: (FromJSON s, ToJSON s) => FilePath -> s -> (a -> State s b) -> App (Events a -> Events b)
withPersistentStateM path initial f = withPersistentState path initial $ \(a, s) -> runState (f a) s

removeRepeats :: (Eq a) => App (Events a -> Events a)
removeRepeats = (flatten .) <$> withState Nothing (\(a, prev) -> ([a | prev /= Just a], Just a))

-- Time-based Events

data TimerUpdate t a b = TimerUpdate
  { onEvent :: a -> TimeoutM t [b],
    onTimeout :: t -> [b],
    fireOnShutdown :: t -> Bool
  }

withTimeout :: TimerUpdate t a b -> App (Events a -> Events b)
withTimeout (TimerUpdate {onEvent, onTimeout, fireOnShutdown}) = do
  timeouts <- liftIO newTimeouts
  fireTimeoutRef <- liftIO $ newIORef Nothing

  onShutdown $ do
    fireTimeout <- fromJust <$> readIORef fireTimeoutRef
    runTimeoutM timeouts fireTimeout $
      Timeout.fire =<< Timeout.find fireOnShutdown

  return $ \(Events register) ->
    Events $ \callback -> do
      let fireTimeout = mapM_ callback . onTimeout
      writeIORef fireTimeoutRef $ Just fireTimeout
      register $ \a -> mapM_ callback =<< runTimeoutM timeouts fireTimeout (onEvent a)

-- Constant stream of events
every :: TimeSpan -> Events ()
every ts = Events $ \callback ->
  void $ forkIO $ forever $ do
    threadDelay ts
    callback ()

-- Postpone event delivery
delay :: TimeSpan -> TimerUpdate a a a
delay ts =
  TimerUpdate
    { onEvent = \a -> Timeout.schedule ts a >> return [],
      onTimeout = (: []),
      fireOnShutdown = const True
    }

-- Delay event emission until inactivity
debounce :: (a -> a -> Bool) -> TimeSpan -> TimerUpdate a a a
debounce f ts =
  TimerUpdate
    { onEvent = \a -> do
        Timeout.clear =<< Timeout.find (f a)
        Timeout.schedule ts a
        return [],
      onTimeout = (: []),
      fireOnShutdown = const True
    }

debounceAll :: TimeSpan -> TimerUpdate a a a
debounceAll = debounce $ \_ _ -> True

debounceByValue :: (Eq a) => TimeSpan -> TimerUpdate a a a
debounceByValue = debounce (==)

-- Limit event frequency
throttle :: (a -> a -> Bool) -> TimeSpan -> TimerUpdate a a a
throttle f ts =
  TimerUpdate
    { onEvent = \a -> do
        throttleactive <- not . Set.null <$> Timeout.find (f a)
        if throttleactive
          then return []
          else Timeout.schedule ts a >> return [a],
      onTimeout = const [],
      fireOnShutdown = const False
    }

throttleAll :: TimeSpan -> TimerUpdate a a a
throttleAll = throttle $ \_ _ -> True

throttleByValue :: (Eq a) => TimeSpan -> TimerUpdate a a a
throttleByValue = throttle (==)
