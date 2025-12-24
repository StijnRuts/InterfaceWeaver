module Control.Timeout where

import Control.Monad (void)
import Control.Monad.Free (Free (..), liftF)
import Data.Foldable (traverse_)
import Data.IO.Map as IOMap
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import GHC.Event (TimeoutKey, getSystemTimerManager, registerTimeout, unregisterTimeout, updateTimeout)

type TimeSpan = Int

microseconds :: Int -> TimeSpan
microseconds = id

milliseconds :: Int -> TimeSpan
milliseconds t = t * 1000

seconds :: Double -> TimeSpan
seconds t = floor $ t * 1000000

(@) :: (Num a) => a -> (a -> TimeSpan) -> TimeSpan
n @ f = f n

infixl 9 @

--

newtype Timeouts t = Timeouts {getIOMap :: IOMap TimeoutKey t}

newTimeouts :: IO (Timeouts t)
newTimeouts = Timeouts <$> IOMap.new

data TimeoutFree t next
  = Schedule TimeSpan t next
  | GetMap (Map TimeoutKey t -> next)
  | Update TimeSpan TimeoutKey next
  | Clear TimeoutKey next
  | Fire TimeoutKey next
  deriving (Functor)

type TimeoutM t = Free (TimeoutFree t)

schedule :: TimeSpan -> t -> TimeoutM t ()
schedule ts t = liftF $ Schedule ts t ()

find :: (t -> Bool) -> TimeoutM t (Set TimeoutKey)
find predicate = Map.keysSet . Map.filter predicate <$> liftF (GetMap id)

update :: TimeSpan -> Set TimeoutKey -> TimeoutM t ()
update ts = mapM_ (\tk -> liftF $ Update ts tk ())

clear :: Set TimeoutKey -> TimeoutM t ()
clear = mapM_ (\tk -> liftF $ Clear tk ())

fire :: Set TimeoutKey -> TimeoutM t ()
fire = mapM_ (\tk -> liftF $ Fire tk ())

runTimeoutM :: Timeouts t -> (t -> IO ()) -> TimeoutM t a -> IO a
runTimeoutM timeouts fireTimeout x = do
  tm <- getSystemTimerManager
  runFree tm x
  where
    runFree _ (Pure a) = return a
    --
    runFree tm (Free (Schedule ts t next)) = do
      tkRef <- newIORef Nothing
      tk <- registerTimeout tm ts $ do
        fireTimeout t
        tk <- fromJust <$> readIORef tkRef
        void $ IOMap.delete tk $ getIOMap timeouts
      writeIORef tkRef $ Just tk
      IOMap.add (getIOMap timeouts) tk t
      runFree tm next
    --
    runFree tm (Free (GetMap f)) = do
      m <- IOMap.get $ getIOMap timeouts
      runFree tm (f m)
    --
    runFree tm (Free (Update ts tk next)) = do
      updateTimeout tm tk ts
      runFree tm next
    --
    runFree tm (Free (Clear tk next)) = do
      unregisterTimeout tm tk
      void $ IOMap.delete tk $ getIOMap timeouts
      runFree tm next
    --
    runFree tm (Free (Fire tk next)) = do
      unregisterTimeout tm tk
      traverse_ fireTimeout =<< IOMap.delete tk (getIOMap timeouts)
      runFree tm next
