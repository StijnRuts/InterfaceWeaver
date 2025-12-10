{-# LANGUAGE GADTs #-}

module Main (main) where

import Control.Category (Category, (>>>))
import qualified Control.Category as C
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Free
import Data.Profunctor
import Data.Void
import System.Random (randomRIO)
import Witherable

--

data ChannelF i o next
  = Output o next
  | Input (i -> next)
  deriving (Functor)

type ChannelT i o m a = FreeT (ChannelF i o) m a

output :: (Monad m) => o -> ChannelT i o m ()
output o = liftF $ Output o ()

input :: (Monad m) => ChannelT i o m i
input = liftF $ Input id

data Graph m a i o
  = Embed (ChannelT i o m a)
  | forall x. Sequential (Graph m a i x) (Graph m a x o)
  | Parallel (Graph m a i o) (Graph m a i o)

instance Semigroup (Graph m a i o) where
  (<>) :: Graph m a i o -> Graph m a i o -> Graph m a i o
  (<>) = Parallel

instance (Monad m) => Category (Graph m a) where
  id :: Graph m a x x
  id = Embed $ forever $ input >>= output
  (.) :: Graph m a x o -> Graph m a i x -> Graph m a i o
  (.) = flip Sequential

instance (Monad m) => Functor (Graph m a i) where
  fmap :: (o -> o') -> Graph m a i o -> Graph m a i o'
  fmap = rmap

instance (Monad m) => Profunctor (Graph m a) where
  dimap :: (i' -> i) -> (o -> o') -> Graph m a i o -> Graph m a i' o'
  dimap fi fo (Sequential left right) = Sequential (lmap fi left) (rmap fo right)
  dimap fi fo (Parallel left right) = Parallel (dimap fi fo left) (dimap fi fo right)
  dimap fi fo (Embed channel) = Embed (transFreeT (dimapChan fi fo) channel)
    where
      dimapChan :: (i' -> i) -> (o -> o') -> ChannelF i o next -> ChannelF i' o' next
      dimapChan _ fo (Output o next) = Output (fo o) next
      dimapChan fi _ (Input next) = Input (next . fi)

instance (Monad m) => Filterable (Graph m a i) where
  mapMaybe :: (o -> Maybe o') -> Graph m a i o -> Graph m a i o'
  mapMaybe p (Sequential left right) = Sequential left (mapMaybe p right)
  mapMaybe p (Parallel left right) = Parallel (mapMaybe p left) (mapMaybe p right)
  mapMaybe p (Embed channel) = Embed (mapMaybeChanT p channel)
    where
      mapMaybeChanT :: (o -> Maybe o') -> ChannelT i o m a -> ChannelT i o' m a
      mapMaybeChanT p chan = FreeT $ do
        chanF <- runFreeT chan
        case chanF of
          Pure a -> pure $ Pure a
          Free (Input next) -> pure $ Free $ Input $ \i -> mapMaybeChanT p (next i)
          Free (Output o next) ->
            case p o of
              Just o' -> pure $ Free $ Output o' $ mapMaybeChanT p next
              Nothing -> runFreeT $ mapMaybeChanT p next

--

runChannel :: (Monad m) => (o -> m ()) -> m i -> ChannelT i o m a -> m a
runChannel put get channel = go channel
  where
    go chan = do
      step <- runFreeT chan
      case step of
        Pure a -> pure a
        Free (Output o next) -> put o >> go next
        Free (Input next) -> go . next =<< get

runChannel_ :: (Monad m) => ChannelT Void Void m a -> m a
runChannel_ = runChannel (error "Can't output Void") (error "Can't input Void")

runGraph :: (Monad m) => (o -> m ()) -> m i -> Graph m a i o -> m a
runGraph put get = runChannel put get . flattenGraph

runGraph_ :: (Monad m) => Graph m a Void Void -> m a
runGraph_ = runChannel_ . flattenGraph

flattenGraph :: (Monad m) => Graph m a i o -> ChannelT i o m a
flattenGraph (Embed channel) = channel
flattenGraph (Sequential left right) = sequential (flattenGraph left) (flattenGraph right)
flattenGraph (Parallel left right) = parallel (flattenGraph left) (flattenGraph right)

sequential :: (Monad m) => ChannelT i x m a -> ChannelT x o m a -> ChannelT i o m a
sequential lChan rChan = FreeT $ do
  lChanF <- runFreeT lChan
  rChanF <- runFreeT rChan
  case (lChanF, rChanF) of
    (Free (Output x next), Free (Input f)) -> runFreeT $ sequential next (f x)
    (left, Free (Output o next)) -> pure $ Free $ Output o $ sequential (FreeT $ pure left) next
    (Free (Input f), right) -> pure $ Free $ Input $ \i -> sequential (f i) (FreeT $ pure right)
    ((Pure _), (Pure a)) -> pure (Pure a)
    ((Pure a), (Free (Input _))) -> pure (Pure a)
    ((Free (Output _ _)), (Pure a)) -> pure (Pure a)

parallel :: (Monad m) => ChannelT i o m a -> ChannelT i o m a -> ChannelT i o m a
parallel lChan rChan = FreeT $ do
  lChanF <- runFreeT lChan
  rChanF <- runFreeT rChan
  case (lChanF, rChanF) of
    (Free (Input f1), Free (Input f2)) -> pure $ Free $ Input $ \i -> parallel (f1 i) (f2 i)
    (Free (Output o1 next1), Free (Output o2 next2)) -> pure $ Free $ Output o1 $ FreeT $ pure $ Free $ Output o2 $ parallel next1 next2
    (Free (Output o next), right) -> pure $ Free $ Output o $ parallel next (FreeT $ pure right)
    (left, Free (Output o next)) -> pure $ Free $ Output o $ parallel (FreeT $ pure left) next
    (left, Pure _) -> pure left
    (Pure _, right) -> pure right

--

{-
data Merge s a b o = Merge
  { initialState :: s,
    getState :: o -> s,
    inputA :: s -> a -> o,
    inputB :: s -> b -> o
  }

statelessMerge :: (a -> o) -> (b -> o) -> Merge () a b o
statelessMerge fa fb =
  Merge
    { initialState = (),
      getState = const (),
      inputA = const fa,
      inputB = const fb
    }

sumMerge :: Merge () a b (Either a b)
sumMerge = statelessMerge Left Right

appendMerge :: Merge () a a a
appendMerge = statelessMerge id id

productMerge :: a -> b -> Merge (a, b) a b (a, b)
productMerge initA initB =
  Merge
    { initialState = (initA, initB),
      getState = id,
      inputA = flip (,) . snd,
      inputB = (,) . fst
    }

monoidMerge :: (Monoid a, Monoid b) => Merge (a, b) a b (a, b)
monoidMerge = productMerge mempty mempty
-}

--

fiveProducer :: Graph IO () Void Int
fiveProducer = Embed $ do
  output 1
  lift $ threadDelay 1000000
  output 2
  lift $ threadDelay 1000000
  output 3
  lift $ threadDelay 1000000
  output 4
  lift $ threadDelay 1000000
  output 5

fibProducer :: Graph IO () Void Int
fibProducer = Embed $ go 0 1
  where
    go a b = do
      output a
      lift $ threadDelay 1000000
      go b (a + b)

randomProducer :: Graph IO () Void Int
randomProducer = Embed $ forever $ do
  n <- lift $ randomRIO (0, 999)
  output n
  lift $ threadDelay 1000000

printer :: (Show s) => Graph IO () s Void
printer = Embed $ forever $ do
  s <- input
  lift $ print s

main :: IO ()
main = runGraph_ $ fibProducer >>> printer
