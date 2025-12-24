type Listener m a = a -> m ()

newtype Events m a = Events (Listener m a -> m ())

-- Sourcing and sinking events

source :: (MonadIO m) => m (Events m a, a -> m ())
source = do
  listeners <- liftIO IOSeq.new
  let events = Events $ liftIO . IOSeq.add listeners
  let push a = liftIO (IOSeq.get listeners) >>= mapM_ ($ a)
  return (events, push)

sink :: Listener m a -> Events m a -> m ()
sink listener (Events register) = register listener

-- Transforming Events

transformEvent :: (Listener m b -> Listener m a) -> Events m a -> Events m b
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
