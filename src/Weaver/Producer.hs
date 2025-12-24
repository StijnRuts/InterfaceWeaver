module Weaver.Producer where

import Data.Bifunctor
import Data.Functor ((<&>))

{-
data FlipFreeT m a f
  = Pure a
  | Lift (m (FlipFreeT m a f))
  | Free (f (FlipFreeT m a f))
-}

-- TODO unify with Consumer into a FreeT like type
data Producer m a o
  = Pure a
  | Lift (m (Producer m a o))
  | Yield o (Producer m a o)

-- TODO rename a to r and b to r'

runProducer :: (Monad m) => (o -> m ()) -> Producer m a o -> m a
runProducer _ (Pure a) = pure a
runProducer put (Lift mnext) = mnext >>= runProducer put
runProducer put (Yield o next) = put o >> runProducer put next

instance (Applicative m, Semigroup a) => Semigroup (Producer m a o) where
  (<>) :: Producer m a o -> Producer m a o -> Producer m a o
{- ORMOLU_DISABLE -}
  Pure r1        <> Pure r2        = Pure $ r1 <> r2
  Pure r1        <> Lift mnext2    = Lift $ (Pure r1 <>) <$> mnext2
  Pure r1        <> Yield o2 next2 = Yield o2 $ Pure r1 <> next2
  Lift mnext1    <> Pure r2        = Lift $ mnext1 <&> (<> Pure r2)
  Lift mnext1    <> Lift mnext2    = Lift $ liftA2 (<>) mnext1 mnext2
  Lift mnext1    <> Yield o2 next2 = Lift $ mnext1 <&> \next1 -> Yield o2 $ next1 <> next2
  Yield o1 next1 <> Pure r2        = Yield o1 $ next1 <> Pure r2
  Yield o1 next1 <> Lift mnext2    = Yield o1 $ Lift $ (next1 <>) <$> mnext2
  Yield o1 next1 <> Yield o2 next2 = Yield o1 $ Yield o2 $ next1 <> next2
{- ORMOLU_ENABLE -}

instance (Applicative m, Monoid a) => Monoid (Producer m a o) where
  mempty :: Producer m a o
  mempty = Pure mempty

instance (Functor m) => Functor (Producer m a) where
  fmap :: (o -> o') -> Producer m a o -> Producer m a o'
  fmap _ (Pure a) = Pure a
  fmap f (Lift mnext) = Lift $ fmap f <$> mnext
  fmap f (Yield o next) = Yield (f o) $ fmap f next

instance (Functor m) => Bifunctor (Producer m) where
  bimap :: (a -> b) -> (o -> o') -> Producer m a o -> Producer m b o'
  bimap f _ (Pure a) = Pure (f a)
  bimap f g (Lift mnext) = Lift $ bimap f g <$> mnext
  bimap f g (Yield o next) = Yield (g o) $ bimap f g next

-- TODO
-- class Functor f <= Apply f where
--   apply :: ∀ a b. f (a -> b) -> f a -> f b
-- class Apply f <= Applicative f where
--   pure :: ∀ a. a -> f a

{-
-- TODO
Data.Functor.Monoidal
  ProducerT m r o1 |&| ProducerT m r o2 = ProducerT m r (These o1 o2)
  ProducerT m r o1 |+| ProducerT m r o2 = ProducerT m r (Either o1 o2)
  ProducerT m r o1 |*| ProducerT m r o2 = ProducerT m r (o1, o2)

  Data.Align / Semialign
  -- align :: f a -> f b -> f (These a b)
  -- race :: m a -> m b -> m (Either a b)
  -- concurrently :: m a -> m b -> m (a, b)

-- TODO
Coroutine / Cont ??
-}
