module Weaver.Producer where

import Data.Bifunctor
import Data.Functor ((<&>))

-- TODO Rewrite to FreeT
data Producer m r o
  = Pure r
  | Lift (m (Producer m r o))
  | Yield o (Producer m r o)

runProducer :: (Monad m) => (o -> m ()) -> Producer m r o -> m r
runProducer _ (Pure r) = pure r
runProducer put (Lift mnext) = mnext >>= runProducer put
runProducer put (Yield o next) = put o >> runProducer put next

instance (Applicative m, Semigroup r) => Semigroup (Producer m r o) where
  (<>) :: Producer m r o -> Producer m r o -> Producer m r o
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

instance (Applicative m, Monoid r) => Monoid (Producer m r o) where
  mempty :: Producer m r o
  mempty = Pure mempty

instance (Functor m) => Functor (Producer m r) where
  fmap :: (o -> o') -> Producer m r o -> Producer m r o'
  fmap _ (Pure r) = Pure r
  fmap f (Lift mnext) = Lift $ fmap f <$> mnext
  fmap f (Yield o next) = Yield (f o) $ fmap f next

instance (Functor m) => Bifunctor (Producer m) where
  bimap :: (r -> r') -> (o -> o') -> Producer m r o -> Producer m r' o'
  bimap f _ (Pure r) = Pure (f r)
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
