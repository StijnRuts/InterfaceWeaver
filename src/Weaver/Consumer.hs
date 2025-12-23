module Weaver.Consumer where

import Data.Functor ((<&>))
import Data.Functor.Contravariant

-- TODO unify with Producer into a FreeT like type
data Consumer m a i
  = Pure a
  | Lift (m (Consumer m a i))
  | Await (i -> (Consumer m a i))

runConsumer :: (Monad m) => (m i) -> Consumer m a i -> m a
runConsumer _ (Pure a) = pure a
runConsumer get (Lift mnext) = mnext >>= runConsumer get
runConsumer get (Await inext) = runConsumer get . inext =<< get

instance (Applicative m, Semigroup a) => Semigroup (Consumer m a i) where
  (<>) :: Consumer m a i -> Consumer m a i -> Consumer m a i
{- ORMOLU_DISABLE -}
  Pure r1      <> Pure r2      = Pure  $ r1 <> r2
  Pure r1      <> Lift mnext2  = Lift  $ (Pure r1 <>) <$> mnext2
  Pure r1      <> Await inext2 = Await $ \i2 -> Pure r1 <> inext2 i2
  Lift mnext1  <> Pure r2      = Lift  $ mnext1 <&> (<> Pure r2)
  Lift mnext1  <> Lift mnext2  = Lift  $ liftA2 (<>) mnext1 mnext2
  Lift mnext1  <> Await inext2 = Lift  $ mnext1 <&> \next1 -> Await $ \i2 -> next1 <> inext2 i2
  Await inext1 <> Pure r2      = Await $ \i1 -> inext1 i1 <> Pure r2
  Await inext1 <> Lift mnext2  = Await $ \i1 -> Lift $ (inext1 i1 <>) <$> mnext2
  Await inext1 <> Await inext2 = Await $ \i1 -> Await $ \i2 -> inext1 i1 <> inext2 i2
{- ORMOLU_ENABLE -}

instance (Applicative m, Monoid a) => Monoid (Consumer m a i) where
  mempty :: Consumer m a i
  mempty = Pure mempty

instance (Functor m) => Contravariant (Consumer m a) where
  contramap :: (i' -> i) -> Consumer m a i -> Consumer m a i'
  contramap _ (Pure a) = Pure a
  contramap f (Lift mnext) = Lift $ contramap f <$> mnext
  contramap f (Await inext) = Await $ contramap f . inext . f

---------------------------------------------------------

-- TODO move to separate file
class Contrabivariant p where
  cbimap :: (a -> b) -> (d -> c) -> p a c -> p b d
  cbimap f g = cfirst f . csecond g
  {-# INLINE cbimap #-}

  cfirst :: (a -> b) -> p a c -> p b c
  cfirst f = cbimap f id
  {-# INLINE cfirst #-}

  csecond :: (d -> c) -> p a c -> p a d
  csecond g = cbimap id g
  {-# INLINE csecond #-}

---------------------------------------------------------

instance (Functor m) => Contrabivariant (Consumer m) where
  cbimap :: (a -> b) -> (i' -> i) -> Consumer m a i -> Consumer m b i'
  cbimap f _ (Pure a) = Pure (f a)
  cbimap f g (Lift mnext) = Lift $ cbimap f g <$> mnext
  cbimap f g (Await inext) = Await $ cbimap f g . inext . g

-- class Functor f <= Apply f where
--   apply :: ∀ a b. f (a -> b) -> f a -> f b
-- class Apply f <= Applicative f where
--   pure :: ∀ a. a -> f a

{-
Data.Functor.Monoidal
  -- https://hackage.haskell.org/package/monoidal-functors-0.2.3.0
  ConsumerT m r i1 |&| ConsumerT m r i2 = ConsumerT m r (These i1 i2)
  ConsumerT m r i1 |+| ConsumerT m r i2 = ConsumerT m r (Either i1 i2)
  ConsumerT m r i1 |*| ConsumerT m r i2 = ConsumerT m r (i1, i2)

Coroutine / Cont ??
-}
