{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Data.Polyfunctor where

import Data.Bifunctor
import Data.Functor.Contravariant
import Data.Profunctor

class RCoPolyfunctor p where
  rCoMap :: (b -> d) -> p x b -> p x d

class RContraPolyfunctor p where
  rContraMap :: (d -> b) -> p x b -> p x d

class LCoPolyfunctor p where
  lCoMap :: (a -> c) -> p a x -> p c x

class LContraPolyfunctor p where
  lContraMap :: (c -> a) -> p a x -> p c x

class (LCoPolyfunctor p, RCoPolyfunctor p) => CoCoPolyfunctor p where
  coCoMap :: (a -> c) -> (b -> d) -> p a b -> p c d
  coCoMap f g = lCoMap f . rCoMap g
  {-# INLINE coCoMap #-}

class (LCoPolyfunctor p, RContraPolyfunctor p) => CoContraPolyfunctor p where
  coContraMap :: (a -> c) -> (d -> b) -> p a b -> p c d
  coContraMap f g = lCoMap f . rContraMap g
  {-# INLINE coContraMap #-}

class (LContraPolyfunctor p, RCoPolyfunctor p) => ContraCoPolyfunctor p where
  contraCoMap :: (c -> a) -> (b -> d) -> p a b -> p c d
  contraCoMap f g = lContraMap f . rCoMap g
  {-# INLINE contraCoMap #-}

class (LContraPolyfunctor p, RContraPolyfunctor p) => ContraContraPolyfunctor p where
  contraContraMap :: (c -> a) -> (d -> b) -> p a b -> p c d
  contraContraMap f g = lContraMap f . rContraMap g
  {-# INLINE contraContraMap #-}

instance (RCoPolyfunctor p) => Functor (p x) where
  fmap = rCoMap

instance (RContraPolyfunctor p) => Contravariant (p x) where
  contramap = rContraMap

instance (CoCoPolyfunctor p) => Bifunctor p where
  bimap = coCoMap
  first = lCoMap
  second = rCoMap

instance (ContraCoPolyfunctor p) => Profunctor p where
  dimap = contraCoMap
  lmap = lContraMap
  rmap = rCoMap
