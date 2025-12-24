module Data.Functor.Bicontravariant where

class Bicontravariant p where
  cbimap :: (a -> b) -> (d -> c) -> p a c -> p b d
  cbimap f g = cfirst f . csecond g
  {-# INLINE cbimap #-}

  cfirst :: (a -> b) -> p a c -> p b c
  cfirst f = cbimap f id
  {-# INLINE cfirst #-}

  csecond :: (d -> c) -> p a c -> p a d
  csecond = cbimap id
  {-# INLINE csecond #-}
