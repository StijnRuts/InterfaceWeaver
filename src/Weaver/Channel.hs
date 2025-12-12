module Weaver.Channel where

import Control.Arrow
import Control.Category (Category)
import qualified Control.Category as C
import Data.Profunctor
import Witherable

data Channel eff i o where
  Pure :: (i -> o) -> Channel eff i o
  Eff :: eff i o -> Channel eff i o
  {-
  Lift :: a i o -> Channel a eff i o
  Input :: Channel eff i ()
  Output :: o -> Channel eff () o
  -}
  Seq :: Channel eff i x -> Channel eff x o -> Channel eff i o
  Par :: Channel eff i1 o1 -> Channel eff i2 o2 -> Channel eff (i1, i2) (o1, o2)
  Choice :: Channel eff i1 o1 -> Channel eff i2 o2 -> Channel eff (Either i1 i2) (Either o1 o2)
  Plus :: Channel eff i o -> Channel eff i o -> Channel eff i o
  Zero :: Channel eff i o
  App :: Channel eff (Channel eff i o, i) o
  Loop :: Channel eff (i, s) (o, s) -> Channel eff i o
  MapMaybe :: (o -> Maybe o') -> Channel eff i o -> Channel eff i o'

instance Category (Channel eff) where
  id :: Channel eff a a
  id = Pure id

  (.) :: Channel eff x o -> Channel eff i x -> Channel eff i o
  (.) = flip Seq

instance Arrow (Channel eff) where
  arr :: (i -> o) -> Channel eff i o
  arr = Pure

  (***) :: Channel eff i1 o1 -> Channel eff i2 o2 -> Channel eff (i1, i2) (o1, o2)
  (***) = Par

instance ArrowChoice (Channel eff) where
  (+++) :: Channel eff i1 o1 -> Channel eff i2 o2 -> Channel eff (Either i1 i2) (Either o1 o2)
  (+++) = Choice

instance ArrowZero (Channel eff) where
  zeroArrow :: Channel eff i o
  zeroArrow = Zero

instance ArrowPlus (Channel eff) where
  (<+>) :: Channel eff i o -> Channel eff i o -> Channel eff i o
  (<+>) = Plus

instance ArrowApply (Channel eff) where
  app :: Channel eff (Channel eff i o, i) o
  app = App

instance ArrowLoop (Channel eff) where
  loop :: Channel eff (i, s) (o, s) -> Channel eff i o
  loop = Loop

instance Profunctor (Channel eff) where
  lmap :: (i' -> i) -> Channel eff i o -> Channel eff i' o
  lmap = (^>>)

  rmap :: (o -> o') -> Channel eff i o -> Channel eff i o'
  rmap = flip (>>^)

instance Functor (Channel eff i) where
  fmap :: (o -> o') -> Channel eff i o -> Channel eff i o'
  fmap = rmap

instance Applicative (Channel eff i) where
  pure :: o -> Channel eff i o
  pure = Pure . const

  (<*>) :: Channel eff i (o -> o') -> Channel eff i o -> Channel eff i o'
  fArr <*> oArr = (C.id &&& C.id) >>> (fArr *** oArr) >>> arr (uncurry ($))

instance (Semigroup o) => Semigroup (Channel eff i o) where
  (<>) :: Channel eff i o -> Channel eff i o -> Channel eff i o
  lArr <> rArr = (<>) <$> lArr <*> rArr

instance (Monoid o) => Monoid (Channel eff i o) where
  mempty :: Channel eff i o
  mempty = pure mempty

instance Monad (Channel eff i) where
  (>>=) :: Channel eff i o -> (o -> Channel eff i o') -> Channel eff i o'
  a >>= f = (a >>^ f) &&& C.id >>> App

instance Filterable (Channel eff i) where
  mapMaybe :: (o -> Maybe o') -> Channel eff i o -> Channel eff i o'
  mapMaybe = MapMaybe

(>>|) :: (Filterable f) => f a -> (a -> Maybe b) -> f b
(>>|) = flip mapMaybe

(>|>) :: (Filterable f) => f a -> (a -> Bool) -> f a
(>|>) = flip Witherable.filter

