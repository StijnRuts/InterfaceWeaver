{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Union where

import Data.Kind (Type)

data Union (r :: [Type]) where
  This :: t -> Union (t ': r)
  That :: Union r -> Union (t ': r)

class ShowUnion (r :: [Type]) where
  showUnion :: Union r -> String

instance {-# OVERLAPPING #-} (Show t) => ShowUnion (t ': '[]) where
  showUnion (This t) = show t
  showUnion (That r) = undefined

instance {-# OVERLAPPABLE #-} (Show t, ShowUnion r) => ShowUnion (t ': r) where
  showUnion (This t) = show t
  showUnion (That r) = showUnion r

instance (ShowUnion r) => Show (Union r) where
  show = showUnion

class EqUnion (r :: [Type]) where
  eqUnion :: Union r -> Union r -> Bool

instance {-# OVERLAPPING #-} (Eq t) => EqUnion (t ': '[]) where
  eqUnion (This t1) (This t2) = t1 == t2
  eqUnion _ _ = undefined

instance {-# OVERLAPPABLE #-} (Eq t, EqUnion r) => EqUnion (t ': r) where
  eqUnion (This t1) (This t2) = t1 == t2
  eqUnion (That r1) (That r2) = eqUnion r1 r2
  eqUnion _ _ = False

instance (EqUnion r) => Eq (Union r) where
  (==) = eqUnion

class Member t u where
  inject :: t -> Union u
  project :: Union u -> Maybe t

instance {-# OVERLAPPING #-} Member t (t ': r) where
  inject = This
  project (This x) = Just x
  project _ = Nothing

instance {-# OVERLAPPABLE #-} (Member t r) => Member t (x ': r) where
  inject = That . inject
  project (That u) = project u
  project _ = Nothing

type family Remove t u where
  Remove t '[] = '[]
  Remove t (t ': xs) = xs
  Remove t (x ': xs) = x ': Remove t xs

class RemoveMember t u r where
  remove :: Union u -> Either (Union r) t

instance {-# OVERLAPPING #-} RemoveMember t (t ': r) r where
  remove (This t) = Right t
  remove (That r) = Left r

instance {-# OVERLAPPABLE #-} (RemoveMember t r s) => RemoveMember t (x ': r) (x ': s) where
  remove (This x) = Left (This x)
  remove (That r) = case remove r of
    Right t -> Right t
    Left s -> Left (That s)

type family Replace a b r where
  Replace a b '[] = '[]
  Replace a b (a ': r) = b ': r
  Replace a b (x ': r) = x ': Replace a b r

class ReplaceMember a b ua ub where
  replace :: (a -> b) -> Union ua -> Union ub

instance {-# OVERLAPPING #-} ReplaceMember a a (a ': r) (a ': r) where
  replace f (This a) = This (f a)
  replace _ (That r) = That r

instance {-# OVERLAPPING #-} ReplaceMember a b (a ': r) (b ': r) where
  replace f (This a) = This (f a)
  replace _ (That r) = That r

instance {-# OVERLAPPABLE #-} (ReplaceMember a b ua ub) => ReplaceMember a b (x ': ua) (x ': ub) where
  replace _ (This x) = This x
  replace f (That ua) = That (replace f ua)
