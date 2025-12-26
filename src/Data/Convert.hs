module Data.Convert where

class Convert a b where
  convert :: a -> b


  a -> b
Corresponding optic: Getter
  view :: Getter s a -> s -> a

  a -> b
  b -> Maybe a
Corresponding optic: Prism
  preview :: Prism s t a b -> s -> Maybe a
  review :: Prism s t a b -> b -> t
Implies Getter a b, Prism a b

  a -> b
  b -> a
Corresponding optic: Iso
  to :: Iso a b -> a -> b
  from :: Iso a b -> b -> a
Implies Getter a b + Getter b a







maybe : Just a | Nothing

either : Left a | Right b
these : This a | That b | These a b

tuple type type: Tuple a b
tuple maybe type:
tuple type maybe:
tuple maybe maybe:

versions with void





https://hackage.haskell.org/package/optics-core-0.4.1.1

Data.Either.Optics
  _Left :: Prism (Either a b) (Either c b) a c
  _Right :: Prism (Either a b) (Either a c) b c

Data.Maybe.Optics
  _Nothing :: Prism' (Maybe a) ()
  _Just :: Prism (Maybe a) (Maybe b) a b

Data.Tuple.Optics
  _1 :: Lens s t a b
  _2 :: Lens s t a b
  _3 :: Lens s t a b

Optics.Getter
  to :: (s -> a) -> Getter s a
  view :: Is k A_Getter => Optic' k is s a -> s -> a

Optics.Review
  unto :: (b -> t) -> Review t b
  review :: Is k A_Review => Optic' k is t b -> b -> t

Optics.Setter
  sets :: ((a -> b) -> s -> t) -> Setter s t a b
  over :: Is k A_Setter => Optic k is s t a b -> (a -> b) -> s -> t

Optics.Prism
  prism :: (b -> t) -> (s -> Either t a) -> Prism s t a b
A Prism is in particular an AffineFold, an AffineTraversal, a Review and a Setter, therefore you can specialise types to obtain:
  preview  :: Prism' s a -> s -> Maybe a
  review   :: Prism' s a -> a -> s

Optics.Iso
  iso :: (s -> a) -> (b -> t) -> Iso s t a b
  type Iso' s a = Optic' An_Iso NoIx s a
An Iso is in particular a Getter, a Review and a Setter, therefore you can specialise types to obtain:
  view   :: Iso' s a -> s -> a
  review :: Iso' s a -> a -> s
  over   :: Iso s t a b -> (a -> b) -> s -> t
  set    :: Iso s t a b ->       b  -> s -> t









