module Weaver.Channel where

import Data.Bifunctor
import Data.Functor ((<&>))
import Data.Functor.Contravariant

{-
-- TODO

data Channel m r i o = Channel (Input m r i) (Output m r o)
-- type Program m r = Channel m r Void Void

runChannel (m i) -> (o -> m ()) -> Channel i o -> m ()

ISOMORPHISMs

data Iso k a b
  embed :: k a b
  project :: k b a

-- Iso specialized to the function arrow
type IsoF a b = Iso (->) a b

swapIso :: IsoF (a, b) (b, a)
swapIso = Iso
  { embed   = \(x, y) -> (y, x)
  , project = \(y, x) -> (x, y)
  }

swapIso :: Arrow k => Iso k (a, b) (b, a)
swapIso = Iso
  { embed   = arr (\(x, y) -> (y, x))
  , project = arr (\(y, x) -> (x, y))
  }

swapIso :: Arrow k => Iso k (a, b) (b, a)
swapIso = Iso
  { embed   = arr snd &&& arr fst
  , project = arr snd &&& arr fst
  }

class Convert a b where
    convert :: a -> b

instance Convert [a] (Either a a) where
    convert []      = Left undefined
    convert (x:_)   = Right x

instance Convert (a, a) [a] where
    convert (x, y) = [x, y]

instance Convert (Those a a) (Either a a) where
    convert (This x)      = Left x
    convert (That y)      = Right y
    convert (These x _)   = Left x

foo :: Either Int Int
foo = convert [1,2,3]

bar :: [Int]
bar = convert (1,2)

main :: IO ()
main = do
  print $ embed swapIso (1, "hi")     -- ("hi",1)
  print $ project swapIso ("hi", 1)   -- (1,"hi")

Producer (Either Void o) <-> Producer o
Producer (Either o Void) <-> Producer o
  Tuple, These ???

Consumer (Either Void i) <-> Consumer i
Consumer (Either i Void) <-> Consumer i
  Tuple, These ???

Channel i o <-> (Consumer i, Producer o)

Channel Void o <-> Producer o
Channel i Void <-> Consumer i

type Program = Channel Void Void
runProgram :: Program m () -> m ()

Channel (Either i Void) o <-> Channel i o
Channel i (Either o Void) <-> Channel i o
Channel (Either i Void) (Either o Void) <-> Channel i o
  Tuple, These ???

-- Channel i o

Semigroup
Monoid
Category
Functor
Profunctor
Profunctor Arrows
Arrows
Coroutine / Cont ??
Monad ?

zero :: a/b
id   :: a/a

loop :: a&s/b&s -> a/b
      = a/b + State s -> a/b
      = m (a/b) -> a/b

(>>>) :: a/x -> x/b   ->    a/b

(<+>) :: a/b -> a/b   ->    a/b
(+++) :: a/b -> a'/b' -> a|a'/b|b'
(|||) :: a/b -> a'/b  -> a|a'/b

-- | = Either, & = Tuple, * = These
(-+-) :: a/b -> a/b   ->    a/b
(-+|) :: a/b -> a/b'  ->    a/b|b'
(|+-) :: a/b -> a'/b  -> a|a'/b
(|+|) :: a/b -> a'/b' -> a|a'/b|b'
(&+-) :: a/b -> a'/b  -> a&a'/b
(&+|) :: a/b -> a'/b' -> a&a'/b|b'
(*+-) :: a/b -> a'/b  -> a*a'/b
(*+|) :: a/b -> a'/b' -> a*a'/b|b'

---------------------
EMPTY :: a/b -- eat all input, never produce output
---------------------
NEW :: a/a -- pass all input to output
-- NEW :: a/b
--   a -> mb -- in to out == a/a + MAP
---------------------
MAP a/b -> e/f
  e -> ma -- in to in
  b -> mf -- out to out
---------------------
--SEQ a/x -> x/b -> a/b
SEQ a/b -> c/d -> e/f
  -- e -> ma
  b -> mc  -- out to in
  -- d -> mf
---------------------
PAR a/b -> c/d -> e/f
  e -> These ma mc -- in to ins
  Either b d -> mf -- outs to out
---------------------
LOOP a/b -> e/f
  mb -- initial
  -- e -> ma -- in to in
  (e,b) -> ma -- feedback
  -- b -> mf -- out to out
---------------------
CROSS a/b -> c/d -> e/f
  m (Either b d) -- initial
  -- e -> These ma mc -- in to ins
  (e,Either b d) -> These ma mc -- feedback
  -- Either b d -> mf -- outs to out

== merge (= PAR id id)  a/b with c/d = These a c / Either b d
   then use LOOP
---------------------

Goal: ti|i→to|o => i→o
Given by Timer: ∅→ti + to→∅ == ∅|to→ti|∅ == to→ti

∅→ti PAR i→i = ∅|i→ti|i = i→ti|i
to→∅ PAR o→o = to|o→∅|o = to|o→o

i→ti|i SEQ ti|i→to|o SEQ to|o→o = i→o
\^^^^^^^    ^^^^^^^^^     ^^^^^^^  ^^^^
 Timer       Input        Timer  Output

Coroutines !!!
https://www.notion.so/Coroutines-2d247bfaede9802486c3df35d65e0bf5
https://hackage.haskell.org/package/Coroutine-0.1.0.0/docs/Control-Coroutine.html

--

data Coroutine y r
  = Done r
  | Yield y (Coroutine y r)

--

data Step y r
  = Done r
  | Yield y (Cont (Step y r) r)

--

data CoF o i k
  = Yield o k
  | Await (i -> k)
  deriving Functor

type Coroutine o i = Free (CoF o i)

--

data YieldF o k
  = Yield o k
  deriving Functor

data AwaitF i k
  = Await (i -> k)
  deriving Functor

data (f :+: g) x
  = InL (f x)
  | InR (g x)
  deriving Functor

type Coroutine o i = Free (YieldF o :+: AwaitF i)
type CoroutineT o i m = FreeT (YieldF o :+: AwaitF i) m

------------

Control.Monad.Cont
cont :: ((a -> r) -> r) -> Cont r a
runCont :: Cont r a -> (a -> r)	-> r

--

data Free f a
  Pure a
  Free (f (Free f a))

newtype FreeT f m a
  runFreeT :: m (FreeF f a (FreeT f m a))
iterT :: (f (m a) -> m a) -> FreeT f m a -> m a
runFreeT :: FreeT f m a -> m (FreeF f a (FreeT f m a))

-}
