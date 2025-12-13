{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}

module Weaver.Channel where

import Control.Arrow
import Control.Category (Category)
import qualified Control.Category as C
import Data.Profunctor
import Witherable

data Channel eff i o where
  Pure :: (i -> o) -> Channel eff i o
  Eff :: eff i o -> Channel eff i o
  -- Input :: Channel eff i ()
  -- Output :: o -> Channel eff () o
  Seq :: Channel eff i x -> Channel eff x o -> Channel eff i o
  Par :: Channel eff i1 o1 -> Channel eff i2 o2 -> Channel eff (i1, i2) (o1, o2)
  Choice :: Channel eff i1 o1 -> Channel eff i2 o2 -> Channel eff (Either i1 i2) (Either o1 o2)
  Plus :: Channel eff i o -> Channel eff i o -> Channel eff i o
  Zero :: Channel eff i o
  App :: Channel eff (Channel eff i o, i) o
  Loop :: Channel eff (i, s) (o, s) -> Channel eff i o

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
  fChan <*> oChan = proc i -> do
    f <- fChan -< i
    o <- oChan -< i
    returnA -< f o

instance (Semigroup o) => Semigroup (Channel eff i o) where
  (<>) :: Channel eff i o -> Channel eff i o -> Channel eff i o
  lArr <> rArr = (<>) <$> lArr <*> rArr

instance (Monoid o) => Monoid (Channel eff i o) where
  mempty :: Channel eff i o
  mempty = pure mempty

instance Monad (Channel eff i) where
  (>>=) :: Channel eff i o -> (o -> Channel eff i o') -> Channel eff i o'
  chan >>= f = (chan >>^ f) &&& C.id >>> app

{-
  chan >>= f = proc i -> do
    o <- chan -< i
    o' <- f o -< i -- Variable not in scope: o   ಠ_ಠ
    returnA -< o'
-}

instance Filterable (Channel eff i) where
  mapMaybe :: (o -> Maybe o') -> Channel eff i o -> Channel eff i o'
  mapMaybe f chan = proc i -> do
    o <- chan -< i
    case f o of
      Just o' -> returnA -< o'
      Nothing -> zeroArrow -< ()

(>||) :: (Filterable f) => f a -> (a -> Bool) -> f a
(>||) = flip Witherable.filter

(>|^) :: (Filterable f) => f a -> (a -> Maybe b) -> f b
(>|^) = flip mapMaybe

(||<) :: (Filterable f) => (a -> Bool) -> f a -> f a
(||<) = Witherable.filter

(^|<) :: (Filterable f) => (a -> Maybe b) -> f a -> f b
(^|<) = mapMaybe


runChannel :: (forall a b. eff a b -> a -> IO b) -> Channel eff i o -> (i -> IO o)
runChannel runEff chan =
  case chan of
    (Pure f) -> pure . f
    (Eff e) -> runEff e
    (Seq l r) -> \i -> do
      x <- runChannel runEff l i
      o <- runChannel runEff r x
      pure o {- HLint ignore "Redundant pure" -}
    (Par l r) -> \(i1, i2) -> do
      o1 <- runChannel runEff l i1
      o2 <- runChannel runEff r i2
      pure (o1, o2)
    (Choice l r) -> error "Not yet implemented" -- Choice :: Channel eff i1 o1 -> Channel eff i2 o2 -> Channel eff (Either i1 i2) (Either o1 o2)
    (Plus l r) -> error "Not yet implemented" -- Plus :: Channel eff i o -> Channel eff i o -> Channel eff i o
    Zero -> error "Not yet implemented" -- Zero :: Channel eff i o
    App -> error "Not yet implemented" -- App :: Channel eff (Channel eff i o, i) o
    (Loop c) -> error "Not yet implemented" -- Loop :: Channel eff (i, s) (o, s) -> Channel eff i o

--

data MyEff i o where
  ReadLine :: MyEff () String
  WriteLine :: MyEff String ()

readLine :: Channel MyEff () String
readLine = Eff ReadLine

writeLine :: String -> Channel MyEff () ()
writeLine s = pure s >>> Eff WriteLine

runMyEff :: MyEff i o -> i -> IO o
runMyEff ReadLine () = getLine
runMyEff WriteLine s = putStrLn s

program :: Channel MyEff () ()
program = do
  writeLine "Enter your name:"
  name <- readLine
  writeLine $ "Hello, " ++ name ++ "!"

main :: IO ()
main = runChannel runMyEff program ()

