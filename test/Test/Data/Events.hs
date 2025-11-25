{-# LANGUAGE DataKinds #-}

module Test.Data.Events where

{- HLint ignore "Functor law" -}

import Data.Char as Char
import Data.Events
import Data.Functor ((<&>))
import Data.IO.Seq as IOSeq
import qualified Data.Sequence as Seq
import Data.Union
import InterfaceWeaver.App
import Test.Hspec

spec :: Spec
spec = do
  describe "Events Functor instance" $ do
    it "should map over events" $
      runTest
        ["Hello"]
        (fmap length)
        [5]

  describe "Events Semigroup instance" $ do
    it "should combine events" $
      runTest
        [Left "first", Right "second"]
        (split <&> uncurry (<>))
        ["first", "second"]

  describe "Events Monoid instance" $ do
    it "should ignore mempty events" $
      runTest
        ["first", "second"]
        {- HLint ignore "Monoid law, left identity" -}
        {- HLint ignore "Monoid law, right identity" -}
        (\events -> mempty <> events <> mempty)
        ["first", "second"]

  describe "Events flattening" $ do
    it "should flatten lists" $
      runTest
        [["a", "b"], ["c", "d"]]
        flatten
        ["a", "b", "c", "d"]

  describe "Events filtering" $ do
    it "should allow only values that satisfy the predicate" $
      runTest
        ([5, 15, 6, 16] :: [Int])
        (filterPredicate (> 10))
        [15, 16]

    it "should transform and filter values using Maybe" $
      runTest
        ([3, 4, 5, 6] :: [Int])
        (filterMap (\x -> if even x then Just (x * 2) else Nothing))
        [8, 12]

  describe "Parallel Events streams" $ do
    it "should partition values into Eithers" $
      runTest
        ([5, 15, 6, 16] :: [Int])
        (partition (> 10))
        [Left 5, Right 15, Left 6, Right 16]

    it "should unpartition Eithers into values" $
      runTest
        [Left 5, Right 15, Left 6, Right 16]
        unpartition
        ([5, 15, 6, 16] :: [Int])

    it "should partition Events based on a predicate" $
      runTest
        ([5, 15, 6, 16] :: [Int])
        (partition' (> 10) <&> join)
        [Left 5, Right 15, Left 6, Right 16]

    it "should unpartition Events" $
      runTest
        ([Left 5, Right 15, Left 6, Right 16] :: [Either Int Int])
        (split <&> unpartition')
        [5, 15, 6, 16]

    it "should map both halves" $
      runTest
        ([Left 5, Right 15, Left 6, Right 16] :: [Either Int Int])
        (mapLeft (* 2) . mapRight (+ 1))
        [Left 10, Right 16, Left 12, Right 17]

    it "should map both halves" $
      runTest
        ([Left 5, Right 15, Left 6, Right 16] :: [Either Int Int])
        (split <&> mapLeft' (fmap (* 2)) . mapRight' (fmap (+ 1)) <&> join)
        [Left 10, Right 16, Left 12, Right 17]

  describe "Events of Union types" $ do
    let lengthOrToUpper :: Union '[String, Char] -> Union '[Int, Char]
        lengthOrToUpper =
          replace (length :: String -> Int)
            . replace Char.toUpper

    it "should handle Events of Unions" $
      runTest
        ([inject "Hello", inject 'a', inject "world!"] :: [Union '[String, Char]])
        (fmap lengthOrToUpper)
        ([inject (5 :: Int), inject 'A', inject (6 :: Int)] :: [Union '[Int, Char]])

    it "should wrap an Events into an Events Union" $
      runTest
        ['a', 'b']
        relax
        ([inject 'a', inject 'b'] :: [Union '[Int, Char, Bool]])

    it "should extract an Events from an Events Union" $
      runTest
        ([inject 'a', inject True, inject 'b'] :: [Union '[Int, Char, Bool]])
        specialize
        ['a', 'b']

    it "should widen the Events Union" $
      runTest
        ([inject 'a', inject True, inject 'b'] :: [Union '[Char, Bool]])
        widen
        ([inject 'a', inject True, inject 'b'] :: [Union '[Int, Char, Bool]])

    it "should wrap an Events function into Events Unions" $
      runTest
        ([inject "Hello", inject True, inject "world!"] :: [Union '[String, Bool]])
        (relaxF (fmap length :: Events String -> Events Int))
        ([inject (5 :: Int), inject (6 :: Int)] :: [Union '[Int, Char]])

    it "should extract an Events function from an Events Unions function" $
      runTest
        ["Hello", "world!"]
        (specializeF (fmap lengthOrToUpper))
        [5 :: Int, 6 :: Int]

  describe "Events with state" $ do
    it "should keep track of state" $
      runStateTest
        0
        ["Hello", "world", "!"]
        (\(a, s) -> (length a, s + length a))
        [5, 5, 1]
        11

    it "should remove repeats" $ runApp Testing $ do
      rm <- removeRepeats
      liftIO $
        runTest
          ['a', 'a', 'b', 'b', 'b', 'a']
          rm
          ['a', 'b', 'a']


runTest :: (Eq b, Show b) => [a] -> (Events a -> Events b) -> [b] -> IO ()
runTest inputs f outputs = do
  var <- IOSeq.new
  (events, push) <- source
  sink (IOSeq.add var) (f events)
  mapM_ push inputs
  IOSeq.get var `shouldReturn` Seq.fromList outputs

runStateTest :: (Eq s, Show s, Eq b, Show b) => s -> [a] -> ((a, s) -> (b, s)) -> [b] -> s -> IO ()
runStateTest beginState inputs f outputs endState = runApp Testing $ do
  statefull <- withStateIO (pure beginState) (`shouldBe` endState) f
  liftIO $ runTest inputs statefull outputs

