{-# LANGUAGE DataKinds #-}

module Test.Data.Events where

{- HLint ignore "Functor law" -}

import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Control.Timeout
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
        (matching (> 10))
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
        (relaxF (fmap length :: Events IO String -> Events IO Int))
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

    it "should remove repeats" $
      runAppTest
        ['a', 'a', 'b', 'b', 'b', 'a']
        removeRepeats
        ['a', 'b', 'a']

  describe "Events with time" $ do
    it "provide a constant stream of events" $
      runTimedTest
        [[], [], [], [], [], [], []]
        (const $ every (20 @ milliseconds))
        [[], [], [()], [], [()], [], [()]]
        []

    it "should postpone event delivery" $
      runTimedTest
        [['a'], ['b'], ['c'], ['d'], ['e'], ['f'], ['g']]
        (delay (30 @ milliseconds))
        [[], [], [], ['a'], ['b'], ['c'], ['d']]
        ['e', 'f', 'g']

    it "should delay event emission until inactivity" $
      runTimedTest
        [['a'], ['a'], ['b'], ['b'], [], [], [], ['a'], ['a', 'b']]
        (debounceAll (20 @ milliseconds))
        [[], [], [], [], [], ['b'], [], [], []]
        ['b']

    it "should delay event emission by value until inactivity" $
      runTimedTest
        [['a'], ['a'], ['b'], ['b'], [], [], [], ['a'], ['a', 'b']]
        (debounceByValue (20 @ milliseconds))
        [[], [], [], ['a'], [], ['b'], [], [], []]
        ['a', 'b']

    it "should limit event frequency" $
      runTimedTest
        [['a'], ['a'], ['a'], ['b'], [], [], [], ['a'], ['a', 'b']]
        (throttleAll (20 @ milliseconds))
        [['a'], [], ['a'], [], [], [], [], ['a'], []]
        []

    it "should limit event frequency by value" $
      runTimedTest
        [['a'], ['a'], ['a'], ['b'], [], [], [], ['a'], ['a', 'b']]
        (throttleByValue (20 @ milliseconds))
        [['a'], [], ['a'], ['b'], [], [], [], ['a'], ['b']]
        []

runTest :: (Eq b, Show b) => [a] -> (Events IO a -> Events IO b) -> [b] -> IO ()
runTest inputs f = runAppTest inputs (return . f)

runAppTest :: (Eq b, Show b) => [a] -> (Events IO a -> App (Events IO b)) -> [b] -> IO ()
runAppTest inputs f outputs =
  runApp $ do
    var <- liftIO IOSeq.new
    (events, push) <- liftIO source
    liftIO . sink (IOSeq.add var) =<< f events
    liftIO $ mapM_ push inputs
    liftIO $ IOSeq.get var `shouldReturn` Seq.fromList outputs

runStateTest :: (Eq s, Show s, Eq b, Show b) => s -> [a] -> ((a, s) -> (b, s)) -> [b] -> s -> IO ()
runStateTest beginState inputs f outputs endState =
  runAppTest
    inputs
    (withStateIO (pure beginState) (`shouldBe` endState) f)
    outputs

runTimedTest :: (Eq b, Show b) => [[a]] -> (Events IO a -> App (Events IO b)) -> [[b]] -> [b] -> IO ()
runTimedTest inputs f outputs afterwards = do
  total <- IOSeq.new
  var <- IOSeq.new
  runApp $ do
    (events, push) <- liftIO source
    liftIO . sink (IOSeq.add var) =<< f events
    liftIO $ forM_ inputs $ \inputspart -> do
      mapM_ push inputspart
      threadDelay (5 @ milliseconds)
      IOSeq.empty var >>= IOSeq.add total
      threadDelay (5 @ milliseconds)
  IOSeq.empty total `shouldReturn` Seq.fromList (Seq.fromList <$> outputs)
  IOSeq.empty var `shouldReturn` Seq.fromList afterwards
