{-# LANGUAGE DataKinds #-}

module Test.Data.Events where

import Control.Concurrent.MVar (modifyMVar_, newMVar, swapMVar)
import Control.Monad (forM_)
import Data.Char as Char
import Data.Events
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
      runTest2to1
        [Left "first", Right "second"]
        (uncurry (<>))
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
      runTest1to2
        ([5, 15, 6, 16] :: [Int])
        (partition' (> 10))
        [Left 5, Right 15, Left 6, Right 16]

    it "should unpartition Events" $
      runTest2to1
        ([Left 5, Right 15, Left 6, Right 16] :: [Either Int Int])
        unpartition'
        [5, 15, 6, 16]

    it "should split Events of Either" $
      runTest1to2
        [Left 'a', Right True, Left 'b', Right False]
        split
        [Left 'a', Right True, Left 'b', Right False]

    it "should unpartition Events" $
      runTest2to1
        ([Left 5, Right 15, Left 6, Right 16] :: [Either Int Int])
        join
        [Left 5, Right 15, Left 6, Right 16]

    it "should map both halves" $
      runTest
        ([Left 5, Right 15, Left 6, Right 16] :: [Either Int Int])
        (mapLeft (* 2) . mapRight (+ 1))
        [Left 10, Right 16, Left 12, Right 17]

    it "should map both halves" $
      runTest2to2
        ([Left 5, Right 15, Left 6, Right 16] :: [Either Int Int])
        (mapLeft' (fmap (* 2)) . mapRight' (fmap (+ 1)))
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
    let runStateTests configs = runApp Testing $ do
          forM_ configs $ \(beginState, inputs, f, outputs, endState) -> do
            statefull <- withStateIO (pure beginState) (`shouldBe` endState) f
            liftIO $ runTest inputs statefull outputs

    it "should keep track of state" $
      runStateTests
        [ ( 0,
            ["Hello", "world", "!"],
            \(a, s) -> (length a, s + length a),
            [5, 5, 1],
            11
          )
        ]

    it "should keep track of separate states" $
      runStateTests
        [ (0 :: Int, ['a'], \(a, s) -> (a, s + 1), ['a'], 1),
          (0 :: Int, ['b', 'c'], \(a, s) -> (a, s + 1), ['b', 'c'], 2)
        ]

    it "should remove repeats" $ runApp Testing $ do
      rm <- removeRepeats
      liftIO $
        runTest
          ['a', 'a', 'b', 'b', 'b', 'a']
          rm
          ['a', 'b', 'a']

capture :: Events a -> IO (IO [a])
capture events = do
  var <- newMVar []
  sink (\val -> modifyMVar_ var $ \vals -> pure $ val : vals) events
  return $ reverse <$> swapMVar var []

capture2 :: Events a -> Events b -> IO (IO [Either a b])
capture2 eventsA eventsB = do
  var <- newMVar []
  sink (\val -> modifyMVar_ var $ \vals -> pure $ Left val : vals) eventsA
  sink (\val -> modifyMVar_ var $ \vals -> pure $ Right val : vals) eventsB
  return $ reverse <$> swapMVar var []

source2 :: IO (Events a, Events b, Either a b -> IO ())
source2 = do
  (eventsA, pushA) <- source
  (eventsB, pushB) <- source
  let pushAB (Left a) = pushA a
      pushAB (Right b) = pushB b
  return (eventsA, eventsB, pushAB)

runTest :: (Eq b, Show b) => [a] -> (Events a -> Events b) -> [b] -> IO ()
runTest inputs f outputs = do
  (eventsA, push) <- source
  let eventsB = f eventsA
  await <- capture eventsB
  mapM_ push inputs
  await `shouldReturn` outputs

runTest1to2 :: (Eq b, Show b, Eq c, Show c) => [a] -> (Events a -> (Events b, Events c)) -> [Either b c] -> IO ()
runTest1to2 inputs f outputs = do
  (eventsA, push) <- source
  let (eventsB, eventsC) = f eventsA
  await <- capture2 eventsB eventsC
  mapM_ push inputs
  await `shouldReturn` outputs

runTest2to1 :: (Eq c, Show c) => [Either a b] -> ((Events a, Events b) -> Events c) -> [c] -> IO ()
runTest2to1 inputs f outputs = do
  (eventsA, eventsB, push) <- source2
  let eventsC = f (eventsA, eventsB)
  await <- capture eventsC
  mapM_ push inputs
  await `shouldReturn` outputs

runTest2to2 :: (Eq c, Show c, Eq d, Show d) => [Either a b] -> ((Events a, Events b) -> (Events c, Events d)) -> [Either c d] -> IO ()
runTest2to2 inputs f outputs = do
  (eventsA, eventsB, push) <- source2
  let (eventsC, eventsD) = f (eventsA, eventsB)
  await <- capture2 eventsC eventsD
  mapM_ push inputs
  await `shouldReturn` outputs
