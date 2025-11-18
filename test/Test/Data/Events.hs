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
    it "should combine events" $ do
      (events1, push1) <- source
      (events2, push2) <- source
      let events = events1 <> events2
      await <- capture events
      push1 "first"
      await `shouldReturn` ["first"]
      push2 "second"
      await `shouldReturn` ["second"]

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

  describe "Events of Union types" $ do
    let lengthOrToUpper :: Union '[String, Char] -> Union '[Int, Char]
        lengthOrToUpper x =
          case (project @String x, project @Char x) of
            (Just str, _) -> inject $ length str
            (_, Just c) -> inject $ Char.toUpper c
            _ -> error "Unknown type"

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
    let runStateTests configs = runAppTest $ do
          forM_ configs $ \(beginState, inputs, f, outputs, endState) -> do
            statefull <- withStateIO (pure beginState) (`shouldBe` endState) f
            run $ runTest inputs statefull outputs

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

    it "should remove repeats" $ runAppTest $ do
      rm <- removeRepeats
      run $
        runTest
          ['a', 'a', 'b', 'b', 'b', 'a']
          rm
          ['a', 'b', 'a']

capture :: Events a -> IO (IO [a])
capture events = do
  var <- newMVar []
  sink (\val -> modifyMVar_ var $ \vals -> pure $ val : vals) events
  return $ reverse <$> swapMVar var []

runTest :: (Eq b, Show b) => [a] -> (Events a -> Events b) -> [b] -> IO ()
runTest inputs f outputs = do
  (events, push) <- source
  let fEvents = f events
  await <- capture fEvents
  mapM_ push inputs
  await `shouldReturn` outputs
