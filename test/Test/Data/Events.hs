module Test.Data.Events where

import Control.Concurrent.MVar (modifyMVar_, newMVar, swapMVar)
import Data.Events
import Test.Hspec

spec :: Spec
spec = do
  describe "Events.source and Events.sink" $ do
    it "should allow sending and receiving events" $ do
      (events, push) <- source
      await <- capture events
      push "message1"
      push "message2"
      await `shouldReturn` ["message1", "message2"]

  describe "Events Functor instance" $ do
    it "should map over events" $ do
      (events, push) <- source
      let lengthEvents = length <$> events
      await <- capture lengthEvents
      push "hello"
      await `shouldReturn` [5]

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

  describe "Events flattening" $ do
    it "should flatten lists" $ do
      (events, push) <- source
      let flattenedEvents = flatten events
      await <- capture flattenedEvents
      push [1, 2, 3]
      push [4, 5, 6]
      await `shouldReturn` [1, 2, 3, 4, 5, 6]

  describe "Events filtering" $ do
    it "should allow only values that satisfy the predicate" $ do
      (events, push) <- source
      let filteredEvents = filterPredicate (> 10) events
      await <- capture filteredEvents
      push (5 :: Int)
      push (15 :: Int)
      push (6 :: Int)
      push (16 :: Int)
      await `shouldReturn` [15, 16]

    it "should transform and filter values using Maybe" $ do
      (events, push) <- source
      let mappedEvents = filterMap (\x -> if even x then Just (x * 2) else Nothing) events
      await <- capture mappedEvents
      push (3 :: Int)
      push (4 :: Int)
      push (5 :: Int)
      push (6 :: Int)
      await `shouldReturn` [8, 12]

capture :: Events a -> IO (IO [a])
capture events = do
  var <- newMVar []
  sink (\val -> modifyMVar_ var $ \vals -> pure $ val : vals) events
  return $ reverse <$> swapMVar var []
