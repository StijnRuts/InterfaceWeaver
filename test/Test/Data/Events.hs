module Test.Data.Events where

import Control.Concurrent.MVar (modifyMVar_, newMVar, swapMVar)
import Control.Monad (forM_)
import Data.Events
import InterfaceWeaver.App
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

  describe "Events Monoid instance" $ do
    it "should ignore mempty events" $ do
      (events, push) <- source
      {- HLint ignore "Monoid law, left identity" -}
      {- HLint ignore "Monoid law, right identity" -}
      let mergedEvents = mempty <> events <> mempty
      await <- capture mergedEvents
      push "first"
      await `shouldReturn` ["first"]
      push "second"
      await `shouldReturn` ["second"]

  describe "Events flattening" $ do
    it "should flatten lists" $ do
      (events, push) <- source
      let flattenedEvents = flatten events
      await <- capture flattenedEvents
      push ["a", "b"]
      push ["c", "d"]
      await `shouldReturn` ["a", "b", "c", "d"]

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

  describe "Events with state" $ do
    let runStateTests configs = interfaceWeaverTest $ do
          forM_ configs $ \(update, pushes, expected, endstate) -> do
            statefull <-
              withStateIO
                (pure (0 :: Int))
                (`shouldBe` endstate)
                update
            (events, push) <- run source
            let statefullEvents = statefull events
            await <- run $ capture statefullEvents
            mapM_ (run . push) pushes
            run $ await `shouldReturn` expected

    it "should keep track of state" $
      runStateTests
        [ ( \(a, s) -> (s + a, s + a),
            [1, 2, 3],
            [1, 3, 6],
            6
          )
        ]

    it "should keep track of separate states" $
      runStateTests
        [ ( \(a, s) -> (a, s + 1),
            [1],
            [1],
            1
          ),
          ( \(a, s) -> (a, s + 1),
            [2, 3],
            [2, 3],
            2
          )
        ]

capture :: Events a -> IO (IO [a])
capture events = do
  var <- newMVar []
  sink (\val -> modifyMVar_ var $ \vals -> pure $ val : vals) events
  return $ reverse <$> swapMVar var []
