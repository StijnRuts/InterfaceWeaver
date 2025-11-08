module Test.InterfaceWeaver.Events (main) where

import Control.Concurrent.MVar (modifyMVar_, newMVar, swapMVar)
import Events
import Test.Hspec

main :: IO ()
main = hspec $ do
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

capture :: Events a -> IO (IO [a])
capture ev = do
  var <- newMVar []
  sink ev $ \val -> modifyMVar_ var $ \vals -> pure $ val : vals
  return $ reverse <$> swapMVar var []
