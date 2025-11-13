module Test.Main where

import qualified Test.Data.Events
import qualified Test.Data.Union
import Test.Hspec

main :: IO ()
main = hspec $ do
  Test.Data.Events.spec
  Test.Data.Union.spec
