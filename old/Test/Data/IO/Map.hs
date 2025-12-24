module Test.Data.IO.Map where

import Data.IO.Map as IOMap
import qualified Data.Map as Map
import Test.Hspec

spec :: Spec
spec = do
  describe "IOMap" $ do
    it "should manipulate data in a Map" $ do
      iomap :: IOMap Int Char <- IOMap.new
      IOMap.get iomap `shouldReturn` Map.empty
      IOMap.add iomap 1 'a'
      IOMap.add iomap 2 'b'
      IOMap.get iomap `shouldReturn` Map.fromList [(1, 'a'), (2, 'b')]
      IOMap.delete 1 iomap `shouldReturn` Just 'a'
      IOMap.get iomap `shouldReturn` Map.fromList [(2, 'b')]
