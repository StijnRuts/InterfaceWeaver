module Test.Control.Timeout where

import Control.Concurrent (threadDelay)
import Control.Timeout
import Data.Char as Char
import Data.IO.Seq as IOSeq
import qualified Data.Sequence as Seq
import Test.Hspec

spec :: Spec
spec = do
  describe "TimeoutM" $ do
    it "setTimeout" $ do
      Helpers {run, tick} <- getHelpers
      run $ schedule (10 @ milliseconds) 'a'
      run $ schedule (20 @ milliseconds) 'b'
      run $ schedule (11 @ milliseconds) 'c'
      tick []
      run $ schedule (11 @ milliseconds) 'd'
      tick ['a', 'c']
      tick ['b', 'd']

    it "update" $ do
      Helpers {run, tick} <- getHelpers
      run $ schedule (10 @ milliseconds) 'a'
      run $ schedule (10 @ milliseconds) 'A'
      run $ schedule (11 @ milliseconds) 'b'
      run $ schedule (11 @ milliseconds) 'B'
      run $ find Char.isUpper >>= update (20 @ milliseconds)
      tick []
      tick ['a', 'b']
      tick ['A', 'B']

    it "clear" $ do
      Helpers {run, tick} <- getHelpers
      run $ schedule (10 @ milliseconds) 'a'
      run $ schedule (10 @ milliseconds) 'A'
      run $ schedule (11 @ milliseconds) 'b'
      run $ schedule (11 @ milliseconds) 'B'
      run $ find Char.isUpper >>= clear
      tick []
      tick ['a', 'b']
      tick []

    it "fire" $ do
      Helpers {run, tick} <- getHelpers
      run $ schedule (10 @ milliseconds) 'a'
      run $ schedule (10 @ milliseconds) 'A'
      run $ schedule (11 @ milliseconds) 'b'
      run $ schedule (11 @ milliseconds) 'B'
      run $ find Char.isUpper >>= fire
      tick ['A', 'B']
      tick ['a', 'b']

data Helpers t = Helpers
  { run :: forall x. TimeoutM t x -> IO x,
    tick :: [t] -> IO ()
  }

getHelpers :: (Show t, Eq t) => IO (Helpers t)
getHelpers = do
  timeouts <- newTimeouts
  var <- IOSeq.new
  let run = runTimeoutM timeouts (IOSeq.add var)
  let tick expected = do
        threadDelay (5 @ milliseconds)
        result <- IOSeq.empty var
        threadDelay (5 @ milliseconds)
        result `shouldBe` Seq.fromList expected
  return Helpers {run, tick}
