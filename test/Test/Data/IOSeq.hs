module Test.Data.IOSeq where

import Data.IOSeq as IOSeq
import qualified Data.Sequence as Seq
import Test.Hspec

spec :: Spec
spec = do
  describe "IOSeq" $ do
    it "should manipulate data in a sequence" $ do
      ioseq :: IOSeq Int <- IOSeq.new
      IOSeq.get ioseq `shouldReturn` Seq.empty
      IOSeq.add ioseq 1
      IOSeq.add ioseq 2
      IOSeq.get ioseq `shouldReturn` Seq.fromList [1, 2]
      IOSeq.empty ioseq `shouldReturn` Seq.fromList [1, 2]
      IOSeq.get ioseq `shouldReturn` Seq.empty
