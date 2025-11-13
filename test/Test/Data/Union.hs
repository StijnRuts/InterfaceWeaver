{-# LANGUAGE DataKinds #-}

module Test.Data.Union where

import Data.Union
import Test.Hspec
import Test.QuickCheck

instance Arbitrary (Union '[Int]) where
  arbitrary = This <$> arbitrary

instance Arbitrary (Union '[Bool]) where
  arbitrary = This <$> arbitrary

instance Arbitrary (Union '[Int, Bool]) where
  arbitrary = oneof [This <$> arbitrary, That . This <$> arbitrary]

spec :: Spec
spec = do
  describe "Show instance for Union" $ do
    it "shows Int correctly" $ do
      let u = inject (42 :: Int) :: Union '[Int, Bool]
      show u `shouldBe` "42"

    it "shows Bool correctly" $ do
      let u = inject True :: Union '[Int, Bool]
      show u `shouldBe` "True"

  describe "Eq instance for Union" $ do
    it "compares equal Int values" $ do
      let u1 = inject (42 :: Int) :: Union '[Int, Bool]
      let u2 = inject (42 :: Int) :: Union '[Int, Bool]
      u1 == u2 `shouldBe` True

    it "compares different Int values" $ do
      let u1 = inject (42 :: Int) :: Union '[Int, Bool]
      let u2 = inject (99 :: Int) :: Union '[Int, Bool]
      u1 == u2 `shouldBe` False

    it "compares equal Bool values" $ do
      let u1 = inject True :: Union '[Int, Bool]
      let u2 = inject True :: Union '[Int, Bool]
      u1 == u2 `shouldBe` True

    it "compares different Bool values" $ do
      let u1 = inject True :: Union '[Int, Bool]
      let u2 = inject False :: Union '[Int, Bool]
      u1 == u2 `shouldBe` False

    it "compares different types as unequal" $ do
      let u1 = inject (42 :: Int) :: Union '[Int, Bool]
      let u2 = inject True :: Union '[Int, Bool]
      u1 == u2 `shouldBe` False

  describe "Member" $ do
    it "inject/project works for head element" $ do
      let u = inject True :: Union '[Int, Bool]
      (project u :: Maybe Bool) `shouldBe` Just True
      (project u :: Maybe Int) `shouldBe` Nothing

    it "inject/project works for non-head element" $ do
      let u = inject (42 :: Int) :: Union '[Int, Bool]
      (project u :: Maybe Bool) `shouldBe` Nothing
      (project u :: Maybe Int) `shouldBe` Just 42

  describe "RemoveMember" $ do
    it "removes head element and returns Right" $ do
      let u = inject True :: Union '[Int, Bool]
      (remove u :: Either (Union '[Int]) Bool) `shouldBe` Right True

    it "removes non-head element and returns Right" $ do
      let u = inject (42 :: Int) :: Union '[Int, Bool]
      (remove u :: Either (Union '[Bool]) Int) `shouldBe` Right 42

    it "removes head but returns Left when value is not target" $ do
      let u = inject True :: Union '[Int, Bool]
      case (remove u :: Either (Union '[Bool]) Int) of
        Left rest -> (project rest :: Maybe Bool) `shouldBe` Just True
        Right _ -> expectationFailure "Expected Left"

  describe "ReplaceMember" $ do
    it "replaces head element with same type" $ do
      let u = inject (10 :: Int) :: Union '[Int, Bool]
      let u' = replace (+ (1 :: Int)) u :: Union '[Int, Bool]
      project u' `shouldBe` Just (11 :: Int)

    it "replaces head element with different type" $ do
      let u = inject (10 :: Int) :: Union '[Int, Bool]
      let u' = replace (show :: Int -> String) u :: Union '[String, Bool]
      project u' `shouldBe` Just "10"

    it "replaces non-head element" $ do
      let u = inject True :: Union '[Int, Bool]
      let u' = replace (show :: Bool -> String) u :: Union '[Int, String]
      project u' `shouldBe` Just "True"
      (project u' :: Maybe Int) `shouldBe` Nothing

  describe "QuickCheck properties for Union" $ do
    it "Show matches projection when successful" $ do
      property $ \(x :: Int) ->
        let u = inject x :: Union '[Int, Bool]
         in show u == show x

    it "Eq reflexivity" $ do
      property $ \(u :: Union '[Int, Bool]) ->
        u == u

    it "inject/project roundtrip for Int" $ do
      property $ \(x :: Int) ->
        project (inject x :: Union '[Int, Bool]) == Just x

    it "inject/project roundtrip for Bool" $ do
      property $ \(x :: Bool) ->
        project (inject x :: Union '[Int, Bool]) == Just x

    it "remove roundtrip for Int" $ do
      property $ \(x :: Int) ->
        (remove (inject x :: Union '[Int, Bool]) :: Either (Union '[Bool]) Int) == Right x

    it "remove roundtrip for Bool" $ do
      property $ \(x :: Bool) ->
        (remove (inject x :: Union '[Int, Bool]) :: Either (Union '[Int]) Bool) == Right x

    it "replace id leaves union unchanged" $ do
      property $ \(u :: Union '[Int, Bool]) ->
        (replace (id :: Int -> Int) u == u)
          && (replace (id :: Bool -> Bool) u == u)
