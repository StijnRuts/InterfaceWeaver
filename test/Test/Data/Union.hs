{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Data.Union where

import Data.Char as Char
import Data.Union
import Test.Hspec
import Test.QuickCheck

instance Arbitrary (Union '[Char]) where
  arbitrary = This <$> arbitrary

instance Arbitrary (Union '[Bool]) where
  arbitrary = This <$> arbitrary

instance Arbitrary (Union '[Char, Bool]) where
  arbitrary = oneof [This <$> arbitrary, That . This <$> arbitrary]

spec :: Spec
spec = do
  describe "Show instance for Union" $ do
    it "shows Char correctly" $ do
      let u = inject 'a' :: Union '[Char, Bool]
      show u `shouldBe` "'a'"

    it "shows Bool correctly" $ do
      let u = inject True :: Union '[Char, Bool]
      show u `shouldBe` "True"

  describe "Eq instance for Union" $ do
    it "compares equal Char values" $ do
      let u1 = inject 'a' :: Union '[Char, Bool]
      let u2 = inject 'a' :: Union '[Char, Bool]
      u1 == u2 `shouldBe` True

    it "compares different Char values" $ do
      let u1 = inject 'a' :: Union '[Char, Bool]
      let u2 = inject 'z' :: Union '[Char, Bool]
      u1 == u2 `shouldBe` False

    it "compares equal Bool values" $ do
      let u1 = inject True :: Union '[Char, Bool]
      let u2 = inject True :: Union '[Char, Bool]
      u1 == u2 `shouldBe` True

    it "compares different Bool values" $ do
      let u1 = inject True :: Union '[Char, Bool]
      let u2 = inject False :: Union '[Char, Bool]
      u1 == u2 `shouldBe` False

    it "compares different types as unequal" $ do
      let u1 = inject 'a' :: Union '[Char, Bool]
      let u2 = inject True :: Union '[Char, Bool]
      u1 == u2 `shouldBe` False

  describe "Member" $ do
    it "inject/project works for head element" $ do
      let u = inject True :: Union '[Char, Bool]
      (project u :: Maybe Bool) `shouldBe` Just True
      (project u :: Maybe Char) `shouldBe` Nothing

    it "inject/project works for non-head element" $ do
      let u = inject 'a' :: Union '[Char, Bool]
      (project u :: Maybe Bool) `shouldBe` Nothing
      (project u :: Maybe Char) `shouldBe` Just 'a'

  describe "RemoveMember" $ do
    it "removes head element and returns Right" $ do
      let u = inject True :: Union '[Char, Bool]
      (remove u :: Either (Union '[Char]) Bool) `shouldBe` Right True

    it "removes non-head element and returns Right" $ do
      let u = inject 'a' :: Union '[Char, Bool]
      (remove u :: Either (Union '[Bool]) Char) `shouldBe` Right 'a'

    it "removes head but returns Left when value is not target" $ do
      let u = inject True :: Union '[Char, Bool]
      case (remove u :: Either (Union '[Bool]) Char) of
        Left rest -> (project rest :: Maybe Bool) `shouldBe` Just True
        Right _ -> expectationFailure "Expected Left"

  describe "ReplaceMember" $ do
    it "replaces head element with same type" $ do
      let u = inject 'a' :: Union '[Char, Bool]
      let u' = replace Char.toUpper u :: Union '[Char, Bool]
      project u' `shouldBe` Just 'A'
      project u' `shouldBe` (Nothing :: Maybe Bool)

    it "replaces head element with different type" $ do
      let u = inject 'a' :: Union '[Char, Bool]
      let u' = replace (show :: Char -> String) u :: Union '[String, Bool]
      project u' `shouldBe` Just "'a'"
      project u' `shouldBe` (Nothing :: Maybe Bool)

    it "replaces non-head element" $ do
      let u = inject True :: Union '[Char, Bool]
      let u' = replace (show :: Bool -> String) u :: Union '[Char, String]
      project u' `shouldBe` Just "True"
      project u' `shouldBe` (Nothing :: Maybe Char)

  describe "QuickCheck properties for Union" $ do
    it "Show matches projection when successful" $
      do
        property (\(x :: Char) -> let u = inject x :: Union '[Char, Bool] in show u == show x)
        .&&. property (\(x :: Bool) -> let u = inject x :: Union '[Char, Bool] in show u == show x)

    it "Eq reflexivity" $ do
      property $ \(u :: Union '[Char, Bool]) -> u == u

    it "inject/project roundtrip for Char" $ do
      property $ \(x :: Char) ->
        project (inject x :: Union '[Char, Bool]) == Just x

    it "inject/project roundtrip for Bool" $ do
      property $ \(x :: Bool) ->
        project (inject x :: Union '[Char, Bool]) == Just x

    it "remove roundtrip for Char" $ do
      property $ \(x :: Char) ->
        (remove (inject x :: Union '[Char, Bool]) :: Either (Union '[Bool]) Char) == Right x

    it "remove roundtrip for Bool" $ do
      property $ \(x :: Bool) ->
        (remove (inject x :: Union '[Char, Bool]) :: Either (Union '[Char]) Bool) == Right x

    it "replace id leaves union unchanged" $
      do
        property (\(u :: Union '[Char, Bool]) -> replace (id :: Char -> Char) u == u)
        .&&. property (\(u :: Union '[Char, Bool]) -> replace (id :: Bool -> Bool) u == u)
