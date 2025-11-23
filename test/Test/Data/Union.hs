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

  describe "Special cases for Unions of one or two types" $ do
    it "injects Either into a Union" $ do
      injectEither (Left 'a') `shouldBe` (inject 'a' :: Union '[Char, Bool])
      injectEither (Right True) `shouldBe` (inject True :: Union '[Char, Bool])

    it "projects a Union of two types onto an Either" $ do
      projectEither (inject 'a' :: Union '[Char, Bool]) `shouldBe` Left 'a'
      projectEither (inject True :: Union '[Char, Bool]) `shouldBe` Right True

    it "projects a Union of a single type onto that type" $ do
      projectSingle (inject 'a' :: Union '[Char]) `shouldBe` 'a'

  describe "Subset" $ do
    it "weakens a Union" $ do
      weaken (inject 'a' :: Union '[Char, Bool]) `shouldBe` (inject 'a' :: Union '[Char, Bool])
      weaken (inject 'a' :: Union '[Char, Bool]) `shouldBe` (inject 'a' :: Union '[String, Char, Bool])
      weaken (inject 'a' :: Union '[Char, Bool]) `shouldBe` (inject 'a' :: Union '[Char, String, Bool])
      weaken (inject 'a' :: Union '[Char, Bool]) `shouldBe` (inject 'a' :: Union '[Bool, String, Char])
      weaken (inject True :: Union '[Char, Bool]) `shouldBe` (inject True :: Union '[Char, Bool])
      weaken (inject True :: Union '[Char, Bool]) `shouldBe` (inject True :: Union '[String, Char, Bool])
      weaken (inject True :: Union '[Char, Bool]) `shouldBe` (inject True :: Union '[Char, String, Bool])
      weaken (inject True :: Union '[Char, Bool]) `shouldBe` (inject True :: Union '[Bool, String, Char])

  describe "RemoveMember" $ do
    it "removes head element and returns Right" $ do
      remove (inject True :: Union '[Char, Bool]) `shouldBe` Right True

    it "removes non-head element and returns Right" $ do
      remove (inject 'a' :: Union '[Char, Bool]) `shouldBe` Right 'a'

    it "removes head but returns Left when value is not target" $ do
      let u = inject True :: Union '[Char, Bool]
      case (remove u :: Either (Union '[Bool]) Char) of
        Left rest -> project rest `shouldBe` Just True
        Right _ -> expectationFailure "Expected Left"

  describe "ReplaceMember" $ do
    it "replaces head element with same type" $ do
      let u = replace Char.toUpper (inject 'a' :: Union '[Char, Bool])
      project u `shouldBe` Just 'A'
      project u `shouldBe` (Nothing :: Maybe Bool)

    it "replaces head element with different type" $ do
      let u = replace (show :: Char -> String) (inject 'a' :: Union '[Char, Bool])
      project u `shouldBe` Just "'a'"
      project u `shouldBe` (Nothing :: Maybe Bool)

    it "replaces non-head element" $ do
      let u = replace (show :: Bool -> String) (inject True :: Union '[Char, Bool])
      project u `shouldBe` Just "True"
      project u `shouldBe` (Nothing :: Maybe Char)
