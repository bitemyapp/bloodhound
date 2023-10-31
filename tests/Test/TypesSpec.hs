{-# LANGUAGE QuasiQuotes #-}

module Test.TypesSpec (spec) where

import qualified Data.Text as T
import TestsUtils.Common
import TestsUtils.Generators ()
import TestsUtils.Import
import Prelude

spec :: Spec
spec = do
  describe "error parsing" $
    it "can parse EsErrors for >= 2.0" $
      withTestEnv $ do
        errorResp <- tryPerformBHRequest $ verifySnapshotRepo (SnapshotRepoName "bogus")
        liftIO (errorResp `shouldBe` Left (EsError (Just 404) "[bogus] missing"))

  describe "Monoid (SearchHits a)" $
    prop "abides the monoid laws" $
      eq $
        prop_Monoid (T :: T (SearchHits ()))

  describe "mkDocVersion" $
    prop "can never construct an out of range docVersion" $ \i ->
      let res = mkDocVersion i
       in case res of
            Nothing -> property True
            Just dv ->
              (dv >= minBound)
                .&&. (dv <= maxBound)
                .&&. docVersionNumber dv
                === i

  describe "Enum DocVersion" $
    it "follows the laws of Enum, Bounded" $ do
      evaluate (succ maxBound :: DocVersion) `shouldThrow` anyErrorCall
      evaluate (pred minBound :: DocVersion) `shouldThrow` anyErrorCall
      evaluate (toEnum 0 :: DocVersion) `shouldThrow` anyErrorCall
      evaluate (toEnum 9200000000000000001 :: DocVersion) `shouldThrow` anyErrorCall
      enumFrom (pred maxBound :: DocVersion) `shouldBe` [pred maxBound, maxBound]
      enumFrom (pred maxBound :: DocVersion) `shouldBe` [pred maxBound, maxBound]
      enumFromThen minBound (pred maxBound :: DocVersion) `shouldBe` [minBound, pred maxBound]

  describe "IndexName" $ do
    it "Empty should fail" $
      mkIndexName "" `shouldBe` Left "Is empty"
    it "Long should fail" $
      mkIndexName (T.replicate 256 "x") `shouldBe` Left "Is longer than 255 bytes"
    it "Upper case should fail" $
      mkIndexName "azerTY" `shouldBe` Left "Contains uppercase letter(s)"
    it "Special symbols should fail" $
      mkIndexName "hello#world" `shouldBe` Left "Includes [\\/*?\"<>| ,#:]"
    it "Not starting with a letter should fail" $
      mkIndexName "-test" `shouldBe` Left "Starts with [-_+.]"
    it "'.' should fail" $
      mkIndexName "." `shouldBe` Left "Is (.|..)"
    it "'..' should fail" $
      mkIndexName ".." `shouldBe` Left "Is (.|..)"
    it "Regular should work" $
      mkIndexName "hello-world_42" `shouldBe` Right [qqIndexName|hello-world_42|]
