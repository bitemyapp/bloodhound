{-# LANGUAGE OverloadedStrings #-}

module Test.Sorting where

import Test.Common
import Test.Import

spec :: Spec
spec =
  describe "sorting" $
    it "returns documents in the right order" $ withTestEnv $ do
      _ <- insertData
      _ <- insertOther
      let sortSpec = DefaultSortSpec $ mkSort (FieldName "age") Ascending
      let search = Search Nothing
                   Nothing (Just [sortSpec]) Nothing Nothing
                   False (From 0) (Size 10) SearchTypeQueryThenFetch Nothing Nothing Nothing
                   Nothing Nothing Nothing
      result <- searchTweets search
      let myTweet = grabFirst result
      liftIO $
        myTweet `shouldBe` Right otherTweet
