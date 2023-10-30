{-# LANGUAGE OverloadedStrings #-}

module Test.SortingSpec (spec) where

import TestsUtils.Common
import TestsUtils.Import

spec :: Spec
spec =
  describe "sorting" $
    it "returns documents in the right order" $
      withTestEnv $ do
        _ <- insertData
        _ <- insertOther
        let sortSpec = DefaultSortSpec $ mkSort (FieldName "age") Ascending
        let search =
              Search
                { queryBody = Nothing,
                  filterBody = Nothing,
                  sortBody = Just [sortSpec],
                  aggBody = Nothing,
                  highlight = Nothing,
                  trackSortScores = False,
                  from = From 0,
                  size = Size 10,
                  searchType = SearchTypeDfsQueryThenFetch,
                  searchAfterKey = Nothing,
                  fields = Nothing,
                  scriptFields = Nothing,
                  source = Nothing,
                  suggestBody = Nothing,
                  pointInTime = Nothing
                }
        result <- searchTweets search
        let myTweet = result >>= grabFirst
        liftIO $
          myTweet `shouldBe` Right otherTweet
