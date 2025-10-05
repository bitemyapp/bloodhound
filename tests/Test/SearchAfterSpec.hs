module Test.SearchAfterSpec (spec) where

import Control.Concurrent (threadDelay)
import qualified Data.Aeson as Aeson
import TestsUtils.Common
import TestsUtils.Import
import Prelude

spec :: Spec
spec =
  describe "Search After API" $
    it "returns document for search after query" $ do
      majorVersion <- fetchMajorVersion
      withTestEnv $ do
        _ <- insertData
        _ <- insertOther
        let sortSpec = DefaultSortSpec $ mkSort (FieldName "user") Ascending
            searchAfterKey' = [Aeson.toJSON ("bitemyapp" :: String)]
            search =
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
                  searchAfterKey = Just searchAfterKey',
                  fields = Nothing,
                  scriptFields = Nothing,
                  docvalueFields = Nothing,
                  source = Nothing,
                  suggestBody = Nothing,
                  pointInTime = Nothing
                }
        when (majorVersion == 2) $
          liftIO $
            threadDelay 50000
        result <- searchTweets search
        let myTweet = result >>= grabFirst
        liftIO $
          myTweet `shouldBe` Right otherTweet
