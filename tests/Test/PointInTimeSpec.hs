module Test.PointInTimeSpec (spec) where

import qualified Data.Text as Text
import TestsUtils.Common
import TestsUtils.Import
import Prelude

spec :: Spec
spec = do
  describe "Point in time (PIT) API (ElasticSearch 7)" $ do
    it' <- runIO esOnlyIT
    it' "returns a single document using the point in time (PIT) API" $
      withTestEnv $ do
        _ <- insertData
        _ <- insertOther
        let search =
              ( mkSearch
                  (Just $ MatchAllQuery Nothing)
                  Nothing
              )
                { size = Size 1
                }
        regular_search <- searchTweet search
        pit_search' <- pitSearch testIndex search :: BH IO [Hit Tweet]
        let pit_search = map hitSource pit_search'
        liftIO $
          regular_search `shouldBe` Right exampleTweet -- Check that the size restriction is being honored
        liftIO $
          pit_search `shouldMatchList` [Just exampleTweet]
    it' "returns many documents using the point in time (PIT) API" $
      withTestEnv $ do
        resetIndex
        let ids = [1 .. 1000]
        let docs = map exampleTweetWithAge ids
        let docIds = map (Text.pack . show) ids
        mapM_ (uncurry insertTweetWithDocId) (docs `zip` docIds)
        let sort = mkSort (FieldName "postDate") Ascending
        let search =
              ( mkSearch
                  (Just $ MatchAllQuery Nothing)
                  Nothing
              )
                { sortBody = Just [DefaultSortSpec sort]
                }
        scan_search' <- scanSearch testIndex search :: BH IO [Hit Tweet]
        let scan_search = map hitSource scan_search'
        pit_search' <- pitSearch testIndex search :: BH IO [Hit Tweet]
        let pit_search = map hitSource pit_search'
        let expectedHits = map Just docs
        liftIO $
          scan_search `shouldMatchList` expectedHits
        liftIO $
          pit_search `shouldMatchList` expectedHits
  xdescribe "Point in time (PIT) API (OpenSearch 2)" $ do
    it' <- runIO os2OnlyIT
    it' "returns a single document using the point in time (PIT) API" $
      withTestEnv $ do
        _ <- insertData
        _ <- insertOther
        let search =
              ( mkSearch
                  (Just $ MatchAllQuery Nothing)
                  Nothing
              )
                { size = Size 1
                }
        regular_search <- searchTweet search
        pit_search' <- pitSearchOpenSearch2 testIndex search :: BH IO [Hit Tweet]
        let pit_search = map hitSource pit_search'
        liftIO $
          regular_search `shouldBe` Right exampleTweet -- Check that the size restriction is being honored
        liftIO $
          pit_search `shouldMatchList` [Just exampleTweet]
    it' "returns many documents using the point in time (PIT) API" $
      withTestEnv $ do
        resetIndex
        let ids = [1 .. 1000]
        let docs = map exampleTweetWithAge ids
        let docIds = map (Text.pack . show) ids
        mapM_ (uncurry insertTweetWithDocId) (docs `zip` docIds)
        let sort = mkSort (FieldName "postDate") Ascending
        let search =
              ( mkSearch
                  (Just $ MatchAllQuery Nothing)
                  Nothing
              )
                { sortBody = Just [DefaultSortSpec sort]
                }
        scan_search' <- scanSearch testIndex search :: BH IO [Hit Tweet]
        let scan_search = map hitSource scan_search'
        pit_search' <- pitSearchOpenSearch2 testIndex search :: BH IO [Hit Tweet]
        let pit_search = map hitSource pit_search'
        let expectedHits = map Just docs
        liftIO $
          scan_search `shouldMatchList` expectedHits
        liftIO $
          pit_search `shouldMatchList` expectedHits
