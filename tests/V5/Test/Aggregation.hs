{-# LANGUAGE OverloadedStrings #-}

module Test.Aggregation (spec) where

import Test.Common
import Test.Import

import Control.Error (fmapL, note)
import qualified Data.Map as M
import qualified Database.V5.Bloodhound

spec :: Spec
spec =
  describe "Aggregation API" $ do
    it "returns term aggregation results" $ withTestEnv $ do
      _ <- insertData
      let terms = TermsAgg $ mkTermsAggregation "user"
      let search = mkAggregateSearch Nothing $ mkAggregations "users" terms
      searchExpectAggs search
      searchValidBucketAgg search "users" toTerms

    it "return sub-aggregation results" $ withTestEnv $ do
      _ <- insertData
      let subaggs = mkAggregations "age_agg" . TermsAgg $ mkTermsAggregation "age"
          agg = TermsAgg $ (mkTermsAggregation "user") { termAggs = Just subaggs}
          search = mkAggregateSearch Nothing $ mkAggregations "users" agg
      reply <- searchByIndex testIndex search
      let result = decode (responseBody reply) :: Maybe (SearchResult Tweet)
          usersAggResults = result >>= aggregations >>= toTerms "users"
          subAggResults = usersAggResults >>= (listToMaybe . buckets) >>= termsAggs >>= toTerms "age_agg"
          subAddResultsExists = isJust subAggResults
      liftIO $ subAddResultsExists `shouldBe` True

    it "returns cardinality aggregation results" $ withTestEnv $ do
      _ <- insertData
      let cardinality = CardinalityAgg $ mkCardinalityAggregation $ FieldName "user"
      let search = mkAggregateSearch Nothing $ mkAggregations "users" cardinality
      let search' = search { Database.V5.Bloodhound.from = From 0, size = Size 0 }
      searchExpectAggs search'
      let docCountPair k n = (k, object ["value" .= Number n])
      res <- searchTweets search'
      liftIO $
        fmap aggregations res `shouldBe` Right (Just (M.fromList [ docCountPair "users" 1]))

    it "returns stats aggregation results" $ withTestEnv $ do
      _ <- insertData
      let stats = StatsAgg $ mkStatsAggregation $ FieldName "age"
      let search = mkAggregateSearch Nothing $ mkAggregations "users" stats
      let search' = search { Database.V5.Bloodhound.from = From 0, size = Size 0 }
      searchExpectAggs search'
      let statsAggRes k n = (k, object [ "max" .= Number n
                                       , "avg" .= Number n
                                       , "count" .= Number 1
                                       , "min" .= Number n
                                       , "sum" .= Number n])
      res <- searchTweets search'
      liftIO $
        fmap aggregations res `shouldBe` Right (Just (M.fromList [ statsAggRes "users" 10000]))

    it "can give collection hint parameters to term aggregations" $ when' (atleast es13) $ withTestEnv $ do
      _ <- insertData
      let terms = TermsAgg $ (mkTermsAggregation "user") { termCollectMode = Just BreadthFirst }
      let search = mkAggregateSearch Nothing $ mkAggregations "users" terms
      searchExpectAggs search
      searchValidBucketAgg search "users" toTerms

    -- One of these fails with 1.7.3
    it "can give execution hint parameters to term aggregations" $ when' (atmost es11) $ withTestEnv $ do
      _ <- insertData
      searchTermsAggHint [Map, Ordinals]

    it "can give execution hint parameters to term aggregations" $ when' (is es12) $ withTestEnv $ do
      _ <- insertData
      searchTermsAggHint [GlobalOrdinals, GlobalOrdinalsHash, GlobalOrdinalsLowCardinality, Map, Ordinals]

    it "can give execution hint parameters to term aggregations" $ when' (atleast es12) $ withTestEnv $ do
      _ <- insertData
      searchTermsAggHint [GlobalOrdinals, GlobalOrdinalsHash, GlobalOrdinalsLowCardinality, Map]
    -- One of the above.

    it "can execute value_count aggregations" $ withTestEnv $ do
      _ <- insertData
      _ <- insertOther
      let ags = mkAggregations "user_count" (ValueCountAgg (FieldValueCount (FieldName "user"))) <>
                mkAggregations "bogus_count" (ValueCountAgg (FieldValueCount (FieldName "bogus")))
      let search = mkAggregateSearch Nothing ags
      let docCountPair k n = (k, object ["value" .= Number n])
      res <- searchTweets search
      liftIO $
        fmap aggregations res `shouldBe` Right (Just (M.fromList [ docCountPair "user_count" 2
                                                                 , docCountPair "bogus_count" 0
                                                                 ]))

    it "can execute date_range aggregations" $ withTestEnv $ do
      let now = fromGregorian 2015 3 14
      let ltAMonthAgo = UTCTime (fromGregorian 2015 3 1) 0
      let ltAWeekAgo = UTCTime (fromGregorian 2015 3 10) 0
      let oldDoc = exampleTweet { postDate = ltAMonthAgo }
      let newDoc = exampleTweet { postDate = ltAWeekAgo }
      _ <- indexDocument testIndex testMapping defaultIndexDocumentSettings oldDoc (DocId "1")
      _ <- indexDocument testIndex testMapping defaultIndexDocumentSettings newDoc (DocId "2")
      _ <- refreshIndex testIndex
      let thisMonth = DateRangeFrom (DateMathExpr (DMDate now) [SubtractTime 1 DMMonth])
      let thisWeek = DateRangeFrom (DateMathExpr (DMDate now) [SubtractTime 1 DMWeek])
      let agg = DateRangeAggregation (FieldName "postDate") Nothing (thisMonth :| [thisWeek])
      let ags = mkAggregations "date_ranges" (DateRangeAgg agg)
      let search = mkAggregateSearch Nothing ags
      res <- searchTweets search
      liftIO $ hitsTotal . searchHits <$> res `shouldBe` Right 2
      let bucks = do magrs <- fmapL show (aggregations <$> res)
                     agrs <- note "no aggregations returned" magrs
                     rawBucks <- note "no date_ranges aggregation" $ M.lookup "date_ranges" agrs
                     parseEither parseJSON rawBucks
      let fromMonthT = UTCTime (fromGregorian 2015 2 14) 0
      let fromWeekT = UTCTime (fromGregorian 2015 3 7) 0
      liftIO $ buckets <$> bucks `shouldBe` Right [ DateRangeResult "2015-02-14T00:00:00.000Z-*"
                                                                    (Just fromMonthT)
                                                                    (Just "2015-02-14T00:00:00.000Z")
                                                                    Nothing
                                                                    Nothing
                                                                    2
                                                                    Nothing
                                                  , DateRangeResult "2015-03-07T00:00:00.000Z-*"
                                                                    (Just fromWeekT)
                                                                    (Just "2015-03-07T00:00:00.000Z")
                                                                    Nothing
                                                                    Nothing
                                                                    1
                                                                    Nothing
                                     ]

    it "returns date histogram aggregation results" $ withTestEnv $ do
      _ <- insertData
      let histogram = DateHistogramAgg $ mkDateHistogram (FieldName "postDate") Minute
      let search = mkAggregateSearch Nothing (mkAggregations "byDate" histogram)
      searchExpectAggs search
      searchValidBucketAgg search "byDate" toDateHistogram

    it "can execute missing aggregations" $ withTestEnv $ do
      _ <- insertData
      _ <- insertExtra
      let ags = mkAggregations "missing_agg" (MissingAgg (MissingAggregation "extra"))
      let search = mkAggregateSearch Nothing ags
      let docCountPair k n = (k, object ["doc_count" .= Number n])
      res <- searchTweets search
      liftIO $
        fmap aggregations res `shouldBe` Right (Just (M.fromList [docCountPair "missing_agg" 1]))
