{-# LANGUAGE OverloadedStrings #-}

module Test.Query where

import Test.Common
import Test.Import

import qualified Data.HashMap.Strict as HM

spec :: Spec
spec =
  describe "query API" $ do
    it "returns document for term query and identity filter" $ withTestEnv $ do
      _ <- insertData
      let query = TermQuery (Term "user" "bitemyapp") Nothing
      let filter' = IdentityFilter
      let search = mkSearch (Just query) (Just filter')
      myTweet <- searchTweet search
      liftIO $
        myTweet `shouldBe` Right exampleTweet

    it "handles constant score queries" $ withTestEnv $ do
      _ <- insertData
      let query = TermsQuery "user" ("bitemyapp" :| [])
      let cfQuery = ConstantScoreQuery query (Boost 1.0)
      let filter' = IdentityFilter
      let search = mkSearch (Just cfQuery) (Just filter')
      myTweet <- searchTweet search
      liftIO $
        myTweet `shouldBe` Right exampleTweet

    it "returns document for terms query and identity filter" $ withTestEnv $ do
      _ <- insertData
      let query = TermsQuery "user" ("bitemyapp" :| [])
      let filter' = IdentityFilter
      let search = mkSearch (Just query) (Just filter')
      myTweet <- searchTweet search
      liftIO $
        myTweet `shouldBe` Right exampleTweet

    it "returns document for match query" $ withTestEnv $ do
      _ <- insertData
      let query = QueryMatchQuery $ mkMatchQuery (FieldName "user") (QueryString "bitemyapp")
      let search = mkSearch (Just query) Nothing
      myTweet <- searchTweet search
      liftIO $
        myTweet `shouldBe` Right exampleTweet

    it "returns document for multi-match query" $ withTestEnv $ do
      _ <- insertData
      let flds = [FieldName "user", FieldName "message"]
      let query = QueryMultiMatchQuery $ mkMultiMatchQuery flds (QueryString "bitemyapp")
      let search = mkSearch (Just query) Nothing
      myTweet <- searchTweet search
      liftIO $
        myTweet `shouldBe` Right exampleTweet

    it "returns document for multi-match query with a custom tiebreaker" $ withTestEnv $ do
      _ <- insertData
      let tiebreaker = Just $ Tiebreaker 0.3
          flds = [FieldName "user", FieldName "message"]
          multiQuery' = mkMultiMatchQuery flds (QueryString "bitemyapp")
          query =  QueryMultiMatchQuery $ multiQuery' { multiMatchQueryTiebreaker = tiebreaker }
          search = mkSearch (Just query) Nothing
      myTweet <- searchTweet search
      liftIO $
        myTweet `shouldBe` Right exampleTweet

    it "returns document for bool query" $ withTestEnv $ do
      _ <- insertData
      let innerQuery = QueryMatchQuery $
                       mkMatchQuery (FieldName "user") (QueryString "bitemyapp")
      let query = QueryBoolQuery $
                  mkBoolQuery [innerQuery] [] []
      let search = mkSearch (Just query) Nothing
      myTweet <- searchTweet search
      liftIO $
        myTweet `shouldBe` Right exampleTweet

    it "returns document for boosting query" $ withTestEnv $ do
      _ <- insertData
      let posQuery = QueryMatchQuery $ mkMatchQuery (FieldName "user") (QueryString "bitemyapp")
      let negQuery = QueryMatchQuery $ mkMatchQuery (FieldName "user") (QueryString "notmyapp")
      let query = QueryBoostingQuery $ BoostingQuery posQuery negQuery (Boost 0.2)
      let search = mkSearch (Just query) Nothing
      myTweet <- searchTweet search
      liftIO $
        myTweet `shouldBe` Right exampleTweet

    it "returns document for common terms query" $ withTestEnv $ do
      _ <- insertData
      let query = QueryCommonTermsQuery $
                  CommonTermsQuery (FieldName "user")
                  (QueryString "bitemyapp")
                  (CutoffFrequency 0.0001)
                  Or Or Nothing Nothing Nothing Nothing
      let search = mkSearch (Just query) Nothing
      myTweet <- searchTweet search
      liftIO $
        myTweet `shouldBe` Right exampleTweet

    it "returns document for for inline template query" $ withTestEnv $ do
      _ <- insertData
      let innerQuery = QueryMatchQuery $
                         mkMatchQuery (FieldName "{{userKey}}")
                                      (QueryString "{{bitemyappKey}}")
          templateParams = TemplateQueryKeyValuePairs $ HM.fromList
                            [ ("userKey", "user")
                            , ("bitemyappKey", "bitemyapp")
                            ]
          templateQuery = QueryTemplateQueryInline $
                            TemplateQueryInline innerQuery templateParams
          search = mkSearch (Just templateQuery) Nothing
      myTweet <- searchTweet search
      liftIO $ myTweet `shouldBe` Right exampleTweet
