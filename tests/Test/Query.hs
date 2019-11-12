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
      let filter' = Filter $ MatchAllQuery Nothing
      let search = mkSearch (Just query) (Just filter')
      myTweet <- searchTweet search
      liftIO $
        myTweet `shouldBe` Right exampleTweet

    it "handles constant score queries" $ withTestEnv $ do
      _ <- insertData
      let query = TermsQuery "user" ("bitemyapp" :| [])
      let cfQuery = ConstantScoreQuery query (Boost 1.0)
      let filter' = Filter $ MatchAllQuery Nothing
      let search = mkSearch (Just cfQuery) (Just filter')
      myTweet <- searchTweet search
      liftIO $
        myTweet `shouldBe` Right exampleTweet

    it "returns document for terms query and identity filter" $ withTestEnv $ do
      _ <- insertData
      let query = TermsQuery "user" ("bitemyapp" :| [])
      let filter' = Filter $ MatchAllQuery Nothing
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

    it "returns document for match query with fuzziness" $ withTestEnv $ do
      _ <- insertData
      let match = mkMatchQuery (FieldName "user") (QueryString "bidemyapp")
      let query = QueryMatchQuery $ match { matchQueryFuzziness = Just FuzzinessAuto }
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
                  mkBoolQuery [innerQuery] [] [] []
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

    it "returns document for template query" $ withTestEnv $ do
      _ <- insertData
      let query = SearchTemplateSource "{\"query\": { \"match\" : { \"{{my_field}}\" : \"{{my_value}}\" } } }"
          templateParams = TemplateQueryKeyValuePairs $ HM.fromList
            [ ("my_field", "user")
            , ("my_value", "bitemyapp")
            ]
          search = mkSearchTemplate (Right query) templateParams
      response <- parseEsResponse =<< searchByIndexTemplate testIndex search
      let myTweet = grabFirst response
      liftIO $ myTweet `shouldBe` Right exampleTweet

    it "can save, use, read and delete template queries" $ withTestEnv $ do
      _ <- insertData
      let query = SearchTemplateSource "{\"query\": { \"match\" : { \"{{my_field}}\" : \"{{my_value}}\" } } }"
          templateParams = TemplateQueryKeyValuePairs $ HM.fromList
            [ ("my_field", "user")
            , ("my_value", "bitemyapp")
            ]
          tid = SearchTemplateId "myTemplate"
          search = mkSearchTemplate (Left tid) templateParams
      _ <- storeSearchTemplate tid query
      r1 <- getSearchTemplate tid
      let t1 = eitherDecode $ responseBody r1 :: Either String GetTemplateScript
      liftIO $ t1 `shouldBe` Right (GetTemplateScript {getTemplateScriptLang = Just "mustache", getTemplateScriptSource = Just (SearchTemplateSource "{\"query\": { \"match\" : { \"{{my_field}}\" : \"{{my_value}}\" } } }"), getTemplateScriptOptions = Nothing, getTemplateScriptId = "myTemplate", getTemplateScriptFound = True})
      response <- parseEsResponse =<< searchByIndexTemplate testIndex search
      _ <- deleteSearchTemplate tid
      r2 <- getSearchTemplate tid
      let myTweet = grabFirst response
          t2 = eitherDecode $ responseBody r2 :: Either String GetTemplateScript
      liftIO $ do
        t2 `shouldBe` Right (GetTemplateScript {getTemplateScriptLang = Nothing, getTemplateScriptSource = Nothing, getTemplateScriptOptions = Nothing, getTemplateScriptId = "myTemplate", getTemplateScriptFound = False})
        myTweet `shouldBe` Right exampleTweet

    it "returns document for wildcard query" $ withTestEnv $ do
      _ <- insertData
      let query = QueryWildcardQuery $ WildcardQuery (FieldName "user") "bitemy*" (Nothing)
      let search = mkSearch (Just query) Nothing
      myTweet <- searchTweet search
      liftIO $
        myTweet `shouldBe` Right exampleTweet
