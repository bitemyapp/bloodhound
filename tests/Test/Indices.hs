{-# LANGUAGE OverloadedStrings #-}

module Test.Indices where

import Test.Common
import Test.Import

import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
spec :: Spec
spec = do
  describe "Index create/delete API" $ do
    it "creates and then deletes the requested index" $ withTestEnv $ do
      -- priming state.
      _ <- deleteExampleIndex
      resp <- createExampleIndex
      deleteResp <- deleteExampleIndex
      liftIO $ do
        validateStatus resp 200
        validateStatus deleteResp 200

  describe "Index aliases" $ do
    let aname = IndexAliasName (IndexName "bloodhound-tests-twitter-1-alias")
    let alias = IndexAlias (testIndex) aname
    let create = IndexAliasCreate Nothing Nothing
    let action = AddAlias alias create
    it "handles the simple case of aliasing an existing index" $ do
      withTestEnv $ do
        resetIndex
        resp <- updateIndexAliases (action :| [])
        liftIO $ validateStatus resp 200
      let cleanup = withTestEnv (updateIndexAliases (RemoveAlias alias :| []))
      (do aliases <- withTestEnv getIndexAliases
          let expected = IndexAliasSummary alias create
          case aliases of
            Right (IndexAliasesSummary summs) ->
              L.find ((== alias) . indexAliasSummaryAlias) summs `shouldBe` Just expected
            Left e -> expectationFailure ("Expected an IndexAliasesSummary but got " <> show e)) `finally` cleanup
    it "allows alias deletion" $ do
      aliases <- withTestEnv $ do
        resetIndex
        resp <- updateIndexAliases (action :| [])
        liftIO $ validateStatus resp 200
        _ <- deleteIndexAlias aname
        getIndexAliases
      -- let expected = IndexAliasSummary alias create
      case aliases of
        Right (IndexAliasesSummary summs) ->
          L.find ( (== aname)
                   . indexAlias
                   . indexAliasSummaryAlias
                 ) summs
          `shouldBe` Nothing
        Left e -> expectationFailure ("Expected an IndexAliasesSummary but got " <> show e)

  describe "Index Listing" $ do
    it "returns a list of index names" $ withTestEnv $ do
      _ <- createExampleIndex
      ixns <- listIndices
      liftIO (ixns `shouldContain` [testIndex])

  describe "Index Settings" $ do
    it "persists settings" $ withTestEnv $ do
      _ <- deleteExampleIndex
      _ <- createExampleIndex
      let updates = BlocksWrite False :| []
      updateResp <- updateIndexSettings updates testIndex
      liftIO $ validateStatus updateResp 200
      getResp <- getIndexSettings testIndex
      liftIO $
        getResp `shouldBe` Right (IndexSettingsSummary
                                    testIndex
                                    (IndexSettings (ShardCount 1) (ReplicaCount 0))
                                    (NE.toList updates))

    it "allows total fields to be set" $ when' (atleast es50) $ withTestEnv $ do
      _ <- deleteExampleIndex
      _ <- createExampleIndex
      let updates = MappingTotalFieldsLimit 2500 :| []
      updateResp <- updateIndexSettings updates testIndex
      liftIO $ validateStatus updateResp 200
      getResp <- getIndexSettings testIndex
      liftIO $
        getResp `shouldBe` Right (IndexSettingsSummary
                                    testIndex
                                    (IndexSettings (ShardCount 1) (ReplicaCount 0))
                                    (NE.toList updates))

    it "accepts customer analyzers" $ when' (atleast es50) $ withTestEnv $ do
      _ <- deleteExampleIndex
      let analysis = Analysis
            (M.singleton "ex_analyzer"
              ( AnalyzerDefinition
                (Just (Tokenizer "ex_tokenizer"))
                (map TokenFilter
                  [ "ex_filter_lowercase","ex_filter_uppercase","ex_filter_apostrophe"
                  , "ex_filter_reverse","ex_filter_snowball"
                  , "ex_filter_shingle"
                  ]
                )
                (map CharFilter
                  ["html_strip", "ex_mapping", "ex_pattern_replace"]
                )
              )
            )
            (M.singleton "ex_tokenizer"
              ( TokenizerDefinitionNgram
                ( Ngram 3 4 [TokenLetter,TokenDigit])
              )
            )
            (M.fromList
              [ ("ex_filter_lowercase",TokenFilterDefinitionLowercase (Just Greek))
              , ("ex_filter_uppercase",TokenFilterDefinitionUppercase Nothing)
              , ("ex_filter_apostrophe",TokenFilterDefinitionApostrophe)
              , ("ex_filter_reverse",TokenFilterDefinitionReverse)
              , ("ex_filter_snowball",TokenFilterDefinitionSnowball English)
              , ("ex_filter_shingle",TokenFilterDefinitionShingle (Shingle 3 3 True False " " "_"))
              , ("ex_filter_stemmer",TokenFilterDefinitionStemmer German)
              , ("ex_filter_stop1", TokenFilterDefinitionStop (Left French))
              , ("ex_filter_stop2",
                 TokenFilterDefinitionStop
                 (Right
                  $ map StopWord ["a", "is", "the"]))
             ]
            )
            (M.fromList
             [ ("ex_mapping", CharFilterDefinitionMapping (M.singleton "١" "1"))
             , ("ex_pattern_replace", CharFilterDefinitionPatternReplace "(\\d+)-(?=\\d)" "$1_" Nothing)
             ]
            )
          updates = [AnalysisSetting analysis]
      createResp <- createIndexWith (updates ++ [NumberOfReplicas (ReplicaCount 0)]) 1 testIndex
      liftIO $ validateStatus createResp 200
      getResp <- getIndexSettings testIndex
      liftIO $
        getResp `shouldBe` Right (IndexSettingsSummary
                                    testIndex
                                    (IndexSettings (ShardCount 1) (ReplicaCount 0))
                                    updates
                                 )

    it "accepts default compression codec" $ when' (atleast es50) $ withTestEnv $ do
      _ <- deleteExampleIndex
      let updates = [CompressionSetting CompressionDefault]
      createResp <- createIndexWith (updates ++ [NumberOfReplicas (ReplicaCount 0)]) 1 testIndex
      liftIO $ validateStatus createResp 200
      getResp <- getIndexSettings testIndex
      liftIO $ getResp `shouldBe` Right
        (IndexSettingsSummary testIndex (IndexSettings (ShardCount 1) (ReplicaCount 0)) updates)

    it "accepts best compression codec" $ when' (atleast es50) $ withTestEnv $ do
      _ <- deleteExampleIndex
      let updates = [CompressionSetting CompressionBest]
      createResp <- createIndexWith (updates ++ [NumberOfReplicas (ReplicaCount 0)]) 1 testIndex
      liftIO $ validateStatus createResp 200
      getResp <- getIndexSettings testIndex
      liftIO $ getResp `shouldBe` Right
        (IndexSettingsSummary testIndex (IndexSettings (ShardCount 1) (ReplicaCount 0)) updates)


  describe "Index Optimization" $ do
    it "returns a successful response upon completion" $ withTestEnv $ do
      _ <- createExampleIndex
      resp <- forceMergeIndex (IndexList (testIndex :| [])) defaultForceMergeIndexSettings
      liftIO $ validateStatus resp 200


  describe "Index flushing" $ do
    it "returns a successful response upon flushing" $ withTestEnv $ do
      _ <- createExampleIndex
      resp <- flushIndex testIndex
      liftIO $ validateStatus resp 200
