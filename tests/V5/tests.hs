{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#if __GLASGOW_HASKELL__ < 800
{-# OPTIONS_GHC -fcontext-stack=100 #-}
#endif
#if __GLASGOW_HASKELL__ >= 802
{-# LANGUAGE MonoLocalBinds #-}
#endif
module Main where

import           Test.Common
import           Test.Import

import           Prelude

import qualified Test.Aggregation as Aggregation
import qualified Test.BulkAPI as Bulk
import qualified Test.Documents as Documents
import qualified Test.Highlights as Highlights
import qualified Test.Indices as Indices
import qualified Test.JSON as JSON
import qualified Test.Query as Query
import qualified Test.Script as Script
import qualified Test.Snapshots as Snapshots
import qualified Test.Sorting as Sorting
import qualified Test.SourceFiltering as SourceFiltering
import qualified Test.Suggest as Suggest
import qualified Test.Templates as Templates

main :: IO ()
main = hspec $ do
  Aggregation.spec
  Bulk.spec
  Documents.spec
  Highlights.spec
  Indices.spec
  JSON.spec
  Query.spec
  Script.spec
  Snapshots.spec
  Sorting.spec
  SourceFiltering.spec
  Suggest.spec
  Templates.spec

  describe "error parsing"  $ do
    it "can parse EsErrors for < 2.0" $ when' (atmost es16) $ withTestEnv $ do
      res <- getDocument (IndexName "bogus") (MappingName "also_bogus") (DocId "bogus_as_well")
      let errorResp = eitherDecode (responseBody res)
      liftIO (errorResp `shouldBe` Right (EsError 404 "IndexMissingException[[bogus] missing]"))

    it "can parse EsErrors for >= 2.0" $ when' (atleast es20) $ withTestEnv $ do
      res <- getDocument (IndexName "bogus") (MappingName "also_bogus") (DocId "bogus_as_well")
      let errorResp = eitherDecode (responseBody res)
      liftIO (errorResp `shouldBe` Right (EsError 404 "no such index"))

  describe "Monoid (SearchHits a)" $
    prop "abides the monoid laws" $ eq $
      prop_Monoid (T :: T (SearchHits ()))

  describe "mkDocVersion" $
    prop "can never construct an out of range docVersion" $ \i ->
      let res = mkDocVersion i
      in case res of
        Nothing -> property True
        Just dv -> (dv >= minBound) .&&.
                   (dv <= maxBound) .&&.
                   docVersionNumber dv === i

  describe "getNodesInfo" $
     it "fetches the responding node when LocalNode is used" $ withTestEnv $ do
       res <- getNodesInfo LocalNode
       liftIO $ case res of
         -- This is really just a smoke test for response
         -- parsing. Node info is so variable, there's not much I can
         -- assert here.
         Right NodesInfo {..} -> length nodesInfo `shouldBe` 1
         Left e -> expectationFailure ("Expected NodesInfo but got " <> show e)

  describe "getNodesStats" $
     it "fetches the responding node when LocalNode is used" $ withTestEnv $ do
       res <- getNodesStats LocalNode
       liftIO $ case res of
         -- This is really just a smoke test for response
         -- parsing. Node stats is so variable, there's not much I can
         -- assert here.
         Right NodesStats {..} -> length nodesStats `shouldBe` 1
         Left e -> expectationFailure ("Expected NodesStats but got " <> show e)

  describe "Enum DocVersion" $
    it "follows the laws of Enum, Bounded" $ do
      evaluate (succ maxBound :: DocVersion) `shouldThrow` anyErrorCall
      evaluate (pred minBound :: DocVersion) `shouldThrow` anyErrorCall
      evaluate (toEnum 0 :: DocVersion) `shouldThrow` anyErrorCall
      evaluate (toEnum 9200000000000000001 :: DocVersion) `shouldThrow` anyErrorCall
      enumFrom (pred maxBound :: DocVersion) `shouldBe` [pred maxBound, maxBound]
      enumFrom (pred maxBound :: DocVersion) `shouldBe` [pred maxBound, maxBound]
      enumFromThen minBound (pred maxBound :: DocVersion) `shouldBe` [minBound, pred maxBound]

  describe "Scan & Scroll API" $
    it "returns documents using the scan&scroll API" $ withTestEnv $ do
      _ <- insertData
      _ <- insertOther
      let search =
            (mkSearch
             (Just $ MatchAllQuery Nothing) Nothing)
             { size = Size 1 }
      regular_search <- searchTweet search
      scan_search' <- scanSearch testIndex testMapping search :: BH IO [Hit Tweet]
      let scan_search = map hitSource scan_search'
      liftIO $
        regular_search `shouldBe` Right exampleTweet -- Check that the size restrtiction is being honored
      liftIO $
        scan_search `shouldMatchList` [Just exampleTweet, Just otherTweet]
