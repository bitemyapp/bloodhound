{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.ReindexSpec (spec) where

import Control.Concurrent
import Numeric.Natural
import TestsUtils.Common
import TestsUtils.Import

spec :: Spec
spec =
  describe "Reindex" $ do
    it "reindexes synchronously" $ withTestEnv $ do
      _ <- insertData
      let newIndex = [qqIndexName|bloodhound-tests-twitter-index-1-reindexed|]
      _ <- tryPerformBHRequest $ deleteIndex newIndex
      _ <- performBHRequest $ createIndex (IndexSettings (ShardCount 1) (ReplicaCount 0) defaultIndexMappingsLimits) newIndex
      _ <- assertRight =<< tryPerformBHRequest (reindex $ mkReindexRequest testIndex newIndex)
      _ <- performBHRequest $ refreshIndex newIndex
      searchResult <- assertRight =<< tryPerformBHRequest (searchByIndex newIndex $ mkSearch Nothing Nothing)
      liftIO $
        map hitSource (hits (searchHits searchResult)) `shouldBe` [Just exampleTweet]

    it "reindexes asynchronously" $ withTestEnv $ do
      _ <- insertData
      let newIndex = [qqIndexName|bloodhound-tests-twitter-index-1-reindexed|]
      _ <- tryPerformBHRequest $ deleteIndex newIndex
      _ <- performBHRequest $ createIndex (IndexSettings (ShardCount 1) (ReplicaCount 0) defaultIndexMappingsLimits) newIndex
      taskNodeId <- assertRight =<< tryPerformBHRequest (reindexAsync $ mkReindexRequest testIndex newIndex)
      _ <- waitForTaskToComplete 10 taskNodeId
      _ <- performBHRequest $ refreshIndex newIndex
      searchResult <- assertRight =<< tryPerformBHRequest (searchByIndex newIndex (mkSearch Nothing Nothing))
      liftIO $
        (map hitSource (hits (searchHits searchResult))) `shouldBe` [Just exampleTweet]

assertRight :: (Show a, MonadFail m) => Either a b -> m b
assertRight (Left x) = fail $ "Expected Right, got Left: " <> show x
assertRight (Right x) = pure x

-- | The response is not used, but make sure we return it so it gets parsed
waitForTaskToComplete :: (MonadBH m, MonadFail m) => Natural -> TaskNodeId -> m (TaskResponse ReindexResponse)
waitForTaskToComplete 0 taskNodeId = fail $ "Timed out waiting for task to complete, taskNodeId = " <> show taskNodeId
waitForTaskToComplete n taskNodeId = do
  task <- assertRight =<< tryPerformBHRequest (getTask taskNodeId)
  if taskResponseCompleted task
    then return task
    else liftIO (threadDelay 100000) >> waitForTaskToComplete (n - 1) taskNodeId
