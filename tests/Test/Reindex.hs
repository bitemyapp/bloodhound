{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Reindex (spec) where

import           Control.Concurrent
import           Numeric.Natural
import           Test.Common
import           Test.Import

spec :: Spec
spec =
  describe "Reindex" $ do
    it "reindexes synchronously" $ withTestEnv $ do
      _ <- insertData
      let newIndex = IndexName "bloodhound-tests-twitter-index-1-reindexed"
      _ <- deleteIndex newIndex
      _ <- createIndex (IndexSettings (ShardCount 1) (ReplicaCount 0)) newIndex
      _ <- assertRight =<< reindex (mkReindexRequest testIndex newIndex)
      _ <- refreshIndex newIndex
      searchResult <- assertRight =<< parseEsResponse =<< searchByIndex newIndex (mkSearch Nothing Nothing)
      liftIO $
        (map hitSource (hits (searchHits searchResult))) `shouldBe` [Just exampleTweet]

    it "reindexes asynchronously" $ withTestEnv $ do
      _ <- insertData
      let newIndex = IndexName "bloodhound-tests-twitter-index-1-reindexed"
      _ <- deleteIndex newIndex
      _ <- createIndex (IndexSettings (ShardCount 1) (ReplicaCount 0)) newIndex
      taskNodeId <- assertRight =<< reindexAsync (mkReindexRequest testIndex newIndex)
      _ <- waitForTaskToComplete 10 taskNodeId
      _ <- refreshIndex newIndex
      searchResult <- assertRight =<< parseEsResponse =<< searchByIndex newIndex (mkSearch Nothing Nothing)
      liftIO $
        (map hitSource (hits (searchHits searchResult))) `shouldBe` [Just exampleTweet]

assertRight :: (Show a , MonadIO m) => Either a b -> m b
assertRight (Left x)  = fail $ "Expected Right, got Left: " <> show x
assertRight (Right x) = pure x

-- | The response is not used, but make sure we return it so it gets parsed
waitForTaskToComplete :: (MonadBH m, MonadThrow m) => Natural -> TaskNodeId -> m (TaskResponse ReindexResponse)
waitForTaskToComplete 0 taskNodeId = fail $ "Timed out waiting for task to complete, taskNodeId = " <> show taskNodeId
waitForTaskToComplete n taskNodeId = do
  task <- assertRight =<< getTask taskNodeId
  if taskResponseCompleted task
    then return task
    else (liftIO $ threadDelay 100000) >> waitForTaskToComplete (n - 1) taskNodeId
