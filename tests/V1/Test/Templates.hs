{-# LANGUAGE OverloadedStrings #-}

module Test.Templates where

import Test.Common
import Test.Import

spec :: Spec
spec =
  describe "template API" $ do
    it "can create a template" $ withTestEnv $ do
      let idxTpl = IndexTemplate (TemplatePattern "tweet-*") (Just (IndexSettings (ShardCount 1) (ReplicaCount 1))) [toJSON TweetMapping]
      resp <- putTemplate idxTpl (TemplateName "tweet-tpl")
      liftIO $ validateStatus resp 200

    it "can detect if a template exists" $ withTestEnv $ do
      exists <- templateExists (TemplateName "tweet-tpl")
      liftIO $ exists `shouldBe` True

    it "can delete a template" $ withTestEnv $ do
      resp <- deleteTemplate (TemplateName "tweet-tpl")
      liftIO $ validateStatus resp 200

    it "can detect if a template doesn't exist" $ withTestEnv $ do
      exists <- templateExists (TemplateName "tweet-tpl")
      liftIO $ exists `shouldBe` False
