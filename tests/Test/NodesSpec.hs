{-# LANGUAGE RecordWildCards #-}

module Test.NodesSpec (spec) where

import TestsUtils.Common
import TestsUtils.Import
import Prelude

spec :: Spec
spec = do
  describe "getNodesInfo" $
    it "fetches the responding node when LocalNode is used" $
      withTestEnv $ do
        NodesInfo {..} <- performBHRequest $ getNodesInfo LocalNode
        -- This is really just a smoke test for response
        -- parsing. Node info is so variable, there's not much I can
        -- assert here.
        liftIO $ length nodesInfo `shouldBe` 1

  describe "getNodesStats" $
    it "fetches the responding node when LocalNode is used" $
      withTestEnv $ do
        NodesStats {..} <- performBHRequest $ getNodesStats LocalNode
        -- This is really just a smoke test for response
        -- parsing. Node stats is so variable, there's not much I can
        -- assert here.
        liftIO $ length nodesStats `shouldBe` 1
