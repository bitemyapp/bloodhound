{-# LANGUAGE OverloadedStrings #-}

module Test.CountSpec (spec) where

import TestsUtils.Common
import TestsUtils.Import

spec :: Spec
spec =
  describe "Count" $
    it "returns count of a query" $
      withTestEnv $ do
        _ <- insertData
        let query = MatchAllQuery Nothing
            count = CountQuery query
        c <- performBHRequest $ countByIndex testIndex count
        liftIO $ crCount c `shouldBe` 1
