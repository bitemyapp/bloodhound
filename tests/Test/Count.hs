{-# LANGUAGE OverloadedStrings #-}

module Test.Count (spec) where

import Test.Common
import Test.Import

spec :: Spec
spec =
  describe "Count" $
    it "returns count of a query" $ withTestEnv $ do
      _ <- insertData
      let query = MatchAllQuery Nothing
          count = CountQuery query
      c <- countByIndex testIndex count
      liftIO $ (crCount <$> c) `shouldBe` (Right 1)
