{-# LANGUAGE OverloadedStrings #-}

module Test.SourceFiltering where

import Test.Common
import Test.Import

import qualified Data.HashMap.Strict as HM

spec :: Spec
spec =
  describe "Source filtering" $ do

    it "doesn't include source when sources are disabled" $ withTestEnv $ do
      searchExpectSource
        NoSource
        (Left (EsError 500 "Source was missing"))

    it "includes a source" $ withTestEnv $ do
      searchExpectSource
        (SourcePatterns (PopPattern (Pattern "message")))
        (Right (Object (HM.fromList [("message", String "Use haskell!")])))

    it "includes sources" $ withTestEnv $ do
      searchExpectSource
        (SourcePatterns (PopPatterns [Pattern "user", Pattern "message"]))
        (Right (Object (HM.fromList [("user",String "bitemyapp"),("message", String "Use haskell!")])))

    it "includes source patterns" $ withTestEnv $ do
      searchExpectSource
        (SourcePatterns (PopPattern (Pattern "*ge")))
        (Right (Object (HM.fromList [("age", Number 10000),("message", String "Use haskell!")])))

    it "excludes source patterns" $ withTestEnv $ do
      searchExpectSource
        (SourceIncludeExclude (Include [])
        (Exclude [Pattern "l*", Pattern "*ge", Pattern "postDate", Pattern "extra"]))
        (Right (Object (HM.fromList [("user",String "bitemyapp")])))
