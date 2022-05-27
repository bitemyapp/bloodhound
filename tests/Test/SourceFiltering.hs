{-# LANGUAGE OverloadedStrings #-}

module Test.SourceFiltering where

import qualified Data.Aeson.KeyMap as X
import Test.Common
import Test.Import

spec :: Spec
spec =
  describe "Source filtering" $ do
    it "doesn't include source when sources are disabled" $
      withTestEnv $
        searchExpectSource
          NoSource
          (Left (EsError 500 "Source was missing"))

    it "includes a source" $
      withTestEnv $
        searchExpectSource
          (SourcePatterns (PopPattern (Pattern "message")))
          (Right (Object (X.fromList [("message", String "Use haskell!")])))

    it "includes sources" $
      withTestEnv $
        searchExpectSource
          (SourcePatterns (PopPatterns [Pattern "user", Pattern "message"]))
          (Right (Object (X.fromList [("user", String "bitemyapp"), ("message", String "Use haskell!")])))

    it "includes source patterns" $
      withTestEnv $
        searchExpectSource
          (SourcePatterns (PopPattern (Pattern "*ge")))
          (Right (Object (X.fromList [("age", Number 10000), ("message", String "Use haskell!")])))

    it "excludes source patterns" $
      withTestEnv $
        searchExpectSource
          ( SourceIncludeExclude
              (Include [])
              (Exclude [Pattern "l*", Pattern "*ge", Pattern "postDate", Pattern "extra"])
          )
          (Right (Object (X.fromList [("user", String "bitemyapp")])))
