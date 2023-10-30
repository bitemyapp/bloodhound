{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.ScriptSpec (spec) where

import qualified Data.Aeson.KeyMap as X
import qualified Data.Map as M
import TestsUtils.Common
import TestsUtils.Import

spec :: Spec
spec =
  describe "Script" $
    it "returns a transformed document based on the script field" $
      withTestEnv $ do
        _ <- insertData
        let query = MatchAllQuery Nothing
            sfv =
              toJSON $
                Script
                  (Just (ScriptLanguage "painless"))
                  (ScriptInline "doc['age'].value * 2")
                  Nothing
            sf =
              ScriptFields $
                X.fromList [("test1", sfv)]
            search' = mkSearch (Just query) Nothing
            search = search' {scriptFields = Just sf}
        sr <- performBHRequest $ searchByIndex @Value testIndex search
        let Just results =
              hitFields (head (hits (searchHits sr)))
        liftIO $
          results `shouldBe` HitFields (M.fromList [("test1", [Number 20000.0])])
