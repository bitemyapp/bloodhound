{-# LANGUAGE OverloadedStrings #-}

module Test.Script where

import Test.Common
import Test.Import

import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M

spec :: Spec
spec =
  describe "Script" $
    it "returns a transformed document based on the script field" $ withTestEnv $ do
      _ <- insertData
      let query = MatchAllQuery Nothing
          sfv = toJSON $
            Script
            (Just (ScriptLanguage "painless"))
            (Just (ScriptInline "doc['age'].value * 2"))
            Nothing
            Nothing
          sf = ScriptFields $
            HM.fromList [("test1", sfv)]
          search' = mkSearch (Just query) Nothing
          search = search' { scriptFields = Just sf }
      resp <- searchByIndex testIndex search
      parsed <- parseEsResponse resp :: BH IO (Either EsError (SearchResult Value))
      case parsed of
        Left e ->
          liftIO $ expectationFailure ("Expected a script-transformed result but got: " <> show e)
        Right sr -> do
          let Just results =
                hitFields (head (hits (searchHits sr)))
          liftIO $
            results `shouldBe` HitFields (M.fromList [("test1", [Number 20000.0])])
