{-# LANGUAGE OverloadedStrings #-}

module Test.Suggest where

import Test.Common
import Test.Import

spec :: Spec
spec =
  describe "Suggest" $
    it "returns a search suggestion using the phrase suggester" $ withTestEnv $ do
      _ <- insertData
      let query = QueryMatchNoneQuery
          phraseSuggester = mkPhraseSuggester (FieldName "message")
          namedSuggester = Suggest "Use haskel" "suggest_name" (SuggestTypePhraseSuggester phraseSuggester)
          search' = mkSearch (Just query) Nothing
          search = search' { suggestBody = Just namedSuggester }
          expectedText = Just "use haskell"
      resp <- searchByIndex testIndex search
      parsed <- parseEsResponse resp :: BH IO (Either EsError (SearchResult Tweet))
      case parsed of
        Left e -> liftIO $ expectationFailure ("Expected an search suggestion but got " <> show e)
        Right sr -> liftIO $ (suggestOptionsText . head . suggestResponseOptions . head . nsrResponses  <$> suggest sr) `shouldBe` expectedText
