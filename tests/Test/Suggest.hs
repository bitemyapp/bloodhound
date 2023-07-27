{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Suggest where

import Test.Common
import Test.Import

spec :: Spec
spec =
  describe "Suggest" $
    it "returns a search suggestion using the phrase suggester" $
      withTestEnv $ do
        _ <- insertData
        let query = QueryMatchNoneQuery
            phraseSuggester = mkPhraseSuggester (FieldName "message")
            namedSuggester = Suggest "Use haskel" "suggest_name" (SuggestTypePhraseSuggester phraseSuggester)
            search' = mkSearch (Just query) Nothing
            search = search' {suggestBody = Just namedSuggester}
            expectedText = Just "use haskell"
        sr <- searchByIndex @Tweet testIndex search
        liftIO $ (suggestOptionsText . head . suggestResponseOptions . head . nsrResponses <$> suggest sr) `shouldBe` expectedText
