{-# LANGUAGE OverloadedStrings #-}

module Test.Highlights where

import Test.Common
import Test.Import

import qualified Data.Map as M

initHighlights :: Text -> BH IO (Either EsError (Maybe HitHighlight))
initHighlights fieldName = do
  _ <- insertData
  _ <- insertOther
  let query = QueryMatchQuery $ mkMatchQuery (FieldName fieldName) (QueryString "haskell")
  let testHighlight = Highlights Nothing [FieldHighlight (FieldName fieldName) Nothing]
  let search = mkHighlightSearch (Just query) testHighlight
  searchTweetHighlight search

spec :: Spec
spec =
  describe "Highlights API" $ do
    it "returns highlight from query when there should be one" $ withTestEnv $ do
      myHighlight <- initHighlights "message"
      liftIO $
        myHighlight `shouldBe`
          Right (Just (M.fromList [("message", ["Use <em>haskell</em>!"])]))

    it "doesn't return highlight from a query when it shouldn't" $ withTestEnv $ do
      myHighlight <- initHighlights "user"
      liftIO $
        myHighlight `shouldBe`
          Right Nothing
