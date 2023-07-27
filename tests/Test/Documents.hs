{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Documents where

import Test.Common
import Test.Import

spec :: Spec
spec =
  describe "document API" $ do
    it "indexes, updates, gets, and then deletes the generated document" $
      withTestEnv $ do
        _ <- insertData
        _ <- updateData
        newTweet <- getDocument @Tweet testIndex (DocId "1")
        liftIO $ getSource newTweet `shouldBe` Just patchedTweet

    it "indexes, gets, and then deletes the generated document with a DocId containing a space" $
      withTestEnv $ do
        _ <- insertWithSpaceInId
        newTweet <- getDocument @Tweet testIndex (DocId "Hello World")
        liftIO $ getSource newTweet `shouldBe` Just exampleTweet

    it "produces a parseable result when looking up a bogus document" $
      withTestEnv $ do
        noTweet <- getDocument @Tweet testIndex (DocId "bogus")
        liftIO $ foundResult noTweet `shouldBe` Nothing

    it "can use optimistic concurrency control" $
      withTestEnv $ do
        let ev = ExternalDocVersion minBound
        let cfg = defaultIndexDocumentSettings {idsVersionControl = ExternalGT ev}
        resetIndex
    -- TODO Exposing BHResponse
    -- res <- insertData' cfg
    -- liftIO $ isCreated res `shouldBe` True
    -- res' <- insertData' cfg
    -- liftIO $ isVersionConflict res' `shouldBe` True

    it "indexes two documents in a parent/child relationship and checks that the child exists" $
      withTestEnv $ do
        resetIndex
        _ <- putMapping @Value testIndex ConversationMapping

        let parentSettings = defaultIndexDocumentSettings {idsJoinRelation = Just (ParentDocument (FieldName "reply_join") (RelationName "message"))}
        let childSettings = defaultIndexDocumentSettings {idsJoinRelation = Just (ChildDocument (FieldName "reply_join") (RelationName "reply") (DocId "1"))}

        _ <- indexDocument testIndex parentSettings exampleTweet (DocId "1")
        _ <- indexDocument testIndex childSettings otherTweet (DocId "2")

        _ <- refreshIndex testIndex

        let query = QueryHasParentQuery $ HasParentQuery (RelationName "message") (MatchAllQuery Nothing) Nothing Nothing
        let search = mkSearch (Just query) Nothing

        response <- searchByIndex @Value testIndex search
        liftIO $ hitsTotal (searchHits response) `shouldBe` HitsTotal 1 HTR_EQ
