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
        newTweet <- performBHRequest $ getDocument @Tweet testIndex (DocId "1")
        liftIO $ getSource newTweet `shouldBe` Just patchedTweet

    it "indexes, gets, and then deletes the generated document with a DocId containing a space" $
      withTestEnv $ do
        _ <- insertWithSpaceInId
        newTweet <- performBHRequest $ getDocument @Tweet testIndex (DocId "Hello World")
        liftIO $ getSource newTweet `shouldBe` Just exampleTweet

    it "produces a parseable result when looking up a bogus document" $
      withTestEnv $ do
        noTweet <- performBHRequest $ getDocument @Tweet testIndex (DocId "bogus")
        liftIO $ foundResult noTweet `shouldBe` Nothing

    it "can use optimistic concurrency control" $
      withTestEnv $ do
        let ev = ExternalDocVersion minBound
        let cfg = defaultIndexDocumentSettings {idsVersionControl = ExternalGT ev}
        resetIndex
        (res, _) <- insertData' cfg
        liftIO $ isCreated res `shouldBe` True
        insertedConflict <- tryEsError $ insertData' cfg
        case insertedConflict of
          Right (res', _) -> liftIO $ isVersionConflict res' `shouldBe` True
          Left e -> liftIO $ errorStatus e `shouldBe` 409

    it "indexes two documents in a parent/child relationship and checks that the child exists" $
      withTestEnv $ do
        resetIndex
        _ <- performBHRequest $ putMapping @Value testIndex ConversationMapping

        let parentSettings = defaultIndexDocumentSettings {idsJoinRelation = Just (ParentDocument (FieldName "reply_join") (RelationName "message"))}
        let childSettings = defaultIndexDocumentSettings {idsJoinRelation = Just (ChildDocument (FieldName "reply_join") (RelationName "reply") (DocId "1"))}

        _ <- performBHRequest $ indexDocument testIndex parentSettings exampleTweet (DocId "1")
        _ <- performBHRequest $ indexDocument testIndex childSettings otherTweet (DocId "2")

        _ <- performBHRequest $ refreshIndex testIndex

        let query = QueryHasParentQuery $ HasParentQuery (RelationName "message") (MatchAllQuery Nothing) Nothing Nothing
        let search = mkSearch (Just query) Nothing

        response <- performBHRequest $ searchByIndex @Value testIndex search
        liftIO $ hitsTotal (searchHits response) `shouldBe` HitsTotal 1 HTR_EQ
