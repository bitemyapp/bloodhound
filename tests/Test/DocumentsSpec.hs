{-# LANGUAGE OverloadedStrings #-}

module Test.DocumentsSpec where

import TestsUtils.Common
import TestsUtils.Import

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
          Right _ -> liftIO $ expectationFailure "This should never not happen."
          Left e -> liftIO $ errorStatus e `shouldBe` Just 409

    it "can use predicates on the response" $
      withTestEnv $ do
        let ev = ExternalDocVersion minBound
        let cfg = defaultIndexDocumentSettings {idsVersionControl = ExternalGT ev}
        resetIndex
        (res, _) <- insertData' cfg
        liftIO $ isCreated res `shouldBe` True
        liftIO $ isSuccess res `shouldBe` True
        liftIO $ isVersionConflict res `shouldBe` False

        res' <- insertData'' cfg
        liftIO $ isCreated res' `shouldBe` False
        liftIO $ isSuccess res' `shouldBe` False
        liftIO $ isVersionConflict res' `shouldBe` True

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

    it "updates documents by query" $ withTestEnv $ do
      _ <- insertData
      _ <- insertOther
      _ <- insertExtra
      let query = (TermQuery (Term "user" "bitemyapp") Nothing)
          script =
            Script
              (Just (ScriptLanguage "painless"))
              (ScriptInline "ctx._source.age *= 2")
              Nothing
      _ <- performBHRequest $ updateByQuery @Value testIndex query (Just script)
      _ <- performBHRequest $ refreshIndex testIndex
      let search = mkSearch (Just query) Nothing
      parsed <- searchTweets search
      case parsed of
        Left e ->
          liftIO $ expectationFailure ("Expected tweets as search result but got: " <> show e)
        Right sr -> do
          let results =
                map (fmap age . hitSource) (hits (searchHits sr))
          liftIO $
            results `shouldBe` [Just $ age exampleTweet * 2, Just $ age tweetWithExtra * 2]
