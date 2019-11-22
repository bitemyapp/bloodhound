{-# LANGUAGE OverloadedStrings #-}

module Test.Documents where

import Test.Common
import Test.Import

spec :: Spec
spec =
  describe "document API" $ do
    it "indexes, updates, gets, and then deletes the generated document" $ withTestEnv $ do
      _ <- insertData
      _ <- updateData
      docInserted <- getDocument testIndex (DocId "1")
      let newTweet = eitherDecode
                     (responseBody docInserted) :: Either String (EsResult Tweet)
      liftIO $ fmap getSource newTweet `shouldBe` Right (Just patchedTweet)

    it "indexes, gets, and then deletes the generated document with a DocId containing a space" $ withTestEnv $ do
      _ <- insertWithSpaceInId
      docInserted <- getDocument testIndex (DocId "Hello World")
      let newTweet = eitherDecode
                     (responseBody docInserted) :: Either String (EsResult Tweet)
      liftIO $ fmap getSource newTweet `shouldBe` Right (Just exampleTweet)

    it "produces a parseable result when looking up a bogus document" $ withTestEnv $ do
      doc <- getDocument testIndex (DocId "bogus")
      let noTweet = eitherDecode
                    (responseBody doc) :: Either String (EsResult Tweet)
      liftIO $ fmap foundResult noTweet `shouldBe` Right Nothing

    it "can use optimistic concurrency control" $ withTestEnv $ do
      let ev = ExternalDocVersion minBound
      let cfg = defaultIndexDocumentSettings { idsVersionControl = ExternalGT ev }
      resetIndex
      res <- insertData' cfg
      liftIO $ isCreated res `shouldBe` True
      res' <- insertData' cfg
      liftIO $ isVersionConflict res' `shouldBe` True

    it "indexes two documents in a parent/child relationship and checks that the child exists" $ withTestEnv $ do
      resetIndex
      _ <- putMapping testIndex ConversationMapping

      let parentSettings = defaultIndexDocumentSettings { idsJoinRelation = Just (ParentDocument (FieldName "reply_join") (RelationName "message")) }
      let childSettings = defaultIndexDocumentSettings { idsJoinRelation = Just (ChildDocument (FieldName "reply_join") (RelationName "reply") (DocId "1")) }

      _ <- indexDocument testIndex parentSettings exampleTweet (DocId "1")
      _ <- indexDocument testIndex childSettings otherTweet (DocId "2")

      _ <- refreshIndex testIndex

      let query = QueryHasParentQuery $ HasParentQuery (RelationName "message") (MatchAllQuery Nothing) Nothing Nothing
      let search = mkSearch (Just query) Nothing

      resp <- searchByIndex testIndex search
      parsed <- parseEsResponse resp :: BH IO (Either EsError (SearchResult Value))

      liftIO $ fmap (hitsTotal . searchHits) parsed `shouldBe` Right (HitsTotal 1 HTR_EQ)
