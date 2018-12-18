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
      docInserted <- getDocument testIndex testMapping (DocId "1")
      let newTweet = eitherDecode
                     (responseBody docInserted) :: Either String (EsResult Tweet)
      liftIO $ fmap getSource newTweet `shouldBe` Right (Just patchedTweet)

    it "indexes, gets, and then deletes the generated document with a DocId containing a space" $ withTestEnv $ do
      _ <- insertWithSpaceInId
      docInserted <- getDocument testIndex testMapping (DocId "Hello World")
      let newTweet = eitherDecode
                     (responseBody docInserted) :: Either String (EsResult Tweet)
      liftIO $ fmap getSource newTweet `shouldBe` Right (Just exampleTweet)

    it "produces a parseable result when looking up a bogus document" $ withTestEnv $ do
      doc <- getDocument testIndex testMapping  (DocId "bogus")
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
      let validateStatus' stat = liftIO . flip validateStatus stat
      _ <- validateStatus' 200 =<< putMapping testIndex (MappingName "child") ChildMapping
      _ <- validateStatus' 200 =<< putMapping testIndex (MappingName "parent") ParentMapping
      _ <- validateStatus' 201 =<< indexDocument testIndex (MappingName "parent") defaultIndexDocumentSettings exampleTweet (DocId "1")
      let parent = (Just . DocumentParent . DocId) "1"
          ids = IndexDocumentSettings NoVersionControl parent
      _ <- validateStatus' 201 =<< indexDocument testIndex (MappingName "child") ids otherTweet (DocId "2")
      _ <- refreshIndex testIndex
      exists <- documentExists testIndex (MappingName "child") parent (DocId "2")
      liftIO $ exists `shouldBe` True
