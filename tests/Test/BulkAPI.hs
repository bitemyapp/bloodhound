{-# LANGUAGE OverloadedStrings #-}

module Test.BulkAPI (spec) where

import           Data.Function       ((&))
import           Data.Functor        ((<&>))

import           Test.Common
import           Test.Import

import qualified Data.HashMap.Strict as HM
import qualified Data.Vector         as V
import qualified Lens.Micro.Aeson    as LMA

newtype BulkTest =
  BulkTest Text
  deriving (Eq, Show)

instance ToJSON BulkTest where
  toJSON (BulkTest name') =
    object ["name" .= name']

instance FromJSON BulkTest where
  parseJSON = withObject "BulkTest" parse
    where
      parse o = do
        t <- o .: "name"
        BulkTest <$> parseJSON t

data BulkScriptTest = BulkScriptTest
  { bstName    :: Text
  , bstCounter :: Int
  }
  deriving (Eq, Show)

instance ToJSON BulkScriptTest where
  toJSON (BulkScriptTest name' count) =
    object ["name" .= name', "counter" .= count]

instance FromJSON BulkScriptTest where
  parseJSON = withObject "BulkScriptTest" $ \v -> BulkScriptTest
    <$> v.: "name"
    <*> v .: "counter"

assertDocs :: (FromJSON a, Show a, Eq a) => [(DocId, a)] -> BH IO ()
assertDocs as = do
  let (ids, docs) = unzip as
  res <- ids &   traverse (getDocument testIndex testMapping)
             <&> traverse (fmap getSource . eitherDecode . responseBody)

  liftIO $ res `shouldBe` Right (Just <$> docs)

upsertDocs :: (ToJSON a, Show a, Eq a)
  => (Value -> UpsertPayload)
  -> [(DocId, a)]
  -> BH IO ()
upsertDocs f as = do
  let batch = as <&> (\(id_, doc) -> BulkUpsert testIndex testMapping id_ (f $ toJSON doc) []) & V.fromList
  bulk batch >> refreshIndex testIndex >> pure ()

spec :: Spec
spec =
  describe "Bulk API" $ do
    it "upsert operations" $ withTestEnv $ do
      _ <- insertData

      -- Upserting in a to a fresh index should insert
      let toInsert = [(DocId "3", BulkTest "stringer"), (DocId "5", BulkTest "sobotka"), (DocId "7", BulkTest "snoop")]
      upsertDocs UpsertDoc toInsert
      assertDocs toInsert

      -- Upserting existing documents should update
      let toUpsert = [(DocId "3", BulkTest "bell"), (DocId "5", BulkTest "frank"), (DocId "7", BulkTest "snoop")]
      upsertDocs UpsertDoc toUpsert
      assertDocs toUpsert

    it "upsert with a script" $ withTestEnv $ do
      _ <- insertData

      -- first insert the batch
      let batch = [(DocId "3", BulkScriptTest "stringer" 0), (DocId "5", BulkScriptTest "sobotka" 3)]
      upsertDocs UpsertDoc batch

      -- then upsert with the script

      let script = Script
                    { scriptLanguage = Just $ ScriptLanguage "painless"
                    , scriptInline = Just $ ScriptInline "ctx._source.counter += params.count"
                    , scriptStored = Nothing
                    , scriptParams = Just $ ScriptParams $ HM.fromList [("count", Number 2)]
                    }

      upsertDocs (UpsertScript False script) batch
      assertDocs (batch <&> (\(i, v) -> (i, v { bstCounter = bstCounter v + 2 })))

    it "script upsert without scripted_upsert" $ withTestEnv $ do
      _ <- insertData

      let batch = [(DocId "3", BulkScriptTest "stringer" 0), (DocId "5", BulkScriptTest "sobotka" 3)]

      let script = Script
                    { scriptLanguage = Just $ ScriptLanguage "painless"
                    , scriptInline = Just $ ScriptInline "ctx._source.counter += params.count"
                    , scriptStored = Nothing
                    , scriptParams = Just $ ScriptParams $ HM.fromList [("count", Number 2)]
                    }

      -- Without "script_upsert" flag new documents are simply inserted and are not handled by the script
      upsertDocs (UpsertScript False script) batch
      assertDocs batch

    it "script upsert with scripted_upsert" $ withTestEnv $ do
      _ <- insertData

      let batch = [(DocId "3", BulkScriptTest "stringer" 0), (DocId "5", BulkScriptTest "sobotka" 3)]

      let script = Script
                    { scriptLanguage = Just $ ScriptLanguage "painless"
                    , scriptInline = Just $ ScriptInline "ctx._source.counter += params.count"
                    , scriptStored = Nothing
                    , scriptParams = Just $ ScriptParams $ HM.fromList [("count", Number 2)]
                    }

      -- Without "script_upsert" flag new documents are simply inserted and are not handled by the script
      upsertDocs (UpsertScript True script) batch
      assertDocs (batch <&> (\(i, v) -> (i, v { bstCounter = bstCounter v + 2 })))

    it "inserts all documents we request" $ withTestEnv $ do
      _ <- insertData
      let firstTest = BulkTest "blah"
      let secondTest = BulkTest "bloo"
      let thirdTest = BulkTest "graffle"
      let fourthTest = BulkTest "garabadoo"
      let fifthTest = BulkTest "serenity"
      let firstDoc = BulkIndex testIndex
                     testMapping (DocId "2") (toJSON firstTest)
      let secondDoc = BulkCreate testIndex
                     testMapping (DocId "3") (toJSON secondTest)
      let thirdDoc = BulkCreateEncoding testIndex
                     testMapping (DocId "4") (toEncoding thirdTest)
      let fourthDoc = BulkIndexAuto testIndex
                      testMapping (toJSON fourthTest)
      let fifthDoc = BulkIndexEncodingAuto testIndex
                     testMapping (toEncoding fifthTest)
      let stream = V.fromList [firstDoc, secondDoc, thirdDoc, fourthDoc, fifthDoc]
      _ <- bulk stream
      -- liftIO $ pPrint bulkResp
      _ <- refreshIndex testIndex
      -- liftIO $ pPrint refreshResp
      fDoc <- getDocument testIndex testMapping (DocId "2")
      sDoc <- getDocument testIndex testMapping (DocId "3")
      tDoc <- getDocument testIndex testMapping (DocId "4")
      -- note that we cannot query for fourthDoc and fifthDoc since we
      -- do not know their autogenerated ids.
      let maybeFirst =
            eitherDecode
            $ responseBody fDoc
              :: Either String (EsResult BulkTest)
      let maybeSecond =
            eitherDecode
            $ responseBody sDoc
            :: Either String (EsResult BulkTest)
      let maybeThird =
            eitherDecode
            $ responseBody tDoc
            :: Either String (EsResult BulkTest)
      -- liftIO $ pPrint [maybeFirst, maybeSecond, maybeThird]
      liftIO $ do
        fmap getSource maybeFirst `shouldBe` Right (Just firstTest)
        fmap getSource maybeSecond `shouldBe` Right (Just secondTest)
        fmap getSource maybeThird `shouldBe` Right (Just thirdTest)
      -- Since we can't get the docs by doc id, we check for their existence in
      -- a match all query.
      let query = MatchAllQuery Nothing
      let search = mkSearch (Just query) Nothing
      resp <- searchByIndex testIndex search
      parsed <- parseEsResponse resp :: BH IO (Either EsError (SearchResult Value))
      case parsed of
        Left e ->
          liftIO $ expectationFailure ("Expected a script-transformed result but got: " <> show e)
        (Right sr) -> do
          liftIO $
            hitsTotal (searchHits sr) `shouldBe` 6
          let nameList :: [Text]
              nameList =
                hits (searchHits sr)
                ^.. traverse
                  . to hitSource
                  . _Just
                  . LMA.key "name"
                  . _String
          liftIO $
            nameList
            `shouldBe` ["blah","bloo","graffle","garabadoo","serenity"]
