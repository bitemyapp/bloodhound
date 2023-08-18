{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.BulkAPI (spec) where

import qualified Data.Aeson.KeyMap as X
import qualified Data.Aeson.Optics as LMA
import qualified Data.Vector as V
import Optics
import Test.Common
import Test.Import

newtype BulkTest
  = BulkTest Text
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
  { bstName :: Text,
    bstCounter :: Int
  }
  deriving (Eq, Show)

instance ToJSON BulkScriptTest where
  toJSON (BulkScriptTest name' count) =
    object ["name" .= name', "counter" .= count]

instance FromJSON BulkScriptTest where
  parseJSON = withObject "BulkScriptTest" $ \v ->
    BulkScriptTest
      <$> v
        .: "name"
      <*> v
        .: "counter"

assertDocs :: (FromJSON a, Show a, Eq a) => [(DocId, a)] -> BH IO ()
assertDocs as = do
  let (ids, docs) = unzip as
  res <- traverse (fmap getSource . getDocument testIndex) ids
  liftIO $ res `shouldBe` map Just docs

upsertDocs ::
  (ToJSON a, Show a, Eq a) =>
  (Value -> UpsertPayload) ->
  [(DocId, a)] ->
  BH IO ()
upsertDocs f as = do
  let batch = as <&> (\(id_, doc) -> BulkUpsert testIndex id_ (f $ toJSON doc) []) & V.fromList
  bulk batch >> refreshIndex testIndex >> pure ()

spec :: Spec
spec =
  describe "Bulk API" $ do
    it "upsert operations" $
      withTestEnv $ do
        _ <- insertData

        -- Upserting in a to a fresh index should insert
        let toInsert = [(DocId "3", BulkTest "stringer"), (DocId "5", BulkTest "sobotka"), (DocId "7", BulkTest "snoop")]
        upsertDocs UpsertDoc toInsert
        assertDocs toInsert

        -- Upserting existing documents should update
        let toUpsert = [(DocId "3", BulkTest "bell"), (DocId "5", BulkTest "frank"), (DocId "7", BulkTest "snoop")]
        upsertDocs UpsertDoc toUpsert
        assertDocs toUpsert

    it "upsert with a script" $
      withTestEnv $ do
        _ <- insertData

        -- first insert the batch
        let batch = [(DocId "3", BulkScriptTest "stringer" 0), (DocId "5", BulkScriptTest "sobotka" 3)]
        upsertDocs UpsertDoc batch

        -- then upsert with the script

        let script =
              Script
                { scriptLanguage = Just $ ScriptLanguage "painless",
                  scriptSource = ScriptInline "ctx._source.counter += params.count",
                  scriptParams = Just $ ScriptParams $ X.fromList [("count", Number 2)]
                }

        upsertDocs (UpsertScript False script) batch
        assertDocs (batch <&> (\(i, v) -> (i, v {bstCounter = bstCounter v + 2})))

    it "script upsert without scripted_upsert" $
      withTestEnv $ do
        _ <- insertData

        let batch = [(DocId "3", BulkScriptTest "stringer" 0), (DocId "5", BulkScriptTest "sobotka" 3)]

        let script =
              Script
                { scriptLanguage = Just $ ScriptLanguage "painless",
                  scriptSource = ScriptInline "ctx._source.counter += params.count",
                  scriptParams = Just $ ScriptParams $ X.fromList [("count", Number 2)]
                }

        -- Without "script_upsert" flag new documents are simply inserted and are not handled by the script
        upsertDocs (UpsertScript False script) batch
        assertDocs batch

    it "script upsert with scripted_upsert -- will fail if a bug on elasticsearch is fix, delete patch line" $
      withTestEnv $ do
        _ <- insertData

        let batch = [(DocId "3", BulkScriptTest "stringer" 0), (DocId "5", BulkScriptTest "sobotka" 3)]

        let script =
              Script
                { scriptLanguage = Just $ ScriptLanguage "painless",
                  scriptSource = ScriptInline "ctx._source.counter += params.count",
                  scriptParams = Just $ ScriptParams $ X.fromList [("count", Number 2)]
                }

        -- Without "script_upsert" flag new documents are simply inserted and are not handled by the script
        upsertDocs (UpsertScript True script) batch

        -- if this test fails due to a bug in ES7: https://github.com/elastic/elasticsearch/issues/48670, delete next line when it is solved.
        assertDocs (batch <&> (\(i, v) -> (i, v {bstCounter = bstCounter v + 2})))

    it "inserts all documents we request" $
      withTestEnv $ do
        _ <- insertData
        let firstTest = BulkTest "blah"
        let secondTest = BulkTest "bloo"
        let thirdTest = BulkTest "graffle"
        let fourthTest = BulkTest "garabadoo"
        let fifthTest = BulkTest "serenity"
        let firstDoc = BulkIndex testIndex (DocId "2") (toJSON firstTest)
        let secondDoc = BulkCreate testIndex (DocId "3") (toJSON secondTest)
        let thirdDoc = BulkCreateEncoding testIndex (DocId "4") (toEncoding thirdTest)
        let fourthDoc = BulkIndexAuto testIndex (toJSON fourthTest)
        let fifthDoc = BulkIndexEncodingAuto testIndex (toEncoding fifthTest)
        let stream = V.fromList [firstDoc, secondDoc, thirdDoc, fourthDoc, fifthDoc]
        _ <- bulk stream
        -- liftIO $ pPrint bulkResp
        _ <- refreshIndex testIndex
        -- liftIO $ pPrint refreshResp
        maybeFirst <- getDocument @BulkTest testIndex (DocId "2")
        maybeSecond <- getDocument @BulkTest testIndex (DocId "3")
        maybeThird <- getDocument @BulkTest testIndex (DocId "4")
        -- note that we cannot query for fourthDoc and fifthDoc since we
        -- do not know their autogenerated ids.
        -- liftIO $ pPrint [maybeFirst, maybeSecond, maybeThird]
        liftIO $ do
          getSource maybeFirst `shouldBe` Just firstTest
          getSource maybeSecond `shouldBe` Just secondTest
          getSource maybeThird `shouldBe` Just thirdTest
        -- Since we can't get the docs by doc id, we check for their existence in
        -- a match all query.
        let query = MatchAllQuery Nothing
        let search = mkSearch (Just query) Nothing
        sr <- searchByIndex @Value testIndex search
        liftIO $
          hitsTotal (searchHits sr) `shouldBe` HitsTotal 6 HTR_EQ
        let nameList :: [Text]
            nameList =
              hits (searchHits sr)
                ^.. traversed
                % to hitSource
                % _Just
                % LMA.key "name"
                % LMA._String

        liftIO $
          nameList
            `shouldBe` ["blah", "bloo", "graffle", "garabadoo", "serenity"]
