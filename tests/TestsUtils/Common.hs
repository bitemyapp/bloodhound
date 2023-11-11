{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module TestsUtils.Common where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Versions as Versions
import Lens.Micro (toListOf)
import qualified Network.HTTP.Types.Status as NHTS
import TestsUtils.Import

testServer :: Server
testServer = Server "http://localhost:9200"

testIndex :: IndexName
testIndex = [qqIndexName|bloodhound-tests-twitter-1|]

withTestEnv :: BH IO a -> IO a
withTestEnv = withBH defaultManagerSettings testServer

data Location = Location
  { lat :: Double,
    lon :: Double
  }
  deriving (Eq, Show)

data Tweet = Tweet
  { user :: Text,
    postDate :: UTCTime,
    message :: Text,
    age :: Int,
    location :: Location,
    extra :: Maybe Text
  }
  deriving (Eq, Show)

$(deriveJSON defaultOptions ''Location)
$(deriveJSON defaultOptions ''Tweet)

data ConversationMapping = ConversationMapping deriving (Eq, Show)

instance ToJSON ConversationMapping where
  toJSON ConversationMapping =
    object
      [ "properties"
          .= object
            [ "reply_join"
                .= object
                  [ "type" .= ("join" :: Text),
                    "relations" .= object ["message" .= ("reply" :: Text)]
                  ],
              "user"
                .= object
                  [ "type" .= ("text" :: Text),
                    "fielddata" .= True
                  ],
              -- Serializing the date as a date is breaking other tests, mysteriously.
              -- , "postDate" .= object [ "type"   .= ("date" :: Text)
              --                        , "format" .= ("YYYY-MM-dd`T`HH:mm:ss.SSSZZ" :: Text)]
              "message" .= object ["type" .= ("text" :: Text)],
              "age" .= object ["type" .= ("integer" :: Text)],
              "location" .= object ["type" .= ("geo_point" :: Text)],
              "extra" .= object ["type" .= ("keyword" :: Text)]
            ]
      ]

getServerVersion :: IO Versions.Version
getServerVersion = extractVersion <$> withTestEnv (performBHRequest getStatus)
  where
    extractVersion = versionNumber . number . version

createExampleIndex :: (MonadBH m) => m (BHResponse StatusDependant Acknowledged, Acknowledged)
createExampleIndex = do
  result <- tryPerformBHRequest (keepBHResponse $ createIndex (IndexSettings (ShardCount 1) (ReplicaCount 0) defaultIndexMappingsLimits) testIndex)
  case result of
    Left e
      | T.isSuffixOf "already exists" (errorMessage e) -> return (error "TODO rewrite this part too", Acknowledged False)
      | otherwise -> throwEsError e
    Right ack -> return ack

deleteExampleIndex :: (MonadBH m) => m (BHResponse StatusDependant Acknowledged, Acknowledged)
deleteExampleIndex =
  performBHRequest $ keepBHResponse $ deleteIndex testIndex

validateStatus :: (Show body) => BHResponse contextualized body -> Int -> Expectation
validateStatus resp expected =
  if actual == expected
    then return ()
    else expectationFailure ("Expected " <> show expected <> " but got " <> show actual <> ": " <> show body)
  where
    actual = NHTS.statusCode (responseStatus $ getResponse resp)
    body = responseBody $ getResponse resp

data TweetMapping = TweetMapping deriving (Eq, Show)

instance ToJSON TweetMapping where
  toJSON TweetMapping =
    object
      [ "properties"
          .= object
            [ "user"
                .= object
                  [ "type" .= ("text" :: Text),
                    "fielddata" .= True
                  ],
              -- Serializing the date as a date is breaking other tests, mysteriously.
              -- , "postDate" .= object [ "type"   .= ("date" :: Text)
              --                        , "format" .= ("YYYY-MM-dd`T`HH:mm:ss.SSSZZ" :: Text)]
              "message" .= object ["type" .= ("text" :: Text)],
              "age" .= object ["type" .= ("integer" :: Text)],
              "location" .= object ["type" .= ("geo_point" :: Text)],
              "extra" .= object ["type" .= ("keyword" :: Text)]
            ]
      ]

exampleTweet :: Tweet
exampleTweet =
  Tweet
    { user = "bitemyapp",
      postDate =
        UTCTime
          (ModifiedJulianDay 55000)
          (secondsToDiffTime 10),
      message = "Use haskell!",
      age = 10000,
      location = Location 40.12 (-71.34),
      extra = Nothing
    }

tweetWithExtra :: Tweet
tweetWithExtra =
  Tweet
    { user = "bitemyapp",
      postDate =
        UTCTime
          (ModifiedJulianDay 55000)
          (secondsToDiffTime 10),
      message = "Use haskell!",
      age = 10000,
      location = Location 40.12 (-71.34),
      extra = Just "blah blah"
    }

exampleTweetWithAge :: Int -> Tweet
exampleTweetWithAge age' =
  Tweet
    { user = "bitemyapp",
      postDate =
        UTCTime
          (ModifiedJulianDay 55000)
          (secondsToDiffTime 10),
      message = "Use haskell!",
      age = age',
      location = Location 40.12 (-71.34),
      extra = Nothing
    }

newAge :: Int
newAge = 31337

newUser :: Text
newUser = "someotherapp"

tweetPatch :: Value
tweetPatch =
  object
    [ "age" .= newAge,
      "user" .= newUser
    ]

patchedTweet :: Tweet
patchedTweet = exampleTweet {age = newAge, user = newUser}

otherTweet :: Tweet
otherTweet =
  Tweet
    { user = "notmyapp",
      postDate =
        UTCTime
          (ModifiedJulianDay 55000)
          (secondsToDiffTime 11),
      message = "Use haskell!",
      age = 1000,
      location = Location 40.12 (-71.34),
      extra = Nothing
    }

resetIndex :: BH IO ()
resetIndex = do
  _ <- tryEsError deleteExampleIndex
  _ <- createExampleIndex
  _ <- performBHRequest $ putMapping @Value testIndex TweetMapping
  return ()

insertData :: BH IO (BHResponse StatusDependant IndexedDocument, IndexedDocument)
insertData = do
  _ <- tryEsError resetIndex
  insertData' defaultIndexDocumentSettings

insertData' :: IndexDocumentSettings -> BH IO (BHResponse StatusDependant IndexedDocument, IndexedDocument)
insertData' ids = do
  r <- performBHRequest $ keepBHResponse $ indexDocument testIndex ids exampleTweet (DocId "1")
  _ <- performBHRequest $ refreshIndex testIndex
  return r

insertTweetWithDocId :: Tweet -> Text -> BH IO IndexedDocument
insertTweetWithDocId tweet docId = do
  let ids = defaultIndexDocumentSettings
  r <- performBHRequest $ indexDocument testIndex ids tweet (DocId docId)
  _ <- performBHRequest $ refreshIndex testIndex
  return r

updateData :: BH IO IndexedDocument
updateData = do
  r <- performBHRequest $ updateDocument testIndex defaultIndexDocumentSettings tweetPatch (DocId "1")
  _ <- performBHRequest $ refreshIndex testIndex
  return r

insertOther :: BH IO ()
insertOther = do
  _ <- performBHRequest $ indexDocument testIndex defaultIndexDocumentSettings otherTweet (DocId "2")
  _ <- performBHRequest $ refreshIndex testIndex
  return ()

insertExtra :: BH IO ()
insertExtra = do
  _ <- performBHRequest $ indexDocument testIndex defaultIndexDocumentSettings tweetWithExtra (DocId "4")
  _ <- performBHRequest $ refreshIndex testIndex
  return ()

insertWithSpaceInId :: BH IO ()
insertWithSpaceInId = do
  _ <- performBHRequest $ indexDocument testIndex defaultIndexDocumentSettings exampleTweet (DocId "Hello World")
  _ <- performBHRequest $ refreshIndex testIndex
  return ()

searchTweet :: Search -> BH IO (Either EsError Tweet)
searchTweet search = (>>= grabFirst) <$> searchTweets search

searchTweets :: Search -> BH IO (Either EsError (SearchResult Tweet))
searchTweets search = tryPerformBHRequest $ searchByIndex testIndex search

searchExpectNoResults :: Search -> BH IO ()
searchExpectNoResults search = do
  result <- searchTweets search
  let emptyHits = fmap (hits . searchHits) result
  liftIO $
    emptyHits `shouldBe` Right []

searchExpectAggs :: Search -> BH IO ()
searchExpectAggs search = do
  result <- performBHRequest $ searchByIndex @Tweet testIndex search
  let isEmpty x = return (M.null x)
  liftIO $
    (aggregations result >>= isEmpty) `shouldBe` Just False

searchValidBucketAgg ::
  (BucketAggregation a, FromJSON a, Show a) =>
  Search ->
  Key ->
  (Key -> AggregationResults -> Maybe (Bucket a)) ->
  BH IO ()
searchValidBucketAgg search aggKey extractor = do
  result <- performBHRequest $ searchByIndex @Tweet testIndex search
  let bucketDocs = docCount . head . buckets
  let count = aggregations result >>= extractor aggKey >>= \x -> return (bucketDocs x)
  liftIO $
    count `shouldBe` Just 1

searchTermsAggHint :: [ExecutionHint] -> BH IO ()
searchTermsAggHint hints = do
  let terms hint = TermsAgg $ (mkTermsAggregation "user.keyword") {termExecutionHint = Just hint}
  let search hint = mkAggregateSearch Nothing $ mkAggregations "users" $ terms hint
  forM_ hints $ searchExpectAggs . search

-- forM_ hints (\x -> searchValidBucketAgg (search x) "users" toTerms)

searchTweetHighlight ::
  Search ->
  BH IO (Either EsError (Maybe HitHighlight))
searchTweetHighlight search = do
  result <- searchTweets search
  let tweetHit :: Either EsError (Maybe (Hit Tweet))
      tweetHit = fmap (headMay . hits . searchHits) result
      myHighlight :: Either EsError (Maybe HitHighlight)
      myHighlight = (join . fmap hitHighlight) <$> tweetHit
  return myHighlight

searchExpectSource :: Source -> Either EsError Value -> BH IO ()
searchExpectSource src expected = do
  _ <- insertData
  let query = QueryMatchQuery $ mkMatchQuery (FieldName "message") (QueryString "haskell")
  let search = (mkSearch (Just query) Nothing) {source = Just src}
  result <- performBHRequest $ searchByIndex testIndex search
  let value_ = grabFirst result
  liftIO $
    value_ `shouldBe` expected

is :: Versions.Version -> IO Bool
is v = getServerVersion >>= \x -> return $ x == v

esOnlyIT :: (HasCallStack, Example a) => IO (String -> a -> SpecWith (Arg a))
esOnlyIT = withMajorVersionIT (>= 6)

os2OnlyIT :: (HasCallStack, Example a) => IO (String -> a -> SpecWith (Arg a))
os2OnlyIT = withMajorVersionIT (== 2)

withMajorVersionIT :: (HasCallStack, Example a) => (Word -> Bool) -> IO (String -> a -> SpecWith (Arg a))
withMajorVersionIT p = do
  majoreVersion <- fetchMajorVersion
  return $
    if p majoreVersion
      then it
      else xit

fetchMajorVersion :: IO Word
fetchMajorVersion =
  withTestEnv $ do
    x <- performBHRequest $ getNodesInfo LocalNode
    let majoreVersion = versionNumber $ nodeInfoESVersion $ head $ nodesInfo x
    return $ head $ toListOf Versions.major majoreVersion
