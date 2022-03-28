{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Test.Common where

import Test.Import

import qualified Data.Map as M
import qualified Data.SemVer as SemVer
import qualified Network.HTTP.Types.Status as NHTS

testServer  :: Server
testServer  = Server "http://localhost:9200"
testIndex   :: IndexName
testIndex   = IndexName "bloodhound-tests-twitter-1"

withTestEnv :: BH IO a -> IO a
withTestEnv = withBH defaultManagerSettings testServer

data Location = Location { lat :: Double
                         , lon :: Double } deriving (Eq, Show)

data Tweet = Tweet { user     :: Text
                   , postDate :: UTCTime
                   , message  :: Text
                   , age      :: Int
                   , location :: Location
                   , extra    :: Maybe Text }
           deriving (Eq, Show)

$(deriveJSON defaultOptions ''Location)
$(deriveJSON defaultOptions ''Tweet)

data ConversationMapping = ConversationMapping deriving (Eq, Show)

instance ToJSON ConversationMapping where
  toJSON ConversationMapping =
    object ["properties" .=
      object ["reply_join"  .= object [ "type"       .= ("join" :: Text)
                                      , "relations"  .= object [ "message" .= ("reply" :: Text) ]
                                      ]
            , "user"        .= object [ "type"       .= ("text" :: Text)
                                      , "fielddata"  .= True
                                      ]
            -- Serializing the date as a date is breaking other tests, mysteriously.
            -- , "postDate" .= object [ "type"   .= ("date" :: Text)
            --                        , "format" .= ("YYYY-MM-dd`T`HH:mm:ss.SSSZZ" :: Text)]
            , "message"  .= object ["type" .= ("text" :: Text)]
            , "age"      .= object ["type" .= ("integer" :: Text)]
            , "location" .= object ["type" .= ("geo_point" :: Text)]
            , "extra"    .= object ["type" .= ("keyword" :: Text)]
            ]]

getServerVersion :: IO (Maybe SemVer.Version)
getServerVersion = fmap extractVersion <$> withTestEnv getStatus
  where
    extractVersion              = versionNumber . number . version

createExampleIndex :: (MonadBH m) => m Reply
createExampleIndex =
  createIndex (IndexSettings (ShardCount 1) (ReplicaCount 0) defaultIndexMappingsLimits) testIndex

deleteExampleIndex :: (MonadBH m) => m Reply
deleteExampleIndex =
  deleteIndex testIndex

validateStatus :: Show body => Response body -> Int -> Expectation
validateStatus resp expected =
  if actual == expected
    then return ()
    else expectationFailure ("Expected " <> show expected <> " but got " <> show actual <> ": " <> show body)
  where
    actual = NHTS.statusCode (responseStatus resp)
    body = responseBody resp

data TweetMapping = TweetMapping deriving (Eq, Show)

instance ToJSON TweetMapping where
  toJSON TweetMapping =
    object ["properties" .=
      object [ "user"     .= object [ "type"    .= ("text" :: Text)
                                    , "fielddata" .= True
                                    ]
              -- Serializing the date as a date is breaking other tests, mysteriously.
              -- , "postDate" .= object [ "type"   .= ("date" :: Text)
              --                        , "format" .= ("YYYY-MM-dd`T`HH:mm:ss.SSSZZ" :: Text)]
              , "message"  .= object ["type" .= ("text" :: Text)]
              , "age"      .= object ["type" .= ("integer" :: Text)]
              , "location" .= object ["type" .= ("geo_point" :: Text)]
              , "extra"    .= object ["type" .= ("keyword" :: Text)]
              ]]

exampleTweet :: Tweet
exampleTweet = Tweet { user     = "bitemyapp"
                     , postDate = UTCTime
                                  (ModifiedJulianDay 55000)
                                  (secondsToDiffTime 10)
                     , message  = "Use haskell!"
                     , age      = 10000
                     , location = Location 40.12 (-71.34)
                     , extra = Nothing }

tweetWithExtra :: Tweet
tweetWithExtra = Tweet { user     = "bitemyapp"
                       , postDate = UTCTime
                                    (ModifiedJulianDay 55000)
                                    (secondsToDiffTime 10)
                       , message  = "Use haskell!"
                       , age      = 10000
                       , location = Location 40.12 (-71.34)
                       , extra = Just "blah blah" }

exampleTweetWithAge :: Int -> Tweet
exampleTweetWithAge age = Tweet { user     = "bitemyapp"
                                , postDate = UTCTime
                                            (ModifiedJulianDay 55000)
                                            (secondsToDiffTime 10)
                                , message  = "Use haskell!"
                                , age      = age
                                , location = Location 40.12 (-71.34)
                                , extra = Nothing }

newAge :: Int
newAge = 31337

newUser :: Text
newUser = "someotherapp"

tweetPatch :: Value
tweetPatch =
  object [ "age" .= newAge
         , "user" .= newUser
         ]

patchedTweet :: Tweet
patchedTweet = exampleTweet{age = newAge, user = newUser}

otherTweet :: Tweet
otherTweet = Tweet { user     = "notmyapp"
                   , postDate = UTCTime
                                (ModifiedJulianDay 55000)
                                (secondsToDiffTime 11)
                   , message  = "Use haskell!"
                   , age      = 1000
                   , location = Location 40.12 (-71.34)
                   , extra = Nothing }

resetIndex :: BH IO ()
resetIndex = do
  _ <- deleteExampleIndex
  _ <- createExampleIndex
  _ <- putMapping testIndex TweetMapping
  return ()

insertData :: BH IO Reply
insertData = do
  resetIndex
  insertData' defaultIndexDocumentSettings

insertData' :: IndexDocumentSettings -> BH IO Reply
insertData' ids = do
  r <- indexDocument testIndex ids exampleTweet (DocId "1")
  _ <- refreshIndex testIndex
  return r

insertTweetWithDocId :: Tweet -> Text -> BH IO Reply
insertTweetWithDocId tweet docId = do
  let ids = defaultIndexDocumentSettings
  r <- indexDocument testIndex ids tweet (DocId docId)
  _ <- refreshIndex testIndex
  return r

updateData :: BH IO Reply
updateData = do
  r <- updateDocument testIndex defaultIndexDocumentSettings tweetPatch (DocId "1")
  _ <- refreshIndex testIndex
  return r

insertOther :: BH IO ()
insertOther = do
  _ <- indexDocument testIndex defaultIndexDocumentSettings otherTweet (DocId "2")
  _ <- refreshIndex testIndex
  return ()

insertExtra :: BH IO ()
insertExtra = do
  _ <- indexDocument testIndex defaultIndexDocumentSettings tweetWithExtra (DocId "4")
  _ <- refreshIndex testIndex
  return ()

insertWithSpaceInId :: BH IO ()
insertWithSpaceInId = do
  _ <- indexDocument testIndex defaultIndexDocumentSettings exampleTweet (DocId "Hello World")
  _ <- refreshIndex testIndex
  return ()

searchTweet :: Search -> BH IO (Either EsError Tweet)
searchTweet search = do
  result <- searchTweets search
  let myTweet :: Either EsError Tweet
      myTweet = grabFirst result
  return myTweet

searchTweets :: Search -> BH IO (Either EsError (SearchResult Tweet))
searchTweets search = parseEsResponse =<< searchByIndex testIndex search

searchExpectNoResults :: Search -> BH IO ()
searchExpectNoResults search = do
  result <- searchTweets search
  let emptyHits = fmap (hits . searchHits) result
  liftIO $
    emptyHits `shouldBe` Right []

searchExpectAggs :: Search -> BH IO ()
searchExpectAggs search = do
  reply <- searchByIndex testIndex search
  let isEmpty x = return (M.null x)
  let result = decode (responseBody reply) :: Maybe (SearchResult Tweet)
  liftIO $
    (result >>= aggregations >>= isEmpty) `shouldBe` Just False

searchValidBucketAgg :: (BucketAggregation a, FromJSON a, Show a) =>
                        Search -> Key -> (Key -> AggregationResults -> Maybe (Bucket a)) -> BH IO ()
searchValidBucketAgg search aggKey extractor = do
  reply <- searchByIndex testIndex search
  let bucketDocs = docCount . head . buckets
  let result = decode (responseBody reply) :: Maybe (SearchResult Tweet)
  let count = result >>= aggregations >>= extractor aggKey >>= \x -> return (bucketDocs x)
  liftIO $
    count `shouldBe` Just 1

searchTermsAggHint :: [ExecutionHint] -> BH IO ()
searchTermsAggHint hints = do
      let terms hint = TermsAgg $ (mkTermsAggregation "user.keyword") { termExecutionHint = Just hint }
      let search hint = mkAggregateSearch Nothing $ mkAggregations "users" $ terms hint
      forM_ hints $ searchExpectAggs . search
      -- forM_ hints (\x -> searchValidBucketAgg (search x) "users" toTerms)

searchTweetHighlight :: Search
                     -> BH IO (Either EsError (Maybe HitHighlight))
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
  let search = (mkSearch (Just query) Nothing) { source = Just src }
  reply <- searchByIndex testIndex search
  result <- parseEsResponse reply
  let value_ = grabFirst result
  liftIO $
    value_ `shouldBe` expected

atleast :: SemVer.Version -> IO Bool
atleast v = getServerVersion >>= \x -> return $ x >= Just v

atmost :: SemVer.Version -> IO Bool
atmost v = getServerVersion >>= \x -> return $ x <= Just v

is :: SemVer.Version -> IO Bool
is v = getServerVersion >>= \x -> return $ x == Just v
