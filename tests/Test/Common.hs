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
testMapping :: MappingName
testMapping = MappingName "tweet"

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

data ParentMapping = ParentMapping deriving (Eq, Show)

instance ToJSON ParentMapping where
  toJSON ParentMapping =
    object ["properties" .=
      object [ "user"     .= object ["type"    .= ("string" :: Text)
                                    , "fielddata" .= True
                                    ]
            -- Serializing the date as a date is breaking other tests, mysteriously.
            -- , "postDate" .= object [ "type"   .= ("date" :: Text)
            --                        , "format" .= ("YYYY-MM-dd`T`HH:mm:ss.SSSZZ" :: Text)]
            , "message"  .= object ["type" .= ("string" :: Text)]
            , "age"      .= object ["type" .= ("integer" :: Text)]
            , "location" .= object ["type" .= ("geo_point" :: Text)]
            , "extra"    .= object ["type" .= ("keyword" :: Text)]
            ]]

es11 :: SemVer.Version
es11 = SemVer.version 1 1 0 [] []

es13 :: SemVer.Version
es13 = SemVer.version 1 3 0 [] []

es12 :: SemVer.Version
es12 = SemVer.version 1 2 0 [] []

es14 :: SemVer.Version
es14 = SemVer.version 1 4 0 [] []

es15 :: SemVer.Version
es15 = SemVer.version 1 5 0 [] []

es16 :: SemVer.Version
es16 = SemVer.version 1 6 0 [] []

es20 :: SemVer.Version
es20 = SemVer.version 2 0 0 [] []

es50 :: SemVer.Version
es50 = SemVer.version 5 0 0 [] []

getServerVersion :: IO (Maybe SemVer.Version)
getServerVersion = fmap extractVersion <$> withTestEnv getStatus
  where
    extractVersion              = versionNumber . number . version

createExampleIndex :: (MonadBH m) => m Reply
createExampleIndex =
  createIndex (IndexSettings (ShardCount 1) (ReplicaCount 0)) testIndex

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

data ChildMapping = ChildMapping deriving (Eq, Show)

instance ToJSON ChildMapping where
  toJSON ChildMapping =
    object ["_parent" .= object ["type" .= ("parent" :: Text)]
           , "properties" .=
                object [ "user"     .= object ["type"    .= ("string" :: Text)
                                              , "fielddata" .= True
                                              ]
                  -- Serializing the date as a date is breaking other tests, mysteriously.
                  -- , "postDate" .= object [ "type"   .= ("date" :: Text)
                  --                        , "format" .= ("YYYY-MM-dd`T`HH:mm:ss.SSSZZ" :: Text)]
                  , "message"  .= object ["type" .= ("string" :: Text)]
                  , "age"      .= object ["type" .= ("integer" :: Text)]
                  , "location" .= object ["type" .= ("geo_point" :: Text)]
                  , "extra"    .= object ["type" .= ("keyword" :: Text)]
                  ]]

data TweetMapping = TweetMapping deriving (Eq, Show)

instance ToJSON TweetMapping where
  toJSON TweetMapping =
    object ["tweet" .=
      object ["properties" .=
        object [ "user"     .= object [ "type"    .= ("string" :: Text)
                                      , "fielddata" .= True
                                      ]
               -- Serializing the date as a date is breaking other tests, mysteriously.
               -- , "postDate" .= object [ "type"   .= ("date" :: Text)
               --                        , "format" .= ("YYYY-MM-dd`T`HH:mm:ss.SSSZZ" :: Text)]
               , "message"  .= object ["type" .= ("string" :: Text)]
               , "age"      .= object ["type" .= ("integer" :: Text)]
               , "location" .= object ["type" .= ("geo_point" :: Text)]
               , "extra"    .= object ["type" .= ("keyword" :: Text)]
               ]]]

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
  _ <- putMapping testIndex testMapping TweetMapping
  return ()

insertData :: BH IO Reply
insertData = do
  resetIndex
  insertData' defaultIndexDocumentSettings

insertData' :: IndexDocumentSettings -> BH IO Reply
insertData' ids = do
  r <- indexDocument testIndex testMapping ids exampleTweet (DocId "1")
  _ <- refreshIndex testIndex
  return r

updateData :: BH IO Reply
updateData = do
  r <- updateDocument testIndex testMapping defaultIndexDocumentSettings tweetPatch (DocId "1")
  _ <- refreshIndex testIndex
  return r

insertOther :: BH IO ()
insertOther = do
  _ <- indexDocument testIndex testMapping defaultIndexDocumentSettings otherTweet (DocId "2")
  _ <- refreshIndex testIndex
  return ()

insertExtra :: BH IO ()
insertExtra = do
  _ <- indexDocument testIndex testMapping defaultIndexDocumentSettings tweetWithExtra (DocId "4")
  _ <- refreshIndex testIndex
  return ()

insertWithSpaceInId :: BH IO ()
insertWithSpaceInId = do
  _ <- indexDocument testIndex testMapping defaultIndexDocumentSettings exampleTweet (DocId "Hello World")
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
                        Search -> Text -> (Text -> AggregationResults -> Maybe (Bucket a)) -> BH IO ()
searchValidBucketAgg search aggKey extractor = do
  reply <- searchByIndex testIndex search
  let bucketDocs = docCount . head . buckets
  let result = decode (responseBody reply) :: Maybe (SearchResult Tweet)
  let count = result >>= aggregations >>= extractor aggKey >>= \x -> return (bucketDocs x)
  liftIO $
    count `shouldBe` Just 1

searchTermsAggHint :: [ExecutionHint] -> BH IO ()
searchTermsAggHint hints = do
      let terms hint = TermsAgg $ (mkTermsAggregation "user") { termExecutionHint = Just hint }
      let search hint = mkAggregateSearch Nothing $ mkAggregations "users" $ terms hint
      forM_ hints $ searchExpectAggs . search
      forM_ hints (\x -> searchValidBucketAgg (search x) "users" toTerms)

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
  let query = QueryMatchQuery $ mkMatchQuery (FieldName "_all") (QueryString "haskell")
  let search = (mkSearch (Just query) Nothing) { source = Just src }
  reply <- searchByIndex testIndex search
  result <- parseEsResponse reply
  let value = grabFirst result
  liftIO $
    value `shouldBe` expected

atleast :: SemVer.Version -> IO Bool
atleast v = getServerVersion >>= \x -> return $ x >= Just v

atmost :: SemVer.Version -> IO Bool
atmost v = getServerVersion >>= \x -> return $ x <= Just v

is :: SemVer.Version -> IO Bool
is v = getServerVersion >>= \x -> return $ x == Just v
