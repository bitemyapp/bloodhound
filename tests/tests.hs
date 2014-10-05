{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative
import Database.Bloodhound
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.List (nub, elemIndex)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Time.Calendar (Day(..))
import Data.Time.Clock (secondsToDiffTime, UTCTime(..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.HTTP.Client
import qualified Network.HTTP.Types.Status as NHTS
import Prelude hiding (filter, putStrLn)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import qualified Data.Map.Strict as M

testServer  :: Server
testServer  = Server "http://localhost:9200"
testIndex   :: IndexName
testIndex   = IndexName "bloodhound-tests-twitter-1"
testMapping :: MappingName
testMapping = MappingName "tweet"

validateStatus :: Response body -> Int -> Expectation
validateStatus resp expected =
  (NHTS.statusCode $ responseStatus resp)
  `shouldBe` (expected :: Int)

createExampleIndex :: IO Reply
createExampleIndex = createIndex testServer defaultIndexSettings testIndex
deleteExampleIndex :: IO Reply
deleteExampleIndex = deleteIndex testServer testIndex

data Location = Location { lat :: Double
                         , lon :: Double } deriving (Eq, Generic, Show)

data Tweet = Tweet { user     :: Text
                   , postDate :: UTCTime
                   , message  :: Text
                   , age      :: Int
                   , location :: Location }
           deriving (Eq, Generic, Show)

instance ToJSON   Tweet
instance FromJSON Tweet
instance ToJSON   Location
instance FromJSON Location

data TweetMapping = TweetMapping deriving (Eq, Show)

instance ToJSON TweetMapping where
  toJSON TweetMapping =
    object ["tweet" .=
      object ["properties" .=
        object ["location" .= object ["type" .= ("geo_point" :: Text)]]]]

exampleTweet :: Tweet
exampleTweet = Tweet { user     = "bitemyapp"
                     , postDate = UTCTime
                                  (ModifiedJulianDay 55000)
                                  (secondsToDiffTime 10)
                     , message  = "Use haskell!"
                     , age      = 10000
                     , location = Location 40.12 (-71.34) }

otherTweet :: Tweet
otherTweet = Tweet { user     = "notmyapp"
                   , postDate = UTCTime
                                (ModifiedJulianDay 55000)
                                (secondsToDiffTime 11)
                   , message  = "Use haskell!"
                   , age      = 1000
                   , location = Location 40.12 (-71.34) }

insertData :: IO ()
insertData = do
  _ <- deleteExampleIndex
  _ <- createExampleIndex
  _ <- putMapping testServer testIndex testMapping TweetMapping
  _ <- indexDocument testServer testIndex testMapping exampleTweet (DocId "1")
  _ <- refreshIndex testServer testIndex
  return ()

insertOther :: IO ()
insertOther = do
  _ <- indexDocument testServer testIndex testMapping otherTweet (DocId "2")
  _ <- refreshIndex testServer testIndex
  return ()

searchTweet :: Search -> IO (Either String Tweet)
searchTweet search = do
  reply <- searchByIndex testServer testIndex search
  let result = eitherDecode (responseBody reply) :: Either String (SearchResult Tweet)
  let myTweet = fmap (hitSource . head . hits . searchHits) result
  return myTweet

searchExpectNoResults :: Search -> IO ()
searchExpectNoResults search = do
  reply <- searchByIndex testServer testIndex search
  let result = eitherDecode (responseBody reply) :: Either String (SearchResult Tweet)
  let emptyHits = fmap (hits . searchHits) result
  emptyHits `shouldBe` Right []

searchExpectAggs :: Search -> IO ()
searchExpectAggs search = do
  reply <- searchAll testServer search
  let isEmpty x = return (M.null x) 
  let result = decode (responseBody reply) :: Maybe (SearchResult Tweet)
  (result >>= aggregations >>= isEmpty) `shouldBe` Just False
  
searchValidBucketAgg :: (BucketAggregation a, FromJSON a, Show a) => Search -> Text -> (Text -> AggregationResults -> Maybe (Bucket a)) -> IO ()
searchValidBucketAgg search aggKey extractor = do
  reply <- searchAll testServer search
  let bucketDocs = docCount . head . buckets
  let result = decode (responseBody reply) :: Maybe (SearchResult Tweet)
  let count = result >>= aggregations >>= extractor aggKey >>= \x -> return (bucketDocs x)
  count `shouldBe` Just 1

data BulkTest = BulkTest { name :: Text } deriving (Eq, Generic, Show)
instance FromJSON BulkTest
instance ToJSON BulkTest

noDuplicates :: Eq a => [a] -> Bool
noDuplicates xs = nub xs == xs

instance Arbitrary RegexpFlags where
  arbitrary = oneof [ pure AllRegexpFlags
                    , pure NoRegexpFlags
                    , SomeRegexpFlags <$> arbitrary
                    ]

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = liftA2 (:|) arbitrary arbitrary

instance Arbitrary RegexpFlag where
  arbitrary = oneof [ pure AnyString
                    , pure Automaton
                    , pure Complement
                    , pure Empty
                    , pure Intersection
                    , pure Interval
                    ]

main :: IO ()
main = hspec $ do

  describe "index create/delete API" $ do
    it "creates and then deletes the requested index" $ do
      -- priming state.
      _ <- deleteExampleIndex
      resp <- createExampleIndex
      deleteResp <- deleteExampleIndex
      validateStatus resp 200
      validateStatus deleteResp 200


  describe "document API" $ do
    it "indexes, gets, and then deletes the generated document" $ do
      _ <- insertData
      docInserted <- getDocument testServer testIndex testMapping (DocId "1")
      let newTweet = eitherDecode
                     (responseBody docInserted) :: Either String (EsResult Tweet)
      fmap _source newTweet `shouldBe` Right exampleTweet


  describe "bulk API" $ do
    it "inserts all documents we request" $ do
      _ <- insertData
      let firstTest = BulkTest "blah"
      let secondTest = BulkTest "bloo"
      let firstDoc = BulkIndex testIndex
                     testMapping (DocId "2") (toJSON firstTest)
      let secondDoc = BulkCreate testIndex
                     testMapping (DocId "3") (toJSON secondTest)
      let stream = [firstDoc, secondDoc]
      _ <- bulk testServer stream
      _ <- refreshIndex testServer testIndex
      fDoc <- getDocument testServer testIndex testMapping (DocId "2")
      sDoc <- getDocument testServer testIndex testMapping (DocId "3")
      let maybeFirst  = eitherDecode $ responseBody fDoc :: Either String (EsResult BulkTest)
      let maybeSecond = eitherDecode $ responseBody sDoc :: Either String (EsResult BulkTest)
      fmap _source maybeFirst `shouldBe` Right firstTest
      fmap _source maybeSecond `shouldBe` Right secondTest


  describe "query API" $ do
    it "returns document for term query and identity filter" $ do
      _ <- insertData
      let query = TermQuery (Term "user" "bitemyapp") Nothing
      let filter = IdentityFilter <&&> IdentityFilter
      let search = mkSearch (Just query) (Just filter)
      myTweet <- searchTweet search
      myTweet `shouldBe` Right exampleTweet

    it "returns document for match query" $ do
      _ <- insertData
      let query = QueryMatchQuery $ mkMatchQuery (FieldName "user") (QueryString "bitemyapp")
      let search = mkSearch (Just query) Nothing
      myTweet <- searchTweet search
      myTweet `shouldBe` Right exampleTweet

    it "returns document for multi-match query" $ do
      _ <- insertData
      let fields = [FieldName "user", FieldName "message"]
      let query = QueryMultiMatchQuery $ mkMultiMatchQuery fields (QueryString "bitemyapp")
      let search = mkSearch (Just query) Nothing
      myTweet <- searchTweet search
      myTweet `shouldBe` Right exampleTweet

    it "returns document for bool query" $ do
      _ <- insertData
      let innerQuery = QueryMatchQuery $
                       mkMatchQuery (FieldName "user") (QueryString "bitemyapp")
      let query = QueryBoolQuery $
                  mkBoolQuery (Just innerQuery) Nothing Nothing
      let search = mkSearch (Just query) Nothing
      myTweet <- searchTweet search
      myTweet `shouldBe` Right exampleTweet

    it "returns document for boosting query" $ do
      _ <- insertData
      let posQuery = QueryMatchQuery $ mkMatchQuery (FieldName "user") (QueryString "bitemyapp")
      let negQuery = QueryMatchQuery $ mkMatchQuery (FieldName "user") (QueryString "notmyapp")
      let query = QueryBoostingQuery $ BoostingQuery posQuery negQuery (Boost 0.2)
      let search = mkSearch (Just query) Nothing
      myTweet <- searchTweet search
      myTweet `shouldBe` Right exampleTweet

    it "returns document for common terms query" $ do
      _ <- insertData
      let query = QueryCommonTermsQuery $
                  CommonTermsQuery (FieldName "user")
                  (QueryString "bitemyapp")
                  (CutoffFrequency 0.0001)
                  Or Or Nothing Nothing Nothing Nothing
      let search = mkSearch (Just query) Nothing
      myTweet <- searchTweet search
      myTweet `shouldBe` Right exampleTweet


  describe "sorting" $ do
    it "returns documents in the right order" $ do
      _ <- insertData
      _ <- insertOther
      let sortSpec = DefaultSortSpec $ mkSort (FieldName "age") Ascending
      let search = Search Nothing
                   (Just IdentityFilter) (Just [sortSpec]) Nothing
                   False 0 10
      reply <- searchByIndex testServer testIndex search
      let result = eitherDecode (responseBody reply) :: Either String (SearchResult Tweet)
      let myTweet = fmap (hitSource . head . hits . searchHits) result
      myTweet `shouldBe` Right otherTweet


  describe "filtering API" $ do
    it "returns document for composed boolmatch and identity" $ do
      _ <- insertData
      let queryFilter = BoolFilter (MustMatch (Term "user" "bitemyapp") False)
                        <&&> IdentityFilter
      let search = mkSearch Nothing (Just queryFilter)
      myTweet <- searchTweet search
      myTweet `shouldBe` Right exampleTweet

    it "returns document for term filter" $ do
      _ <- insertData
      let termFilter = TermFilter (Term "user" "bitemyapp") False
      let search = mkSearch Nothing (Just termFilter)
      myTweet <- searchTweet search
      myTweet `shouldBe` Right exampleTweet

    it "returns document for existential filter" $ do
      _ <- insertData
      let search = mkSearch Nothing (Just (ExistsFilter (FieldName "user")))
      myTweet <- searchTweet search
      myTweet `shouldBe` Right exampleTweet

    it "returns document for geo boundingbox filter" $ do
      _ <- insertData
      let box = GeoBoundingBox (LatLon 40.73 (-74.1)) (LatLon 40.10 (-71.12))
      let bbConstraint = GeoBoundingBoxConstraint (FieldName "tweet.location") box False GeoFilterMemory
      let geoFilter = GeoBoundingBoxFilter bbConstraint
      let search = mkSearch Nothing (Just geoFilter)
      myTweet <- searchTweet search
      myTweet `shouldBe` Right exampleTweet

    it "doesn't return document for nonsensical boundingbox filter" $ do
      _ <- insertData
      let box          = GeoBoundingBox (LatLon 0.73 (-4.1)) (LatLon 0.10 (-1.12))
      let bbConstraint = GeoBoundingBoxConstraint (FieldName "tweet.location") box False GeoFilterMemory
      let geoFilter    = GeoBoundingBoxFilter bbConstraint
      let search       = mkSearch Nothing (Just geoFilter)
      searchExpectNoResults search

    it "returns document for geo distance filter" $ do
      _ <- insertData
      let geoPoint = GeoPoint (FieldName "tweet.location") (LatLon 40.12 (-71.34))
      let distance = Distance 10.0 Miles
      let optimizeBbox = OptimizeGeoFilterType GeoFilterMemory
      let geoFilter = GeoDistanceFilter geoPoint distance SloppyArc optimizeBbox False
      let search = mkSearch Nothing (Just geoFilter)
      myTweet <- searchTweet search
      myTweet `shouldBe` Right exampleTweet

    it "returns document for geo distance range filter" $ do
      _ <- insertData
      let geoPoint = GeoPoint (FieldName "tweet.location") (LatLon 40.12 (-71.34))
      let distanceRange = DistanceRange (Distance 0.0 Miles) (Distance 10.0 Miles)
      let geoFilter = GeoDistanceRangeFilter geoPoint distanceRange
      let search = mkSearch Nothing (Just geoFilter)
      myTweet <- searchTweet search
      myTweet `shouldBe` Right exampleTweet

    it "doesn't return document for wild geo distance range filter" $ do
      _ <- insertData
      let geoPoint = GeoPoint (FieldName "tweet.location") (LatLon 40.12 (-71.34))
      let distanceRange = DistanceRange (Distance 100.0 Miles) (Distance 1000.0 Miles)
      let geoFilter = GeoDistanceRangeFilter geoPoint distanceRange
      let search = mkSearch Nothing (Just geoFilter)
      searchExpectNoResults search

    it "returns document for geo polygon filter" $ do
      _ <- insertData
      let points = [LatLon 40.0 (-70.00),
                    LatLon 40.0 (-72.00),
                    LatLon 41.0 (-70.00),
                    LatLon 41.0 (-72.00)]
      let geoFilter = GeoPolygonFilter (FieldName "tweet.location") points
      let search = mkSearch Nothing (Just geoFilter)
      myTweet <- searchTweet search
      myTweet `shouldBe` Right exampleTweet

    it "doesn't return document for bad geo polygon filter" $ do
      _ <- insertData
      let points = [LatLon 40.0 (-70.00),
                    LatLon 40.0 (-71.00),
                    LatLon 41.0 (-70.00),
                    LatLon 41.0 (-71.00)]
      let geoFilter = GeoPolygonFilter (FieldName "tweet.location") points
      let search = mkSearch Nothing (Just geoFilter)
      searchExpectNoResults search

    it "returns document for ids filter" $ do
      _ <- insertData
      let filter = IdsFilter (MappingName "tweet") [DocId "1"]
      let search = mkSearch Nothing (Just filter)
      myTweet <- searchTweet search
      myTweet `shouldBe` Right exampleTweet

    it "returns document for range filter" $ do
      _ <- insertData
      let filter = RangeFilter (FieldName "age")
                   (Right (RangeLtGt (LessThan 100000.0) (GreaterThan 1000.0)))
                   RangeExecutionIndex False
      let search = mkSearch Nothing (Just filter)
      myTweet <- searchTweet search
      myTweet `shouldBe` Right exampleTweet

    it "returns document for regexp filter" $ do
      _ <- insertData
      let filter = RegexpFilter (FieldName "user") (Regexp "bite.*app")
                   AllRegexpFlags (CacheName "test") False (CacheKey "key")
      let search = mkSearch Nothing (Just filter)
      myTweet <- searchTweet search
      myTweet `shouldBe` Right exampleTweet

    it "doesn't return document for non-matching regexp filter" $ do
      _ <- insertData
      let filter = RegexpFilter (FieldName "user")
                   (Regexp "boy") AllRegexpFlags
                   (CacheName "test") False (CacheKey "key")
      let search = mkSearch Nothing (Just filter)
      searchExpectNoResults search

  describe "Aggregation API" $ do
    it "returns term aggregation results" $ do
      _ <- insertData
      let aggs = M.insert "users" (mkTermsAggregation "user") M.empty
      let search = (mkSearch Nothing Nothing) {aggBody = Just(aggs)}
      searchExpectAggs search
      searchValidBucketAgg search "users" toTerms
    
    it "returns date histogram aggregation results" $ do
      _ <- insertData
      let aggs = M.insert "byDate" (mkDateHistogram (FieldName "postDate") Minute) M.empty
      let search = (mkSearch Nothing Nothing) {aggBody = Just(aggs)}
      searchExpectAggs search
      searchValidBucketAgg search "byDate" toDateHistogram


  describe "ToJSON RegexpFlags" $ do
    it "generates the correct JSON for AllRegexpFlags" $
      toJSON AllRegexpFlags `shouldBe` String "ALL"

    it "generates the correct JSON for NoRegexpFlags" $
      toJSON NoRegexpFlags `shouldBe` String "NONE"

    it "generates the correct JSON for SomeRegexpFlags" $
      let flags = AnyString :| [ Automaton
                               , Complement
                               , Empty
                               , Intersection
                               , Interval ]
      in toJSON (SomeRegexpFlags flags) `shouldBe` String "ANYSTRING|AUTOMATON|COMPLEMENT|EMPTY|INTERSECTION|INTERVAL"

    prop "removes duplicates from flags" $ \(flags :: RegexpFlags) ->
      let String str = toJSON flags
          flagStrs   = T.splitOn "|" str
      in noDuplicates flagStrs
      

