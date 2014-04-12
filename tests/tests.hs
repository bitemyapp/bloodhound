{-# LANGUAGE DeriveGeneric #-}

module Main where

import Database.Bloodhound.Client
import Data.Aeson
import Data.DeriveTH
import Data.Either (Either(..))
import Data.Maybe (fromJust)
import Data.Time.Calendar (Day(..))
import Data.Time.Clock (secondsToDiffTime, UTCTime(..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Conduit
import qualified Network.HTTP.Types.Status as NHTS
import Test.Hspec

testServer  = Server "http://localhost:9200"
testIndex   = IndexName "twitter"
testMapping = MappingName "tweet"

validateStatus resp expected =
  (NHTS.statusCode $ responseStatus resp)
  `shouldBe` (expected :: Int)

createExampleIndex = createIndex testServer defaultIndexSettings testIndex
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

exampleTweet = Tweet { user     = "bitemyapp"
                     , postDate = UTCTime
                                  (ModifiedJulianDay 55000)
                                  (secondsToDiffTime 10)
                     , message  = "Use haskell!"
                     , age      = 10000
                     , location = Location 40.12 (-71.34) }

insertData :: IO ()
insertData = do
  let encoded = encode exampleTweet
  _ <- deleteExampleIndex
  created <- createExampleIndex
  mappingCreated <- createMapping testServer testIndex testMapping TweetMapping
  docCreated <- indexDocument testServer testIndex testMapping exampleTweet (DocId "1")
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

data BulkTest = BulkTest { name :: Text } deriving (Eq, Generic, Show)
instance FromJSON BulkTest
instance ToJSON BulkTest

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
      let firstDoc = BulkIndex (IndexName "twitter")
                     (MappingName "tweet") (DocId "2") (object ["name" .= String "blah"])
      let secondDoc = BulkCreate (IndexName "twitter")
                     (MappingName "tweet") (DocId "3") (object ["name" .= String "bloo"])
      let stream = [firstDoc, secondDoc]
      response <- bulk testServer stream
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

  describe "filtering API" $ do
    it "returns document for composed boolmatch and identity" $ do
      _ <- insertData
      let queryFilter = BoolFilter (MustMatch (Term "user" "bitemyapp") False)
                        <&&> IdentityFilter
      let search = mkSearch Nothing (Just queryFilter)
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
      let bbConstraint = GeoBoundingBoxConstraint (FieldName "tweet.location") box False
      let geoFilter = GeoBoundingBoxFilter bbConstraint GeoFilterMemory
      let search = mkSearch Nothing (Just geoFilter)
      myTweet <- searchTweet search
      myTweet `shouldBe` Right exampleTweet

    it "doesn't return document for nonsensical boundingbox filter" $ do
      _ <- insertData
      let box          = GeoBoundingBox (LatLon 0.73 (-4.1)) (LatLon 0.10 (-1.12))
      let bbConstraint = GeoBoundingBoxConstraint (FieldName "tweet.location") box False
      let geoFilter    = GeoBoundingBoxFilter bbConstraint GeoFilterMemory
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
                   RegexpAll (CacheName "test") False (CacheKey "key")
      let search = mkSearch Nothing (Just filter)
      myTweet <- searchTweet search
      myTweet `shouldBe` Right exampleTweet

    it "doesn't return document for non-matching regexp filter" $ do
      _ <- insertData
      let filter = RegexpFilter (FieldName "user")
                   (Regexp "boy") RegexpAll
                   (CacheName "test") False (CacheKey "key")
      let search = mkSearch Nothing (Just filter)
      searchExpectNoResults search
