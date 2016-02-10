Bloodhound [![TravisCI](https://travis-ci.org/bitemyapp/bloodhound.svg)](https://travis-ci.org/bitemyapp/bloodhound) [![Hackage](https://img.shields.io/hackage/v/bloodhound.svg?style=flat)](https://hackage.haskell.org/package/bloodhound)
==========

![Bloodhound (dog)](./bloodhound.jpg)

Elasticsearch client and query DSL for Haskell
==============================================

Why?
----

Search doesn't have to be hard. Let the dog do it.

Endorsements
------------

"Bloodhound makes Elasticsearch almost tolerable!" - Almost-gruntled user

"ES is a nightmare but Bloodhound at least makes it tolerable." - Same user, later opinion.

Version compatibility
---------------------

Elasticsearch \>=1.0 && \<2.0 is recommended. Bloodhound mostly works with 0.9.x, but I don't recommend it if you expect everything to work. As of Bloodhound 0.3 all \>=1.0 && \<2.0 versions of Elasticsearch work. Some (or even most?) features will work with versions \>=2.0, but it is not officially supported yet.

Current versions we test against are 1.2.4, 1.3.6, 1.4.1, 1.5.2, 1.6.0, and 1.7.2. We also check that GHC 7.6 and 7.8 both build and pass tests. See our [TravisCI](https://travis-ci.org/bitemyapp/bloodhound) to learn more.

Stability
---------

Bloodhound is stable for production use. I will strive to avoid breaking API compatibility from here on forward, but dramatic features like a type-safe, fully integrated mapping API may require breaking things in the future.

Testing
---------

The TravisCI tests are run using [Stack](http://docs.haskellstack.org/en/stable/README.html). You should use Stack instead of `cabal` to build and test Bloodhound to avoid compatibility problems. You will also need to have an ElasticSearch instance running at `localhost:9200` in order to execute some of the tests. See the "Version compatibility" section above for a list of ElasticSearch versions that are officially validated against in TravisCI.

Steps to run the tests locally:
  1. Dig through the [past releases] (https://www.elastic.co/downloads/past-releases) section of the ElasticSearch download page and install the desired ElasticSearch versions.
  2. Install [Stack] (http://docs.haskellstack.org/en/stable/README.html#how-to-install)
  3. In your local Bloodhound directory, run `stack setup && stack build`
  4. Start the desired version of ElasticSearch at `localhost:9200`, which should be the default.
  5. Run `stack test` in your local Bloodhound directory.
  6. The unit tests will pass if you re-execute `stack test`, but some of the doctests might fail due to existing data in ElasticSearch. If you want to start with a clean slate, stop your ElasticSearch instance, delete the `data/` folder in the ElasticSearch installation, restart ElasticSearch, and re-run `stack test`.


Hackage page and Haddock documentation
======================================

<http://hackage.haskell.org/package/bloodhound>

Elasticsearch Tutorial
======================

It's not using Bloodhound, but if you need an introduction to or overview of Elasticsearch and how to use it, you can use [this screencast](https://vimeo.com/106463167).

Examples
========

Index Operations
----------------

### Create Index

``` {.haskell}

-- Formatted for use in ghci, so there are "let"s in front of the decls.

-- if you see :{ and :}, they're so you can copy-paste
-- the multi-line examples into your ghci REPL.

:set -XDeriveGeneric
:{
import Control.Applicative
import Database.Bloodhound
import Data.Aeson
import Data.Either (Either(..))
import Data.Maybe (fromJust)
import Data.Time.Calendar (Day(..))
import Data.Time.Clock (secondsToDiffTime, UTCTime(..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Client
import qualified Network.HTTP.Types.Status as NHTS

-- no trailing slashes in servers, library handles building the path.
let testServer = (Server "http://localhost:9200")
let testIndex = IndexName "twitter"
let testMapping = MappingName "tweet"
let withBH' = withBH defaultManagerSettings testServer

-- defaultIndexSettings is exported by Database.Bloodhound as well
let defaultIndexSettings = IndexSettings (ShardCount 3) (ReplicaCount 2)

-- createIndex returns MonadBH m => m Reply. You can use withBH for
   one-off commands or you can use runBH to group together commands
   and to pass in your own HTTP manager for pipelining.

-- response :: Reply, Reply is a synonym for Network.HTTP.Conduit.Response
response <- withBH' $ createIndex defaultIndexSettings testIndex
:}

```

### Delete Index

#### Code

``` {.haskell}

-- response :: Reply
response <- withBH' $ deleteIndex testIndex

```

#### Example Response

``` {.haskell}

-- print response if it was a success
Response {responseStatus = Status {statusCode = 200, statusMessage = "OK"}
        , responseVersion = HTTP/1.1
        , responseHeaders = [("Content-Type", "application/json; charset=UTF-8")
                           , ("Content-Length", "21")]
        , responseBody = "{\"acknowledged\":true}"
        , responseCookieJar = CJ {expose = []}
        , responseClose' = ResponseClose}

-- if the index to be deleted didn't exist anyway
Response {responseStatus = Status {statusCode = 404, statusMessage = "Not Found"}
        , responseVersion = HTTP/1.1
        , responseHeaders = [("Content-Type", "application/json; charset=UTF-8")
                           , ("Content-Length","65")]
        , responseBody = "{\"error\":\"IndexMissingException[[twitter] missing]\",\"status\":404}"
        , responseCookieJar = CJ {expose = []}
        , responseClose' = ResponseClose}

```

### Refresh Index

#### Note, you **have** to do this if you expect to read what you just wrote

``` {.haskell}

resp <- withBH' $ refreshIndex testIndex

```

#### Example Response

``` {.haskell}

-- print resp on success
Response {responseStatus = Status {statusCode = 200, statusMessage = "OK"}
        , responseVersion = HTTP/1.1
        , responseHeaders = [("Content-Type", "application/json; charset=UTF-8")
                           , ("Content-Length","50")]
        , responseBody = "{\"_shards\":{\"total\":10,\"successful\":5,\"failed\":0}}"
        , responseCookieJar = CJ {expose = []}
        , responseClose' = ResponseClose}

```

Mapping Operations
------------------

### Create Mapping

``` {.haskell}

-- don't forget imports and the like at the top.

data TweetMapping = TweetMapping deriving (Eq, Show)

-- I know writing the JSON manually sucks.
-- I don't have a proper data type for Mappings yet.
-- Let me know if this is something you need.

:{
instance ToJSON TweetMapping where
  toJSON TweetMapping =
    object ["tweet" .=
      object ["properties" .=
        object ["location" .=
          object ["type" .= ("geo_point" :: Text)]]]]
:}

resp <- withBH' $ putMapping testIndex testMapping TweetMapping

```

### Delete Mapping

``` {.haskell}

resp <- withBH' $ deleteMapping testIndex testMapping

```

Document Operations
-------------------

### Indexing Documents

``` {.haskell}

-- don't forget the imports and derive generic setting for ghci
-- at the beginning of the examples.

:{
data Location = Location { lat :: Double
                         , lon :: Double } deriving (Eq, Generic, Show)

data Tweet = Tweet { user     :: Text
                   , postDate :: UTCTime
                   , message  :: Text
                   , age      :: Int
                   , location :: Location } deriving (Eq, Generic, Show)

exampleTweet = Tweet { user     = "bitemyapp"
                     , postDate = UTCTime
                                  (ModifiedJulianDay 55000)
                                  (secondsToDiffTime 10)
                     , message  = "Use haskell!"
                     , age      = 10000
                     , location = Location 40.12 (-71.34) }

-- automagic (generic) derivation of instances because we're lazy.
instance ToJSON   Tweet
instance FromJSON Tweet
instance ToJSON   Location
instance FromJSON Location
:}

-- Should be able to toJSON and encode the data structures like this:
-- λ> toJSON $ Location 10.0 10.0
-- Object fromList [("lat",Number 10.0),("lon",Number 10.0)]
-- λ> encode $ Location 10.0 10.0
-- "{\"lat\":10,\"lon\":10}"

resp <- withBH' $ indexDocument testIndex testMapping defaultIndexDocumentSettings exampleTweet (DocId "1")

```

#### Example Response

``` {.haskell}

Response {responseStatus =
  Status {statusCode = 200, statusMessage = "OK"}
    , responseVersion = HTTP/1.1, responseHeaders =
    [("Content-Type","application/json; charset=UTF-8"),
     ("Content-Length","75")]
    , responseBody = "{\"_index\":\"twitter\",\"_type\":\"tweet\",\"_id\":\"1\",\"_version\":2,\"created\":false}"
    , responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}

```

### Deleting Documents

``` {.haskell}

resp <- withBH' $ deleteDocument testIndex testMapping (DocId "1")

```

### Getting Documents

``` {.haskell}

-- n.b., you'll need the earlier imports. responseBody is from http-conduit

resp <- withBH' $ getDocument testIndex testMapping (DocId "1")

-- responseBody :: Response body -> body
let body = responseBody resp

-- you have two options, you use decode and just get Maybe (EsResult Tweet)
-- or you can use eitherDecode and get Either String (EsResult Tweet)

let maybeResult = decode body :: Maybe (EsResult Tweet)
-- the explicit typing is so Aeson knows how to parse the JSON.

-- use either if you want to know why something failed to parse.
-- (string errors, sadly)
let eitherResult = eitherDecode body :: Either String (EsResult Tweet)

-- print eitherResult should look like:
Right (EsResult {_index = "twitter"
               , _type = "tweet"
               , _id = "1"
               , foundResult = Just (EsResultFound { _version = 2
                                                   , _source = Tweet {user = "bitemyapp"
                                                                     , postDate = 2009-06-18 00:00:10 UTC
                                                                     , message = "Use haskell!"
                                                                     , age = 10000
                                                                     , location = Location {lat = 40.12, lon = -71.34}}})})

-- _source in EsResultFound is parametric, we dispatch the type by passing in what we expect (Tweet) as a parameter to EsResult.

-- use the _source record accessor to get at your document
fmap (fmap _source . foundResult) eitherResult
Right (Just (Tweet {user = "bitemyapp"
                   , postDate = 2009-06-18 00:00:10 UTC
                   , message = "Use haskell!"
                   , age = 10000
                   , location = Location {lat = 40.12, lon = -71.34}}))

```

Bulk Operations
---------------

### Bulk create, index

``` {.haskell}

-- don't forget the imports and derive generic setting for ghci
-- at the beginning of the examples.

:{
-- Using the earlier Tweet datatype and exampleTweet data

-- just changing up the data a bit.
let bulkTest = exampleTweet { user = "blah" }
let bulkTestTwo = exampleTweet { message = "woohoo!" }

-- create only bulk operation
-- BulkCreate :: IndexName -> MappingName -> DocId -> Value -> BulkOperation
let firstOp = BulkCreate testIndex
              testMapping (DocId "3") (toJSON bulkTest)

-- index operation "create or update"
let sndOp   = BulkIndex testIndex
              testMapping (DocId "4") (toJSON bulkTestTwo)

-- Some explanation, the final "Value" type that BulkIndex,
-- BulkCreate, and BulkUpdate accept is the actual document
-- data that your operation applies to. BulkDelete doesn't
-- take a value because it's just deleting whatever DocId
-- you pass.

-- list of bulk operations
let stream = [firstDoc, secondDoc]

-- Fire off the actual bulk request
-- bulk :: Vector BulkOperation -> IO Reply
resp <- withBH' $ bulk stream
:}

```

### Encoding individual bulk API operations

``` {.haskell}
-- the following functions are exported in Bloodhound so
-- you can build up bulk operations yourself
encodeBulkOperations :: V.Vector BulkOperation -> L.ByteString
encodeBulkOperation :: BulkOperation -> L.ByteString

-- How to use the above:
data BulkTest = BulkTest { name :: Text } deriving (Eq, Generic, Show)
instance FromJSON BulkTest
instance ToJSON BulkTest

_ <- insertData
let firstTest = BulkTest "blah"
let secondTest = BulkTest "bloo"
let firstDoc = BulkIndex testIndex
               testMapping (DocId "2") (toJSON firstTest)
let secondDoc = BulkCreate testIndex
               testMapping (DocId "3") (toJSON secondTest)
let stream = V.fromList [firstDoc, secondDoc] :: V.Vector BulkOperation

-- to encode yourself
let firstDocEncoded = encode firstDoc :: L.ByteString

-- to encode a vector of bulk operations
let encodedOperations = encodeBulkOperations stream

-- to insert into a particular server
-- bulk :: V.Vector BulkOperation -> IO Reply
_ <- withBH' $ bulk streamp

```

Search
------

### Querying

#### Term Query

``` {.haskell}

-- exported by the Client module, just defaults some stuff.
-- mkSearch :: Maybe Query -> Maybe Filter -> Search
-- mkSearch query filter = Search query filter Nothing False (From 0) (Size 10) Nothing

let query = TermQuery (Term "user" "bitemyapp") Nothing

-- AND'ing identity filter with itself and then tacking it onto a query
-- search should be a null-operation. I include it for the sake of example.
-- <||> (or/plus) should make it into a search that returns everything.

let filter = IdentityFilter <&&> IdentityFilter

-- constructing the search object the searchByIndex function dispatches on.
let search = mkSearch (Just query) (Just filter)

-- you can also searchByType and specify the mapping name.
reply <- withBH' $ searchByIndex testIndex search

let result = eitherDecode (responseBody reply) :: Either String (SearchResult Tweet)

λ> fmap (hits . searchHits) result
Right [Hit {hitIndex = IndexName "twitter"
          , hitType = MappingName "tweet"
          , hitDocId = DocId "1"
          , hitScore = 0.30685282
          , hitSource = Tweet {user = "bitemyapp"
                             , postDate = 2009-06-18 00:00:10 UTC
                             , message = "Use haskell!"
                             , age = 10000
                             , location = Location {lat = 40.12, lon = -71.34}}}]

```

#### Match Query

``` {.haskell}

let query = QueryMatchQuery $ mkMatchQuery (FieldName "user") (QueryString "bitemyapp")
let search = mkSearch (Just query) Nothing

```

#### Multi-Match Query

``` {.haskell}

let fields = [FieldName "user", FieldName "message"]
let query = QueryMultiMatchQuery $ mkMultiMatchQuery fields (QueryString "bitemyapp")
let search = mkSearch (Just query) Nothing

```

#### Bool Query

``` {.haskell}

let innerQuery = QueryMatchQuery $
                 mkMatchQuery (FieldName "user") (QueryString "bitemyapp")
let query = QueryBoolQuery $
            mkBoolQuery [innerQuery] [] []
let search = mkSearch (Just query) Nothing

```

#### Boosting Query

``` {.haskell}

let posQuery = QueryMatchQuery $
               mkMatchQuery (FieldName "user") (QueryString "bitemyapp")
let negQuery = QueryMatchQuery $
               mkMatchQuery (FieldName "user") (QueryString "notmyapp")
let query = QueryBoostingQuery $
            BoostingQuery posQuery negQuery (Boost 0.2)

```

#### Rest of the query/filter types

Just follow the pattern you've seen here and check the Hackage API documentation.

### Sorting

``` {.haskell}

let sortSpec = DefaultSortSpec $ mkSort (FieldName "age") Ascending

-- mkSort is a shortcut function that takes a FieldName and a SortOrder
-- to generate a vanilla DefaultSort.
-- checkt the DefaultSort type for the full list of customizable options.

-- From and size are integers for pagination.

-- When sorting on a field, scores are not computed. By setting TrackSortScores to true, scores will still be computed and tracked.

-- type Sort = [SortSpec]
-- type TrackSortScores = Bool
-- type From = Int
-- type Size = Int

-- Search takes Maybe Query
--              -> Maybe Filter
--              -> Maybe Sort
--              -> TrackSortScores
--              -> From -> Size
--              -> Maybe [FieldName]

-- just add more sortspecs to the list if you want tie-breakers.
let search = Search Nothing (Just IdentityFilter) (Just [sortSpec]) False (From 0) (Size 10) Nothing

```

### Field selection

If you only want certain fields from the source document returned, you can
set the "fields" field of the Search record.

``` {.haskell}

let search' = mkSearch (Just (MatchAllQuery Nothing)) Nothing
    search  = search' { fields = Just [FieldName "updated"] }

```

### Filtering

#### And, Not, and Or filters

Filters form a monoid and seminearring.

``` {.haskell}

instance Monoid Filter where
  mempty = IdentityFilter
  mappend a b = AndFilter [a, b] defaultCache

instance Seminearring Filter where
  a <||> b = OrFilter [a, b] defaultCache

-- AndFilter and OrFilter take [Filter] as an argument.

-- This will return anything, because IdentityFilter returns everything
OrFilter [IdentityFilter, someOtherFilter] False

-- This will return exactly what someOtherFilter returns
AndFilter [IdentityFilter, someOtherFilter] False

-- Thanks to the seminearring and monoid, the above can be expressed as:

-- "and"
IdentityFilter <&&> someOtherFilter

-- "or"
IdentityFilter <||> someOtherFilter

-- Also there is a NotFilter, it only accepts a single filter, not a list.

NotFilter someOtherFilter False

```

#### Identity Filter

``` {.haskell}

-- And'ing two Identity
let queryFilter = IdentityFilter <&&> IdentityFilter

let search = mkSearch Nothing (Just queryFilter)

reply <- withBH' $ searchByType testIndex testMapping search

```

#### Boolean Filter

Similar to boolean queries.

``` {.haskell}

-- Will return only items whose "user" field contains the term "bitemyapp"
let queryFilter = BoolFilter (MustMatch (Term "user" "bitemyapp") False)

-- Will return only items whose "user" field does not contain the term "bitemyapp"
let queryFilter = BoolFilter (MustNotMatch (Term "user" "bitemyapp") False)

-- The clause (query) should appear in the matching document.
-- In a boolean query with no must clauses, one or more should
-- clauses must match a document. The minimum number of should
-- clauses to match can be set using the minimum_should_match parameter.
let queryFilter = BoolFilter (ShouldMatch [(Term "user" "bitemyapp")] False)

```

#### Exists Filter

``` {.haskell}

-- Will filter for documents that have the field "user"
let existsFilter = ExistsFilter (FieldName "user")

```

#### Geo BoundingBox Filter

``` {.haskell}

-- topLeft and bottomRight
let box = GeoBoundingBox (LatLon 40.73 (-74.1)) (LatLon 40.10 (-71.12))

let constraint = GeoBoundingBoxConstraint (FieldName "tweet.location") box False GeoFilterMemory

```

#### Geo Distance Filter

``` {.haskell}

let geoPoint = GeoPoint (FieldName "tweet.location") (LatLon 40.12 (-71.34))

-- coefficient and units
let distance = Distance 10.0 Miles

-- GeoFilterType or NoOptimizeBbox
let optimizeBbox = OptimizeGeoFilterType GeoFilterMemory

-- SloppyArc is the usual/default optimization in Elasticsearch today
-- but pre-1.0 versions will need to pick Arc or Plane.

let geoFilter = GeoDistanceFilter geoPoint distance SloppyArc optimizeBbox False

```

#### Geo Distance Range Filter

Think of a donut and you won't be far off.

``` {.haskell}

let geoPoint = GeoPoint (FieldName "tweet.location") (LatLon 40.12 (-71.34))

let distanceRange = DistanceRange (Distance 0.0 Miles) (Distance 10.0 Miles)

let geoFilter = GeoDistanceRangeFilter geoPoint distanceRange

```

#### Geo Polygon Filter

``` {.haskell}

-- I think I drew a square here.
let points = [LatLon 40.0 (-70.00),
              LatLon 40.0 (-72.00),
              LatLon 41.0 (-70.00),
              LatLon 41.0 (-72.00)]

let geoFilter = GeoPolygonFilter (FieldName "tweet.location") points

```

#### Document IDs filter

``` {.haskell}

-- takes a mapping name and a list of DocIds
IdsFilter (MappingName "tweet") [DocId "1"]

```

#### Range Filter

``` {.haskell}

-- RangeFilter :: FieldName
--                -> RangeValue
--                -> RangeExecution
--                -> Cache -> Filter

let filter = RangeFilter (FieldName "age")
             (RangeGtLt (GreaterThan 1000.0) (LessThan 100000.0))
             RangeExecutionIndex False

```

``` {.haskell}

let filter = RangeFilter (FieldName "age")
             (RangeLte (LessThanEq 100000.0))
             RangeExecutionIndex False

```

##### Date Ranges

Date ranges are expressed in UTCTime. Date ranges use the same range bound constructors as numerics, except that they end in "D".

Note that compatibility with ES is tested only down to seconds.

``` {.haskell}

let filter = RangeFilter (FieldName "postDate")
             (RangeDateGtLte
              (GreaterThanD (UTCTime
                          (ModifiedJulianDay 55000)
                          (secondsToDiffTime 9)))
              (LessThanEqD (UTCTime
                            (ModifiedJulianDay 55000)
                            (secondsToDiffTime 11))))
             RangeExecutionIndex False
```

#### Regexp Filter

``` {.haskell}

-- RegexpFilter
--   :: FieldName
--      -> Regexp
--      -> RegexpFlags
--      -> CacheName
--      -> Cache
--      -> CacheKey
--      -> Filter
let filter = RegexpFilter (FieldName "user") (Regexp "bite.*app")
             AllRegexpFlags (CacheName "test") False (CacheKey "key")

-- n.b.
-- data RegexpFlags = AllRegexpFlags
--                 | NoRegexpFlags
--                 | SomeRegexpFlags (NonEmpty RegexpFlag) deriving (Eq, Show)

-- data RegexpFlag = AnyString
--                | Automaton
--                | Complement
--                | Empty
--                | Intersection
--                | Interval deriving (Eq, Show)

```

### Aggregations

#### Adding aggregations to search

Aggregations can now be added to search queries, or made on their own.

``` {.haskell}
type Aggregations = M.Map Text Aggregation
data Aggregation
  = TermsAgg TermsAggregation
  | DateHistogramAgg DateHistogramAggregation
```

For convenience, \`\`\`mkAggregations\`\`\` exists, that will create an \`\`\`Aggregations\`\`\` with the aggregation provided.

For example:

``` {.haskell}
 let a = mkAggregations "users" $ TermsAgg $ mkTermsAggregation "user"
 let search = mkAggregateSearch Nothing a
```

Aggregations can be added to an existing search, using the \`\`\`aggBody\`\`\` field

``` {.haskell}
 let search  = mkSearch (Just (MatchAllQuery Nothing)) Nothing
 let search' = search {aggBody = Just a}
```

Since the \`\`\`Aggregations\`\`\` structure is just a Map Text Aggregation, M.insert can be used to add additional aggregations.

``` {.haskell}
 let a' = M.insert "age" (TermsAgg $ mkTermsAggregation "age") a
```

#### Extracting aggregations from results

Aggregations are part of the reply structure of every search, in the
form of `Maybe AggregationResults`

``` {.haskell}
-- Lift decode and response body to be in the IO monad.
let decode' = liftM decode
let responseBody' = liftM responseBody
let reply = withBH' $ searchByIndex testIndex search
let response = decode' $ responseBody' reply :: IO (Maybe (SearchResult Tweet))

-- Now that we have our response, we can extract our terms aggregation result -- which is a list of buckets.

let terms = do { response' <- response; return $ response' >>= aggregations >>= toTerms "users" }
terms
Just (Bucket {buckets = [TermsResult {termKey = "bitemyapp", termsDocCount = 1, termsAggs = Nothing}]})
```

Note that bucket aggregation results, such as the TermsResult is a member of the type class `BucketAggregation`:

``` {.haskell}
class BucketAggregation a where
  key :: a -> Text
  docCount :: a -> Int
  aggs :: a -> Maybe AggregationResults
```

You can use the `aggs` function to get any nested results, if there
were any. For example, if there were a nested terms aggregation keyed
to "age" in a TermsResult named `termresult` , you would call `aggs
termresult >>= toTerms "age"`

#### Terms Aggregation

``` {.haskell}
data TermsAggregation
  = TermsAggregation {term :: Either Text Text,
                      termInclude :: Maybe TermInclusion,
                      termExclude :: Maybe TermInclusion,
                      termOrder :: Maybe TermOrder,
                      termMinDocCount :: Maybe Int,
                      termSize :: Maybe Int,
                      termShardSize :: Maybe Int,
                      termCollectMode :: Maybe CollectionMode,
                      termExecutionHint :: Maybe ExecutionHint,
                      termAggs :: Maybe Aggregations}
```

Term Aggregations have two factory functions, `mkTermsAggregation`, and
`mkTermsScriptAggregation`, and can be used as follows:

``` {.haskell}
let ta = TermsAgg $ mkTermsAggregation "user"
```

There are of course other options that can be added to a Terms Aggregation, such as the collection mode:

``` {.haskell}
let ta   = mkTermsAggregation "user"
let ta'  = ta { termCollectMode = Just BreadthFirst }
let ta'' = TermsAgg ta'
```

For more documentation on how the Terms Aggregation works, see <https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-bucket-terms-aggregation.html>

#### Date Histogram Aggregation

``` {.haskell}
data DateHistogramAggregation
  = DateHistogramAggregation {dateField :: FieldName,
                              dateInterval :: Interval,
                              dateFormat :: Maybe Text,
                              datePreZone :: Maybe Text,
                              datePostZone :: Maybe Text,
                              datePreOffset :: Maybe Text,
                              datePostOffset :: Maybe Text,
                              dateAggs :: Maybe Aggregations}
```

The Date Histogram Aggregation works much the same as the Terms Aggregation.

Relevant functions include `mkDateHistogram`, and `toDateHistogram`

``` {.haskell}
let dh = DateHistogramAgg (mkDateHistogram (FieldName "postDate") Minute)
```

Date histograms also accept a `FractionalInterval`:

``` {.haskell}
FractionalInterval :: Float -> TimeInterval -> Interval
-- TimeInterval is the following:
data TimeInterval = Weeks | Days | Hours | Minutes | Seconds
```

It can be used as follows:

``` {.haskell}
let dh = DateHistogramAgg (mkDateHistogram (FieldName "postDate") (FractionalInterval 1.5 Minutes))
```

The `DateHistogramResult` is defined as:

``` {.haskell}
data DateHistogramResult
  = DateHistogramResult {dateKey :: Int,
                         dateKeyStr :: Maybe Text,
                         dateDocCount :: Int,
                         dateHistogramAggs :: Maybe AggregationResults}
```

It is an instance of `BucketAggregation`, and can have nested aggregations in each bucket.

Buckets can be extracted from an `AggregationResult` using
`toDateHistogram name`

For more information on the Date Histogram Aggregation, see: <https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-bucket-datehistogram-aggregation.html>


Contributors
============

* [Chris Allen](https://github.com/bitemyapp)
* [Liam Atkinson](https://github.com/latkins)
* [Christopher Guiney](https://github.com/chrisguiney)
* [Curtis Carter](https://github.com/ccarter)
* [Michael Xavier](https://github.com/MichaelXavier)
* [Bob Long](https://github.com/bobjflong)
* [Maximilian Tagher](https://github.com/MaxGabriel)
* [Anna Kopp](https://github.com/annakopp)
* [Matvey B. Aksenov](https://github.com/supki)
* [Jan-Philip Loos](https://github.com/MaxDaten)

Possible future functionality
=============================

Span Queries
------------

Beginning here: <https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-span-first-query.html>

Function Score Query
--------------------

<https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-function-score-query.html>

Node discovery and failover
---------------------------

Might require TCP support.

Support for TCP access to Elasticsearch
---------------------------------------

Pretend to be a transport client?

Bulk cluster-join merge
-----------------------

Might require making a lucene index on disk with the appropriate format.

GeoShapeQuery
-------------

<https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-geo-shape-query.html>

GeoShapeFilter
--------------

<https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-geo-shape-filter.html>

Geohash cell filter
-------------------

<https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-geohash-cell-filter.html>

HasChild Filter
---------------

<https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-has-child-filter.html>

HasParent Filter
----------------

<https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-has-parent-filter.html>

Indices Filter
--------------

<https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-indices-filter.html>

Query Filter
------------

<https://www.elastic.co/guide/en/elasticsearch/reference/current/query-dsl-query-filter.html>

Script based sorting
--------------------

<https://www.elastic.co/guide/en/elasticsearch/reference/current/search-request-sort.html#_script_based_sorting>

Collapsing redundantly nested and/or structures
-----------------------------------------------

The Seminearring instance, if deeply nested can possibly produce nested structure that is redundant. Depending on how this affects ES performance, reducing this structure might be valuable.

Runtime checking for cycles in data structures
----------------------------------------------

check for n \> 1 occurrences in DFS:

<http://hackage.haskell.org/package/stable-maps-0.0.5/docs/System-Mem-StableName-Dynamic.html>

<http://hackage.haskell.org/package/stable-maps-0.0.5/docs/System-Mem-StableName-Dynamic-Map.html>

Photo Origin
============

Photo from HA! Designs: <https://www.flickr.com/photos/hadesigns/>
