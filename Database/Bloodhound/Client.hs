{-# LANGUAGE DeriveGeneric, ExistentialQuantification, StandaloneDeriving #-}

module Database.Bloodhound.Client
       ( createIndex
       , deleteIndex
       , defaultIndexSettings
       , createMapping
       , deleteMapping
       , indexDocument
       , getDocument
       , documentExists
       , deleteDocument
       , searchAll
       , searchByIndex
       , searchByType
       , refreshIndex
       , mkSearch
       , bulk
       , IndexSettings(..)
       , Server(..)
       , Reply(..)
       , EsResult(..)
       , Query(..)
       , Search(..)
       , SearchResult(..)
       , SearchHits(..)
       , ShardResult(..)
       , Hit(..)
       , Filter(..)
       , Seminearring(..)
       , BoolMatch(..)
       , Term(..)
       , GeoPoint(..)
       , GeoBoundingBoxConstraint(..)
       , GeoBoundingBox(..)
       , GeoFilterType(..)
       , Distance(..)
       , DistanceUnit(..)
       , DistanceType(..)
       , DistanceRange(..)
       , OptimizeBbox(..)
       , LatLon(..)
       , Range(..)
       , HalfRange(..)
       , RangeExecution(..)
       , LessThan(..)
       , LessThanEq(..)
       , GreaterThan(..)
       , GreaterThanEq(..)
       , Regexp(..)
       , RegexpFlags(..)
       , FieldName(..)
       , IndexName(..)
       , MappingName(..)
       , DocId(..)
       , CacheName(..)
       , CacheKey(..)
       , BulkOperation(..)
       )
       where

import Control.Applicative
import Control.Monad (liftM)
import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.ByteString.Builder
import Data.List (foldl', intercalate, intersperse)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import Network.HTTP.Conduit
import qualified Network.HTTP.Types.Method as NHTM
import qualified Network.HTTP.Types.Status as NHTS

data Version = Version { number          :: Text
                       , build_hash      :: Text
                       , build_timestamp :: UTCTime
                       , build_snapshot  :: Bool
                       , lucene_version  :: Text } deriving (Show, Generic)

data (FromJSON a, ToJSON a) => Status a =
                     Status { ok      :: Bool
                            , status  :: Int
                            , name    :: Text
                            , version :: a
                            , tagline :: Text } deriving (Eq, Show)

instance ToJSON Version
instance FromJSON Version

instance (FromJSON a, ToJSON a) => FromJSON (Status a) where
  parseJSON (Object v) = Status <$>
                         v .: "ok" <*>
                         v .: "status" <*>
                         v .: "name" <*>
                         v .: "version" <*>
                         v .: "tagline"
  parseJSON _          = empty

main = simpleHttp "http://localhost:9200/events/event/_search?q=hostname:localhost&size=1" >>= L.putStrLn

newtype ShardCount   = ShardCount   Int deriving (Eq, Show, Generic)
newtype ReplicaCount = ReplicaCount Int deriving (Eq, Show, Generic)

mkShardCount :: Int -> Maybe ShardCount
mkShardCount n
  | n < 1 = Nothing
  | n > 1000 = Nothing -- seriously, what the fuck?
  | otherwise = Just (ShardCount n)

mkReplicaCount :: Int -> Maybe ReplicaCount
mkReplicaCount n
  | n < 1 = Nothing
  | n > 1000 = Nothing -- ...
  | otherwise = Just (ReplicaCount n)

data IndexSettings =
  IndexSettings { indexShards   :: ShardCount
                , indexReplicas :: ReplicaCount } deriving (Eq, Show)

instance ToJSON IndexSettings where
  toJSON (IndexSettings s r) = object ["settings" .= object ["shards" .= s, "replicas" .= r]]

instance ToJSON ReplicaCount
instance ToJSON ShardCount

defaultIndexSettings = IndexSettings (ShardCount 3) (ReplicaCount 2)

data Strategy = RoundRobinStrat | RandomStrat | HeadStrat deriving (Eq, Show)

newtype Server = Server String deriving (Eq, Show)

type Reply = Network.HTTP.Conduit.Response L.ByteString
type Method = NHTM.Method

responseIsError :: Reply -> Bool
responseIsError resp = NHTS.statusCode (responseStatus resp) > 299

emptyBody = L.pack ""

dispatch :: String -> Method -> Maybe L.ByteString
            -> IO Reply
dispatch url method body = do
  initReq <- parseUrl url
  let reqBody = RequestBodyLBS $ fromMaybe emptyBody body
  let req = initReq { method = method
                    , requestBody = reqBody
                    , checkStatus = \_ _ _ -> Nothing}
  withManager $ httpLbs req

joinPath :: [String] -> String
joinPath = intercalate "/"

getStatus :: Server -> IO (Maybe (Status Version))
getStatus (Server server) = do
  request <- parseUrl $ joinPath [server]
  response <- withManager $ httpLbs request
  return $ decode (responseBody response)

createIndex :: Server -> IndexSettings -> IndexName -> IO Reply
createIndex (Server server) indexSettings (IndexName indexName) =
  dispatch url method body where
    url = joinPath [server, indexName]
    method = NHTM.methodPut
    body = Just $ encode indexSettings

deleteIndex :: Server -> IndexName -> IO Reply
deleteIndex (Server server) (IndexName indexName) =
  dispatch url method body where
    url = joinPath [server, indexName]
    method = NHTM.methodDelete
    body = Nothing

respIsTwoHunna :: Reply -> Bool
respIsTwoHunna resp = NHTS.statusCode (responseStatus resp) == 200

existentialQuery url = do
  reply <- dispatch url NHTM.methodHead Nothing
  return (reply, respIsTwoHunna reply)

indexExists :: Server -> IndexName -> IO Bool
indexExists (Server server) (IndexName indexName) = do
  (reply, exists) <- existentialQuery url
  return exists where
    url = joinPath [server, indexName]

refreshIndex :: Server -> IndexName -> IO Reply
refreshIndex (Server server) (IndexName indexName) = dispatch url method body where
  url = joinPath [server, indexName, "_refresh"]
  method = NHTM.methodPost
  body = Nothing

data OpenCloseIndex = OpenIndex | CloseIndex deriving (Eq, Show)

stringifyOCIndex oci = case oci of
  OpenIndex  -> "_open"
  CloseIndex -> "_close"

openOrCloseIndexes :: OpenCloseIndex -> Server -> IndexName -> IO Reply
openOrCloseIndexes oci (Server server) (IndexName indexName) =
  dispatch url method body where
    ociString = stringifyOCIndex oci
    url = joinPath [server, indexName, ociString]
    method = NHTM.methodPost
    body = Nothing

openIndex :: Server -> IndexName -> IO Reply
openIndex = openOrCloseIndexes OpenIndex

closeIndex :: Server -> IndexName -> IO Reply
closeIndex = openOrCloseIndexes CloseIndex

data FieldType = GeoPointType
               | GeoShapeType
               | FloatType
               | IntegerType
               | LongType
               | ShortType
               | ByteType deriving (Eq, Show)

data FieldDefinition =
  FieldDefinition { fieldType :: FieldType } deriving (Eq, Show)

data MappingField =
  MappingField   { mappingFieldName       :: FieldName
                 , fieldDefinition :: FieldDefinition } deriving (Eq, Show)

data Mapping = Mapping { typeName :: Text
                       , fields   :: [MappingField] } deriving (Eq, Show)

createMapping :: ToJSON a => Server -> IndexName
                 -> MappingName -> a -> IO Reply
createMapping (Server server) (IndexName indexName) (MappingName mappingName) mapping =
  dispatch url method body where
    url = joinPath [server, indexName, mappingName, "_mapping"]
    method = NHTM.methodPut
    body = Just $ encode mapping

deleteMapping :: Server -> IndexName -> MappingName -> IO Reply
deleteMapping (Server server) (IndexName indexName) (MappingName mappingName) =
  dispatch url method body where
    url = joinPath [server, indexName, mappingName, "_mapping"]
    method = NHTM.methodDelete
    body = Nothing

indexDocument :: ToJSON doc => Server -> IndexName -> MappingName
                 -> doc -> DocId -> IO Reply
indexDocument (Server server) (IndexName indexName)
  (MappingName mappingName) document (DocId docId) =
  dispatch url method body where
    url = joinPath [server, indexName, mappingName, docId]
    method = NHTM.methodPut
    body = Just (encode document)

deleteDocument :: Server -> IndexName -> MappingName
                  -> DocId -> IO Reply
deleteDocument (Server server) (IndexName indexName)
  (MappingName mappingName) (DocId docId) =
  dispatch url method body where
    url = joinPath [server, indexName, mappingName, docId]
    method = NHTM.methodDelete
    body = Nothing

bulk :: Server -> [BulkOperation] -> IO Reply
bulk (Server server) bulkOps = dispatch url method body where
  url = joinPath [server, "_bulk"]
  method = NHTM.methodPost
  body = Just $ collapseStream bulkOps

collapseStream :: [BulkOperation] -> L.ByteString
collapseStream stream = collapsed where
  blobs = intersperse "\n" $ concat $ fmap getStreamChunk stream
  mashedTaters = mash (mempty :: Builder) blobs
  collapsed = toLazyByteString $ mappend mashedTaters "\n"

mash :: Builder -> [L.ByteString] -> Builder
mash builder xs = foldl' (\b x -> mappend b (lazyByteString x)) builder xs

data BulkOperation =
    BulkIndex  IndexName MappingName DocId Value
  | BulkCreate IndexName MappingName DocId Value
  | BulkDelete IndexName MappingName DocId
  | BulkUpdate IndexName MappingName DocId Value deriving (Eq, Show)

mkMetadataValue :: Text -> String -> String -> String -> Value
mkMetadataValue operation indexName mappingName docId =
  object [operation .=
          object ["_index" .= indexName
                 , "_type" .= mappingName
                 , "_id"   .= docId]]

getStreamChunk :: BulkOperation -> [L.ByteString]
getStreamChunk (BulkIndex (IndexName indexName)
                (MappingName mappingName)
                (DocId docId) value) = blob where
  metadata = mkMetadataValue "index" indexName mappingName docId
  blob = [encode metadata, encode value]

getStreamChunk (BulkCreate (IndexName indexName)
                (MappingName mappingName)
                (DocId docId) value) = blob where
  metadata = mkMetadataValue "create" indexName mappingName docId
  blob = [encode metadata, encode value]

getStreamChunk (BulkDelete (IndexName indexName)
                (MappingName mappingName)
                (DocId docId)) = blob where
  metadata = mkMetadataValue "delete" indexName mappingName docId
  blob = [encode metadata]

getStreamChunk (BulkUpdate (IndexName indexName)
                (MappingName mappingName)
                (DocId docId) value) = blob where
  metadata = mkMetadataValue "update" indexName mappingName docId
  doc = object ["doc" .= value]
  blob = [encode metadata, encode doc]

data EsResult a = EsResult { _index   :: Text
                           , _type    :: Text
                           , _id      :: Text
                           , _version :: Int
                           , found    :: Maybe Bool
                           , _source  :: a } deriving (Eq, Show)

instance (FromJSON a, ToJSON a) => FromJSON (EsResult a) where
  parseJSON (Object v) = EsResult <$>
                         v .:  "_index"   <*>
                         v .:  "_type"    <*>
                         v .:  "_id"      <*>
                         v .:  "_version" <*>
                         v .:? "found"    <*>
                         v .:  "_source"
  parseJSON _          = empty

getDocument :: Server -> IndexName -> MappingName
               -> DocId -> IO Reply
getDocument (Server server) (IndexName indexName) (MappingName mappingName) (DocId docId) =
  dispatch url method body where
    url = joinPath [server, indexName, mappingName, docId]
    method = NHTM.methodGet
    body = Nothing

documentExists :: Server -> IndexName -> MappingName
                  -> DocId -> IO Bool
documentExists (Server server) (IndexName indexName) (MappingName mappingName) (DocId docId) = do
  (reply, exists) <- existentialQuery url
  return exists where
    url = joinPath [server, indexName, mappingName, docId]


dispatchSearch :: String -> Search -> IO Reply
dispatchSearch url search = dispatch url NHTM.methodPost (Just (encode search))

searchAll :: Server -> Search -> IO Reply
searchAll (Server server) search = dispatchSearch url search where
  url = joinPath [server, "_search"]

searchByIndex :: Server -> IndexName -> Search -> IO Reply
searchByIndex (Server server) (IndexName indexName) search = dispatchSearch url search where
  url = joinPath [server, indexName, "_search"]

searchByType :: Server -> IndexName -> MappingName -> Search -> IO Reply
searchByType (Server server) (IndexName indexName)
  (MappingName mappingName) search = dispatchSearch url search where
  url = joinPath [server, indexName, mappingName, "_search"]

data Search = Search { queryBody  :: Maybe Query
                     , filterBody :: Maybe Filter
                     , sortBody   :: Maybe Sort
                       -- default False
                     , trackSortScores :: Bool
                     , from :: Int
                     , size :: Int} deriving (Eq, Show)

mkSearch query filter = Search query filter Nothing False 0 10

pageSearch :: Int -> Int -> Search -> Search
pageSearch from size search = search { from = from, size = size }

-- defaultFieldMaybeJSON :: Text -> Maybe a -> 
maybeJson field (Just value) = [field .= toJSON value]
maybeJson _ _ = []
maybeJsonF field (Just value) = [field .= fmap toJSON value]
maybeJsonF _ _ = []

instance ToJSON Search where
  toJSON (Search query filter sort trackSortScores from size) =
    object merged where
      lQuery  = maybeJson  "query" query
      lFilter = maybeJson  "filter" filter
      lSort   = maybeJsonF "sort" sort
      merged  = mconcat [[ "from" .= from
                         , "size" .= size
                         , "track_scores" .= trackSortScores]
                        , lQuery
                        , lFilter
                        , lSort]

type Sort = [SortSpec]

data SortSpec = DefaultSortSpec DefaultSort
              | GeoDistanceSortSpec SortOrder GeoPoint deriving (Eq, Show)

instance ToJSON SortSpec where
  toJSON (DefaultSortSpec
          (DefaultSort (FieldName sortFieldName) sortOrder ignoreUnmapped
           sortMode missingSort nestedFilter)) =
    object [sortFieldName .= object merged] where
      base = ["order" .= toJSON sortOrder
             , "ignore_unmapped" .= ignoreUnmapped]
      lSortMode = maybeJson "mode" sortMode
      lMissingSort = maybeJson "missing" missingSort
      lNestedFilter = maybeJson "nested_filter" nestedFilter
      merged = mconcat [base, lSortMode, lMissingSort, lNestedFilter]

instance ToJSON SortOrder where
  toJSON Ascending  = String "asc"      
  toJSON Descending = String "desc"

instance ToJSON SortMode where
  toJSON SortMin = String "min"
  toJSON SortMax = String "max"
  toJSON SortSum = String "sum"
  toJSON SortAvg = String "avg"

instance ToJSON Missing where
  toJSON LastMissing = String "_last"
  toJSON FirstMissing = String "_first"
  toJSON (CustomMissing txt) = String txt

data DefaultSort =
  DefaultSort { sortFieldName  :: FieldName
              , sortOrder      :: SortOrder
                                  -- default False
              , ignoreUnmapped :: Bool
              , sortMode       :: Maybe SortMode
              , missingSort    :: Maybe Missing
              , nestedFilter   :: Maybe Filter } deriving (Eq, Show)

data SortOrder = Ascending
               | Descending deriving (Eq, Show)

data Missing = LastMissing
             | FirstMissing
             | CustomMissing Text deriving (Eq, Show)

data SortMode = SortMin
              | SortMax
              | SortSum
              | SortAvg deriving (Eq, Show)

-- status:active
-- author:"John Smith"
-- for book.title and book.date containing quick OR brown, book.\*:(quick brown)
-- field title has no value or doesn't exist, _missing_:title
-- field title has any non-null value, _exists:title

-- type DisMax = Bool -- "use_dis_max"
-- newtype TieBreaker = TieBreaker Int deriving (Eq, Show)
-- data QueryField = DefaultField Text
--                 | Fields [Text] DisMax TieBreaker deriving (Eq, Show)

-- -- this is will go away later

-- data QueryStringQuery =
--   QueryStringQuery { query                     :: Text
--                      -- default _all
--                    , field                     :: Maybe QueryField
--                      -- default OR
--                    , defaultOperator           :: Maybe BooleanOperator
--                      -- analyzer name
--                    , analyzer                  :: Maybe Text
--                      -- default true
--                    , allowLeadingWildcard      :: Maybe Bool
--                      -- default true
--                    , lowercaseExpandedTerms    :: Maybe Bool
--                      -- default true
--                    , enablePositionIncrements  :: Maybe Bool
--                      -- default 50
--                    , fuzzyMaxExpansions        :: Maybe Int
--                      -- Fuzziness -- default AUTO, add type later
--                    , fuzziness                 :: Maybe Text
--                    , fuzzyPrefixLength         :: Maybe Int    -- default 0
--                      -- default 0, 0 means exact phrase matches
--                    , phraseSlop                :: Maybe Int
--                      -- default 1.0
--                    , boost                     :: Maybe Double
--                      -- default false, true forces wildcard analysis
--                    , analyzeWildcard           :: Maybe Bool
--                      -- default false
--                    , autoGeneratePhraseQueries :: Maybe Bool
--                      -- # "should" clauses in the boolean query should match
--                    , minimumShouldMatch        :: Maybe Text
--                      -- Text to handle weird % and other cases. Needs type
--                      -- default false, true shuts off format based failures
--                    , lenient                   :: Maybe Bool
--                      -- default ROOT, locale used for string conversions
--                    , locale                    :: Maybe Text
--                      } deriving (Eq, Show)

-- emptyQueryStringQuery = QueryStringQuery "" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
-- queryStringQuery query = emptyQueryStringQuery { query = query }

-- type FieldName = Text

type Cache     = Bool -- caching on/off
defaultCache   = False

type PrefixValue = Text

data BooleanOperator = And | Or deriving (Eq, Show)

newtype IndexName   = IndexName String deriving (Eq, Generic, Show)
newtype MappingName = MappingName String deriving (Eq, Generic, Show)
newtype DocId       = DocId String deriving (Eq, Generic, Show)
newtype QueryString = QueryString Text deriving (Eq, Show)
newtype FieldName   = FieldName Text deriving (Eq, Show)
newtype CacheName   = CacheName Text deriving (Eq, Show)
newtype CacheKey    = CacheKey  Text deriving (Eq, Show)
newtype Existence   = Existence Bool deriving (Eq, Show)
newtype NullValue   = NullValue Bool deriving (Eq, Show)

unpackId :: DocId -> String
unpackId (DocId docId) = docId

instance FromJSON IndexName
instance FromJSON MappingName
instance FromJSON DocId

-- data QueryType = BooleanQueryType
--                | PhraseQueryType
--                | PhrasePrefixQueryType deriving (Eq, Show)
type Boost = Double

data Query = TermQuery Term (Maybe Boost)
           | ConstantScoreFilter Filter Boost
           | ConstantScoreQuery  Query  Boost
             deriving (Eq, Show)

instance ToJSON Query where
  toJSON (TermQuery (Term termField termValue) boost) =
    object ["term" .=
            object [termField .= object merged]]
    where
      base = ["value" .= termValue]
      boosted = case boost of
        (Just boostValue) -> ["boost" .= boostValue]
        Nothing           -> []
      merged = mappend base boosted

data Filter = AndFilter [Filter] Cache
            | OrFilter  [Filter] Cache
            | NotFilter Filter   Cache
            | IdentityFilter
            | BoolFilter BoolMatch
            | ExistsFilter FieldName -- always cached
            | GeoBoundingBoxFilter GeoBoundingBoxConstraint GeoFilterType
            | GeoDistanceFilter GeoPoint Distance DistanceType OptimizeBbox Cache
            | GeoDistanceRangeFilter GeoPoint DistanceRange
            | GeoPolygonFilter FieldName [LatLon]
            | IdsFilter MappingName [DocId]
            | LimitFilter Int
            | MissingFilter FieldName Existence NullValue
            | PrefixFilter  FieldName PrefixValue Cache
            | RangeFilter   FieldName (Either HalfRange Range) RangeExecution Cache
            | RegexpFilter  FieldName Regexp RegexpFlags CacheName Cache CacheKey
              deriving (Eq, Show)

class Monoid a => Seminearring a where
  -- 0, +, *
  (<||>) :: a -> a -> a
  (<&&>) :: a -> a -> a
  (<&&>) = mappend

infixr 5 <||>
infixr 5 <&&>

instance Monoid Filter where
  mempty = IdentityFilter
  mappend a b = AndFilter [a, b] defaultCache

instance Seminearring Filter where
  a <||> b = OrFilter [a, b] defaultCache

instance ToJSON Filter where
  toJSON (AndFilter filters cache) =
    object ["and"     .= fmap toJSON filters
           , "_cache" .= cache]

  toJSON (OrFilter filters cache) =
    object ["or"      .= fmap toJSON filters
           , "_cache" .= cache]

  toJSON (NotFilter filter cache) =
    object ["not" .=
            object ["filter"  .= toJSON filter
                   , "_cache" .= cache]]

  toJSON (IdentityFilter) =
    object ["match_all" .= object []]

  toJSON (ExistsFilter (FieldName fieldName)) =
    object ["exists"  .= object
            ["field"  .= fieldName]]

  toJSON (BoolFilter boolMatch) =
    object ["bool"    .= toJSON boolMatch]

  toJSON (GeoBoundingBoxFilter bbConstraint filterType) =
    object ["geo_bounding_box" .= toJSON bbConstraint
           , "type" .= toJSON filterType]

  toJSON (GeoDistanceFilter (GeoPoint (FieldName geoField) latLon)
          distance distanceType optimizeBbox cache) =
    object ["geo_distance" .=
            object ["distance" .= toJSON distance
                   , "distance_type" .= toJSON distanceType
                   , "optimize_bbox" .= optimizeBbox
                   , geoField .= toJSON latLon
                   , "_cache" .= cache]]                   

  toJSON (GeoDistanceRangeFilter (GeoPoint (FieldName geoField) latLon)
          (DistanceRange distanceFrom distanceTo)) =
    object ["geo_distance_range" .=
            object ["from" .= toJSON distanceFrom
                   , "to"  .= toJSON distanceTo
                   , geoField .= toJSON latLon]]

  toJSON (GeoPolygonFilter (FieldName geoField) latLons) =
    object ["geo_polygon" .=
            object [geoField .=
                    object ["points" .= fmap toJSON latLons]]]

  toJSON (IdsFilter (MappingName mappingName) values) =
    object ["ids" .=
            object ["type" .= mappingName
                   , "values" .= fmap (T.pack . unpackId) values]]

  toJSON (LimitFilter limit) =
    object ["limit" .= object ["value" .= limit]]

  toJSON (MissingFilter (FieldName fieldName) (Existence existence) (NullValue nullValue)) =
    object ["missing" .=
            object ["field"       .= fieldName
                   , "existence"  .= existence
                   , "null_value" .= nullValue]]

  toJSON (PrefixFilter (FieldName fieldName) fieldValue cache) =
    object ["prefix" .=
            object [fieldName .= fieldValue
                   , "_cache" .= cache]]

  toJSON (RangeFilter (FieldName fieldName) (Left halfRange) rangeExecution cache) =
    object ["range" .=
            object [fieldName .=
                    object [key .= val]
                   , "execution" .= toJSON rangeExecution
                   , "_cache" .= cache]]
    where
      (key, val) = halfRangeToKV halfRange

  toJSON (RangeFilter (FieldName fieldName) (Right range) rangeExecution cache) =
    object ["range" .=
            object [fieldName .=
                    object [lessKey .= lessVal
                           , greaterKey .= greaterVal]
                   , "execution" .= toJSON rangeExecution
                   , "_cache" .= cache]]
    where
      (lessKey, lessVal, greaterKey, greaterVal) = rangeToKV range

  toJSON (RegexpFilter (FieldName fieldName)
          (Regexp regexText) flags (CacheName cacheName) cache (CacheKey cacheKey)) =
    object ["regexp" .=
            object [fieldName .=
                    object ["value"  .= regexText
                           , "flags" .= toJSON flags]
                   , "_name"      .= cacheName
                   , "_cache"     .= cache
                   , "_cache_key" .= cacheKey]]

instance ToJSON GeoPoint where
  toJSON (GeoPoint (FieldName geoField) latLon) =
    object [geoField  .= toJSON latLon]

showText :: Show a => a -> Text
showText = T.pack . show

instance ToJSON Distance where
  toJSON (Distance coefficient unit) =
    String boltedTogether where
      coefText = showText coefficient
      (String unitText) = (toJSON unit)
      boltedTogether = mappend coefText unitText

instance ToJSON DistanceUnit where
  toJSON Miles         = String "mi"
  toJSON Yards         = String "yd"
  toJSON Feet          = String "ft"
  toJSON Inches        = String "in"
  toJSON Kilometers    = String "km"
  toJSON Meters        = String "m"
  toJSON Centimeters   = String "cm"
  toJSON Millimeters   = String "mm"
  toJSON NauticalMiles = String "nmi"

instance ToJSON DistanceType where
  toJSON Arc       = String "arc"
  toJSON SloppyArc = String "sloppy_arc"
  toJSON Plane     = String "plane"

instance ToJSON OptimizeBbox where
  toJSON NoOptimizeBbox = String "none"
  toJSON (OptimizeGeoFilterType gft) = toJSON gft

instance ToJSON GeoBoundingBoxConstraint where
  toJSON (GeoBoundingBoxConstraint (FieldName geoBBField) constraintBox cache) =
    object [geoBBField .= toJSON constraintBox
           , "_cache"  .= cache]

instance ToJSON GeoFilterType where
  toJSON GeoFilterMemory  = String "memory"
  toJSON GeoFilterIndexed = String "indexed"

instance ToJSON GeoBoundingBox where
  toJSON (GeoBoundingBox topLeft bottomRight) =
    object ["top_left"      .= toJSON topLeft
           , "bottom_right" .= toJSON bottomRight]

instance ToJSON LatLon where
  toJSON (LatLon lat lon) =
    object ["lat"  .= lat
           , "lon" .= lon]

-- lt, lte | gt, gte
newtype LessThan      = LessThan      Double deriving (Eq, Show)
newtype LessThanEq    = LessThanEq    Double deriving (Eq, Show)
newtype GreaterThan   = GreaterThan   Double deriving (Eq, Show)
newtype GreaterThanEq = GreaterThanEq Double deriving (Eq, Show)

data HalfRange = HalfRangeLt  LessThan
               | HalfRangeLte LessThanEq
               | HalfRangeGt  GreaterThan
               | HalfRangeGte GreaterThanEq deriving (Eq, Show)

data Range = RangeLtGt   LessThan GreaterThan
           | RangeLtGte  LessThan GreaterThanEq
           | RangeLteGt  LessThanEq GreaterThan
           | RangeLteGte LessThanEq GreaterThanEq deriving (Eq, Show)

halfRangeToKV :: HalfRange -> (Text, Double)
halfRangeToKV (HalfRangeLt  (LessThan n))      = ("lt",  n)
halfRangeToKV (HalfRangeLte (LessThanEq n))    = ("lte", n)
halfRangeToKV (HalfRangeGt  (GreaterThan n))   = ("gt",  n)
halfRangeToKV (HalfRangeGte (GreaterThanEq n)) = ("gte", n)

rangeToKV :: Range -> (Text, Double, Text, Double)
rangeToKV (RangeLtGt   (LessThan m)   (GreaterThan n))   = ("lt",  m, "gt",  n)
rangeToKV (RangeLtGte  (LessThan m)   (GreaterThanEq n)) = ("lt",  m, "gte", n)
rangeToKV (RangeLteGt  (LessThanEq m) (GreaterThan n))   = ("lte", m, "gt",  n)
rangeToKV (RangeLteGte (LessThanEq m) (GreaterThanEq n)) = ("lte", m, "gte", n)

data RangeExecution = RangeExecutionIndex
                    | RangeExecutionFielddata deriving (Eq, Show)

-- index for smaller ranges, fielddata for longer ranges
instance ToJSON RangeExecution where
  toJSON RangeExecutionIndex     = "index"
  toJSON RangeExecutionFielddata = "fielddata"

newtype Regexp = Regexp Text deriving (Eq, Show)
data RegexpFlags = RegexpAll
                 | Complement
                 | Interval
                 | Intersection
                 | AnyString
                 | CompInterval
                 | CompIntersection
                 | CompAnyString
                 | IntervalIntersection
                 | IntervalAnyString
                 | IntersectionAnyString deriving (Eq, Show)

instance ToJSON RegexpFlags where
  toJSON RegexpAll             = String "ALL"
  toJSON Complement            = String "COMPLEMENT"
  toJSON Interval              = String "INTERVAL"
  toJSON Intersection          = String "INTERSECTION"
  toJSON AnyString             = String "ANYSTRING"
  toJSON CompInterval          = String "COMPLEMENT|INTERVAL"
  toJSON CompIntersection      = String "COMPLEMENT|INTERSECTION"
  toJSON CompAnyString         = String "COMPLEMENT|ANYSTRING"
  toJSON IntervalIntersection  = String "INTERVAL|INTERSECTION"
  toJSON IntervalAnyString     = String "INTERVAL|ANYSTRING"
  toJSON IntersectionAnyString = String "INTERSECTION|ANYSTRING"

-- phew. Coulda used Agda style case breaking there but, you know, whatever. :)

data Term = Term { termField :: Text
                 , termValue :: Text } deriving (Eq, Show)

instance ToJSON Term where
  toJSON (Term field value) = object ["term" .= object
                                      [field .= value]]

data BoolMatch = MustMatch    Term  Cache
               | MustNotMatch Term  Cache
               | ShouldMatch [Term] Cache deriving (Eq, Show)

instance ToJSON BoolMatch where
  toJSON (MustMatch    term  cache) = object ["must"     .= toJSON term,
                                              "_cache" .= cache]
  toJSON (MustNotMatch term  cache) = object ["must_not" .= toJSON term,
                                              "_cache" .= cache]
  toJSON (ShouldMatch  terms cache) = object ["should"   .= fmap toJSON terms,
                                              "_cache" .= cache]

-- "memory" or "indexed"
data GeoFilterType = GeoFilterMemory
                   | GeoFilterIndexed deriving (Eq, Show)


data LatLon = LatLon { lat :: Double
                     , lon :: Double } deriving (Eq, Show)

data GeoBoundingBox =
  GeoBoundingBox { topLeft     :: LatLon
                 , bottomRight :: LatLon } deriving (Eq, Show)

data GeoBoundingBoxConstraint =
  GeoBoundingBoxConstraint { geoBBField        :: FieldName
                           , constraintBox     :: GeoBoundingBox
                           , bbConstraintcache :: Cache
                           } deriving (Eq, Show)

data GeoPoint =
  GeoPoint { geoField :: FieldName
           , latLon   :: LatLon} deriving (Eq, Show)

data DistanceUnit = Miles
                  | Yards
                  | Feet
                  | Inches
                  | Kilometers
                  | Meters
                  | Centimeters
                  | Millimeters
                  | NauticalMiles deriving (Eq, Show)

data DistanceType = Arc
                  | SloppyArc -- doesn't exist <1.0
                  | Plane deriving (Eq, Show)

data OptimizeBbox = OptimizeGeoFilterType GeoFilterType
                  | NoOptimizeBbox deriving (Eq, Show)

data Distance =
  Distance { coefficient :: Double
           , unit        :: DistanceUnit } deriving (Eq, Show)

data DistanceRange =
  DistanceRange { distanceFrom :: Distance
                , distanceTo   :: Distance } deriving (Eq, Show)

data FromJSON a => SearchResult a =
  SearchResult { took       :: Int
               , timedOut   :: Bool
               , shards     :: ShardResult
               , searchHits :: SearchHits a } deriving (Eq, Show)

type Score = Double

data FromJSON a => SearchHits a =
  SearchHits { hitsTotal :: Int
             , maxScore  :: Score
             , hits      :: [Hit a] } deriving (Eq, Show)

data FromJSON a => Hit a =
  Hit { hitIndex      :: IndexName
      , hitType       :: MappingName
      , hitDocId      :: DocId
      , hitScore      :: Score
      , hitSource     :: a } deriving (Eq, Show)

data ShardResult =
  ShardResult { shardTotal       :: Int
              , shardsSuccessful :: Int
              , shardsFailed     :: Int } deriving (Eq, Show, Generic)

instance (FromJSON a, ToJSON a) => FromJSON (SearchResult a) where
  parseJSON (Object v) = SearchResult <$>
                         v .: "took"      <*>
                         v .: "timed_out" <*>
                         v .: "_shards"   <*>
                         v .: "hits"
  parseJSON _          = empty

instance (FromJSON a, ToJSON a) => FromJSON (SearchHits a) where
  parseJSON (Object v) = SearchHits <$>
                         v .: "total"     <*>
                         v .: "max_score" <*>
                         v .: "hits"
  parseJSON _          = empty

instance (FromJSON a, ToJSON a) => FromJSON (Hit a) where
  parseJSON (Object v) = Hit <$>
                         v .: "_index" <*>
                         v .: "_type"  <*>
                         v .: "_id"    <*>
                         v .: "_score" <*>
                         v .: "_source"
  parseJSON _          = empty

instance FromJSON ShardResult where
  parseJSON (Object v) = ShardResult <$>
                         v .: "total"      <*>
                         v .: "successful" <*>
                         v .: "failed"
  parseJSON _          = empty
