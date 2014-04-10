{-# LANGUAGE DeriveGeneric #-}

module Database.Bloodhound.Client
       ( createIndex
       , deleteIndex
       , defaultIndexSettings
       , indexDocument
       , getDocument
       , documentExists
       , deleteDocument
       , searchAll
       , searchByIndex
       , searchByType
       , refreshIndex
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
       )
       where

import Control.Applicative
import Control.Monad (liftM)
import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Text (Text)
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
                            , tagline :: Text } deriving (Show)

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

newtype ShardCount   = ShardCount   Int deriving (Show, Generic)
newtype ReplicaCount = ReplicaCount Int deriving (Show, Generic)

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
                , indexReplicas :: ReplicaCount } deriving (Show)

instance ToJSON IndexSettings where
  toJSON (IndexSettings s r) = object ["settings" .= object ["shards" .= s, "replicas" .= r]]

instance ToJSON ReplicaCount
instance ToJSON ShardCount

defaultIndexSettings = IndexSettings (ShardCount 3) (ReplicaCount 2)

data Strategy = RoundRobinStrat | RandomStrat | HeadStrat deriving (Show)

newtype Server = Server String deriving (Show)

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

type IndexName = String

joinPath :: [String] -> String
joinPath = intercalate "/"

getStatus :: Server -> IO (Maybe (Status Version))
getStatus (Server server) = do
  request <- parseUrl $ joinPath [server]
  response <- withManager $ httpLbs request
  return $ decode (responseBody response)

createIndex :: Server -> IndexSettings -> IndexName -> IO Reply
createIndex (Server server) indexSettings indexName =
  dispatch url method body where
    url = joinPath [server, indexName]
    method = NHTM.methodPut
    body = Just $ encode indexSettings

deleteIndex :: Server -> IndexName -> IO Reply
deleteIndex (Server server) indexName =
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
indexExists (Server server) indexName = do
  (reply, exists) <- existentialQuery url
  return exists where
    url = joinPath [server, indexName]

data OpenCloseIndex = OpenIndex | CloseIndex deriving (Show)

stringifyOCIndex oci = case oci of
  OpenIndex  -> "_open"
  CloseIndex -> "_close"

openOrCloseIndexes :: OpenCloseIndex -> Server -> IndexName -> IO Reply
openOrCloseIndexes oci (Server server) indexName =
  dispatch url method body where
    ociString = stringifyOCIndex oci
    url = joinPath [server, indexName, ociString]
    method = NHTM.methodPost
    body = Nothing

openIndex :: Server -> IndexName -> IO Reply
openIndex = openOrCloseIndexes OpenIndex

closeIndex :: Server -> IndexName -> IO Reply
closeIndex = openOrCloseIndexes CloseIndex

type MappingName = String

createMapping :: ToJSON a => Server -> IndexName
                 -> MappingName -> a -> IO Reply
createMapping (Server server) indexName mappingName mapping =
  dispatch url method body where
    url = joinPath [server, indexName, mappingName, "_mapping"]
    method = NHTM.methodPut
    body = Just $ encode mapping

deleteMapping :: Server -> IndexName -> MappingName -> IO Reply
deleteMapping (Server server) indexName mappingName =
  dispatch url method body where
    url = joinPath [server, indexName, mappingName, "_mapping"]
    method = NHTM.methodDelete
    body = Nothing

type DocumentID = String

indexDocument :: ToJSON doc => Server -> IndexName -> MappingName
                 -> doc -> DocumentID -> IO Reply
indexDocument (Server server) indexName mappingName document docId =
  dispatch url method body where
    url = joinPath [server, indexName, mappingName, docId]
    method = NHTM.methodPut
    body = Just (encode document)

deleteDocument :: Server -> IndexName -> MappingName
                  -> DocumentID -> IO Reply
deleteDocument (Server server) indexName mappingName docId =
  dispatch url method body where
    url = joinPath [server, indexName, mappingName, docId]
    method = NHTM.methodDelete
    body = Nothing

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
               -> DocumentID -> IO Reply
getDocument (Server server) indexName mappingName docId =
  dispatch url method body where
    url = joinPath [server, indexName, mappingName, docId]
    method = NHTM.methodGet
    body = Nothing

documentExists :: Server -> IndexName -> MappingName
                  -> DocumentID -> IO Bool
documentExists (Server server) indexName mappingName docId = do
  (reply, exists) <- existentialQuery url
  return exists where
    url = joinPath [server, indexName, mappingName, docId]


dispatchSearch :: String -> Search -> IO Reply
dispatchSearch url search = dispatch url NHTM.methodPost (Just (encode search))

searchAll :: Server -> Search -> IO Reply
searchAll (Server server) search = dispatchSearch url search where
  url = joinPath [server, "_search"]

searchByIndex :: Server -> IndexName -> Search -> IO Reply
searchByIndex (Server server) indexName search = dispatchSearch url search where
  url = joinPath [server, indexName, "_search"]

searchByType :: Server -> IndexName -> MappingName -> Search -> IO Reply
searchByType (Server server) indexName mappingName search = dispatchSearch url search where
  url = joinPath [server, indexName, mappingName, "_search"]

refreshIndex :: Server -> IndexName -> IO Reply
refreshIndex (Server server) indexName = dispatch url method body where
  url = joinPath [server, indexName, "_refresh"]
  method = NHTM.methodPost
  body = Nothing

data Search = Search { queryBody  :: Maybe Query
                     , filterBody :: Maybe Filter } deriving (Show)

instance ToJSON Search where
  toJSON (Search query filters) = object [--"query" .= fmap toJSON query,
                                          "filter" .= fmap toJSON filters]

type QueryString = Text
-- status:active
-- author:"John Smith"
-- for book.title and book.date containing quick OR brown, book.\*:(quick brown)
-- field title has no value or doesn't exist, _missing_:title
-- field title has any non-null value, _exists:title

data BooleanOperator = AND | OR deriving (Show)

type DisMax = Bool -- "use_dis_max"
newtype TieBreaker = TieBreaker Int deriving (Show)
data QueryField = DefaultField Text
                | Fields [Text] DisMax TieBreaker deriving (Show)

-- this is will go away later
type Query = QueryStringQuery

data QueryStringQuery =
  QueryStringQuery { query                     :: QueryString
                     -- default _all
                   , field                     :: Maybe QueryField
                     -- default OR
                   , defaultOperator           :: Maybe BooleanOperator
                     -- analyzer name
                   , analyzer                  :: Maybe Text
                     -- default true
                   , allowLeadingWildcard      :: Maybe Bool
                     -- default true
                   , lowercaseExpandedTerms    :: Maybe Bool
                     -- default true
                   , enablePositionIncrements  :: Maybe Bool
                     -- default 50
                   , fuzzyMaxExpansions        :: Maybe Int
                     -- Fuzziness -- default AUTO, add type later
                   , fuzziness                 :: Maybe Text
                   , fuzzyPrefixLength         :: Maybe Int    -- default 0
                     -- default 0, 0 means exact phrase matches
                   , phraseSlop                :: Maybe Int
                     -- default 1.0
                   , boost                     :: Maybe Double
                     -- default false, true forces wildcard analysis
                   , analyzeWildcard           :: Maybe Bool
                     -- default false
                   , autoGeneratePhraseQueries :: Maybe Bool
                     -- # "should" clauses in the boolean query should match
                   , minimumShouldMatch        :: Maybe Text
                     -- Text to handle weird % and other cases. Needs type
                     -- default false, true shuts off format based failures
                   , lenient                   :: Maybe Bool
                     -- default ROOT, locale used for string conversions
                   , locale                    :: Maybe Text
                     } deriving (Show)

emptyQueryStringQuery = QueryStringQuery "" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
queryStringQuery query = emptyQueryStringQuery { query = query }

type FieldName = Text

type Cache = Bool -- caching on/off
defaultCache = False

data Filter = AndFilter [Filter] Cache
            | OrFilter [Filter] Cache
            | IdentityFilter
            | BoolFilter BoolMatch
            | ExistsFilter FieldName -- always cached
            | GeoBoundingBoxFilter GeoBoundingBoxConstraint GeoFilterType Cache
            | GeoDistanceFilter GeoConstraint Distance Cache
              deriving (Show)

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
  toJSON (IdentityFilter) =
    object ["match_all" .= object []]
  toJSON (ExistsFilter fieldName) =
    object ["exists"  .= object
            ["field"  .= fieldName]]
  toJSON (BoolFilter boolMatch) =
    object ["bool"    .= toJSON boolMatch]

data Term = Term { termField :: Text
                 , termValue :: Text } deriving (Show)

instance ToJSON Term where
  toJSON (Term field value) = object ["term" .= object
                                      [field .= value]]

data BoolMatch = MustMatch    Term  Cache
               | MustNotMatch Term  Cache
               | ShouldMatch [Term] Cache deriving (Show)

instance ToJSON BoolMatch where
  toJSON (MustMatch    term  cache)  = object ["must"     .= toJSON term,
                                               "_cache" .= cache]
  toJSON (MustNotMatch term  cache)  = object ["must_not" .= toJSON term,
                                               "_cache" .= cache]
  toJSON (ShouldMatch  terms cache) = object ["should"   .= fmap toJSON terms,
                                              "_cache" .= cache]

-- "memory" or "indexed"
data GeoFilterType = GeoFilterMemory | GeoFilterIndexed deriving (Show)

data LatLon = LatLon { lat :: Double
                     , lon :: Double } deriving (Show)

data GeoBoundingBox =
  GeoBoundingBox { topLeft     :: LatLon
                 , bottomRight :: LatLon } deriving (Show)

data GeoBoundingBoxConstraint =
  GeoBoundingBoxConstraint { geoBBField    :: FieldName
                           , constraintBox :: GeoBoundingBox
                           } deriving (Show)

data GeoConstraint =
  GeoConstraint { geoField :: FieldName
                , latLon   :: LatLon } deriving (Show)

data DistanceUnits = Miles
                   | Yards
                   | Feet
                   | Inches
                   | Kilometers
                   | Meters
                   | Centimeters
                   | Millimeters
                   | NauticalMiles deriving (Show)

data DistanceType = Arc | SloppyArc | Plane deriving (Show)

-- geo_point?
data OptimizeBbox = GeoFilterType | NoOptimizeBbox deriving (Show)

data Distance =
  Distance { coefficient :: Double
           , unit        :: DistanceUnits } deriving (Show)

data FromJSON a => SearchResult a =
  SearchResult { took       :: Int
               , timedOut   :: Bool
               , shards     :: ShardResult
               , searchHits :: SearchHits a } deriving (Show)

type Score = Double

data FromJSON a => SearchHits a =
  SearchHits { hitsTotal :: Int
             , maxScore  :: Score
             , hits      :: [Hit a] } deriving (Show)

data FromJSON a => Hit a =
  Hit { hitIndex      :: IndexName
      , hitType       :: MappingName
      , hitDocumentID :: DocumentID
      , hitScore      :: Score
      , hitSource     :: a } deriving (Show)

data ShardResult =
  ShardResult { shardTotal       :: Int
              , shardsSuccessful :: Int
              , shardsFailed     :: Int } deriving (Show, Generic)

instance (FromJSON a, ToJSON a) => FromJSON (SearchResult a) where
  parseJSON (Object v) = SearchResult <$>
                         v .: "took" <*>
                         v .: "timed_out" <*>
                         v .: "_shards" <*>
                         v .: "hits"
  parseJSON _          = empty

instance (FromJSON a, ToJSON a) => FromJSON (SearchHits a) where
  parseJSON (Object v) = SearchHits <$>
                         v .: "total" <*>
                         v .: "max_score"  <*>
                         v .: "hits"
  parseJSON _          = empty

instance (FromJSON a, ToJSON a) => FromJSON (Hit a) where
  parseJSON (Object v) = Hit <$>
                         v .: "_index" <*>
                         v .: "_type"  <*>
                         v .: "_id" <*>
                         v .: "_score" <*>
                         v .: "_source"
  parseJSON _          = empty

instance FromJSON ShardResult where
  parseJSON (Object v) = ShardResult <$>
                         v .: "total"      <*>
                         v .: "successful" <*>
                         v .: "failed"
  parseJSON _          = empty
