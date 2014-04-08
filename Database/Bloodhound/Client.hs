{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Bloodhound.Client
       ( createIndex
       , deleteIndex
       , IndexSettings(..)
       , Server(..)
       , defaultIndexSettings
       , indexDocument
       , getDocument
       , documentExists
       , deleteDocument
       , EsResult(..)
       )
       where

import Control.Applicative
import Control.Monad (liftM)
import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
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

-- instance ToJSON (Status a)
-- instance FromJSON (Status a)

-- bloo <- (liftM responseBody $ parseUrl "http://localhost:9200/" >>= \r -> withManager (httpLbs r))

-- rootPath :: String
-- rootPath = rollUp $ ["/"]

-- -- Kinda hate this.
-- pathFromType :: String -> String -> String
-- pathFromType index docType = "/" ++ index ++ docType

-- -- Kinda hate this too.
-- rollUp :: [String] -> String
-- rollUp = Prelude.concat

main = simpleHttp "http://localhost:9200/events/event/_search?q=hostname:localhost&size=1" >>= L.putStrLn

-- data Response = Response { blah :: Text } deriving (Show)

-- indexDocument :: ToJSON a => a -> IO Response
-- indexDocument doc = ToJSON a

-- nomenclature is because you're dropping the type baggage. All of this is compile-time erased.
-- newtype ShardCount   = ShardCount   { unShardCount   :: Int } deriving (Show, Generic)
-- newtype ReplicaCount = ReplicaCount { unReplicaCount :: Int } deriving (Show, Generic)

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

-- IndexSettings <$> mkShardCount 10 <*> mkReplicaCount 20

data IndexSettings = IndexSettings { shards   :: ShardCount
                                   , replicas :: ReplicaCount } deriving (Show, Generic)

instance ToJSON IndexSettings where
  toJSON (IndexSettings s r) = object ["settings" .= object ["shards" .= s, "replicas" .= r]]

instance ToJSON ReplicaCount
instance ToJSON ShardCount

defaultIndexSettings = IndexSettings (ShardCount 3) (ReplicaCount 2)

data Strategy = RoundRobinStrat | RandomStrat | HeadStrat deriving (Show)

newtype Server = Server String deriving (Show)

-- data Errors = IndexAlreadyExistsError | GenericError deriving (Show)

-- data ElasticsearchError = ElasticsearchError
--                           { status :: Int
--                           , error  :: Text } deriving (Show, Generic)

-- instance FromJSON ElasticsearchError

-- errorForResponse resp = do
--   let (sts, err) = decode (responseBody resp) :: ElasticsearchError
--   if T.isInfixOf "IndexAlreadyExistsException" error
--     then IndexAlreadyExistsError
--     else 

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
                           , found    :: Bool
                           , _source  :: a } deriving (Eq, Show)

instance (FromJSON a, ToJSON a) => FromJSON (EsResult a) where
  parseJSON (Object v) = EsResult <$>
                         v .: "_index"   <*>
                         v .: "_type"    <*>
                         v .: "_id"      <*>
                         v .: "_version" <*>
                         v .: "found"    <*>
                         v .: "_source"
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

-- data Analyzer = AStandardAnalyzer StandardAnalyzer
--               | SimpleAnalyzer -- just "simple"
--               | WhitespaceAnalyzer
--               | StopAnalyzer
--               | KeywordAnalyzer
--               | PatternAnalyzer
--               | LanguageAnalyzer
--               | SnowballAnalyzer
--               | CustomAnalyzer

-- data StandardAnalyzer =
--   StandardAnalyzer
--   { stopwords        :: Maybe [Text] -- default []
--   , max_token_length :: Maybe Int -- default 255
--   }

-- DefaultField -> "default_field": ""
-- Fields -> "fields": []

type QueryString = Text
-- status:active
-- author:"John Smith"
-- for book.title and book.date containing quick OR brown, book.\*:(quick brown)
-- field title has no value or doesn't exist, _missing_:title
-- field title has any non-null value, _exists:title

data BooleanOperator = AND | OR deriving (Show)

type DisMax = Bool -- "use_dis_max"
newtype TieBreaker = TieBreaker Int deriving (Show)
data QueryField = DefaultField Text | Fields [Text] DisMax TieBreaker deriving (Show)

data QueryStringQuery =
  QueryStringQuery { query                     :: QueryString
                   , field     :: Maybe QueryField      -- default _all
                   , defaultOperator           :: Maybe BooleanOperator -- default OR
                   , analyzer                  :: Maybe Text   -- analyzer name
                   , allowLeadingWildcard      :: Maybe Bool   -- default true
                   , lowercaseExpandedTerms    :: Maybe Bool   -- default true
                   , enablePositionIncrements  :: Maybe Bool   -- default true
                   , fuzzyMaxExpansions        :: Maybe Int    -- default 50
                   , fuzziness                 :: Maybe Text   -- Fuzziness -- default AUTO, add type later
                   , fuzzyPrefixLength         :: Maybe Int    -- default 0
                   , phraseSlop                :: Maybe Int    -- default 0, 0 means exact phrase matches
                   , boost                     :: Maybe Double -- default 1.0
                   , analyzeWildcard           :: Maybe Bool   -- default false, true forces wildcard analysis
                   , autoGeneratePhraseQueries :: Maybe Bool   -- default false
                   , minimumShouldMatch        :: Maybe Text   -- # "should" clauses in the boolean query should match
                                                               -- Text to handle weird % and other cases. Needs type
                   , lenient                   :: Maybe Bool   -- default false, true shuts off format based failures
                   , locale                    :: Maybe Text   -- default ROOT, locale used for string conversions
                     } deriving (Show)

emptyQueryStringQuery = QueryStringQuery "" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
queryStringQuery query = emptyQueryStringQuery { query = query }

-- ugh
type FieldName = Text

type Cache = Bool -- caching on/off
data Filter = AndFilter [Filter] (Maybe Cache)
            | OrFilter [Filter] (Maybe Cache)
            | BoolFilter BoolMatch (Maybe Cache)
            | ExistsFilter FieldName -- always cached
            | GeoBoundingBoxFilter GeoBoundingBoxConstraint GeoFilterType (Maybe Cache)
            | GeoDistanceFilter GeoConstraint Distance (Maybe Cache)
              deriving (Show)

-- I dunno.
data Term = Term { termField :: Text
                 , termValue :: Text } deriving (Show)

data BoolMatch = MustMatch Term
               | MustNotMatch Term
               | ShouldMatch [Term] deriving (Show)

-- "memory" or "indexed"
data GeoFilterType = GeoFilterMemory | GeoFilterIndexed deriving (Show)

data LatLon = LatLon { lat :: Double, lon :: Double } deriving (Show)
data GeoBoundingBox =
  GeoBoundingBox { topLeft :: LatLon
                 , bottomRight :: LatLon } deriving (Show)

data GeoBoundingBoxConstraint =
  GeoBoundingBoxConstraint { geoBBField :: FieldName
                           , constraintBox :: GeoBoundingBox
                           } deriving (Show)

data GeoConstraint =
  GeoConstraint { geoField :: FieldName
                , latLon :: LatLon } deriving (Show)

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
           , unit :: DistanceUnits } deriving (Show)

-- This is turning into a fractal of horror
-- data Fuzziness = FDateFuzziness DateFuzziness |

-- data Query = Query { query :: () } deriving (Show)
-- search :: Server -> IndexName -> Maybe MappingName
--           -> Query -> IO Reply
-- search = fuck

-- getStatus :: String -> IO (Maybe (Status Version))
-- getStatus server = do
--   request <- parseUrl $ server ++ rootPath
--   response <- withManager $ httpLbs request
--   return $ (decode $ responseBody response)

-- mkServer -- (Maybe Server) for URL parsing?

-- data Cluster = Cluster { targets :: [Server]
--                        , strategy :: Strategy } deriving (Show)

-- target :: Cluster -> Server
-- target (Cluster targets HeadStrat) = head targets
