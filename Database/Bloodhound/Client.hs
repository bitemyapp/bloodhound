{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Bloodhound.Client
       ( createIndex
       , deleteIndex
       , IndexSettings(..)
       , Server(..)
       , defaultIndexSettings
       , indexDocument
       )
       where

import Control.Applicative
import Control.Monad (liftM)
import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time.Clock as DTC
import GHC.Generics (Generic)
import Network.HTTP.Conduit
import qualified Network.HTTP.Types.Method as NHTM
import qualified Network.HTTP.Types.Status as NHTS

data Version = Version { number          :: T.Text
                       , build_hash      :: T.Text
                       , build_timestamp :: DTC.UTCTime
                       , build_snapshot  :: Bool
                       , lucene_version  :: T.Text } deriving (Show, Generic)

data (FromJSON a, ToJSON a) => Status a =
                     Status { ok      :: Bool
                            , status  :: Int
                            , name    :: T.Text
                            , version :: a
                            , tagline :: T.Text } deriving (Show)

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

-- data Response = Response { blah :: T.Text } deriving (Show)

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
--                           , error  :: T.Text } deriving (Show, Generic)

-- instance FromJSON ElasticsearchError

-- errorForResponse resp = do
--   let (sts, err) = decode (responseBody resp) :: ElasticsearchError
--   if T.isInfixOf "IndexAlreadyExistsException" error
--     then IndexAlreadyExistsError
--     else 

type Reply = Network.HTTP.Conduit.Response L.ByteString
type Method = NHTM.Method

responseIsError :: Reply -> Bool
responseIsError resp = if (NHTS.statusCode (responseStatus resp)) > 299
                       then True
                       else False

emptyBody = L.pack ""

dispatch :: String -> Method -> Maybe L.ByteString
            -> IO Reply
dispatch url method body = do
  initReq <- parseUrl url
  let reqBody = RequestBodyLBS $ fromMaybe emptyBody body
  let req = initReq { method = method
                    , requestBody = reqBody
                    , checkStatus = \_ _ _ -> Nothing}
  response <- withManager $ httpLbs req
  return response

type IndexName = String

joinPath :: [String] -> String
joinPath path = concat $ intersperse "/" path

getStatus :: Server -> IO (Maybe (Status Version))
getStatus (Server server) = do
  request <- parseUrl $ joinPath [server]
  response <- withManager $ httpLbs request
  return $ (decode $ responseBody response)

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
respIsTwoHunna resp = (NHTS.statusCode $ responseStatus resp) == 200

indexExists :: Server -> IndexName -> IO Bool
indexExists (Server server) indexName =
  exists where
    url = joinPath [server, indexName]
    method = NHTM.methodHead
    body = Nothing
    reply = dispatch url method body
    exists = fmap respIsTwoHunna reply 

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

getDocument :: Server -> IndexName -> MappingName
               -> DocumentID -> IO Reply
getDocument (Server server) indexName mappingName docId =
  dispatch url method body where
    url = joinPath [server, indexName, mappingName, docId]
    method = NHTM.methodGet
    body = Nothing

documentExists :: Server -> IndexName -> MappingName
                  -> DocumentID -> IO Bool
documentExists (Server server) indexName mappingName docId =
  exists where
    url = joinPath [server, indexName, mappingName, docId]
    method = NHTM.methodHead
    body = Nothing
    reply = dispatch url method body
    exists = fmap respIsTwoHunna reply

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
