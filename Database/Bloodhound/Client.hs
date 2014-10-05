module Database.Bloodhound.Client
       ( createIndex
       , deleteIndex
       , indexExists
       , openIndex
       , closeIndex
       , putMapping
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
       , pageSearch
       , mkShardCount
       , mkReplicaCount
       , getStatus
       )
       where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L
import Data.ByteString.Builder
import Data.List (foldl', intercalate, intersperse)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Network.HTTP.Client
import qualified Network.HTTP.Types.Method as NHTM
import qualified Network.HTTP.Types.Status as NHTS
import Prelude hiding (head, filter)

import Database.Bloodhound.Types

-- find way to avoid destructuring Servers and Indexes?
-- make get, post, put, delete helpers.
-- make dispatch take URL last for better variance and
-- utilization of partial application

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

emptyBody :: L.ByteString
emptyBody = L.pack ""

dispatch :: Method -> String -> Maybe L.ByteString
            -> IO Reply
dispatch dMethod url body = do
  initReq <- parseUrl url
  let reqBody = RequestBodyLBS $ fromMaybe emptyBody body
  let req = initReq { method = dMethod
                    , requestBody = reqBody
                    , checkStatus = \_ _ _ -> Nothing}
  withManager defaultManagerSettings $ httpLbs req

joinPath :: [String] -> String
joinPath = intercalate "/"

-- Shortcut functions for HTTP methods
delete :: String -> IO Reply
delete = flip (dispatch NHTM.methodDelete) Nothing
get    :: String -> IO Reply
get    = flip (dispatch NHTM.methodGet) Nothing
head   :: String -> IO Reply
head   = flip (dispatch NHTM.methodHead) Nothing
put    :: String -> Maybe L.ByteString -> IO Reply
put    = dispatch NHTM.methodPost
post   :: String -> Maybe L.ByteString -> IO Reply
post   = dispatch NHTM.methodPost

-- indexDocument s ix name doc = put (root </> s </> ix </> name </> doc) (Just encode doc)
-- http://hackage.haskell.org/package/http-client-lens-0.1.0/docs/Network-HTTP-Client-Lens.html
-- https://github.com/supki/libjenkins/blob/master/src/Jenkins/Rest/Internal.hs

getStatus :: Server -> IO (Maybe Status)
getStatus (Server server) = do
  request <- parseUrl $ joinPath [server]
  response <- withManager defaultManagerSettings $ httpLbs request
  return $ decode (responseBody response)

createIndex :: Server -> IndexSettings -> IndexName -> IO Reply
createIndex (Server server) indexSettings (IndexName indexName) =
  put url body
  where url = joinPath [server, indexName]
        body = Just $ encode indexSettings

deleteIndex :: Server -> IndexName -> IO Reply
deleteIndex (Server server) (IndexName indexName) =
  delete $ joinPath [server, indexName]

respIsTwoHunna :: Reply -> Bool
respIsTwoHunna resp = NHTS.statusCode (responseStatus resp) == 200

existentialQuery :: String -> IO (Reply, Bool)
existentialQuery url = do
  reply <- head url
  return (reply, respIsTwoHunna reply)

indexExists :: Server -> IndexName -> IO Bool
indexExists (Server server) (IndexName indexName) = do
  (_, exists) <- existentialQuery url
  return exists
  where url = joinPath [server, indexName]

refreshIndex :: Server -> IndexName -> IO Reply
refreshIndex (Server server) (IndexName indexName) =
  post url Nothing
  where url = joinPath [server, indexName, "_refresh"]

stringifyOCIndex :: OpenCloseIndex -> String
stringifyOCIndex oci = case oci of
  OpenIndex  -> "_open"
  CloseIndex -> "_close"

openOrCloseIndexes :: OpenCloseIndex -> Server -> IndexName -> IO Reply
openOrCloseIndexes oci (Server server) (IndexName indexName) =
  post url Nothing
  where ociString = stringifyOCIndex oci
        url = joinPath [server, indexName, ociString]

openIndex :: Server -> IndexName -> IO Reply
openIndex = openOrCloseIndexes OpenIndex

closeIndex :: Server -> IndexName -> IO Reply
closeIndex = openOrCloseIndexes CloseIndex

{-| putMapping is an HTTP PUT and has upsert semantics. Mappings are schemas
    for documents in indexes.
-}
putMapping :: ToJSON a => Server -> IndexName
                 -> MappingName -> a -> IO Reply
putMapping (Server server) (IndexName indexName) (MappingName mappingName) mapping =
  put url body
  where url = joinPath [server, indexName, mappingName, "_mapping"]
        body = Just $ encode mapping

deleteMapping :: Server -> IndexName -> MappingName -> IO Reply
deleteMapping (Server server) (IndexName indexName)
  (MappingName mappingName) =
  delete $ joinPath [server, indexName, mappingName, "_mapping"]

indexDocument :: ToJSON doc => Server -> IndexName -> MappingName
                 -> doc -> DocId -> IO Reply
indexDocument (Server server) (IndexName indexName)
  (MappingName mappingName) document (DocId docId) =
  put url body
  where url = joinPath [server, indexName, mappingName, docId]
        body = Just (encode document)

deleteDocument :: Server -> IndexName -> MappingName
                  -> DocId -> IO Reply
deleteDocument (Server server) (IndexName indexName)
  (MappingName mappingName) (DocId docId) =
  delete $ joinPath [server, indexName, mappingName, docId]

bulk :: Server -> [BulkOperation] -> IO Reply
bulk (Server server) bulkOps = post url body where
  url = joinPath [server, "_bulk"]
  body = Just $ collapseStream bulkOps

collapseStream :: [BulkOperation] -> L.ByteString
collapseStream stream = collapsed where
  blobs = intersperse "\n" $ concat $ fmap getStreamChunk stream
  mashedTaters = mash (mempty :: Builder) blobs
  collapsed = toLazyByteString $ mappend mashedTaters (byteString "\n")

mash :: Builder -> [L.ByteString] -> Builder
mash = foldl' (\b x -> mappend b (lazyByteString x))

mkMetadataValue :: Text -> String -> String -> String -> Value
mkMetadataValue operation indexName mappingName docId =
  object [operation .=
          object [ "_index" .= indexName
                 , "_type"  .= mappingName
                 , "_id"    .= docId]]

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

getDocument :: Server -> IndexName -> MappingName
               -> DocId -> IO Reply
getDocument (Server server) (IndexName indexName)
  (MappingName mappingName) (DocId docId) =
  get $ joinPath [server, indexName, mappingName, docId]

documentExists :: Server -> IndexName -> MappingName
                  -> DocId -> IO Bool
documentExists (Server server) (IndexName indexName)
  (MappingName mappingName) (DocId docId) = do
  (_, exists) <- existentialQuery url
  return exists where
    url = joinPath [server, indexName, mappingName, docId]

dispatchSearch :: String -> Search -> IO Reply
dispatchSearch url search = post url (Just (encode search))

searchAll :: Server -> Search -> IO Reply
searchAll (Server server) = dispatchSearch url where
  url = joinPath [server, "_search"]

searchByIndex :: Server -> IndexName -> Search -> IO Reply
searchByIndex (Server server) (IndexName indexName) = dispatchSearch url where
  url = joinPath [server, indexName, "_search"]

searchByType :: Server -> IndexName -> MappingName -> Search -> IO Reply
searchByType (Server server) (IndexName indexName)
  (MappingName mappingName) = dispatchSearch url where
  url = joinPath [server, indexName, mappingName, "_search"]

mkSearch :: Maybe Query -> Maybe Filter -> Search
mkSearch query filter = Search query filter Nothing Nothing False 0 10

pageSearch :: Int -> Int -> Search -> Search
pageSearch pageFrom pageSize search = search { from = pageFrom, size = pageSize }
