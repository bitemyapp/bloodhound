module Database.Bloodhound.Client
       ( createIndex
       , deleteIndex
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
       )
       where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L
import Data.ByteString.Builder
import Data.List (foldl', intercalate, intersperse)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Network.HTTP.Conduit
import qualified Network.HTTP.Types.Method as NHTM
import qualified Network.HTTP.Types.Status as NHTS

import Database.Bloodhound.Types
import Database.Bloodhound.Types.Class
import Database.Bloodhound.Types.Instances

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

mkSearch :: Maybe Query -> Maybe Filter -> Search
mkSearch query filter = Search query filter Nothing False 0 10

pageSearch :: Int -> Int -> Search -> Search
pageSearch from size search = search { from = from, size = size }
