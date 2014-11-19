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
       , mkAggregateSearch
       , mkHighlightSearch
       , bulk
       , pageSearch
       , mkShardCount
       , mkReplicaCount
       , getStatus
       , encodeBulkOperations
       , encodeBulkOperation
       )
       where

import           Data.Aeson
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.List                  (intercalate)
import           Data.Maybe                 (fromMaybe)
import           Data.Text                  (Text)
import qualified Data.Vector                as V
import           Network.HTTP.Client
import qualified Network.HTTP.Types.Method  as NHTM
import qualified Network.HTTP.Types.Status  as NHTS
import           Prelude                    hiding (filter, head)

import           Database.Bloodhound.Types

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

bulk :: Server -> V.Vector BulkOperation -> IO Reply
bulk (Server server) bulkOps = post url body where
  url = joinPath [server, "_bulk"]
  body = Just $ encodeBulkOperations bulkOps

encodeBulkOperations :: V.Vector BulkOperation -> L.ByteString
encodeBulkOperations stream = collapsed where
  blobs = fmap encodeBulkOperation stream
  mashedTaters = mash (mempty :: Builder) blobs
  collapsed = toLazyByteString $ mappend mashedTaters (byteString "\n")

mash :: Builder -> V.Vector L.ByteString -> Builder
mash = V.foldl' (\b x -> b `mappend` "\n" `mappend` (lazyByteString x))

mkBulkStreamValue :: Text -> String -> String -> String -> Value
mkBulkStreamValue operation indexName mappingName docId =
  object [operation .=
          object [ "_index" .= indexName
                 , "_type"  .= mappingName
                 , "_id"    .= docId]]

encodeBulkOperation :: BulkOperation -> L.ByteString
encodeBulkOperation (BulkIndex (IndexName indexName)
                (MappingName mappingName)
                (DocId docId) value) = blob
    where metadata = mkBulkStreamValue "index" indexName mappingName docId
          blob = encode metadata `mappend` "\n" `mappend` encode value

encodeBulkOperation (BulkCreate (IndexName indexName)
                (MappingName mappingName)
                (DocId docId) value) = blob
    where metadata = mkBulkStreamValue "create" indexName mappingName docId
          blob = encode metadata `mappend` "\n" `mappend` encode value

encodeBulkOperation (BulkDelete (IndexName indexName)
                (MappingName mappingName)
                (DocId docId)) = blob
    where metadata = mkBulkStreamValue "delete" indexName mappingName docId
          blob = encode metadata

encodeBulkOperation (BulkUpdate (IndexName indexName)
                (MappingName mappingName)
                (DocId docId) value) = blob
    where metadata = mkBulkStreamValue "update" indexName mappingName docId
          doc = object ["doc" .= value]
          blob = encode metadata `mappend` "\n" `mappend` encode doc


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
mkSearch query filter = Search query filter Nothing Nothing Nothing False 0 0

mkAggregateSearch :: Maybe Query -> Aggregations -> Search
mkAggregateSearch query mkSearchAggs = Search query Nothing Nothing (Just mkSearchAggs) Nothing False 0 0

mkHighlightSearch :: Maybe Query -> Highlights -> Search
mkHighlightSearch query searchHighlights = Search query Nothing Nothing Nothing (Just searchHighlights) False 0 10

pageSearch :: Int -> Int -> Search -> Search
pageSearch pageFrom pageSize search = search { from = pageFrom, size = pageSize }
