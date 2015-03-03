{-# LANGUAGE OverloadedStrings #-}

-------------------------------------------------------------------------------
-- |
-- Module : Database.Bloodhound.Client
-- Copyright : (C) 2014 Chris Allen
-- License : BSD-style (see the file LICENSE)
-- Maintainer : Chris Allen <cma@bitemyapp.com
-- Stability : provisional
-- Portability : OverloadedStrings
--
-- Client side functions for talking to Elasticsearch servers.
--
-------------------------------------------------------------------------------

module Database.Bloodhound.Client
       ( -- * Bloodhound client functions
         -- | The examples in this module assume the following code has been run.
         --   The :{ and :} will only work in GHCi. You'll only need the data types
         --   and typeclass instances for the functions that make use of them.

         -- $setup

         createIndex
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

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.ByteString.Lazy.Builder
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

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XDeriveGeneric
-- >>> import Database.Bloodhound
-- >>> import Test.DocTest.Prop (assert)
-- >>> let testServer = (Server "http://localhost:9200")
-- >>> let testIndex = IndexName "twitter"
-- >>> let testMapping = MappingName "tweet"
-- >>> let defaultIndexSettings = IndexSettings (ShardCount 3) (ReplicaCount 2)
-- >>> data TweetMapping = TweetMapping deriving (Eq, Show)
-- >>> _ <- deleteIndex testServer testIndex
-- >>> _ <- deleteMapping testServer testIndex testMapping
-- >>> import GHC.Generics
-- >>> import           Data.Time.Calendar        (Day (..))
-- >>> import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
-- >>> :{
--instance ToJSON TweetMapping where
--          toJSON TweetMapping =
--            object ["tweet" .=
--              object ["properties" .=
--                object ["location" .=
--                  object ["type" .= ("geo_point" :: Text)]]]]
--data Location = Location { lat :: Double
--                         , lon :: Double } deriving (Eq, Generic, Show)
--data Tweet = Tweet { user     :: Text
--                    , postDate :: UTCTime
--                    , message  :: Text
--                    , age      :: Int
--                    , location :: Location } deriving (Eq, Generic, Show)
--exampleTweet = Tweet { user     = "bitemyapp"
--                      , postDate = UTCTime
--                                   (ModifiedJulianDay 55000)
--                                   (secondsToDiffTime 10)
--                      , message  = "Use haskell!"
--                      , age      = 10000
--                      , location = Location 40.12 (-71.34) }
--instance ToJSON   Tweet
--instance FromJSON Tweet
--instance ToJSON   Location
--instance FromJSON Location
--data BulkTest = BulkTest { name :: Text } deriving (Eq, Generic, Show)
--instance FromJSON BulkTest
--instance ToJSON BulkTest
-- :}

-- | 'mkShardCount' is a straight-forward smart constructor for 'ShardCount'
--   which rejects 'Int' values below 1 and above 1000.
--
-- >>> mkShardCount 10
-- Just (ShardCount 10)
mkShardCount :: Int -> Maybe ShardCount
mkShardCount n
  | n < 1 = Nothing
  | n > 1000 = Nothing
  | otherwise = Just (ShardCount n)

-- | 'mkReplicaCount' is a straight-forward smart constructor for 'ReplicaCount'
--   which rejects 'Int' values below 1 and above 1000.
--
-- >>> mkReplicaCount 10
-- Just (ReplicaCount 10)
mkReplicaCount :: Int -> Maybe ReplicaCount
mkReplicaCount n
  | n < 1 = Nothing
  | n > 1000 = Nothing -- ...
  | otherwise = Just (ReplicaCount n)

emptyBody :: L.ByteString
emptyBody = L.pack ""

dispatch :: MonadBloodhound m => Method -> String -> Maybe L.ByteString
            -> m Reply
dispatch dMethod url body = do
  initReq <- liftIO $ parseUrl url
  let reqBody = RequestBodyLBS $ fromMaybe emptyBody body
  let req = initReq { method = dMethod
                    , requestBody = reqBody
                    , checkStatus = \_ _ _ -> Nothing}
  mgr <- bhManager <$> getBHEnv
  liftIO $ httpLbs req mgr

joinPath' :: [String] -> String
joinPath' = intercalate "/"

joinPath :: MonadBloodhound m => [String] -> m String
joinPath ps = do
  Server s <- bhServer <$> getBHEnv
  return $ joinPath' (s:ps)

bindM2 :: (Applicative m, Monad m) => (a -> b -> m c) -> m a -> m b -> m c
bindM2 f ma mb = join (f <$> ma <*> mb)


-- Shortcut functions for HTTP methods
delete :: MonadBloodhound m => String -> m Reply
delete = flip (dispatch NHTM.methodDelete) Nothing
get    :: MonadBloodhound m => String -> m Reply
get    = flip (dispatch NHTM.methodGet) Nothing
head   :: MonadBloodhound m => String -> m Reply
head   = flip (dispatch NHTM.methodHead) Nothing
put    :: MonadBloodhound m => String -> Maybe L.ByteString -> m Reply
put    = dispatch NHTM.methodPost
post   :: MonadBloodhound m => String -> Maybe L.ByteString -> m Reply
post   = dispatch NHTM.methodPost

-- indexDocument s ix name doc = put (root </> s </> ix </> name </> doc) (Just encode doc)
-- http://hackage.haskell.org/package/http-client-lens-0.1.0/docs/Network-HTTP-Client-Lens.html
-- https://github.com/supki/libjenkins/blob/master/src/Jenkins/Rest/Internal.hs

-- | 'getStatus' fetches the 'Status' of a 'Server'
--
-- >>> serverStatus <- getStatus testServer
-- >>> fmap status (serverStatus)
-- Just 200
getStatus :: MonadBloodhound m => m (Maybe Status)
getStatus = do
  url <- joinPath []
  request <- liftIO $ parseUrl url
  mgr <- bhManager <$> getBHEnv
  response <- liftIO $ httpLbs request mgr
  return $ decode (responseBody response)

-- | 'createIndex' will create an index given a 'Server', 'IndexSettings', and an 'IndexName'.
--
-- >>> response <- createIndex testServer defaultIndexSettings (IndexName "didimakeanindex")
-- >>> respIsTwoHunna response
-- True
-- >>> indexExists testServer (IndexName "didimakeanindex")
-- True
createIndex :: MonadBloodhound m => IndexSettings -> IndexName -> m Reply
createIndex indexSettings (IndexName indexName) =
  bindM2 put url (return body)
  where url = joinPath [indexName]
        body = Just $ encode indexSettings

-- | 'deleteIndex' will delete an index given a 'Server', and an 'IndexName'.
--
-- >>> response <- createIndex testServer defaultIndexSettings (IndexName "didimakeanindex")
-- >>> response <- deleteIndex testServer (IndexName "didimakeanindex")
-- >>> respIsTwoHunna response
-- True
-- >>> indexExists testServer testIndex
-- False
deleteIndex :: MonadBloodhound m => IndexName -> m Reply
deleteIndex (IndexName indexName) =
  delete =<< joinPath [indexName]

statusCodeIs :: Int -> Reply -> Bool
statusCodeIs n resp = NHTS.statusCode (responseStatus resp) == n

respIsTwoHunna :: Reply -> Bool
respIsTwoHunna = statusCodeIs 200

existentialQuery :: MonadBloodhound m => String -> m (Reply, Bool)
existentialQuery url = do
  reply <- head url
  return (reply, respIsTwoHunna reply)

-- | 'indexExists' enables you to check if an index exists. Returns 'Bool'
--   in IO
--
-- >>> exists <- indexExists testServer testIndex
indexExists :: MonadBloodhound m => IndexName -> m Bool
indexExists (IndexName indexName) = do
  (_, exists) <- existentialQuery =<< joinPath [indexName]
  return exists

-- | 'refreshIndex' will force a refresh on an index. You must
-- do this if you want to read what you wrote.
--
-- >>> _ <- createIndex testServer defaultIndexSettings testIndex
-- >>> _ <- refreshIndex testServer testIndex
refreshIndex :: MonadBloodhound m => IndexName -> m Reply
refreshIndex (IndexName indexName) =
  bindM2 post url (return Nothing)
  where url = joinPath [indexName, "_refresh"]

stringifyOCIndex :: OpenCloseIndex -> String
stringifyOCIndex oci = case oci of
  OpenIndex  -> "_open"
  CloseIndex -> "_close"

openOrCloseIndexes :: MonadBloodhound m => OpenCloseIndex -> IndexName -> m Reply
openOrCloseIndexes oci (IndexName indexName) =
  bindM2 post url (return Nothing)
  where ociString = stringifyOCIndex oci
        url = joinPath [indexName, ociString]

-- | 'openIndex' opens an index given a 'Server' and an 'IndexName'. Explained in further detail at 
--   <http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/indices-open-close.html>
--
-- >>> reply <- openIndex testServer testIndex
openIndex :: MonadBloodhound m => IndexName -> m Reply
openIndex = openOrCloseIndexes OpenIndex

-- | 'closeIndex' closes an index given a 'Server' and an 'IndexName'. Explained in further detail at 
--   <http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/indices-open-close.html>
--
-- >>> reply <- closeIndex testServer testIndex
closeIndex :: MonadBloodhound m => IndexName -> m Reply
closeIndex = openOrCloseIndexes CloseIndex

-- | 'putMapping' is an HTTP PUT and has upsert semantics. Mappings are schemas
-- for documents in indexes.
--
-- >>> _ <- createIndex testServer defaultIndexSettings testIndex
-- >>> resp <- putMapping testServer testIndex testMapping TweetMapping
-- >>> print resp
-- Response {responseStatus = Status {statusCode = 200, statusMessage = "OK"}, responseVersion = HTTP/1.1, responseHeaders = [("Content-Type","application/json; charset=UTF-8"),("Content-Length","21")], responseBody = "{\"acknowledged\":true}", responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}
putMapping :: (MonadBloodhound m, ToJSON a) => IndexName
                 -> MappingName -> a -> m Reply
putMapping (IndexName indexName) (MappingName mappingName) mapping =
  bindM2 put url (return body)
  where url = joinPath [indexName, mappingName, "_mapping"]
        body = Just $ encode mapping

-- | 'deleteMapping' is an HTTP DELETE and deletes a mapping for a given index.
-- Mappings are schemas for documents in indexes.
--
-- >>> _ <- createIndex testServer defaultIndexSettings testIndex
-- >>> _ <- putMapping testServer testIndex testMapping TweetMapping
-- >>> resp <- deleteMapping testServer testIndex testMapping
-- >>> print resp
-- Response {responseStatus = Status {statusCode = 200, statusMessage = "OK"}, responseVersion = HTTP/1.1, responseHeaders = [("Content-Type","application/json; charset=UTF-8"),("Content-Length","21")], responseBody = "{\"acknowledged\":true}", responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}
deleteMapping :: MonadBloodhound m => IndexName -> MappingName -> m Reply
deleteMapping (IndexName indexName)
  (MappingName mappingName) =
  delete =<< joinPath [indexName, mappingName, "_mapping"]

-- | 'indexDocument' is the primary way to save a single document in
--   Elasticsearch. The document itself is simply something we can
--   convert into a JSON 'Value'. The 'DocId' will function as the
--   primary key for the document.
--
-- >>> resp <- indexDocument testServer testIndex testMapping exampleTweet (DocId "1")
-- >>> print resp
-- Response {responseStatus = Status {statusCode = 201, statusMessage = "Created"}, responseVersion = HTTP/1.1, responseHeaders = [("Content-Type","application/json; charset=UTF-8"),("Content-Length","74")], responseBody = "{\"_index\":\"twitter\",\"_type\":\"tweet\",\"_id\":\"1\",\"_version\":1,\"created\":true}", responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}
indexDocument :: (ToJSON doc, MonadBloodhound m) => IndexName -> MappingName
                 -> doc -> DocId -> m Reply
indexDocument (IndexName indexName)
  (MappingName mappingName) document (DocId docId) =
  bindM2 put url (return body)
  where url = joinPath [indexName, mappingName, docId]
        body = Just (encode document)

-- | 'deleteDocument' is the primary way to delete a single document.
--
-- >>> _ <- deleteDocument testServer testIndex testMapping (DocId "1")
deleteDocument :: MonadBloodhound m => IndexName -> MappingName
                  -> DocId -> m Reply
deleteDocument (IndexName indexName)
  (MappingName mappingName) (DocId docId) =
  delete =<< joinPath [indexName, mappingName, docId]

-- | 'bulk' uses
--    <http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/docs-bulk.html Elasticsearch's bulk API>
--    to perform bulk operations. The 'BulkOperation' data type encodes the
--    index/update/delete/create operations. You pass a 'V.Vector' of 'BulkOperation's
--    and a 'Server' to 'bulk' in order to send those operations up to your Elasticsearch
--    server to be performed. I changed from [BulkOperation] to a Vector due to memory overhead.
--
-- >>> let stream = V.fromList [BulkIndex testIndex testMapping (DocId "2") (toJSON (BulkTest "blah"))]
-- >>> _ <- bulk testServer stream
-- >>> _ <- refreshIndex testServer testIndex
bulk :: MonadBloodhound m => V.Vector BulkOperation -> m Reply
bulk bulkOps = bindM2 post url (return body)
  where url = joinPath ["_bulk"]
        body = Just $ encodeBulkOperations bulkOps

-- | 'encodeBulkOperations' is a convenience function for dumping a vector of 'BulkOperation'
--   into an 'L.ByteString'
--
-- >>> let bulkOps = V.fromList [BulkIndex testIndex testMapping (DocId "2") (toJSON (BulkTest "blah"))]
-- >>> encodeBulkOperations bulkOps
-- "\n{\"index\":{\"_type\":\"tweet\",\"_id\":\"2\",\"_index\":\"twitter\"}}\n{\"name\":\"blah\"}\n"
encodeBulkOperations :: V.Vector BulkOperation -> L.ByteString
encodeBulkOperations stream = collapsed where
  blobs = fmap encodeBulkOperation stream
  mashedTaters = mash (mempty :: Builder) blobs
  collapsed = toLazyByteString $ mappend mashedTaters (byteString "\n")

mash :: Builder -> V.Vector L.ByteString -> Builder
mash = V.foldl' (\b x -> b `mappend` (byteString "\n") `mappend` (lazyByteString x))

mkBulkStreamValue :: Text -> String -> String -> String -> Value
mkBulkStreamValue operation indexName mappingName docId =
  object [operation .=
          object [ "_index" .= indexName
                 , "_type"  .= mappingName
                 , "_id"    .= docId]]

-- | 'encodeBulkOperation' is a convenience function for dumping a single 'BulkOperation'
--   into an 'L.ByteString'
--
-- >>> let bulkOp = BulkIndex testIndex testMapping (DocId "2") (toJSON (BulkTest "blah"))
-- >>> encodeBulkOperation bulkOp
-- "{\"index\":{\"_type\":\"tweet\",\"_id\":\"2\",\"_index\":\"twitter\"}}\n{\"name\":\"blah\"}"
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

-- | 'getDocument' is a straight-forward way to fetch a single document from
--   Elasticsearch using a 'Server', 'IndexName', 'MappingName', and a 'DocId'.
--   The 'DocId' is the primary key for your Elasticsearch document.
--
-- >>> yourDoc <- getDocument testServer testIndex testMapping (DocId "1")
getDocument :: MonadBloodhound m => IndexName -> MappingName
               -> DocId -> m Reply
getDocument (IndexName indexName)
  (MappingName mappingName) (DocId docId) =
  get =<< joinPath [indexName, mappingName, docId]

-- | 'documentExists' enables you to check if a document exists. Returns 'Bool'
--   in IO
--
-- >>> exists <- documentExists testServer testIndex testMapping (DocId "1")
documentExists :: MonadBloodhound m => IndexName -> MappingName
                  -> DocId -> m Bool
documentExists (IndexName indexName)
  (MappingName mappingName) (DocId docId) = do
  (_, exists) <- existentialQuery =<< url
  return exists
  where url = joinPath [indexName, mappingName, docId]

dispatchSearch :: MonadBloodhound m => String -> Search -> m Reply
dispatchSearch url search = post url (Just (encode search))

-- | 'searchAll', given a 'Search', will perform that search against all indexes
--   on an Elasticsearch server. Try to avoid doing this if it can be helped.
--
-- >>> let query = TermQuery (Term "user" "bitemyapp") Nothing
-- >>> let search = mkSearch (Just query) Nothing
-- >>> reply <- searchAll testServer search
searchAll :: MonadBloodhound m => Search -> m Reply
searchAll = bindM2 dispatchSearch url . return
  where url = joinPath ["_search"]

-- | 'searchByIndex', given a 'Search' and an 'IndexName', will perform that search
--   against all mappings within an index on an Elasticsearch server.
--
-- >>> let query = TermQuery (Term "user" "bitemyapp") Nothing
-- >>> let search = mkSearch (Just query) Nothing
-- >>> reply <- searchByIndex testServer testIndex search
searchByIndex :: MonadBloodhound m => IndexName -> Search -> m Reply
searchByIndex (IndexName indexName) = bindM2 dispatchSearch url . return
  where url = joinPath [indexName, "_search"]

-- | 'searchByType', given a 'Search', 'IndexName', and 'MappingName', will perform that
--   search against a specific mapping within an index on an Elasticsearch server.
--
-- >>> let query = TermQuery (Term "user" "bitemyapp") Nothing
-- >>> let search = mkSearch (Just query) Nothing
-- >>> reply <- searchByType testServer testIndex testMapping search
searchByType :: MonadBloodhound m => IndexName -> MappingName -> Search
                -> m Reply
searchByType (IndexName indexName)
  (MappingName mappingName) = bindM2 dispatchSearch url . return
  where url = joinPath [indexName, mappingName, "_search"]

-- | 'mkSearch' is a helper function for defaulting additional fields of a 'Search'
--   to Nothing in case you only care about your 'Query' and 'Filter'. Use record update
--   syntax if you want to add things like aggregations or highlights while still using
--   this helper function.
--
-- >>> let query = TermQuery (Term "user" "bitemyapp") Nothing
-- >>> mkSearch (Just query) Nothing
-- Search {queryBody = Just (TermQuery (Term {termField = "user", termValue = "bitemyapp"}) Nothing), filterBody = Nothing, sortBody = Nothing, aggBody = Nothing, highlight = Nothing, trackSortScores = False, from = 0, size = 10}
mkSearch :: Maybe Query -> Maybe Filter -> Search
mkSearch query filter = Search query filter Nothing Nothing Nothing False 0 10

-- | 'mkAggregateSearch' is a helper function that defaults everything in a 'Search' except for
--   the 'Query' and the 'Aggregation'.
--
-- >>> let terms = TermsAgg $ (mkTermsAggregation "user") { termCollectMode = Just BreadthFirst }
-- >>> terms
-- TermsAgg (TermsAggregation {term = Left "user", termInclude = Nothing, termExclude = Nothing, termOrder = Nothing, termMinDocCount = Nothing, termSize = Nothing, termShardSize = Nothing, termCollectMode = Just BreadthFirst, termExecutionHint = Nothing, termAggs = Nothing})
-- >>> let myAggregation = mkAggregateSearch Nothing $ mkAggregations "users" terms
mkAggregateSearch :: Maybe Query -> Aggregations -> Search
mkAggregateSearch query mkSearchAggs = Search query Nothing Nothing (Just mkSearchAggs) Nothing False 0 0

-- | 'mkHighlightSearch' is a helper function that defaults everything in a 'Search' except for
--   the 'Query' and the 'Aggregation'.
--
-- >>> let query = QueryMatchQuery $ mkMatchQuery (FieldName "_all") (QueryString "haskell")
-- >>> let testHighlight = Highlights Nothing [FieldHighlight (FieldName "message") Nothing]
-- >>> let search = mkHighlightSearch (Just query) testHighlight
mkHighlightSearch :: Maybe Query -> Highlights -> Search
mkHighlightSearch query searchHighlights = Search query Nothing Nothing Nothing (Just searchHighlights) False 0 10

-- | 'pageSearch' is a helper function that takes a search and assigns the from
--    and size fields for the search. The from parameter defines the offset
--    from the first result you want to fetch. The size parameter allows you to
--    configure the maximum amount of hits to be returned.
--
-- >>> let query = QueryMatchQuery $ mkMatchQuery (FieldName "_all") (QueryString "haskell")
-- >>> let search = mkSearch (Just query) Nothing
-- >>> search
-- Search {queryBody = Just (QueryMatchQuery (MatchQuery {matchQueryField = FieldName "_all", matchQueryQueryString = QueryString "haskell", matchQueryOperator = Or, matchQueryZeroTerms = ZeroTermsNone, matchQueryCutoffFrequency = Nothing, matchQueryMatchType = Nothing, matchQueryAnalyzer = Nothing, matchQueryMaxExpansions = Nothing, matchQueryLenient = Nothing})), filterBody = Nothing, sortBody = Nothing, aggBody = Nothing, highlight = Nothing, trackSortScores = False, from = 0, size = 10}
-- >>> pageSearch 10 100 search
-- Search {queryBody = Just (QueryMatchQuery (MatchQuery {matchQueryField = FieldName "_all", matchQueryQueryString = QueryString "haskell", matchQueryOperator = Or, matchQueryZeroTerms = ZeroTermsNone, matchQueryCutoffFrequency = Nothing, matchQueryMatchType = Nothing, matchQueryAnalyzer = Nothing, matchQueryMaxExpansions = Nothing, matchQueryLenient = Nothing})), filterBody = Nothing, sortBody = Nothing, aggBody = Nothing, highlight = Nothing, trackSortScores = False, from = 10, size = 100}
pageSearch :: Int     -- ^ The result offset
           -> Int     -- ^ The number of results to return
           -> Search  -- ^ The current seach
           -> Search  -- ^ The paged search
pageSearch resultOffset pageSize search = search { from = resultOffset, size = pageSize }
