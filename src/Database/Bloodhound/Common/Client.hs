{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-warnings-deprecations #-}

-- |
-- Module : Database.Bloodhound.Client
-- Copyright : (C) 2014, 2018 Chris Allen
-- License : BSD-style (see the file LICENSE)
-- Maintainer : Chris Allen <cma@bitemyapp.com>
-- Stability : provisional
-- Portability : GHC
--
-- Client side functions for talking to Elasticsearch servers.
module Database.Bloodhound.Common.Client
  ( -- * Bloodhound client functions

    -- | The examples in this module assume the following code has been run.
    --   The :{ and :} will only work in GHCi. You'll only need the data types
    --   and typeclass instances for the functions that make use of them.
    -- $setup
    withBH,

    -- ** Indices
    createIndex,
    createIndexWith,
    flushIndex,
    deleteIndex,
    updateIndexSettings,
    getIndexSettings,
    forceMergeIndex,
    indexExists,
    openIndex,
    closeIndex,
    listIndices,
    catIndices,
    waitForYellowIndex,
    Requests.HealthStatus (..),

    -- *** Index Aliases
    updateIndexAliases,
    getIndexAliases,
    deleteIndexAlias,

    -- *** Index Templates
    putTemplate,
    templateExists,
    deleteTemplate,

    -- ** Mapping
    putMapping,

    -- ** Documents
    indexDocument,
    updateDocument,
    updateByQuery,
    Requests.getDocument,
    documentExists,
    deleteDocument,
    deleteByQuery,
    Requests.IndexedDocument (..),
    Requests.DeletedDocuments (..),
    Requests.DeletedDocumentsRetries (..),

    -- ** Searching
    searchAll,
    searchByIndex,
    searchByIndices,
    searchByIndexTemplate,
    searchByIndicesTemplate,
    scanSearch,
    getInitialScroll,
    getInitialSortedScroll,
    advanceScroll,
    refreshIndex,
    Requests.mkSearch,
    Requests.mkAggregateSearch,
    Requests.mkHighlightSearch,
    Requests.mkSearchTemplate,
    bulk,
    Requests.pageSearch,
    Requests.mkShardCount,
    Requests.mkReplicaCount,
    getStatus,

    -- ** Templates
    storeSearchTemplate,
    getSearchTemplate,
    deleteSearchTemplate,

    -- ** Snapshot/Restore

    -- *** Snapshot Repos
    getSnapshotRepos,
    updateSnapshotRepo,
    verifySnapshotRepo,
    deleteSnapshotRepo,

    -- *** Snapshots
    createSnapshot,
    getSnapshots,
    deleteSnapshot,

    -- *** Restoring Snapshots
    restoreSnapshot,

    -- *** Reindex
    reindex,
    reindexAsync,

    -- *** Task
    getTask,

    -- ** Nodes
    getNodesInfo,
    getNodesStats,

    -- ** Request Utilities
    Requests.encodeBulkOperations,
    Requests.encodeBulkOperation,

    -- * Authentication
    basicAuthHook,

    -- * Count
    countByIndex,

    -- * Generic
    Acknowledged (..),
    Accepted (..),
    IgnoredBody (..),
  )
where

import Control.Monad
import Control.Monad.Catch
import Data.Aeson
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text.Encoding as T
import Data.Time.Clock
import qualified Data.Vector as V
import Database.Bloodhound.Client.Cluster
import qualified Database.Bloodhound.Common.Requests as Requests
import Database.Bloodhound.Common.Types
import Network.HTTP.Client hiding (Proxy)
import Prelude hiding (filter, head)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XDeriveGeneric
-- >>> import Database.Bloodhound
-- >>> import Network.HTTP.Client
-- >>> let testServer = (Server "http://localhost:9200")
-- >>> let runBH' = withBH defaultManagerSettings testServer
-- >>> let testIndex = IndexName "twitter"
-- >>> let defaultIndexSettings = IndexSettings (ShardCount 1) (ReplicaCount 0)
-- >>> data TweetMapping = TweetMapping deriving stock (Eq, Show)
-- >>> _ <- runBH' $ deleteIndex testIndex
-- >>> _ <- runBH' $ deleteIndex (IndexName "didimakeanindex")
-- >>> import GHC.Generics
-- >>> import           Data.Time.Calendar        (Day (..))
-- >>> import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
-- >>> :{
-- instance ToJSON TweetMapping where
--          toJSON TweetMapping =
--            object ["properties" .=
--              object ["location" .=
--                object ["type" .= ("geo_point" :: Text)]]]
-- data Location = Location { lat :: Double
--                         , lon :: Double } deriving stock (Eq, Generic, Show)
-- data Tweet = Tweet { user     :: Text
--                    , postDate :: UTCTime
--                    , message  :: Text
--                    , age      :: Int
--                    , location :: Location } deriving stock (Eq, Generic, Show)
-- exampleTweet = Tweet { user     = "bitemyapp"
--                      , postDate = UTCTime
--                                   (ModifiedJulianDay 55000)
--                                   (secondsToDiffTime 10)
--                      , message  = "Use haskell!"
--                      , age      = 10000
--                      , location = Location 40.12 (-71.34) }
-- instance ToJSON   Tweet where
--  toJSON = genericToJSON defaultOptions
-- instance FromJSON Tweet where
--  parseJSON = genericParseJSON defaultOptions
-- instance ToJSON   Location where
--  toJSON = genericToJSON defaultOptions
-- instance FromJSON Location where
--  parseJSON = genericParseJSON defaultOptions
-- data BulkTest = BulkTest { name :: Text } deriving stock (Eq, Generic, Show)
-- instance FromJSON BulkTest where
--  parseJSON = genericParseJSON defaultOptions
-- instance ToJSON BulkTest where
--  toJSON = genericToJSON defaultOptions
-- :}

-- | Convenience function that sets up a manager and BHEnv and runs
-- the given set of bloodhound operations. Connections will be
-- pipelined automatically in accordance with the given manager
-- settings in IO. If you've got your own monad transformer stack, you
-- should use 'runBH' directly.
withBH :: ManagerSettings -> Server -> BH IO a -> IO a
withBH ms s f = do
  mgr <- newManager ms
  let env = mkBHEnv s mgr
  runBH env f >>= either throwM return

-- | 'getStatus' fetches the 'Status' of a 'Server'
--
-- >>> serverStatus <- runBH' getStatus
-- >>> fmap tagline (serverStatus)
-- Just "You Know, for Search"
getStatus :: (MonadBH m) => m Status
getStatus = performBHRequest $ Requests.getStatus

-- | 'getSnapshotRepos' gets the definitions of a subset of the
-- defined snapshot repos.
getSnapshotRepos :: (MonadBH m) => SnapshotRepoSelection -> m [GenericSnapshotRepo]
getSnapshotRepos sel = performBHRequest $ Requests.getSnapshotRepos sel

-- | Create or update a snapshot repo
updateSnapshotRepo ::
  (MonadBH m) =>
  (SnapshotRepo repo) =>
  -- | Use 'defaultSnapshotRepoUpdateSettings' if unsure
  SnapshotRepoUpdateSettings ->
  repo ->
  m Acknowledged
updateSnapshotRepo settings repo = performBHRequest $ Requests.updateSnapshotRepo settings repo

-- | Verify if a snapshot repo is working. __NOTE:__ this API did not
-- make it into Elasticsearch until 1.4. If you use an older version,
-- you will get an error here.
verifySnapshotRepo :: (MonadBH m) => SnapshotRepoName -> m SnapshotVerification
verifySnapshotRepo repoName = performBHRequest $ Requests.verifySnapshotRepo repoName

deleteSnapshotRepo :: (MonadBH m) => SnapshotRepoName -> m Acknowledged
deleteSnapshotRepo repoName = performBHRequest $ Requests.deleteSnapshotRepo repoName

-- | Create and start a snapshot
createSnapshot ::
  (MonadBH m) =>
  SnapshotRepoName ->
  SnapshotName ->
  SnapshotCreateSettings ->
  m Acknowledged
createSnapshot repoName snapName settings = performBHRequest $ Requests.createSnapshot repoName snapName settings

-- | Get info about known snapshots given a pattern and repo name.
getSnapshots :: (MonadBH m) => SnapshotRepoName -> SnapshotSelection -> m [SnapshotInfo]
getSnapshots repoName sel = performBHRequest $ Requests.getSnapshots repoName sel

-- | Delete a snapshot. Cancels if it is running.
deleteSnapshot :: (MonadBH m) => SnapshotRepoName -> SnapshotName -> m Acknowledged
deleteSnapshot repoName snapName = performBHRequest $ Requests.deleteSnapshot repoName snapName

-- | Restore a snapshot to the cluster See
-- <https://www.elastic.co/guide/en/elasticsearch/reference/1.7/modules-snapshots.html#_restore>
-- for more details.
restoreSnapshot ::
  (MonadBH m) =>
  SnapshotRepoName ->
  SnapshotName ->
  -- | Start with 'defaultSnapshotRestoreSettings' and customize
  -- from there for reasonable defaults.
  SnapshotRestoreSettings ->
  m Accepted
restoreSnapshot repoName snapName settings = performBHRequest $ Requests.restoreSnapshot repoName snapName settings

getNodesInfo :: (MonadBH m) => NodeSelection -> m NodesInfo
getNodesInfo sel = performBHRequest $ Requests.getNodesInfo sel

getNodesStats :: (MonadBH m) => NodeSelection -> m NodesStats
getNodesStats sel = performBHRequest $ Requests.getNodesStats sel

-- | 'createIndex' will create an index given a 'Server', 'IndexSettings', and an 'IndexName'.
--
-- >>> response <- runBH' $ createIndex defaultIndexSettings (IndexName "didimakeanindex")
-- >>> isSuccess response
-- True
-- >>> runBH' $ indexExists (IndexName "didimakeanindex")
-- True
createIndex :: (MonadBH m) => IndexSettings -> IndexName -> m Acknowledged
createIndex indexSettings indexName = performBHRequest $ Requests.createIndex indexSettings indexName

-- | Create an index, providing it with any number of settings. This
--   is more expressive than 'createIndex' but makes is more verbose
--   for the common case of configuring only the shard count and
--   replica count.
createIndexWith ::
  (MonadBH m) =>
  [UpdatableIndexSetting] ->
  -- | shard count
  Int ->
  IndexName ->
  m Acknowledged
createIndexWith updates shards indexName = performBHRequest $ Requests.createIndexWith updates shards indexName

-- | 'flushIndex' will flush an index given a 'Server' and an 'IndexName'.
flushIndex :: (MonadBH m) => IndexName -> m ShardResult
flushIndex indexName = performBHRequest $ Requests.flushIndex indexName

-- | 'deleteIndex' will delete an index given a 'Server' and an 'IndexName'.
--
-- >>> _ <- runBH' $ createIndex defaultIndexSettings (IndexName "didimakeanindex")
-- >>> response <- runBH' $ deleteIndex (IndexName "didimakeanindex")
-- >>> isSuccess response
-- True
-- >>> runBH' $ indexExists (IndexName "didimakeanindex")
-- False
deleteIndex :: (MonadBH m) => IndexName -> m Acknowledged
deleteIndex indexName = performBHRequest $ Requests.deleteIndex indexName

-- | 'updateIndexSettings' will apply a non-empty list of setting updates to an index
--
-- >>> _ <- runBH' $ createIndex defaultIndexSettings (IndexName "unconfiguredindex")
-- >>> response <- runBH' $ updateIndexSettings (BlocksWrite False :| []) (IndexName "unconfiguredindex")
-- >>> isSuccess response
-- True
updateIndexSettings ::
  (MonadBH m) =>
  NonEmpty UpdatableIndexSetting ->
  IndexName ->
  m Acknowledged
updateIndexSettings updates indexName = performBHRequest $ Requests.updateIndexSettings updates indexName

getIndexSettings :: (MonadBH m) => IndexName -> m IndexSettingsSummary
getIndexSettings indexName = performBHRequest $ Requests.getIndexSettings indexName

-- | 'forceMergeIndex'
--
-- The force merge API allows to force merging of one or more indices through
-- an API. The merge relates to the number of segments a Lucene index holds
-- within each shard. The force merge operation allows to reduce the number of
-- segments by merging them.
--
-- This call will block until the merge is complete. If the http connection is
-- lost, the request will continue in the background, and any new requests will
-- block until the previous force merge is complete.

-- For more information see
-- <https://www.elastic.co/guide/en/elasticsearch/reference/current/indices-forcemerge.html#indices-forcemerge>.
-- Nothing
-- worthwhile comes back in the response body, so matching on the status
-- should suffice.
--
-- 'forceMergeIndex' with a maxNumSegments of 1 and onlyExpungeDeletes
-- to True is the main way to release disk space back to the OS being
-- held by deleted documents.
--
-- >>> let ixn = IndexName "unoptimizedindex"
-- >>> _ <- runBH' $ deleteIndex ixn >> createIndex defaultIndexSettings ixn
-- >>> response <- runBH' $ forceMergeIndex (IndexList (ixn :| [])) (defaultIndexOptimizationSettings { maxNumSegments = Just 1, onlyExpungeDeletes = True })
-- >>> isSuccess response
-- True
forceMergeIndex :: (MonadBH m) => IndexSelection -> ForceMergeIndexSettings -> m ShardsResult
forceMergeIndex ixs settings = performBHRequest $ Requests.forceMergeIndex ixs settings

-- | 'indexExists' enables you to check if an index exists. Returns 'Bool'
--   in IO
--
-- >>> exists <- runBH' $ indexExists testIndex
indexExists :: (MonadBH m) => IndexName -> m Bool
indexExists indexName = performBHRequest $ Requests.indexExists indexName

-- | 'refreshIndex' will force a refresh on an index. You must
-- do this if you want to read what you wrote.
--
-- >>> _ <- runBH' $ createIndex defaultIndexSettings testIndex
-- >>> _ <- runBH' $ refreshIndex testIndex
refreshIndex :: (MonadBH m) => IndexName -> m ShardResult
refreshIndex indexName = performBHRequest $ Requests.refreshIndex indexName

-- | Block until the index becomes available for indexing
--   documents. This is useful for integration tests in which
--   indices are rapidly created and deleted.
waitForYellowIndex :: (MonadBH m) => IndexName -> m Requests.HealthStatus
waitForYellowIndex indexName = performBHRequest $ Requests.waitForYellowIndex indexName

-- | 'openIndex' opens an index given a 'Server' and an 'IndexName'. Explained in further detail at
--   <http://www.elastic.co/guide/en/elasticsearch/reference/current/indices-open-close.html>
--
-- >>> response <- runBH' $ openIndex testIndex
openIndex :: (MonadBH m) => IndexName -> m Acknowledged
openIndex indexName = performBHRequest $ Requests.openIndex indexName

-- | 'closeIndex' closes an index given a 'Server' and an 'IndexName'. Explained in further detail at
--   <http://www.elastic.co/guide/en/elasticsearch/reference/current/indices-open-close.html>
--
-- >>> response <- runBH' $ closeIndex testIndex
closeIndex :: (MonadBH m) => IndexName -> m Acknowledged
closeIndex indexName = performBHRequest $ Requests.closeIndex indexName

-- | 'listIndices' returns a list of all index names on a given 'Server'
listIndices :: (MonadBH m) => m [IndexName]
listIndices = performBHRequest $ Requests.listIndices

-- | 'catIndices' returns a list of all index names on a given 'Server' as well as their doc counts
catIndices :: (MonadBH m) => m [(IndexName, Int)]
catIndices = performBHRequest $ Requests.catIndices

-- | 'updateIndexAliases' updates the server's index alias
-- table. Operations are atomic. Explained in further detail at
-- <https://www.elastic.co/guide/en/elasticsearch/reference/current/indices-aliases.html>
--
-- >>> let src = IndexName "a-real-index"
-- >>> let aliasName = IndexName "an-alias"
-- >>> let iAlias = IndexAlias src (IndexAliasName aliasName)
-- >>> let aliasCreate = IndexAliasCreate Nothing Nothing
-- >>> _ <- runBH' $ deleteIndex src
-- >>> isSuccess <$> runBH' (createIndex defaultIndexSettings src)
-- True
-- >>> runBH' $ indexExists src
-- True
-- >>> isSuccess <$> runBH' (updateIndexAliases (AddAlias iAlias aliasCreate :| []))
-- True
-- >>> runBH' $ indexExists aliasName
-- True
updateIndexAliases :: (MonadBH m) => NonEmpty IndexAliasAction -> m Acknowledged
updateIndexAliases actions = performBHRequest $ Requests.updateIndexAliases actions

-- | Get all aliases configured on the server.
getIndexAliases :: (MonadBH m) => m IndexAliasesSummary
getIndexAliases = performBHRequest $ Requests.getIndexAliases

-- | Delete a single alias, removing it from all indices it
--   is currently associated with.
deleteIndexAlias :: (MonadBH m) => IndexAliasName -> m Acknowledged
deleteIndexAlias indexAliasName = performBHRequest $ Requests.deleteIndexAlias indexAliasName

-- | 'putTemplate' creates a template given an 'IndexTemplate' and a 'TemplateName'.
--   Explained in further detail at
--   <https://www.elastic.co/guide/en/elasticsearch/reference/1.7/indices-templates.html>
--
--   >>> let idxTpl = IndexTemplate [IndexPattern "tweet-*"] (Just (IndexSettings (ShardCount 1) (ReplicaCount 1))) [toJSON TweetMapping]
--   >>> resp <- runBH' $ putTemplate idxTpl (TemplateName "tweet-tpl")
putTemplate :: (MonadBH m) => IndexTemplate -> TemplateName -> m Acknowledged
putTemplate indexTemplate templateName = performBHRequest $ Requests.putTemplate indexTemplate templateName

-- | 'templateExists' checks to see if a template exists.
--
--   >>> exists <- runBH' $ templateExists (TemplateName "tweet-tpl")
templateExists :: (MonadBH m) => TemplateName -> m Bool
templateExists templateName = performBHRequest $ Requests.templateExists templateName

-- | 'deleteTemplate' is an HTTP DELETE and deletes a template.
--
--   >>> let idxTpl = IndexTemplate [IndexPattern "tweet-*"] (Just (IndexSettings (ShardCount 1) (ReplicaCount 1))) [toJSON TweetMapping]
--   >>> _ <- runBH' $ putTemplate idxTpl (TemplateName "tweet-tpl")
--   >>> resp <- runBH' $ deleteTemplate (TemplateName "tweet-tpl")
deleteTemplate :: (MonadBH m) => TemplateName -> m Acknowledged
deleteTemplate templateName = performBHRequest $ Requests.deleteTemplate templateName

-- | 'putMapping' is an HTTP PUT and has upsert semantics. Mappings are schemas
-- for documents in indexes.
--
-- >>> _ <- runBH' $ createIndex defaultIndexSettings testIndex
-- >>> resp <- runBH' $ putMapping testIndex TweetMapping
-- >>> print resp
-- Response {responseStatus = Status {statusCode = 200, statusMessage = "OK"}, responseVersion = HTTP/1.1, responseHeaders = [("content-type","application/json; charset=UTF-8"),("content-encoding","gzip"),("transfer-encoding","chunked")], responseBody = "{\"acknowledged\":true}", responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}
putMapping :: forall r a m. (MonadBH m, FromJSON r, ToJSON a) => IndexName -> a -> m r
putMapping indexName mapping = performBHRequest $ Requests.putMapping indexName mapping
{-# DEPRECATED putMapping "See <https://www.elastic.co/guide/en/elasticsearch/reference/7.17/removal-of-types.html>" #-}

-- | 'indexDocument' is the primary way to save a single document in
--   Elasticsearch. The document itself is simply something we can
--   convert into a JSON 'Value'. The 'DocId' will function as the
--   primary key for the document. You are encouraged to generate
--   your own id's and not rely on Elasticsearch's automatic id
--   generation. Read more about it here:
--   https://github.com/bitemyapp/bloodhound/issues/107
--
-- >>> resp <- runBH' $ indexDocument testIndex defaultIndexDocumentSettings exampleTweet (DocId "1")
-- >>> print resp
-- Response {responseStatus = Status {statusCode = 200, statusMessage = "OK"}, responseVersion = HTTP/1.1, responseHeaders = [("content-type","application/json; charset=UTF-8"),("content-encoding","gzip"),("content-length","152")], responseBody = "{\"_index\":\"twitter\",\"_type\":\"_doc\",\"_id\":\"1\",\"_version\":2,\"result\":\"updated\",\"_shards\":{\"total\":1,\"successful\":1,\"failed\":0},\"_seq_no\":1,\"_primary_term\":1}", responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}
indexDocument ::
  forall doc m.
  (MonadBH m, ToJSON doc) =>
  IndexName ->
  IndexDocumentSettings ->
  doc ->
  DocId ->
  m Requests.IndexedDocument
indexDocument indexName cfg document docId = performBHRequest $ Requests.indexDocument indexName cfg document docId

-- | 'updateDocument' provides a way to perform an partial update of a
-- an already indexed document.
updateDocument ::
  forall patch m.
  (MonadBH m, ToJSON patch) =>
  IndexName ->
  IndexDocumentSettings ->
  patch ->
  DocId ->
  m Requests.IndexedDocument
updateDocument indexName cfg patch docId = performBHRequest $ Requests.updateDocument indexName cfg patch docId

updateByQuery :: (MonadBH m, FromJSON a) => IndexName -> Query -> Maybe Script -> m a
updateByQuery indexName q mScript = performBHRequest $ Requests.updateByQuery indexName q mScript

-- | 'deleteDocument' is the primary way to delete a single document.
--
-- >>> _ <- runBH' $ deleteDocument testIndex (DocId "1")
deleteDocument :: (MonadBH m) => IndexName -> DocId -> m Requests.IndexedDocument
deleteDocument indexName docId = performBHRequest $ Requests.deleteDocument indexName docId

-- | 'deleteByQuery' performs a deletion on every document that matches a query.
--
-- >>> let query = TermQuery (Term "user" "bitemyapp") Nothing
-- >>> _ <- runBH' $ deleteDocument testIndex query
deleteByQuery :: (MonadBH m) => IndexName -> Query -> m Requests.DeletedDocuments
deleteByQuery indexName query = performBHRequest $ Requests.deleteByQuery indexName query

-- | 'bulk' uses
--    <http://www.elastic.co/guide/en/elasticsearch/reference/current/docs-bulk.html Elasticsearch's bulk API>
--    to perform bulk operations. The 'BulkOperation' data type encodes the
--    index\/update\/delete\/create operations. You pass a 'V.Vector' of 'BulkOperation's
--    and a 'Server' to 'bulk' in order to send those operations up to your Elasticsearch
--    server to be performed. I changed from [BulkOperation] to a Vector due to memory overhead.
--
-- >>> let stream = V.fromList [BulkIndex testIndex (DocId "2") (toJSON (BulkTest "blah"))]
-- >>> _ <- runBH' $ bulk stream
-- >>> _ <- runBH' $ refreshIndex testIndex
bulk ::
  forall m.
  (MonadBH m) =>
  V.Vector BulkOperation ->
  m BulkResponse
bulk ops = performBHRequest $ Requests.bulk @StatusDependant ops

-- | 'documentExists' enables you to check if a document exists.
documentExists :: (MonadBH m) => IndexName -> DocId -> m Bool
documentExists indexName docId = performBHRequest $ Requests.documentExists indexName docId

-- | 'searchAll', given a 'Search', will perform that search against all indexes
--   on an Elasticsearch server. Try to avoid doing this if it can be helped.
--
-- >>> let query = TermQuery (Term "user" "bitemyapp") Nothing
-- >>> let search = mkSearch (Just query) Nothing
-- >>> response <- runBH' $ searchAll search
searchAll :: forall a m. (MonadBH m, FromJSON a) => Search -> m (SearchResult a)
searchAll search = performBHRequest $ Requests.searchAll search

-- | 'searchByIndex', given a 'Search' and an 'IndexName', will perform that search
--   within an index on an Elasticsearch server.
--
-- >>> let query = TermQuery (Term "user" "bitemyapp") Nothing
-- >>> let search = mkSearch (Just query) Nothing
-- >>> response <- runBH' $ searchByIndex testIndex search
searchByIndex :: forall a m. (MonadBH m, FromJSON a) => IndexName -> Search -> m (SearchResult a)
searchByIndex indexName search = performBHRequest $ Requests.searchByIndex indexName search

-- | 'searchByIndices' is a variant of 'searchByIndex' that executes a
--   'Search' over many indices. This is much faster than using
--   'mapM' to 'searchByIndex' over a collection since it only
--   causes a single HTTP request to be emitted.
searchByIndices :: forall a m. (MonadBH m, FromJSON a) => NonEmpty IndexName -> Search -> m (SearchResult a)
searchByIndices ixs search = performBHRequest $ Requests.searchByIndices ixs search

-- | 'searchByIndexTemplate', given a 'SearchTemplate' and an 'IndexName', will perform that search
--   within an index on an Elasticsearch server.
--
-- >>> let query = SearchTemplateSource "{\"query\": { \"match\" : { \"{{my_field}}\" : \"{{my_value}}\" } }, \"size\" : \"{{my_size}}\"}"
-- >>> let search = mkSearchTemplate (Right query) Nothing
-- >>> response <- runBH' $ searchByIndexTemplate testIndex search
searchByIndexTemplate ::
  forall a m.
  (MonadBH m, FromJSON a) =>
  IndexName ->
  SearchTemplate ->
  m (SearchResult a)
searchByIndexTemplate indexName search = performBHRequest $ Requests.searchByIndexTemplate indexName search

-- | 'searchByIndicesTemplate' is a variant of 'searchByIndexTemplate' that executes a
--   'SearchTemplate' over many indices. This is much faster than using
--   'mapM' to 'searchByIndexTemplate' over a collection since it only
--   causes a single HTTP request to be emitted.
searchByIndicesTemplate ::
  forall a m.
  (MonadBH m, FromJSON a) =>
  NonEmpty IndexName ->
  SearchTemplate ->
  m (SearchResult a)
searchByIndicesTemplate ixs search = performBHRequest $ Requests.searchByIndicesTemplate ixs search

-- | 'storeSearchTemplate', saves a 'SearchTemplateSource' to be used later.
storeSearchTemplate :: (MonadBH m) => SearchTemplateId -> SearchTemplateSource -> m Acknowledged
storeSearchTemplate tid ts = performBHRequest $ Requests.storeSearchTemplate tid ts

-- | 'getSearchTemplate', get info of an stored 'SearchTemplateSource'.
getSearchTemplate :: (MonadBH m) => SearchTemplateId -> m GetTemplateScript
getSearchTemplate tid = performBHRequest $ Requests.getSearchTemplate tid

-- | 'storeSearchTemplate',
deleteSearchTemplate :: (MonadBH m) => SearchTemplateId -> m Acknowledged
deleteSearchTemplate tid = performBHRequest $ Requests.deleteSearchTemplate tid

-- | For a given search, request a scroll for efficient streaming of
-- search results. Note that the search is put into 'SearchTypeScan'
-- mode and thus results will not be sorted. Combine this with
-- 'advanceScroll' to efficiently stream through the full result set
getInitialScroll ::
  forall a m.
  (MonadBH m, FromJSON a) =>
  IndexName ->
  Search ->
  m (ParsedEsResponse (SearchResult a))
getInitialScroll indexName search' = performBHRequest $ Requests.getInitialScroll indexName search'

-- | For a given search, request a scroll for efficient streaming of
-- search results. Combine this with 'advanceScroll' to efficiently
-- stream through the full result set. Note that this search respects
-- sorting and may be less efficient than 'getInitialScroll'.
getInitialSortedScroll ::
  forall a m.
  (MonadBH m, FromJSON a) =>
  IndexName ->
  Search ->
  m (SearchResult a)
getInitialSortedScroll indexName search = performBHRequest $ Requests.getInitialSortedScroll indexName search

-- | Use the given scroll to fetch the next page of documents. If there are no
-- further pages, 'SearchResult.searchHits.hits' will be '[]'.
advanceScroll ::
  forall a m.
  (MonadBH m, FromJSON a) =>
  ScrollId ->
  -- | How long should the snapshot of data be kept around? This timeout is updated every time 'advanceScroll' is used, so don't feel the need to set it to the entire duration of your search processing. Note that durations < 1s will be rounded up. Also note that 'NominalDiffTime' is an instance of Num so literals like 60 will be interpreted as seconds. 60s is a reasonable default.
  NominalDiffTime ->
  m (SearchResult a)
advanceScroll sid scroll = performBHRequest $ Requests.advanceScroll sid scroll

-- | 'scanSearch' uses the 'scroll' API of elastic,
-- for a given 'IndexName'. Note that this will
-- consume the entire search result set and will be doing O(n) list
-- appends so this may not be suitable for large result sets. In that
-- case, 'getInitialScroll' and 'advanceScroll' are good low level
-- tools. You should be able to hook them up trivially to conduit,
-- pipes, or your favorite streaming IO abstraction of choice. Note
-- that ordering on the search would destroy performance and thus is
-- ignored.
scanSearch :: forall a m. (FromJSON a, MonadBH m) => IndexName -> Search -> m [Hit a]
scanSearch indexName search = do
  initialSearchResult <- getInitialScroll indexName search
  let (hits', josh) = case initialSearchResult of
        Right SearchResult {..} -> (hits searchHits, scrollId)
        Left _ -> ([], Nothing)
  (totalHits, _) <- scanAccumulator [] (hits', josh)
  return totalHits
  where
    scanAccumulator :: [Hit a] -> ([Hit a], Maybe ScrollId) -> m ([Hit a], Maybe ScrollId)
    scanAccumulator oldHits (newHits, Nothing) = return (oldHits ++ newHits, Nothing)
    scanAccumulator oldHits ([], _) = return (oldHits, Nothing)
    scanAccumulator oldHits (newHits, msid) = do
      (newHits', msid') <- scroll' msid
      scanAccumulator (oldHits ++ newHits) (newHits', msid')

    scroll' :: Maybe ScrollId -> m ([Hit a], Maybe ScrollId)
    scroll' Nothing = return ([], Nothing)
    scroll' (Just sid) = do
      res <- tryPerformBHRequest $ Requests.advanceScroll sid 60
      case res of
        Right SearchResult {..} -> return (hits searchHits, scrollId)
        Left _ -> return ([], Nothing)

-- | This is a hook that can be set via the 'bhRequestHook' function
-- that will authenticate all requests using an HTTP Basic
-- Authentication header. Note that it is *strongly* recommended that
-- this option only be used over an SSL connection.
--
-- >> (mkBHEnv myServer myManager) { bhRequestHook = basicAuthHook (EsUsername "myuser") (EsPassword "mypass") }
basicAuthHook :: (Monad m) => EsUsername -> EsPassword -> Request -> m Request
basicAuthHook (EsUsername u) (EsPassword p) = return . applyBasicAuth u' p'
  where
    u' = T.encodeUtf8 u
    p' = T.encodeUtf8 p

countByIndex :: (MonadBH m) => IndexName -> CountQuery -> m CountResponse
countByIndex indexName q = performBHRequest $ Requests.countByIndex indexName q

reindex :: (MonadBH m) => ReindexRequest -> m ReindexResponse
reindex = performBHRequest . Requests.reindex

reindexAsync :: (MonadBH m) => ReindexRequest -> m TaskNodeId
reindexAsync = performBHRequest . Requests.reindexAsync

getTask :: (MonadBH m, FromJSON a) => TaskNodeId -> m (TaskResponse a)
getTask = performBHRequest . Requests.getTask
