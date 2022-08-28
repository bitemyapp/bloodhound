{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module : Database.Bloodhound.Client
-- Copyright : (C) 2014, 2018 Chris Allen
-- License : BSD-style (see the file LICENSE)
-- Maintainer : Chris Allen <cma@bitemyapp.com>
-- Stability : provisional
-- Portability : GHC
--
-- Client side functions for talking to Elasticsearch servers.
module Database.Bloodhound.Client
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
    HealthStatus (..),

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
    getDocument,
    documentExists,
    deleteDocument,
    deleteByQuery,
    IndexedDocument (..),
    DeletedDocuments (..),
    DeletedDocumentsRetries (..),

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
    pitSearch,
    openPointInTime,
    closePointInTime,
    refreshIndex,
    mkSearch,
    mkAggregateSearch,
    mkHighlightSearch,
    mkSearchTemplate,
    bulk,
    pageSearch,
    mkShardCount,
    mkReplicaCount,
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

    -- ** Nodes
    getNodesInfo,
    getNodesStats,

    -- ** Request Utilities
    encodeBulkOperations,
    encodeBulkOperation,

    -- * Authentication
    basicAuthHook,

    -- * BHResponse-handling tools
    isVersionConflict,
    isSuccess,
    isCreated,
    parseEsResponse,
    parseEsResponseWith,
    decodeResponse,
    eitherDecodeResponse,

    -- * Count
    countByIndex,

    -- * Generic
    Acknowledged (..),
    Accepted (..),
  )
where

import Control.Applicative as A
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Key
import qualified Data.Aeson.KeyMap as X
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Foldable (toList)
import qualified Data.List as LS (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Time.Clock
import qualified Data.Vector as V
import Database.Bloodhound.Internal.Client.BHRequest
import Database.Bloodhound.Types
import Network.HTTP.Client
import qualified Network.HTTP.Types.Method as NHTM
import qualified Network.URI as URI
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
-- >>> data TweetMapping = TweetMapping deriving (Eq, Show)
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
--                         , lon :: Double } deriving (Eq, Generic, Show)
-- data Tweet = Tweet { user     :: Text
--                    , postDate :: UTCTime
--                    , message  :: Text
--                    , age      :: Int
--                    , location :: Location } deriving (Eq, Generic, Show)
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
-- data BulkTest = BulkTest { name :: Text } deriving (Eq, Generic, Show)
-- instance FromJSON BulkTest where
--  parseJSON = genericParseJSON defaultOptions
-- instance ToJSON BulkTest where
--  toJSON = genericToJSON defaultOptions
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
--   which rejects 'Int' values below 0 and above 1000.
--
-- >>> mkReplicaCount 10
-- Just (ReplicaCount 10)
mkReplicaCount :: Int -> Maybe ReplicaCount
mkReplicaCount n
  | n < 0 = Nothing
  | n > 1000 = Nothing -- ...
  | otherwise = Just (ReplicaCount n)

emptyBody :: L.ByteString
emptyBody = L.pack ""

dispatch ::
  MonadBH m =>
  BHRequest body ->
  m (BHResponse body)
dispatch request = do
  env <- getBHEnv
  let url = getEndpoint (bhServer env) (bhRequestEndpoint request)
  initReq <- liftIO $ parseUrl' url
  let reqHook = bhRequestHook env
  let reqBody = RequestBodyLBS $ fromMaybe emptyBody $ bhRequestBody request
  req <-
    liftIO $
      reqHook $
        setRequestIgnoreStatus $
          initReq
            { method = bhRequestMethod request,
              requestHeaders =
                -- "application/x-ndjson" for bulk
                ("Content-Type", "application/json") : requestHeaders initReq,
              requestBody = reqBody
            }
  -- req <- liftIO $ reqHook $ setRequestIgnoreStatus $ initReq { method = dMethod
  --                                                            , requestBody = reqBody }
  let mgr = bhManager env
  BHResponse <$> liftIO (httpLbs req mgr)

-- | Convenience function that sets up a manager and BHEnv and runs
-- the given set of bloodhound operations. Connections will be
-- pipelined automatically in accordance with the given manager
-- settings in IO. If you've got your own monad transformer stack, you
-- should use 'runBH' directly.
withBH :: ManagerSettings -> Server -> BH IO a -> IO a
withBH ms s f = do
  mgr <- newManager ms
  let env = mkBHEnv s mgr
  runBH env f

-- Shortcut functions for HTTP methods
delete :: MonadBH m => Endpoint -> m (BHResponse body)
delete = dispatch . mkSimpleRequest NHTM.methodDelete

deleteWithBody :: MonadBH m => Endpoint -> L.ByteString -> m (BHResponse body)
deleteWithBody endpoint = dispatch . mkFullRequest NHTM.methodDelete endpoint

get :: MonadBH m => Endpoint -> m (BHResponse body)
get = dispatch . mkSimpleRequest NHTM.methodGet

head :: MonadBH m => Endpoint -> m (BHResponse body)
head = dispatch . mkSimpleRequest NHTM.methodHead

put :: MonadBH m => Endpoint -> L.ByteString -> m (BHResponse body)
put endpoint = dispatch . mkFullRequest NHTM.methodPut endpoint

post :: MonadBH m => Endpoint -> L.ByteString -> m (BHResponse body)
post endpoint = dispatch . mkFullRequest NHTM.methodPost endpoint

-- | 'getStatus' fetches the 'Status' of a 'Server'
--
-- >>> serverStatus <- runBH' getStatus
-- >>> fmap tagline (serverStatus)
-- Just "You Know, for Search"
getStatus :: MonadBH m => m (Either String Status)
getStatus =
  eitherDecodeResponse <$> get []

-- | 'getSnapshotRepos' gets the definitions of a subset of the
-- defined snapshot repos.
getSnapshotRepos ::
  ( MonadBH m,
    MonadThrow m
  ) =>
  SnapshotRepoSelection ->
  m (ParsedEsResponse [GenericSnapshotRepo])
getSnapshotRepos sel =
  fmap (fmap unGSRs) . parseEsResponse =<< get ["_snapshot", selectorSeg]
  where
    selectorSeg = case sel of
      AllSnapshotRepos -> "_all"
      SnapshotRepoList (p :| ps) -> T.intercalate "," (renderPat <$> (p : ps))
    renderPat (RepoPattern t) = t
    renderPat (ExactRepo (SnapshotRepoName t)) = t

-- | Wrapper to extract the list of 'GenericSnapshotRepo' in the
-- format they're returned in
newtype GSRs = GSRs {unGSRs :: [GenericSnapshotRepo]}

instance FromJSON GSRs where
  parseJSON = withObject "Collection of GenericSnapshotRepo" parse
    where
      parse = fmap GSRs . mapM (uncurry go) . X.toList
      go rawName = withObject "GenericSnapshotRepo" $ \o ->
        GenericSnapshotRepo (SnapshotRepoName $ toText rawName)
          <$> o .: "type"
          <*> o .: "settings"

-- | Create or update a snapshot repo
updateSnapshotRepo ::
  ( MonadBH m,
    SnapshotRepo repo
  ) =>
  -- | Use 'defaultSnapshotRepoUpdateSettings' if unsure
  SnapshotRepoUpdateSettings ->
  repo ->
  m (BHResponse Acknowledged)
updateSnapshotRepo SnapshotRepoUpdateSettings {..} repo =
  put endpoint (encode body)
  where
    endpoint = ["_snapshot", snapshotRepoName gSnapshotRepoName] `withQueries` params
    params
      | repoUpdateVerify = []
      | otherwise = [("verify", Just "false")]
    body =
      object
        [ "type" .= gSnapshotRepoType,
          "settings" .= gSnapshotRepoSettings
        ]
    GenericSnapshotRepo {..} = toGSnapshotRepo repo

-- | Verify if a snapshot repo is working. __NOTE:__ this API did not
-- make it into Elasticsearch until 1.4. If you use an older version,
-- you will get an error here.
verifySnapshotRepo ::
  ( MonadBH m,
    MonadThrow m
  ) =>
  SnapshotRepoName ->
  m (ParsedEsResponse SnapshotVerification)
verifySnapshotRepo (SnapshotRepoName n) =
  parseEsResponse =<< post ["_snapshot", n, "_verify"] emptyBody

deleteSnapshotRepo :: MonadBH m => SnapshotRepoName -> m (BHResponse Acknowledged)
deleteSnapshotRepo (SnapshotRepoName n) =
  delete ["_snapshot", n]

-- | Create and start a snapshot
createSnapshot ::
  (MonadBH m) =>
  SnapshotRepoName ->
  SnapshotName ->
  SnapshotCreateSettings ->
  m (BHResponse Acknowledged)
createSnapshot (SnapshotRepoName repoName) (SnapshotName snapName) SnapshotCreateSettings {..} =
  put endpoint body
  where
    endpoint = ["_snapshot", repoName, snapName] `withQueries` params
    params = [("wait_for_completion", Just (boolQP snapWaitForCompletion))]
    body = encode $ object prs
    prs =
      catMaybes
        [ ("indices" .=) . indexSelectionName <$> snapIndices,
          Just ("ignore_unavailable" .= snapIgnoreUnavailable),
          Just ("ignore_global_state" .= snapIncludeGlobalState),
          Just ("partial" .= snapPartial)
        ]

indexSelectionName :: IndexSelection -> Text
indexSelectionName AllIndexes = "_all"
indexSelectionName (IndexList (i :| is)) = T.intercalate "," (renderIndex <$> (i : is))
  where
    renderIndex (IndexName n) = n

-- | Get info about known snapshots given a pattern and repo name.
getSnapshots ::
  ( MonadBH m,
    MonadThrow m
  ) =>
  SnapshotRepoName ->
  SnapshotSelection ->
  m (ParsedEsResponse [SnapshotInfo])
getSnapshots (SnapshotRepoName repoName) sel =
  fmap (fmap unSIs) . parseEsResponse =<< get ["_snapshot", repoName, snapPath]
  where
    snapPath = case sel of
      AllSnapshots -> "_all"
      SnapshotList (s :| ss) -> T.intercalate "," (renderPath <$> (s : ss))
    renderPath (SnapPattern t) = t
    renderPath (ExactSnap (SnapshotName t)) = t

newtype SIs = SIs {unSIs :: [SnapshotInfo]}

instance FromJSON SIs where
  parseJSON = withObject "Collection of SnapshotInfo" parse
    where
      parse o = SIs <$> o .: "snapshots"

-- | Delete a snapshot. Cancels if it is running.
deleteSnapshot :: MonadBH m => SnapshotRepoName -> SnapshotName -> m (BHResponse Acknowledged)
deleteSnapshot (SnapshotRepoName repoName) (SnapshotName snapName) =
  delete ["_snapshot", repoName, snapName]

-- | Restore a snapshot to the cluster See
-- <https://www.elastic.co/guide/en/elasticsearch/reference/1.7/modules-snapshots.html#_restore>
-- for more details.
restoreSnapshot ::
  MonadBH m =>
  SnapshotRepoName ->
  SnapshotName ->
  -- | Start with 'defaultSnapshotRestoreSettings' and customize
  -- from there for reasonable defaults.
  SnapshotRestoreSettings ->
  m (BHResponse Accepted)
restoreSnapshot (SnapshotRepoName repoName) (SnapshotName snapName) SnapshotRestoreSettings {..} =
  post endpoint (encode body)
  where
    endpoint = ["_snapshot", repoName, snapName, "_restore"] `withQueries` params
    params = [("wait_for_completion", Just (boolQP snapRestoreWaitForCompletion))]
    body =
      object $
        catMaybes
          [ ("indices" .=) . indexSelectionName <$> snapRestoreIndices,
            Just ("ignore_unavailable" .= snapRestoreIgnoreUnavailable),
            Just ("include_global_state" .= snapRestoreIncludeGlobalState),
            ("rename_pattern" .=) <$> snapRestoreRenamePattern,
            ("rename_replacement" .=) . renderTokens <$> snapRestoreRenameReplacement,
            Just ("include_aliases" .= snapRestoreIncludeAliases),
            ("index_settings" .=) <$> snapRestoreIndexSettingsOverrides,
            ("ignore_index_settings" .=) <$> snapRestoreIgnoreIndexSettings
          ]
    renderTokens (t :| ts) = mconcat (renderToken <$> (t : ts))
    renderToken (RRTLit t) = t
    renderToken RRSubWholeMatch = "$0"
    renderToken (RRSubGroup g) = T.pack (show (rrGroupRefNum g))

getNodesInfo ::
  ( MonadBH m,
    MonadThrow m
  ) =>
  NodeSelection ->
  m (ParsedEsResponse NodesInfo)
getNodesInfo sel =
  parseEsResponse =<< get ["_nodes", selectionSeg]
  where
    selectionSeg = case sel of
      LocalNode -> "_local"
      NodeList (l :| ls) -> T.intercalate "," (selToSeg <$> (l : ls))
      AllNodes -> "_all"
    selToSeg (NodeByName (NodeName n)) = n
    selToSeg (NodeByFullNodeId (FullNodeId i)) = i
    selToSeg (NodeByHost (Server s)) = s
    selToSeg (NodeByAttribute (NodeAttrName a) v) = a <> ":" <> v

getNodesStats ::
  ( MonadBH m,
    MonadThrow m
  ) =>
  NodeSelection ->
  m (ParsedEsResponse NodesStats)
getNodesStats sel =
  parseEsResponse =<< get ["_nodes", selectionSeg, "stats"]
  where
    selectionSeg = case sel of
      LocalNode -> "_local"
      NodeList (l :| ls) -> T.intercalate "," (selToSeg <$> (l : ls))
      AllNodes -> "_all"
    selToSeg (NodeByName (NodeName n)) = n
    selToSeg (NodeByFullNodeId (FullNodeId i)) = i
    selToSeg (NodeByHost (Server s)) = s
    selToSeg (NodeByAttribute (NodeAttrName a) v) = a <> ":" <> v

-- | 'createIndex' will create an index given a 'Server', 'IndexSettings', and an 'IndexName'.
--
-- >>> response <- runBH' $ createIndex defaultIndexSettings (IndexName "didimakeanindex")
-- >>> isSuccess response
-- True
-- >>> runBH' $ indexExists (IndexName "didimakeanindex")
-- True
createIndex :: MonadBH m => IndexSettings -> IndexName -> m (BHResponse Acknowledged)
createIndex indexSettings (IndexName indexName) =
  put [indexName] $ encode indexSettings

-- | Create an index, providing it with any number of settings. This
--   is more expressive than 'createIndex' but makes is more verbose
--   for the common case of configuring only the shard count and
--   replica count.
createIndexWith ::
  MonadBH m =>
  [UpdatableIndexSetting] ->
  -- | shard count
  Int ->
  IndexName ->
  m (BHResponse Acknowledged)
createIndexWith updates shards (IndexName indexName) =
  put [indexName] body
  where
    body =
      encode $
        object
          [ "settings"
              .= deepMerge
                ( X.singleton "index.number_of_shards" (toJSON shards)
                    : [u | Object u <- toJSON <$> updates]
                )
          ]

-- | 'flushIndex' will flush an index given a 'Server' and an 'IndexName'.
flushIndex :: MonadBH m => IndexName -> m (BHResponse ShardResult)
flushIndex (IndexName indexName) =
  post [indexName, "_flush"] emptyBody

-- | 'deleteIndex' will delete an index given a 'Server' and an 'IndexName'.
--
-- >>> _ <- runBH' $ createIndex defaultIndexSettings (IndexName "didimakeanindex")
-- >>> response <- runBH' $ deleteIndex (IndexName "didimakeanindex")
-- >>> isSuccess response
-- True
-- >>> runBH' $ indexExists (IndexName "didimakeanindex")
-- False
deleteIndex :: MonadBH m => IndexName -> m (BHResponse Acknowledged)
deleteIndex (IndexName indexName) =
  delete [indexName]

-- | 'updateIndexSettings' will apply a non-empty list of setting updates to an index
--
-- >>> _ <- runBH' $ createIndex defaultIndexSettings (IndexName "unconfiguredindex")
-- >>> response <- runBH' $ updateIndexSettings (BlocksWrite False :| []) (IndexName "unconfiguredindex")
-- >>> isSuccess response
-- True
updateIndexSettings ::
  MonadBH m =>
  NonEmpty UpdatableIndexSetting ->
  IndexName ->
  m (BHResponse Acknowledged)
updateIndexSettings updates (IndexName indexName) =
  put [indexName, "_settings"] (encode body)
  where
    body = Object (deepMerge [u | Object u <- toJSON <$> toList updates])

getIndexSettings ::
  (MonadBH m, MonadThrow m) =>
  IndexName ->
  m (ParsedEsResponse IndexSettingsSummary)
getIndexSettings (IndexName indexName) =
  parseEsResponse =<< get [indexName, "_settings"]

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
forceMergeIndex :: MonadBH m => IndexSelection -> ForceMergeIndexSettings -> m (BHResponse ShardCount)
forceMergeIndex ixs ForceMergeIndexSettings {..} =
  post endpoint emptyBody
  where
    endpoint = [indexName, "_forcemerge"] `withQueries` params
    params =
      catMaybes
        [ ("max_num_segments",) . Just . showText <$> maxNumSegments,
          Just ("only_expunge_deletes", Just (boolQP onlyExpungeDeletes)),
          Just ("flush", Just (boolQP flushAfterOptimize))
        ]
    indexName = indexSelectionName ixs

deepMerge :: [Object] -> Object
deepMerge = LS.foldl' (X.unionWith merge) mempty
  where
    merge (Object a) (Object b) = Object (deepMerge [a, b])
    merge _ b = b

doesExist :: MonadBH m => Endpoint -> m Bool
doesExist endpoint =
  isSuccess <$> head endpoint

-- | 'indexExists' enables you to check if an index exists. Returns 'Bool'
--   in IO
--
-- >>> exists <- runBH' $ indexExists testIndex
indexExists :: MonadBH m => IndexName -> m Bool
indexExists (IndexName indexName) =
  doesExist [indexName]

-- | 'refreshIndex' will force a refresh on an index. You must
-- do this if you want to read what you wrote.
--
-- >>> _ <- runBH' $ createIndex defaultIndexSettings testIndex
-- >>> _ <- runBH' $ refreshIndex testIndex
refreshIndex :: MonadBH m => IndexName -> m (BHResponse ShardResult)
refreshIndex (IndexName indexName) =
  post [indexName, "_refresh"] emptyBody

-- | Block until the index becomes available for indexing
--   documents. This is useful for integration tests in which
--   indices are rapidly created and deleted.
waitForYellowIndex :: MonadBH m => IndexName -> m (BHResponse HealthStatus)
waitForYellowIndex (IndexName indexName) =
  get endpoint
  where
    endpoint = ["_cluster", "health", indexName] `withQueries` params
    params = [("wait_for_status", Just "yellow"), ("timeout", Just "10s")]

data HealthStatus = HealthStatus
  { healthStatusClusterName :: Text,
    healthStatusStatus :: Text,
    healthStatusTimedOut :: Bool,
    healthStatusNumberOfNodes :: Int,
    healthStatusNumberOfDataNodes :: Int,
    healthStatusActivePrimaryShards :: Int,
    healthStatusActiveShards :: Int,
    healthStatusRelocatingShards :: Int,
    healthStatusInitializingShards :: Int,
    healthStatusUnassignedShards :: Int,
    healthStatusDelayedUnassignedShards :: Int,
    healthStatusNumberOfPendingTasks :: Int,
    healthStatusNumberOfInFlightFetch :: Int,
    healthStatusTaskMaxWaitingInQueueMillis :: Int,
    healthStatusActiveShardsPercentAsNumber :: Float
  }
  deriving stock (Eq, Show)

instance FromJSON HealthStatus where
  parseJSON =
    withObject "HealthStatus" $ \v ->
      HealthStatus
        <$> v .: "cluster_name"
        <*> v .: "status"
        <*> v .: "timed_out"
        <*> v .: "number_of_nodes"
        <*> v .: "number_of_data_nodes"
        <*> v .: "active_primary_shards"
        <*> v .: "active_shards"
        <*> v .: "relocating_shards"
        <*> v .: "initializing_shards"
        <*> v .: "unassigned_shards"
        <*> v .: "delayed_unassigned_shards"
        <*> v .: "number_of_pending_tasks"
        <*> v .: "number_of_in_flight_fetch"
        <*> v .: "task_max_waiting_in_queue_millis"
        <*> v .: "active_shards_percent_as_number"

openOrCloseIndexes :: MonadBH m => OpenCloseIndex -> IndexName -> m (BHResponse Acknowledged)
openOrCloseIndexes oci (IndexName indexName) =
  post [indexName, stringifyOCIndex] emptyBody
  where
    stringifyOCIndex = case oci of
      OpenIndex -> "_open"
      CloseIndex -> "_close"

-- | 'openIndex' opens an index given a 'Server' and an 'IndexName'. Explained in further detail at
--   <http://www.elastic.co/guide/en/elasticsearch/reference/current/indices-open-close.html>
--
-- >>> response <- runBH' $ openIndex testIndex
openIndex :: MonadBH m => IndexName -> m (BHResponse Acknowledged)
openIndex = openOrCloseIndexes OpenIndex

-- | 'closeIndex' closes an index given a 'Server' and an 'IndexName'. Explained in further detail at
--   <http://www.elastic.co/guide/en/elasticsearch/reference/current/indices-open-close.html>
--
-- >>> response <- runBH' $ closeIndex testIndex
closeIndex :: MonadBH m => IndexName -> m (BHResponse Acknowledged)
closeIndex = openOrCloseIndexes CloseIndex

-- | 'listIndices' returns a list of all index names on a given 'Server'
listIndices :: (MonadThrow m, MonadBH m) => m [IndexName]
listIndices =
  parseEsResponseWith parser =<< get ["_cat/indices?format=json"]
  where
    parser :: [Value] -> Either String [IndexName]
    parser =
      mapM $ \val ->
        case val of
          Object obj ->
            case X.lookup "index" obj of
              (Just (String txt)) -> Right (IndexName txt)
              v -> Left $ "indexVal in listIndices failed on non-string, was: " <> show v
          v -> Left $ "One of the values parsed in listIndices wasn't an object, it was: " <> show v

-- | 'catIndices' returns a list of all index names on a given 'Server' as well as their doc counts
catIndices :: (MonadThrow m, MonadBH m) => m [(IndexName, Int)]
catIndices =
  parseEsResponseWith parser =<< get ["_cat/indices?format=json"]
  where
    parser :: [Value] -> Either String [(IndexName, Int)]
    parser =
      mapM $ \val ->
        case val of
          Object obj ->
            case (X.lookup "index" obj, X.lookup "docs.count" obj) of
              (Just (String txt), Just (String docs)) -> Right (IndexName txt, read (T.unpack docs))
              v -> Left $ "indexVal in catIndices failed on non-string, was: " <> show v
          v -> Left $ "One of the values parsed in catIndices wasn't an object, it was: " <> show v

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
updateIndexAliases :: MonadBH m => NonEmpty IndexAliasAction -> m (BHResponse Acknowledged)
updateIndexAliases actions =
  post ["_aliases"] (encode body)
  where
    body = object ["actions" .= toList actions]

-- | Get all aliases configured on the server.
getIndexAliases ::
  (MonadBH m, MonadThrow m) =>
  m (ParsedEsResponse IndexAliasesSummary)
getIndexAliases =
  parseEsResponse =<< get ["_aliases"]

-- | Delete a single alias, removing it from all indices it
--   is currently associated with.
deleteIndexAlias :: MonadBH m => IndexAliasName -> m (BHResponse Acknowledged)
deleteIndexAlias (IndexAliasName (IndexName name)) =
  delete ["_all", "_alias", name]

-- | 'putTemplate' creates a template given an 'IndexTemplate' and a 'TemplateName'.
--   Explained in further detail at
--   <https://www.elastic.co/guide/en/elasticsearch/reference/1.7/indices-templates.html>
--
--   >>> let idxTpl = IndexTemplate [IndexPattern "tweet-*"] (Just (IndexSettings (ShardCount 1) (ReplicaCount 1))) [toJSON TweetMapping]
--   >>> resp <- runBH' $ putTemplate idxTpl (TemplateName "tweet-tpl")
putTemplate :: MonadBH m => IndexTemplate -> TemplateName -> m (BHResponse Acknowledged)
putTemplate indexTemplate (TemplateName templateName) =
  put ["_template", templateName] (encode indexTemplate)

-- | 'templateExists' checks to see if a template exists.
--
--   >>> exists <- runBH' $ templateExists (TemplateName "tweet-tpl")
templateExists :: MonadBH m => TemplateName -> m Bool
templateExists (TemplateName templateName) =
  doesExist ["_template", templateName]

-- | 'deleteTemplate' is an HTTP DELETE and deletes a template.
--
--   >>> let idxTpl = IndexTemplate [IndexPattern "tweet-*"] (Just (IndexSettings (ShardCount 1) (ReplicaCount 1))) [toJSON TweetMapping]
--   >>> _ <- runBH' $ putTemplate idxTpl (TemplateName "tweet-tpl")
--   >>> resp <- runBH' $ deleteTemplate (TemplateName "tweet-tpl")
deleteTemplate :: MonadBH m => TemplateName -> m (BHResponse Acknowledged)
deleteTemplate (TemplateName templateName) =
  delete ["_template", templateName]

-- | 'putMapping' is an HTTP PUT and has upsert semantics. Mappings are schemas
-- for documents in indexes.
--
-- >>> _ <- runBH' $ createIndex defaultIndexSettings testIndex
-- >>> resp <- runBH' $ putMapping testIndex TweetMapping
-- >>> print resp
-- Response {responseStatus = Status {statusCode = 200, statusMessage = "OK"}, responseVersion = HTTP/1.1, responseHeaders = [("content-type","application/json; charset=UTF-8"),("content-encoding","gzip"),("transfer-encoding","chunked")], responseBody = "{\"acknowledged\":true}", responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}
putMapping :: (MonadBH m, ToJSON a) => IndexName -> a -> m (BHResponse a)
putMapping (IndexName indexName) mapping =
  -- "_mapping" above is originally transposed
  -- erroneously. The correct API call is: "/INDEX/_mapping"
  put [indexName, "_mapping"] (encode mapping)
{-# DEPRECATED putMapping "See <https://www.elastic.co/guide/en/elasticsearch/reference/7.17/removal-of-types.html>" #-}

versionCtlParams :: IndexDocumentSettings -> [(Text, Maybe Text)]
versionCtlParams cfg =
  case idsVersionControl cfg of
    NoVersionControl -> []
    InternalVersion v -> versionParams v "internal"
    ExternalGT (ExternalDocVersion v) -> versionParams v "external_gt"
    ExternalGTE (ExternalDocVersion v) -> versionParams v "external_gte"
    ForceVersion (ExternalDocVersion v) -> versionParams v "force"
  where
    vt = showText . docVersionNumber
    versionParams :: DocVersion -> Text -> [(Text, Maybe Text)]
    versionParams v t =
      [ ("version", Just $ vt v),
        ("version_type", Just t)
      ]

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
  (ToJSON doc, MonadBH m) =>
  IndexName ->
  IndexDocumentSettings ->
  doc ->
  DocId ->
  m (BHResponse IndexedDocument)
indexDocument (IndexName indexName) cfg document (DocId docId) =
  put endpoint (encode body)
  where
    endpoint = [indexName, "_doc", docId] `withQueries` indexQueryString cfg (DocId docId)
    body = encodeDocument cfg document

data IndexedDocument = IndexedDocument
  { idxDocIndex :: Text,
    idxDocType :: Text,
    idxDocId :: Text,
    idxDocVersion :: Int,
    idxDocResult :: Text,
    idxDocShards :: ShardResult,
    idxDocSeqNo :: Int,
    idxDocPrimaryTerm :: Int
  }
  deriving stock (Eq, Show)

instance FromJSON IndexedDocument where
  parseJSON =
    withObject "IndexedDocument" $ \v ->
      IndexedDocument
        <$> v .: "_index"
        <*> v .: "_type"
        <*> v .: "_id"
        <*> v .: "_version"
        <*> v .: "result"
        <*> v .: "_shards"
        <*> v .: "_seq_no"
        <*> v .: "_primary_term"

-- | 'updateDocument' provides a way to perform an partial update of a
-- an already indexed document.
updateDocument ::
  (ToJSON patch, MonadBH m) =>
  IndexName ->
  IndexDocumentSettings ->
  patch ->
  DocId ->
  m (BHResponse IndexedDocument)
updateDocument (IndexName indexName) cfg patch (DocId docId) =
  post endpoint (encode body)
  where
    endpoint = [indexName, "_update", docId] `withQueries` indexQueryString cfg (DocId docId)
    body = object ["doc" .= encodeDocument cfg patch]

{-  From ES docs:
      Parent and child documents must be indexed on the same shard.
      This means that the same routing value needs to be provided when getting, deleting, or updating a child document.

    Parent/Child support in Bloodhound requires MUCH more love.
    To work it around for now (and to support the existing unit test) we route "parent" documents to their "_id"
    (which is the default strategy for the ES), and route all child documents to their parens' "_id"

    However, it may not be flexible enough for some corner cases.

    Buld operations are completely unaware of "routing" and are probably broken in that matter.
    Or perhaps they always were, because the old "_parent" would also have this requirement.
-}
indexQueryString :: IndexDocumentSettings -> DocId -> [(Text, Maybe Text)]
indexQueryString cfg (DocId docId) =
  versionCtlParams cfg <> routeParams
  where
    routeParams = case idsJoinRelation cfg of
      Nothing -> []
      Just (ParentDocument _ _) -> [("routing", Just docId)]
      Just (ChildDocument _ _ (DocId pid)) -> [("routing", Just pid)]

encodeDocument :: ToJSON doc => IndexDocumentSettings -> doc -> Value
encodeDocument cfg document =
  case idsJoinRelation cfg of
    Nothing -> toJSON document
    Just (ParentDocument (FieldName field) name) ->
      mergeObjects (toJSON document) (object [fromText field .= name])
    Just (ChildDocument (FieldName field) name parent) ->
      mergeObjects (toJSON document) (object [fromText field .= object ["name" .= name, "parent" .= parent]])
  where
    mergeObjects (Object a) (Object b) = Object (a <> b)
    mergeObjects _ _ = error "Impossible happened: both document body and join parameters must be objects"

-- | 'deleteDocument' is the primary way to delete a single document.
--
-- >>> _ <- runBH' $ deleteDocument testIndex (DocId "1")
deleteDocument :: MonadBH m => IndexName -> DocId -> m (BHResponse IndexedDocument)
deleteDocument (IndexName indexName) (DocId docId) = delete [indexName, "_doc", docId]

-- | 'deleteByQuery' performs a deletion on every document that matches a query.
--
-- >>> let query = TermQuery (Term "user" "bitemyapp") Nothing
-- >>> _ <- runBH' $ deleteDocument testIndex query
deleteByQuery :: MonadBH m => IndexName -> Query -> m (BHResponse DeletedDocuments)
deleteByQuery (IndexName indexName) query =
  post [indexName, "_delete_by_query"] (encode body)
  where
    body = object ["query" .= query]

data DeletedDocuments = DeletedDocuments
  { delDocsTook :: Int,
    delDocsTimedOut :: Bool,
    delDocsTotal :: Int,
    delDocsDeleted :: Int,
    delDocsBatches :: Int,
    delDocsVersionConflicts :: Int,
    delDocsNoops :: Int,
    delDocsRetries :: DeletedDocumentsRetries,
    delDocsThrottledMillis :: Int,
    delDocsRequestsPerSecond :: Float,
    delDocsThrottledUntilMillis :: Int,
    delDocsFailures :: [Value] -- TODO find examples
  }
  deriving stock (Eq, Show)

instance FromJSON DeletedDocuments where
  parseJSON =
    withObject "DeletedDocuments" $ \v ->
      DeletedDocuments
        <$> v .: "took"
        <*> v .: "timed_out"
        <*> v .: "total"
        <*> v .: "deleted"
        <*> v .: "batches"
        <*> v .: "version_conflicts"
        <*> v .: "noops"
        <*> v .: "retries"
        <*> v .: "throttled_millis"
        <*> v .: "requests_per_second"
        <*> v .: "throttled_until_millis"
        <*> v .: "failures"

data DeletedDocumentsRetries = DeletedDocumentsRetries
  { delDocsRetriesBulk :: Int,
    delDocsRetriesSearch :: Int
  }
  deriving stock (Eq, Show)

instance FromJSON DeletedDocumentsRetries where
  parseJSON =
    withObject "DeletedDocumentsRetries" $ \v ->
      DeletedDocumentsRetries
        <$> v .: "bulk"
        <*> v .: "search"

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
bulk :: MonadBH m => V.Vector BulkOperation -> m (BHResponse a)
bulk =
  post ["_bulk"] . encodeBulkOperations

-- | 'encodeBulkOperations' is a convenience function for dumping a vector of 'BulkOperation'
--   into an 'L.ByteString'
--
-- >>> let bulkOps = V.fromList [BulkIndex testIndex (DocId "2") (toJSON (BulkTest "blah"))]
-- >>> encodeBulkOperations bulkOps
-- "\n{\"index\":{\"_id\":\"2\",\"_index\":\"twitter\"}}\n{\"name\":\"blah\"}\n"
encodeBulkOperations :: V.Vector BulkOperation -> L.ByteString
encodeBulkOperations stream = collapsed
  where
    blobs =
      fmap encodeBulkOperation stream
    mashedTaters =
      mash (mempty :: Builder) blobs
    collapsed =
      toLazyByteString $ mappend mashedTaters (byteString "\n")
    mash :: Builder -> V.Vector L.ByteString -> Builder
    mash = V.foldl' (\b x -> b <> byteString "\n" <> lazyByteString x)

mkBulkStreamValue :: Text -> Text -> Text -> Value
mkBulkStreamValue operation indexName docId =
  object
    [ fromText operation
        .= object
          [ "_index" .= indexName,
            "_id" .= docId
          ]
    ]

mkBulkStreamValueAuto :: Text -> Text -> Value
mkBulkStreamValueAuto operation indexName =
  object
    [ fromText operation
        .= object ["_index" .= indexName]
    ]

mkBulkStreamValueWithMeta :: [UpsertActionMetadata] -> Text -> Text -> Text -> Value
mkBulkStreamValueWithMeta meta operation indexName docId =
  object
    [ fromText operation
        .= object
          ( [ "_index" .= indexName,
              "_id" .= docId
            ]
              <> (buildUpsertActionMetadata <$> meta)
          )
    ]

-- | 'encodeBulkOperation' is a convenience function for dumping a single 'BulkOperation'
--   into an 'L.ByteString'
--
-- >>> let bulkOp = BulkIndex testIndex (DocId "2") (toJSON (BulkTest "blah"))
-- >>> encodeBulkOperation bulkOp
-- "{\"index\":{\"_id\":\"2\",\"_index\":\"twitter\"}}\n{\"name\":\"blah\"}"
encodeBulkOperation :: BulkOperation -> L.ByteString
encodeBulkOperation (BulkIndex (IndexName indexName) (DocId docId) value) = blob
  where
    metadata = mkBulkStreamValue "index" indexName docId
    blob = encode metadata `mappend` "\n" `mappend` encode value
encodeBulkOperation (BulkIndexAuto (IndexName indexName) value) = blob
  where
    metadata = mkBulkStreamValueAuto "index" indexName
    blob = encode metadata `mappend` "\n" `mappend` encode value
encodeBulkOperation (BulkIndexEncodingAuto (IndexName indexName) encoding) = toLazyByteString blob
  where
    metadata = toEncoding (mkBulkStreamValueAuto "index" indexName)
    blob = fromEncoding metadata <> "\n" <> fromEncoding encoding
encodeBulkOperation (BulkCreate (IndexName indexName) (DocId docId) value) = blob
  where
    metadata = mkBulkStreamValue "create" indexName docId
    blob = encode metadata `mappend` "\n" `mappend` encode value
encodeBulkOperation (BulkDelete (IndexName indexName) (DocId docId)) = blob
  where
    metadata = mkBulkStreamValue "delete" indexName docId
    blob = encode metadata
encodeBulkOperation (BulkUpdate (IndexName indexName) (DocId docId) value) = blob
  where
    metadata = mkBulkStreamValue "update" indexName docId
    doc = object ["doc" .= value]
    blob = encode metadata `mappend` "\n" `mappend` encode doc
encodeBulkOperation
  ( BulkUpsert
      (IndexName indexName)
      (DocId docId)
      payload
      actionMeta
    ) = blob
    where
      metadata = mkBulkStreamValueWithMeta actionMeta "update" indexName docId
      blob = encode metadata <> "\n" <> encode doc
      doc = case payload of
        UpsertDoc value -> object ["doc" .= value, "doc_as_upsert" .= True]
        UpsertScript scriptedUpsert script value ->
          let scup = if scriptedUpsert then ["scripted_upsert" .= True] else []
              upsert = ["upsert" .= value]
           in case (object (scup <> upsert), toJSON script) of
                (Object obj, Object jscript) -> Object $ jscript <> obj
                _ -> error "Impossible happened: serialising Script to Json should always be Object"
encodeBulkOperation (BulkCreateEncoding (IndexName indexName) (DocId docId) encoding) =
  toLazyByteString blob
  where
    metadata = toEncoding (mkBulkStreamValue "create" indexName docId)
    blob = fromEncoding metadata <> "\n" <> fromEncoding encoding

-- | 'getDocument' is a straight-forward way to fetch a single document from
--   Elasticsearch using a 'Server', 'IndexName', and a 'DocId'.
--   The 'DocId' is the primary key for your Elasticsearch document.
--
-- >>> yourDoc <- runBH' $ getDocument testIndex (DocId "1")
getDocument :: (FromJSON a, MonadBH m) => IndexName -> DocId -> m (BHResponse (EsResult a))
getDocument (IndexName indexName) (DocId docId) =
  get [indexName, "_doc", docId]

-- | 'documentExists' enables you to check if a document exists.
documentExists :: MonadBH m => IndexName -> DocId -> m Bool
documentExists (IndexName indexName) (DocId docId) =
  doesExist [indexName, "_doc", docId]

dispatchSearch :: MonadBH m => Endpoint -> Search -> m (BHResponse (SearchResult a))
dispatchSearch endpoint search = post url' (encode search)
  where
    url' = appendSearchTypeParam endpoint (searchType search)
    appendSearchTypeParam :: Endpoint -> SearchType -> Endpoint
    appendSearchTypeParam originalUrl st = originalUrl `withQueries` params
      where
        stText = "search_type"
        params
          | st == SearchTypeDfsQueryThenFetch = [(stText, Just "dfs_query_then_fetch")]
          -- used to catch 'SearchTypeQueryThenFetch', which is also the default
          | otherwise = []

-- | 'searchAll', given a 'Search', will perform that search against all indexes
--   on an Elasticsearch server. Try to avoid doing this if it can be helped.
--
-- >>> let query = TermQuery (Term "user" "bitemyapp") Nothing
-- >>> let search = mkSearch (Just query) Nothing
-- >>> response <- runBH' $ searchAll search
searchAll :: MonadBH m => Search -> m (BHResponse (SearchResult a))
searchAll = dispatchSearch ["_search"]

-- | 'searchByIndex', given a 'Search' and an 'IndexName', will perform that search
--   within an index on an Elasticsearch server.
--
-- >>> let query = TermQuery (Term "user" "bitemyapp") Nothing
-- >>> let search = mkSearch (Just query) Nothing
-- >>> response <- runBH' $ searchByIndex testIndex search
searchByIndex :: MonadBH m => IndexName -> Search -> m (BHResponse (SearchResult a))
searchByIndex (IndexName indexName) = dispatchSearch [indexName, "_search"]

-- | 'searchByIndices' is a variant of 'searchByIndex' that executes a
--   'Search' over many indices. This is much faster than using
--   'mapM' to 'searchByIndex' over a collection since it only
--   causes a single HTTP request to be emitted.
searchByIndices :: MonadBH m => NonEmpty IndexName -> Search -> m (BHResponse (SearchResult a))
searchByIndices ixs = dispatchSearch [renderedIxs, "_search"]
  where
    renderedIxs = T.intercalate (T.singleton ',') (map (\(IndexName t) -> t) (toList ixs))

dispatchSearchTemplate ::
  (FromJSON a, MonadBH m) =>
  Endpoint ->
  SearchTemplate ->
  m (BHResponse (SearchResult a))
dispatchSearchTemplate endpoint search = post endpoint $ encode search

-- | 'searchByIndexTemplate', given a 'SearchTemplate' and an 'IndexName', will perform that search
--   within an index on an Elasticsearch server.
--
-- >>> let query = SearchTemplateSource "{\"query\": { \"match\" : { \"{{my_field}}\" : \"{{my_value}}\" } }, \"size\" : \"{{my_size}}\"}"
-- >>> let search = mkSearchTemplate (Right query) Nothing
-- >>> response <- runBH' $ searchByIndexTemplate testIndex search
searchByIndexTemplate ::
  (FromJSON a, MonadBH m) =>
  IndexName ->
  SearchTemplate ->
  m (BHResponse (SearchResult a))
searchByIndexTemplate (IndexName indexName) = dispatchSearchTemplate [indexName, "_search", "template"]

-- | 'searchByIndicesTemplate' is a variant of 'searchByIndexTemplate' that executes a
--   'SearchTemplate' over many indices. This is much faster than using
--   'mapM' to 'searchByIndexTemplate' over a collection since it only
--   causes a single HTTP request to be emitted.
searchByIndicesTemplate ::
  (FromJSON a, MonadBH m) =>
  NonEmpty IndexName ->
  SearchTemplate ->
  m (BHResponse (SearchResult a))
searchByIndicesTemplate ixs = dispatchSearchTemplate [renderedIxs, "_search", "template"]
  where
    renderedIxs = T.intercalate (T.singleton ',') (map (\(IndexName t) -> t) (toList ixs))

-- | 'storeSearchTemplate', saves a 'SearchTemplateSource' to be used later.
storeSearchTemplate :: MonadBH m => SearchTemplateId -> SearchTemplateSource -> m (BHResponse Acknowledged)
storeSearchTemplate (SearchTemplateId tid) ts =
  post ["_scripts", tid] (encode body)
  where
    body = Object $ X.fromList ["script" .= Object ("lang" .= String "mustache" <> "source" .= ts)]

-- | 'getSearchTemplate', get info of an stored 'SearchTemplateSource'.
getSearchTemplate :: MonadBH m => SearchTemplateId -> m (BHResponse GetTemplateScript)
getSearchTemplate (SearchTemplateId tid) = get ["_scripts", tid]

-- | 'storeSearchTemplate',
deleteSearchTemplate :: MonadBH m => SearchTemplateId -> m (BHResponse Acknowledged)
deleteSearchTemplate (SearchTemplateId tid) = delete ["_scripts", tid]

-- | For a given search, request a scroll for efficient streaming of
-- search results. Note that the search is put into 'SearchTypeScan'
-- mode and thus results will not be sorted. Combine this with
-- 'advanceScroll' to efficiently stream through the full result set
getInitialScroll ::
  (FromJSON a, MonadThrow m, MonadBH m) =>
  IndexName ->
  Search ->
  m (ParsedEsResponse (SearchResult a))
getInitialScroll (IndexName indexName) search' =
  parseEsResponse =<< dispatchSearch endpoint search
  where
    endpoint = [indexName, "_search"] `withQueries` [("scroll", Just "1m")]
    sorting = Just [DefaultSortSpec $ mkSort (FieldName "_doc") Descending]
    search = search' {sortBody = sorting}

-- | For a given search, request a scroll for efficient streaming of
-- search results. Combine this with 'advanceScroll' to efficiently
-- stream through the full result set. Note that this search respects
-- sorting and may be less efficient than 'getInitialScroll'.
getInitialSortedScroll ::
  (FromJSON a, MonadThrow m, MonadBH m) =>
  IndexName ->
  Search ->
  m (ParsedEsResponse (SearchResult a))
getInitialSortedScroll (IndexName indexName) search = do
  parseEsResponse =<< dispatchSearch endpoint search
  where
    endpoint = [indexName, "_search"] `withQueries` [("scroll", Just "1m")]

scroll' ::
  (FromJSON a, MonadBH m, MonadThrow m) =>
  Maybe ScrollId ->
  m ([Hit a], Maybe ScrollId)
scroll' Nothing = return ([], Nothing)
scroll' (Just sid) = do
  res <- advanceScroll sid 60
  case res of
    Right SearchResult {..} -> return (hits searchHits, scrollId)
    Left _ -> return ([], Nothing)

-- | Use the given scroll to fetch the next page of documents. If there are no
-- further pages, 'SearchResult.searchHits.hits' will be '[]'.
advanceScroll ::
  ( FromJSON a,
    MonadBH m,
    MonadThrow m
  ) =>
  ScrollId ->
  -- | How long should the snapshot of data be kept around? This timeout is updated every time 'advanceScroll' is used, so don't feel the need to set it to the entire duration of your search processing. Note that durations < 1s will be rounded up. Also note that 'NominalDiffTime' is an instance of Num so literals like 60 will be interpreted as seconds. 60s is a reasonable default.
  NominalDiffTime ->
  m (ParsedEsResponse (SearchResult a))
advanceScroll (ScrollId sid) scroll =
  parseEsResponse =<< post ["_search", "scroll"] (encode scrollObject)
  where
    scrollTime = showText secs <> "s"
    secs :: Integer
    secs = round scroll

    scrollObject =
      object
        [ "scroll" .= scrollTime,
          "scroll_id" .= sid
        ]

scanAccumulator ::
  (FromJSON a, MonadBH m, MonadThrow m) =>
  [Hit a] ->
  ([Hit a], Maybe ScrollId) ->
  m ([Hit a], Maybe ScrollId)
scanAccumulator oldHits (newHits, Nothing) = return (oldHits ++ newHits, Nothing)
scanAccumulator oldHits ([], _) = return (oldHits, Nothing)
scanAccumulator oldHits (newHits, msid) = do
  (newHits', msid') <- scroll' msid
  scanAccumulator (oldHits ++ newHits) (newHits', msid')

-- | 'scanSearch' uses the 'scroll' API of elastic,
-- for a given 'IndexName'. Note that this will
-- consume the entire search result set and will be doing O(n) list
-- appends so this may not be suitable for large result sets. In that
-- case, 'getInitialScroll' and 'advanceScroll' are good low level
-- tools. You should be able to hook them up trivially to conduit,
-- pipes, or your favorite streaming IO abstraction of choice. Note
-- that ordering on the search would destroy performance and thus is
-- ignored.
scanSearch ::
  (FromJSON a, MonadBH m, MonadThrow m) =>
  IndexName ->
  Search ->
  m [Hit a]
scanSearch indexName search = do
  initialSearchResult <- getInitialScroll indexName search
  let (hits', josh) = case initialSearchResult of
        Right SearchResult {..} -> (hits searchHits, scrollId)
        Left _ -> ([], Nothing)
  (totalHits, _) <- scanAccumulator [] (hits', josh)
  return totalHits

pitAccumulator ::
  (FromJSON a, MonadBH m, MonadThrow m) => Search -> [Hit a] -> m [Hit a]
pitAccumulator search oldHits = do
  resp <- searchAll search
  parsed <- parseEsResponse resp
  case parsed of
    Left _ -> return []
    Right searchResult -> case hits (searchHits searchResult) of
      [] -> return oldHits
      newHits -> case (hitSort $ last newHits, pitId searchResult) of
        (Nothing, Nothing) ->
          error "no point in time (PIT) ID or last sort value"
        (Just _, Nothing) -> error "no point in time (PIT) ID"
        (Nothing, _) -> return (oldHits <> newHits)
        (Just lastSort, Just pitId') -> do
          let newSearch =
                search
                  { pointInTime = Just (PointInTime pitId' "1m"),
                    searchAfterKey = Just lastSort
                  }
          pitAccumulator newSearch (oldHits <> newHits)

-- | 'pitSearch' uses the point in time (PIT) API of elastic, for a given
-- 'IndexName'. Requires Elasticsearch >=7.10. Note that this will consume the
-- entire search result set and will be doing O(n) list appends so this may
-- not be suitable for large result sets. In that case, the point in time API
-- should be used directly with `openPointInTime` and `closePointInTime`.
--
-- Note that 'pitSearch' utilizes the 'search_after' parameter under the hood,
-- which requires a non-empty 'sortBody' field in the provided 'Search' value.
-- Otherwise, 'pitSearch' will fail to return all matching documents.
--
-- For more information see
-- <https://www.elastic.co/guide/en/elasticsearch/reference/current/point-in-time-api.html>.
pitSearch ::
  (FromJSON a, MonadBH m, MonadThrow m, Show a) => IndexName -> Search -> m [Hit a]
pitSearch indexName search = do
  openResp <- openPointInTime indexName
  case openResp of
    Left _ -> return []
    Right OpenPointInTimeResponse {..} -> do
      let searchPIT = search {pointInTime = Just (PointInTime oPitId "1m")}
      hits <- pitAccumulator searchPIT []
      closeResp <- closePointInTime (ClosePointInTime oPitId)
      case closeResp of
        Left _ -> return []
        Right (ClosePointInTimeResponse False _) ->
          error "failed to close point in time (PIT)"
        Right (ClosePointInTimeResponse True _) -> return hits

-- | 'mkSearch' is a helper function for defaulting additional fields of a 'Search'
--   to Nothing in case you only care about your 'Query' and 'Filter'. Use record update
--   syntax if you want to add things like aggregations or highlights while still using
--   this helper function.
--
-- >>> let query = TermQuery (Term "user" "bitemyapp") Nothing
-- >>> mkSearch (Just query) Nothing
-- Search {queryBody = Just (TermQuery (Term {termField = "user", termValue = "bitemyapp"}) Nothing), filterBody = Nothing, searchAfterKey = Nothing, sortBody = Nothing, aggBody = Nothing, highlight = Nothing, trackSortScores = False, from = From 0, size = Size 10, searchType = SearchTypeQueryThenFetch, fields = Nothing, source = Nothing}
mkSearch :: Maybe Query -> Maybe Filter -> Search
mkSearch query filter =
  Search
    { queryBody = query,
      filterBody = filter,
      sortBody = Nothing,
      aggBody = Nothing,
      highlight = Nothing,
      trackSortScores = False,
      from = From 0,
      size = Size 10,
      searchType = SearchTypeQueryThenFetch,
      searchAfterKey = Nothing,
      fields = Nothing,
      scriptFields = Nothing,
      source = Nothing,
      suggestBody = Nothing,
      pointInTime = Nothing
    }

-- | 'mkAggregateSearch' is a helper function that defaults everything in a 'Search' except for
--   the 'Query' and the 'Aggregation'.
--
-- >>> let terms = TermsAgg $ (mkTermsAggregation "user") { termCollectMode = Just BreadthFirst }
-- >>> terms
-- TermsAgg (TermsAggregation {term = Left "user", termInclude = Nothing, termExclude = Nothing, termOrder = Nothing, termMinDocCount = Nothing, termSize = Nothing, termShardSize = Nothing, termCollectMode = Just BreadthFirst, termExecutionHint = Nothing, termAggs = Nothing})
-- >>> let myAggregation = mkAggregateSearch Nothing $ mkAggregations "users" terms
mkAggregateSearch :: Maybe Query -> Aggregations -> Search
mkAggregateSearch query mkSearchAggs =
  Search
    { queryBody = query,
      filterBody = Nothing,
      sortBody = Nothing,
      aggBody = Just mkSearchAggs,
      highlight = Nothing,
      trackSortScores = False,
      from = From 0,
      size = Size 0,
      searchType = SearchTypeQueryThenFetch,
      searchAfterKey = Nothing,
      fields = Nothing,
      scriptFields = Nothing,
      source = Nothing,
      suggestBody = Nothing,
      pointInTime = Nothing
    }

-- | 'mkHighlightSearch' is a helper function that defaults everything in a 'Search' except for
--   the 'Query' and the 'Aggregation'.
--
-- >>> let query = QueryMatchQuery $ mkMatchQuery (FieldName "_all") (QueryString "haskell")
-- >>> let testHighlight = Highlights Nothing [FieldHighlight (FieldName "message") Nothing]
-- >>> let search = mkHighlightSearch (Just query) testHighlight
mkHighlightSearch :: Maybe Query -> Highlights -> Search
mkHighlightSearch query searchHighlights =
  Search
    { queryBody = query,
      filterBody = Nothing,
      sortBody = Nothing,
      aggBody = Nothing,
      highlight = Just searchHighlights,
      trackSortScores = False,
      from = From 0,
      size = Size 10,
      searchType = SearchTypeDfsQueryThenFetch,
      searchAfterKey = Nothing,
      fields = Nothing,
      scriptFields = Nothing,
      source = Nothing,
      suggestBody = Nothing,
      pointInTime = Nothing
    }

-- | 'mkSearchTemplate' is a helper function for defaulting additional fields of a 'SearchTemplate'
--   to Nothing. Use record update syntax if you want to add things.
mkSearchTemplate :: Either SearchTemplateId SearchTemplateSource -> TemplateQueryKeyValuePairs -> SearchTemplate
mkSearchTemplate id_ params = SearchTemplate id_ params Nothing Nothing

-- | 'pageSearch' is a helper function that takes a search and assigns the from
--    and size fields for the search. The from parameter defines the offset
--    from the first result you want to fetch. The size parameter allows you to
--    configure the maximum amount of hits to be returned.
--
-- >>> let query = QueryMatchQuery $ mkMatchQuery (FieldName "_all") (QueryString "haskell")
-- >>> let search = mkSearch (Just query) Nothing
-- >>> search
-- Search {queryBody = Just (QueryMatchQuery (MatchQuery {matchQueryField = FieldName "_all", matchQueryQueryString = QueryString "haskell", matchQueryOperator = Or, matchQueryZeroTerms = ZeroTermsNone, matchQueryCutoffFrequency = Nothing, matchQueryMatchType = Nothing, matchQueryAnalyzer = Nothing, matchQueryMaxExpansions = Nothing, matchQueryLenient = Nothing, matchQueryBoost = Nothing})), filterBody = Nothing, sortBody = Nothing, aggBody = Nothing, highlight = Nothing, trackSortScores = False, from = From 0, size = Size 10, searchType = SearchTypeQueryThenFetch, fields = Nothing, source = Nothing}
-- >>> pageSearch (From 10) (Size 100) search
-- Search {queryBody = Just (QueryMatchQuery (MatchQuery {matchQueryField = FieldName "_all", matchQueryQueryString = QueryString "haskell", matchQueryOperator = Or, matchQueryZeroTerms = ZeroTermsNone, matchQueryCutoffFrequency = Nothing, matchQueryMatchType = Nothing, matchQueryAnalyzer = Nothing, matchQueryMaxExpansions = Nothing, matchQueryLenient = Nothing, matchQueryBoost = Nothing})), filterBody = Nothing, sortBody = Nothing, aggBody = Nothing, highlight = Nothing, trackSortScores = False, from = From 10, size = Size 100, searchType = SearchTypeQueryThenFetch, fields = Nothing, source = Nothing}
pageSearch ::
  -- | The result offset
  From ->
  -- | The number of results to return
  Size ->
  -- | The current seach
  Search ->
  -- | The paged search
  Search
pageSearch resultOffset pageSize search = search {from = resultOffset, size = pageSize}

parseUrl' :: MonadThrow m => Text -> m Request
parseUrl' t = parseRequest (URI.escapeURIString URI.isAllowedInURI (T.unpack t))

-- | This is a hook that can be set via the 'bhRequestHook' function
-- that will authenticate all requests using an HTTP Basic
-- Authentication header. Note that it is *strongly* recommended that
-- this option only be used over an SSL connection.
--
-- >> (mkBHEnv myServer myManager) { bhRequestHook = basicAuthHook (EsUsername "myuser") (EsPassword "mypass") }
basicAuthHook :: Monad m => EsUsername -> EsPassword -> Request -> m Request
basicAuthHook (EsUsername u) (EsPassword p) = return . applyBasicAuth u' p'
  where
    u' = T.encodeUtf8 u
    p' = T.encodeUtf8 p

boolQP :: Bool -> Text
boolQP True = "true"
boolQP False = "false"

countByIndex :: (MonadBH m, MonadThrow m) => IndexName -> CountQuery -> m (ParsedEsResponse CountResponse)
countByIndex (IndexName indexName) q =
  parseEsResponse =<< post [indexName, "_count"] (encode q)

-- | 'openPointInTime' opens a point in time for an index given an 'IndexName'.
-- Note that the point in time should be closed with 'closePointInTime' as soon
-- as it is no longer needed.
--
-- For more information see
-- <https://www.elastic.co/guide/en/elasticsearch/reference/current/point-in-time-api.html>.
openPointInTime ::
  (MonadBH m, MonadThrow m) =>
  IndexName ->
  m (ParsedEsResponse OpenPointInTimeResponse)
openPointInTime (IndexName indexName) =
  parseEsResponse =<< post [indexName, "_pit?keep_alive=1m"] emptyBody

-- | 'closePointInTime' closes a point in time given a 'ClosePointInTime'.
--
-- For more information see
-- <https://www.elastic.co/guide/en/elasticsearch/reference/current/point-in-time-api.html>.
closePointInTime ::
  (MonadBH m, MonadThrow m) =>
  ClosePointInTime ->
  m (ParsedEsResponse ClosePointInTimeResponse)
closePointInTime q = do
  parseEsResponse =<< deleteWithBody ["_pit"] (encode q)
