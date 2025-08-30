{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Bloodhound.Internal.Versions.Common.Types.Nodes
  ( BoundTransportAddress (..),
    BuildHash (..),
    CPUInfo (..),
    ClusterName (..),
    DeletedDocuments (..),
    DeletedDocumentsRetries (..),
    EsAddress (..),
    EsPassword (..),
    EsUsername (..),
    FullNodeId (..),
    HealthStatus (..),
    IndexedDocument (..),
    InitialShardCount (..),
    JVMBufferPoolStats (..),
    JVMGCCollector (..),
    JVMGCStats (..),
    JVMMemoryInfo (..),
    JVMMemoryPool (..),
    JVMPoolStats (..),
    JVMVersion (..),
    LoadAvgs (..),
    MacAddress (..),
    NetworkInterfaceName (..),
    NodeAttrFilter (..),
    NodeAttrName (..),
    NodeBreakerStats (..),
    NodeBreakersStats (..),
    NodeDataPathStats (..),
    NodeFSStats (..),
    NodeFSTotalStats (..),
    NodeHTTPInfo (..),
    NodeHTTPStats (..),
    NodeIndicesStats (..),
    NodeInfo (..),
    NodeJVMInfo (..),
    NodeJVMStats (..),
    NodeName (..),
    NodeNetworkInfo (..),
    NodeNetworkInterface (..),
    NodeNetworkStats (..),
    NodeOSInfo (..),
    NodeOSStats (..),
    NodePluginInfo (..),
    NodeProcessInfo (..),
    NodeProcessStats (..),
    NodeSelection (..),
    NodeSelector (..),
    NodeStats (..),
    NodeThreadPoolInfo (..),
    NodeThreadPoolStats (..),
    NodeTransportInfo (..),
    NodeTransportStats (..),
    NodesInfo (..),
    NodesStats (..),
    PID (..),
    PluginName (..),
    ShardResult (..),
    ShardsResult (..),
    ThreadPool (..),
    ThreadPoolSize (..),
    ThreadPoolType (..),
    Version (..),
    VersionNumber (..),

    -- * Optics
    nodeAttrFilterNameLens,
    nodeAttrFilterValuesLens,
    nodeSelectionNodeListPrism,
    nodeSelectorNodeByNamePrism,
    nodeSelectorNodeByFullNodeIdPrism,
    nodeSelectorNodeByHostPrism,
    nodeSelectorNodeByAttributePrism,
    nodesInfoListLens,
    nodesClusterNameLens,
    nodesStatsListLens,
    nodesStatsClusterNameLens,
    nodeStatsNameLens,
    nodeStatsFullIdLens,
    nodeStatsBreakersStatsLens,
    nodeStatsHTTPLens,
    nodeStatsTransportLens,
    nodeStatsFSLens,
    nodeStatsNetworkLens,
    nodeStatsThreadPoolLens,
    nodeStatsJVMLens,
    nodeStatsProcessLens,
    nodeStatsOSLens,
    nodeStatsIndicesLens,
    nodeStatsParentBreakerLens,
    nodeStatsRequestBreakerLens,
    nodeStatsFieldDataBreakerLens,
    nodeBreakersStatsTrippedLens,
    nodeBreakersStatsOverheadLens,
    nodeBreakersStatsEstSizeLens,
    nodeBreakersStatsLimitSizeLens,
    nodeHTTPTotalStatsOpenedLens,
    nodeHTTPStatsCurrentOpenLens,
    nodeTransportStatsTXSizeLens,
    nodeTransportStatsCountLens,
    nodeTransportStatsRXSizeLens,
    nodeTransportStatsRXCountLens,
    nodeTransportStatsServerOpenLens,
    nodeFSStatsDataPathsLens,
    nodeFSStatsTotalLens,
    nodeFSStatsTimestampLens,
    nodeDataPathStatsDiskServiceTimeLens,
    nodeDataPathStatsDiskQueueLens,
    nodeDataPathStatsIOSizeLens,
    nodeDataPathStatsWriteSizeLens,
    nodeDataPathStatsReadSizeLens,
    nodeDataPathStatsIOOpsLens,
    nodeDataPathStatsWritesLens,
    nodeDataPathStatsReadsLens,
    nodeDataPathStatsAvailableLens,
    nodeDataPathStatsFreeLens,
    nodeDataPathStatsTotalLens,
    nodeDataPathStatsTypeLens,
    nodeDataPathStatsDeviceLens,
    nodeDataPathStatsMountLens,
    nodeDataPathStatsPathLens,
    nodeFSTotalStatsDiskServiceTimeLens,
    nodeFSTotalStatsDiskQueueLens,
    nodeFSTotalStatsIOSizeLens,
    nodeFSTotalStatsWriteSizeLens,
    nodeFSTotalStatsReadSizeLens,
    nodeFSTotalStatsIOOpsLens,
    nodeFSTotalStatsWritesLens,
    nodeFSTotalStatsReadsLens,
    nodeFSTotalStatsAvailableLens,
    nodeFSTotalStatsFreeLens,
    nodeFSTotalStatsTotalLens,
    nodeNetworkStatsTCPOutRSTsLens,
    nodeNetworkStatsTCPInErrsLens,
    nodeNetworkStatsTCPAttemptFailsLens,
    nodeNetworkStatsTCPEstabResetsLens,
    nodeNetworkStatsTCPRetransSegsLens,
    nodeNetworkStatsTCPOutSegsLens,
    nodeNetworkStatsTCPInSegsLens,
    nodeNetworkStatsTCPCurrEstabLens,
    nodeNetworkStatsTCPPassiveOpensLens,
    nodeNetworkStatsTCPActiveOpensLens,
    nodeThreadPoolStatsCompletedLens,
    nodeThreadPoolStatsLargestLens,
    nodeThreadPoolStatsRejectedLens,
    nodeThreadPoolStatsActiveLens,
    nodeThreadPoolStatsQueueLens,
    nodeThreadPoolStatsThreadsLens,
    nodeJVMStatsMappedBufferPoolLens,
    nodeJVMStatsDirectBufferPoolLens,
    nodeJVMStatsGCOldCollectorLens,
    nodeJVMStatsGCYoungCollectorLens,
    nodeJVMStatsPeakThreadsCountLens,
    nodeJVMStatsThreadsCountLens,
    nodeJVMStatsOldPoolLens,
    nodeJVMStatsSurvivorPoolLens,
    nodeJVMStatsYoungPoolLens,
    nodeJVMStatsNonHeapCommittedLens,
    nodeJVMStatsNonHeapUsedLens,
    nodeJVMStatsHeapMaxLens,
    nodeJVMStatsHeapCommittedLens,
    nodeJVMStatsHeapUsedPercentLens,
    nodeJVMStatsHeapUsedLens,
    nodeJVMStatsUptimeLens,
    nodeJVMStatsTimestampLens,
    jvmBufferPoolStatsTotalCapacityLens,
    jvmBufferPoolStatsUsedLens,
    jvmBufferPoolStatsCountLens,
    jvmGCStatsCollectionTimeLens,
    jvmGCStatsCollectionCountLens,
    jvmPoolStatsPeakMaxLens,
    jvmPoolStatsPeakUsedLens,
    jvmPoolStatsMaxLens,
    jvmPoolStatsUsedLens,
    nodeProcessStatsTimestampLens,
    nodeProcessStatsOpenFDsLens,
    nodeProcessStatsMaxFDsLens,
    nodeProcessStatsCPUPercentLens,
    nodeProcessStatsCPUTotalLens,
    nodeProcessStatsMemTotalVirtualLens,
    nodeOSStatsTimestampLens,
    nodeOSStatsCPUPercentLens,
    nodeOSStatsLoadLens,
    nodeOSStatsMemTotalLens,
    nodeOSStatsMemFreeLens,
    nodeOSStatsMemFreePercentLens,
    nodeOSStatsMemUsedLens,
    nodeOSStatsMemUsedPercentLens,
    nodeOSStatsSwapTotalLens,
    nodeOSStatsSwapFreeLens,
    nodeOSStatsSwapUsedLens,
    loadAvgs1MinLens,
    loadAvgs5MinLens,
    loadAvgs15MinLens,
    nodeIndicesStatsRecoveryThrottleTimeLens,
    nodeIndicesStatsRecoveryCurrentAsTargetLens,
    nodeIndicesStatsRecoveryCurrentAsSourceLens,
    nodeIndicesStatsQueryCacheMissesLens,
    nodeIndicesStatsQueryCacheHitsLens,
    nodeIndicesStatsQueryCacheEvictionsLens,
    nodeIndicesStatsQueryCacheSizeLens,
    nodeIndicesStatsSuggestCurrentLens,
    nodeIndicesStatsSuggestTimeLens,
    nodeIndicesStatsSuggestTotalLens,
    nodeIndicesStatsTranslogSizeLens,
    nodeIndicesStatsTranslogOpsLens,
    nodeIndicesStatsSegFixedBitSetMemoryLens,
    nodeIndicesStatsSegVersionMapMemoryLens,
    nodeIndicesStatsSegIndexWriterMaxMemoryLens,
    nodeIndicesStatsSegIndexWriterMemoryLens,
    nodeIndicesStatsSegMemoryLens,
    nodeIndicesStatsSegCountLens,
    nodeIndicesStatsCompletionSizeLens,
    nodeIndicesStatsPercolateQueriesLens,
    nodeIndicesStatsPercolateMemoryLens,
    nodeIndicesStatsPercolateCurrentLens,
    nodeIndicesStatsPercolateTimeLens,
    nodeIndicesStatsPercolateTotalLens,
    nodeIndicesStatsFieldDataEvictionsLens,
    nodeIndicesStatsFieldDataMemoryLens,
    nodeIndicesStatsWarmerTotalTimeLens,
    nodeIndicesStatsWarmerTotalLens,
    nodeIndicesStatsWarmerCurrentLens,
    nodeIndicesStatsFlushTotalTimeLens,
    nodeIndicesStatsFlushTotalLens,
    nodeIndicesStatsRefreshTotalTimeLens,
    nodeIndicesStatsRefreshTotalLens,
    nodeIndicesStatsMergesTotalSizeLens,
    nodeIndicesStatsMergesTotalDocsLens,
    nodeIndicesStatsMergesTotalTimeLens,
    nodeIndicesStatsMergesTotalLens,
    nodeIndicesStatsMergesCurrentSizeLens,
    nodeIndicesStatsMergesCurrentDocsLens,
    nodeIndicesStatsMergesCurrentLens,
    nodeIndicesStatsSearchFetchCurrentLens,
    nodeIndicesStatsSearchFetchTimeLens,
    nodeIndicesStatsSearchFetchTotalLens,
    nodeIndicesStatsSearchQueryCurrentLens,
    nodeIndicesStatsSearchQueryTimeLens,
    nodeIndicesStatsSearchQueryTotalLens,
    nodeIndicesStatsSearchOpenContextsLens,
    nodeIndicesStatsGetCurrentLens,
    nodeIndicesStatsGetMissingTimeLens,
    nodeIndicesStatsGetMissingTotalLens,
    nodeIndicesStatsGetExistsTimeLens,
    nodeIndicesStatsGetExistsTotalLens,
    nodeIndicesStatsGetTimeLens,
    nodeIndicesStatsGetTotalLens,
    nodeIndicesStatsIndexingThrottleTimeLens,
    nodeIndicesStatsIndexingIsThrottledLens,
    nodeIndicesStatsIndexingNoopUpdateTotalLens,
    nodeIndicesStatsIndexingDeleteCurrentLens,
    nodeIndicesStatsIndexingDeleteTimeLens,
    nodeIndicesStatsIndexingDeleteTotalLens,
    nodeIndicesStatsIndexingIndexCurrentLens,
    nodeIndicesStatsIndexingIndexTimeLens,
    nodeIndicesStatsIndexingTotalLens,
    nodeIndicesStatsStoreThrottleTimeLens,
    nodeIndicesStatsStoreSizeLens,
    nodeIndicesStatsDocsDeletedLens,
    nodeIndicesStatsDocsCountLens,
    nodeInfoHTTPAddressLens,
    nodeInfoBuildLens,
    nodeInfoESVersionLens,
    nodeInfoIPLens,
    nodeInfoHostLens,
    nodeInfoTransportAddressLens,
    nodeInfoNameLens,
    nodeInfoFullIdLens,
    nodeInfoPluginsLens,
    nodeInfoHTTPLens,
    nodeInfoTransportLens,
    nodeInfoNetworkLens,
    nodeInfoThreadPoolLens,
    nodeInfoJVMLens,
    nodeInfoProcessLens,
    nodeInfoOSLens,
    nodeInfoSettingsLens,
    nodePluginSiteLens,
    nodePluginInfoJVMLens,
    nodePluginInfoDescriptionLens,
    nodePluginInfoVersionLens,
    nodePluginInfoNameLens,
    nodeHTTPInfoMaxContentLengthLens,
    nodeHTTPInfopublishAddressLens,
    nodeHTTPInfoBoundAddressesLens,
    nodeTransportInfoProfilesLens,
    nodeTransportInfoPublishAddressLens,
    nodeTransportInfoBoundAddressLens,
    boundTransportAddressPublishAddressLens,
    boundTransportAddressBoundAddressesLens,
    nodeNetworkInfoPrimaryInterfaceLens,
    nodeNetworkInfoRefreshIntervalLens,
    nodeNetworkInterfaceMacAddressLens,
    nodeNetworkInterfaceNameLens,
    nodeNetworkInterfaceAddressLens,
    threadPoolNodeThreadPoolNameLens,
    threadPoolNodeThreadPoolInfoLens,
    nodeThreadPoolInfoQueueSizeLens,
    nodeThreadPoolInfoKeepaliveLens,
    nodeThreadPoolInfoMinLens,
    nodeThreadPoolInfoMaxLens,
    nodeThreadPoolInfoTypeLens,
    threadPoolSizeBoundedPrism,
    nodeJVMInfoMemoryPoolsLens,
    nodeJVMInfoMemoryPoolsGCCollectorsLens,
    nodeJVMInfoMemoryInfoLens,
    nodeJVMInfoStartTimeLens,
    nodeJVMInfoVMVendorLens,
    nodeJVMInfoVMVersionLens,
    nodeJVMInfoVMNameLens,
    nodeJVMInfoVersionLens,
    nodeJVMInfoPIDLens,
    jvmMemoryInfoDirectMaxLens,
    jvmMemoryInfoNonHeapMaxLens,
    jvmMemoryInfoNonHeapInitLens,
    jvmMemoryInfoHeapMaxLens,
    jvmMemoryInfoHeapInitLens,
    nodeOSInfoRefreshIntervalLens,
    nodeOSInfoNameLens,
    nodeOSInfoArchLens,
    nodeOSInfoVersionLens,
    nodeOSInfoAvailableProcessorsLens,
    nodeOSInfoAllocatedProcessorsLens,
    cpuInfoCacheSizeLens,
    cpuInfoCoresPerSocketLens,
    cpuInfoTotalSocketsLens,
    cpuInfoTotalCoresLens,
    cpuInfoMHZLens,
    cpuInfoModelLens,
    cpuInfoVendorLens,
    nodeProcessInfoMLockAllLens,
    nodeProcessInfoMaxFileDescriptorsLens,
    nodeProcessInfoIdLens,
    nodeProcessInfoRefreshIntervalLens,
    initialShardCountExplicitShardsPrism,
    shardsResultShardsLens,
    shardResultTotalLens,
    shardsResultSuccessfulLens,
    shardsResultResultSkippedLens,
    shardsResultFailedLens,
    versionNumberLens,
    versionBuildHashLens,
    versionBuildDateLens,
    versionBuildSnapshotLens,
    versionLuceneVersionLens,
    healthStatusClusterNameLens,
    healthStatusStatusLens,
    healthStatusTimedOutLens,
    healthStatusNumberOfNodesLens,
    healthStatusNumberOfDataNodesLens,
    healthStatusActivePrimaryShardsLens,
    healthStatusActiveShardsLens,
    healthStatusRelocatingShardsLens,
    healthStatusInitializingShardsLens,
    healthStatusUnassignedShardsLens,
    healthStatusDelayedUnassignedShardsLens,
    healthStatusNumberOfPendingTasksLens,
    healthStatusNumberOfInFlightFetchLens,
    healthStatusTaskMaxWaitingInQueueMillisLens,
    healthStatusActiveShardsPercentAsNumberLens,
    indexedDocumentIndexLens,
    indexedDocumentTypeLens,
    indexedDocumentIdLens,
    indexedDocumentVersionLens,
    indexedDocumentResultLens,
    indexedDocumentShardsLens,
    indexedDocumentSeqNoLens,
    indexedDocumentPrimaryTermLens,
    deletedDocumentsTookLens,
    deletedDocumentsTimedOutLens,
    deletedDocumentsTotalLens,
    deletedDocumentsDeletedLens,
    deletedDocumentsBatchesLens,
    deletedDocumentsVersionConflictsLens,
    deletedDocumentsNoopsLens,
    deletedDocumentsRetriesLens,
    deletedDocumentsThrottledMillisLens,
    deletedDocumentsRequestsPerSecondLens,
    deletedDocumentsThrottledUntilMillisLens,
    deletedDocumentsFailuresLens,
    deletedDocumentsRetriesBulkLens,
    deletedDocumentsRetriesSearchLens,
  )
where

import qualified Data.Aeson.KeyMap as X
import qualified Data.HashMap.Strict as HM
import Data.Map.Strict (Map)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Versions as Versions
import Database.Bloodhound.Internal.Client.BHRequest
import Database.Bloodhound.Internal.Utils.Imports
import Database.Bloodhound.Internal.Utils.StringlyTyped
import Database.Bloodhound.Internal.Versions.Common.Types.Newtypes
import Database.Bloodhound.Internal.Versions.Common.Types.Units
import GHC.Generics

data NodeAttrFilter = NodeAttrFilter
  { nodeAttrFilterName :: NodeAttrName,
    nodeAttrFilterValues :: NonEmpty Text
  }
  deriving stock (Eq, Ord, Show)

nodeAttrFilterNameLens :: Lens' NodeAttrFilter NodeAttrName
nodeAttrFilterNameLens = lens nodeAttrFilterName (\x y -> x {nodeAttrFilterName = y})

nodeAttrFilterValuesLens :: Lens' NodeAttrFilter (NonEmpty Text)
nodeAttrFilterValuesLens = lens nodeAttrFilterValues (\x y -> x {nodeAttrFilterValues = y})

newtype NodeAttrName = NodeAttrName Text deriving stock (Eq, Ord, Show)

-- | 'NodeSelection' is used for most cluster APIs. See <https://www.elastic.co/guide/en/elasticsearch/reference/current/cluster.html#cluster-nodes here> for more details.
data NodeSelection
  = -- | Whatever node receives this request
    LocalNode
  | NodeList (NonEmpty NodeSelector)
  | AllNodes
  deriving stock (Eq, Show)

nodeSelectionNodeListPrism :: Prism' NodeSelection (NonEmpty NodeSelector)
nodeSelectionNodeListPrism = prism NodeList extract
  where
    extract s =
      case s of
        NodeList x -> Right x
        _ -> Left s

-- | An exact match or pattern to identify a node. Note that All of
-- these options support wildcarding, so your node name, server, attr
-- name can all contain * characters to be a fuzzy match.
data NodeSelector
  = NodeByName NodeName
  | NodeByFullNodeId FullNodeId
  | -- | e.g. 10.0.0.1 or even 10.0.0.*
    NodeByHost Server
  | -- | NodeAttrName can be a pattern, e.g. rack*. The value can too.
    NodeByAttribute NodeAttrName Text
  deriving stock (Eq, Show)

nodeSelectorNodeByNamePrism :: Prism' NodeSelector NodeName
nodeSelectorNodeByNamePrism = prism NodeByName extract
  where
    extract s =
      case s of
        NodeByName x -> Right x
        _ -> Left s

nodeSelectorNodeByFullNodeIdPrism :: Prism' NodeSelector FullNodeId
nodeSelectorNodeByFullNodeIdPrism = prism NodeByFullNodeId extract
  where
    extract s =
      case s of
        NodeByFullNodeId x -> Right x
        _ -> Left s

nodeSelectorNodeByHostPrism :: Prism' NodeSelector Server
nodeSelectorNodeByHostPrism = prism NodeByHost extract
  where
    extract s =
      case s of
        NodeByHost x -> Right x
        _ -> Left s

nodeSelectorNodeByAttributePrism :: Prism' NodeSelector (NodeAttrName, Text)
nodeSelectorNodeByAttributePrism = prism (uncurry NodeByAttribute) extract
  where
    extract s =
      case s of
        NodeByAttribute x y -> Right (x, y)
        _ -> Left s

-- | Unique, automatically-generated name assigned to nodes that are
-- usually returned in node-oriented APIs.
newtype FullNodeId = FullNodeId {fullNodeId :: Text}
  deriving newtype (Eq, Ord, Show, FromJSON)

-- | A human-readable node name that is supplied by the user in the
-- node config or automatically generated by Elasticsearch.
newtype NodeName = NodeName {nodeName :: Text}
  deriving newtype (Eq, Ord, Show, FromJSON)

newtype ClusterName = ClusterName {clusterName :: Text}
  deriving newtype (Eq, Ord, Show, FromJSON)

-- | Username type used for HTTP Basic authentication. See 'basicAuthHook'.
newtype EsUsername = EsUsername {esUsername :: Text} deriving (Read, Eq, Show)

-- | Password type used for HTTP Basic authentication. See 'basicAuthHook'.
newtype EsPassword = EsPassword {esPassword :: Text} deriving (Read, Eq, Show)

data NodesInfo = NodesInfo
  { nodesInfo :: [NodeInfo],
    nodesClusterName :: ClusterName
  }
  deriving stock (Eq, Show)

nodesInfoListLens :: Lens' NodesInfo [NodeInfo]
nodesInfoListLens = lens nodesInfo (\x y -> x {nodesInfo = y})

nodesClusterNameLens :: Lens' NodesInfo ClusterName
nodesClusterNameLens = lens nodesClusterName (\x y -> x {nodesClusterName = y})

data NodesStats = NodesStats
  { nodesStats :: [NodeStats],
    nodesStatsClusterName :: ClusterName
  }
  deriving stock (Eq, Show)

nodesStatsListLens :: Lens' NodesStats [NodeStats]
nodesStatsListLens = lens nodesStats (\x y -> x {nodesStats = y})

nodesStatsClusterNameLens :: Lens' NodesStats ClusterName
nodesStatsClusterNameLens = lens nodesStatsClusterName (\x y -> x {nodesStatsClusterName = y})

data NodeStats = NodeStats
  { nodeStatsName :: NodeName,
    nodeStatsFullId :: FullNodeId,
    nodeStatsBreakersStats :: Maybe NodeBreakersStats,
    nodeStatsHTTP :: NodeHTTPStats,
    nodeStatsTransport :: NodeTransportStats,
    nodeStatsFS :: NodeFSStats,
    nodeStatsNetwork :: Maybe NodeNetworkStats,
    nodeStatsThreadPool :: Map Text NodeThreadPoolStats,
    nodeStatsJVM :: NodeJVMStats,
    nodeStatsProcess :: NodeProcessStats,
    nodeStatsOS :: NodeOSStats,
    nodeStatsIndices :: NodeIndicesStats
  }
  deriving stock (Eq, Show)

nodeStatsNameLens :: Lens' NodeStats NodeName
nodeStatsNameLens = lens nodeStatsName (\x y -> x {nodeStatsName = y})

nodeStatsFullIdLens :: Lens' NodeStats FullNodeId
nodeStatsFullIdLens = lens nodeStatsFullId (\x y -> x {nodeStatsFullId = y})

nodeStatsBreakersStatsLens :: Lens' NodeStats (Maybe NodeBreakersStats)
nodeStatsBreakersStatsLens = lens nodeStatsBreakersStats (\x y -> x {nodeStatsBreakersStats = y})

nodeStatsHTTPLens :: Lens' NodeStats NodeHTTPStats
nodeStatsHTTPLens = lens nodeStatsHTTP (\x y -> x {nodeStatsHTTP = y})

nodeStatsTransportLens :: Lens' NodeStats NodeTransportStats
nodeStatsTransportLens = lens nodeStatsTransport (\x y -> x {nodeStatsTransport = y})

nodeStatsFSLens :: Lens' NodeStats NodeFSStats
nodeStatsFSLens = lens nodeStatsFS (\x y -> x {nodeStatsFS = y})

nodeStatsNetworkLens :: Lens' NodeStats (Maybe NodeNetworkStats)
nodeStatsNetworkLens = lens nodeStatsNetwork (\x y -> x {nodeStatsNetwork = y})

nodeStatsThreadPoolLens :: Lens' NodeStats (Map Text NodeThreadPoolStats)
nodeStatsThreadPoolLens = lens nodeStatsThreadPool (\x y -> x {nodeStatsThreadPool = y})

nodeStatsJVMLens :: Lens' NodeStats NodeJVMStats
nodeStatsJVMLens = lens nodeStatsJVM (\x y -> x {nodeStatsJVM = y})

nodeStatsProcessLens :: Lens' NodeStats NodeProcessStats
nodeStatsProcessLens = lens nodeStatsProcess (\x y -> x {nodeStatsProcess = y})

nodeStatsOSLens :: Lens' NodeStats NodeOSStats
nodeStatsOSLens = lens nodeStatsOS (\x y -> x {nodeStatsOS = y})

nodeStatsIndicesLens :: Lens' NodeStats NodeIndicesStats
nodeStatsIndicesLens = lens nodeStatsIndices (\x y -> x {nodeStatsIndices = y})

data NodeBreakersStats = NodeBreakersStats
  { nodeStatsParentBreaker :: NodeBreakerStats,
    nodeStatsRequestBreaker :: NodeBreakerStats,
    nodeStatsFieldDataBreaker :: NodeBreakerStats
  }
  deriving stock (Eq, Show)

nodeStatsParentBreakerLens :: Lens' NodeBreakersStats NodeBreakerStats
nodeStatsParentBreakerLens = lens nodeStatsParentBreaker (\x y -> x {nodeStatsParentBreaker = y})

nodeStatsRequestBreakerLens :: Lens' NodeBreakersStats NodeBreakerStats
nodeStatsRequestBreakerLens = lens nodeStatsRequestBreaker (\x y -> x {nodeStatsRequestBreaker = y})

nodeStatsFieldDataBreakerLens :: Lens' NodeBreakersStats NodeBreakerStats
nodeStatsFieldDataBreakerLens = lens nodeStatsFieldDataBreaker (\x y -> x {nodeStatsFieldDataBreaker = y})

data NodeBreakerStats = NodeBreakerStats
  { nodeBreakersTripped :: Int,
    nodeBreakersOverhead :: Double,
    nodeBreakersEstSize :: Bytes,
    nodeBreakersLimitSize :: Bytes
  }
  deriving stock (Eq, Show)

nodeBreakersStatsTrippedLens :: Lens' NodeBreakerStats Int
nodeBreakersStatsTrippedLens = lens nodeBreakersTripped (\x y -> x {nodeBreakersTripped = y})

nodeBreakersStatsOverheadLens :: Lens' NodeBreakerStats Double
nodeBreakersStatsOverheadLens = lens nodeBreakersOverhead (\x y -> x {nodeBreakersOverhead = y})

nodeBreakersStatsEstSizeLens :: Lens' NodeBreakerStats Bytes
nodeBreakersStatsEstSizeLens = lens nodeBreakersEstSize (\x y -> x {nodeBreakersEstSize = y})

nodeBreakersStatsLimitSizeLens :: Lens' NodeBreakerStats Bytes
nodeBreakersStatsLimitSizeLens = lens nodeBreakersLimitSize (\x y -> x {nodeBreakersLimitSize = y})

data NodeHTTPStats = NodeHTTPStats
  { nodeHTTPTotalOpened :: Int,
    nodeHTTPCurrentOpen :: Int
  }
  deriving stock (Eq, Show)

nodeHTTPTotalStatsOpenedLens :: Lens' NodeHTTPStats Int
nodeHTTPTotalStatsOpenedLens = lens nodeHTTPTotalOpened (\x y -> x {nodeHTTPTotalOpened = y})

nodeHTTPStatsCurrentOpenLens :: Lens' NodeHTTPStats Int
nodeHTTPStatsCurrentOpenLens = lens nodeHTTPCurrentOpen (\x y -> x {nodeHTTPCurrentOpen = y})

data NodeTransportStats = NodeTransportStats
  { nodeTransportTXSize :: Bytes,
    nodeTransportCount :: Int,
    nodeTransportRXSize :: Bytes,
    nodeTransportRXCount :: Int,
    nodeTransportServerOpen :: Int
  }
  deriving stock (Eq, Show)

nodeTransportStatsTXSizeLens :: Lens' NodeTransportStats Bytes
nodeTransportStatsTXSizeLens = lens nodeTransportTXSize (\x y -> x {nodeTransportTXSize = y})

nodeTransportStatsCountLens :: Lens' NodeTransportStats Int
nodeTransportStatsCountLens = lens nodeTransportCount (\x y -> x {nodeTransportCount = y})

nodeTransportStatsRXSizeLens :: Lens' NodeTransportStats Bytes
nodeTransportStatsRXSizeLens = lens nodeTransportRXSize (\x y -> x {nodeTransportRXSize = y})

nodeTransportStatsRXCountLens :: Lens' NodeTransportStats Int
nodeTransportStatsRXCountLens = lens nodeTransportRXCount (\x y -> x {nodeTransportRXCount = y})

nodeTransportStatsServerOpenLens :: Lens' NodeTransportStats Int
nodeTransportStatsServerOpenLens = lens nodeTransportServerOpen (\x y -> x {nodeTransportServerOpen = y})

data NodeFSStats = NodeFSStats
  { nodeFSDataPaths :: [NodeDataPathStats],
    nodeFSTotal :: NodeFSTotalStats,
    nodeFSTimestamp :: UTCTime
  }
  deriving stock (Eq, Show)

nodeFSStatsDataPathsLens :: Lens' NodeFSStats [NodeDataPathStats]
nodeFSStatsDataPathsLens = lens nodeFSDataPaths (\x y -> x {nodeFSDataPaths = y})

nodeFSStatsTotalLens :: Lens' NodeFSStats NodeFSTotalStats
nodeFSStatsTotalLens = lens nodeFSTotal (\x y -> x {nodeFSTotal = y})

nodeFSStatsTimestampLens :: Lens' NodeFSStats UTCTime
nodeFSStatsTimestampLens = lens nodeFSTimestamp (\x y -> x {nodeFSTimestamp = y})

data NodeDataPathStats = NodeDataPathStats
  { nodeDataPathDiskServiceTime :: Maybe Double,
    nodeDataPathDiskQueue :: Maybe Double,
    nodeDataPathIOSize :: Maybe Bytes,
    nodeDataPathWriteSize :: Maybe Bytes,
    nodeDataPathReadSize :: Maybe Bytes,
    nodeDataPathIOOps :: Maybe Int,
    nodeDataPathWrites :: Maybe Int,
    nodeDataPathReads :: Maybe Int,
    nodeDataPathAvailable :: Bytes,
    nodeDataPathFree :: Bytes,
    nodeDataPathTotal :: Bytes,
    nodeDataPathType :: Maybe Text,
    nodeDataPathDevice :: Maybe Text,
    nodeDataPathMount :: Text,
    nodeDataPathPath :: Text
  }
  deriving stock (Eq, Show)

nodeDataPathStatsDiskServiceTimeLens :: Lens' NodeDataPathStats (Maybe Double)
nodeDataPathStatsDiskServiceTimeLens = lens nodeDataPathDiskServiceTime (\x y -> x {nodeDataPathDiskServiceTime = y})

nodeDataPathStatsDiskQueueLens :: Lens' NodeDataPathStats (Maybe Double)
nodeDataPathStatsDiskQueueLens = lens nodeDataPathDiskQueue (\x y -> x {nodeDataPathDiskQueue = y})

nodeDataPathStatsIOSizeLens :: Lens' NodeDataPathStats (Maybe Bytes)
nodeDataPathStatsIOSizeLens = lens nodeDataPathIOSize (\x y -> x {nodeDataPathIOSize = y})

nodeDataPathStatsWriteSizeLens :: Lens' NodeDataPathStats (Maybe Bytes)
nodeDataPathStatsWriteSizeLens = lens nodeDataPathWriteSize (\x y -> x {nodeDataPathWriteSize = y})

nodeDataPathStatsReadSizeLens :: Lens' NodeDataPathStats (Maybe Bytes)
nodeDataPathStatsReadSizeLens = lens nodeDataPathReadSize (\x y -> x {nodeDataPathReadSize = y})

nodeDataPathStatsIOOpsLens :: Lens' NodeDataPathStats (Maybe Int)
nodeDataPathStatsIOOpsLens = lens nodeDataPathIOOps (\x y -> x {nodeDataPathIOOps = y})

nodeDataPathStatsWritesLens :: Lens' NodeDataPathStats (Maybe Int)
nodeDataPathStatsWritesLens = lens nodeDataPathWrites (\x y -> x {nodeDataPathWrites = y})

nodeDataPathStatsReadsLens :: Lens' NodeDataPathStats (Maybe Int)
nodeDataPathStatsReadsLens = lens nodeDataPathReads (\x y -> x {nodeDataPathReads = y})

nodeDataPathStatsAvailableLens :: Lens' NodeDataPathStats Bytes
nodeDataPathStatsAvailableLens = lens nodeDataPathAvailable (\x y -> x {nodeDataPathAvailable = y})

nodeDataPathStatsFreeLens :: Lens' NodeDataPathStats Bytes
nodeDataPathStatsFreeLens = lens nodeDataPathFree (\x y -> x {nodeDataPathFree = y})

nodeDataPathStatsTotalLens :: Lens' NodeDataPathStats Bytes
nodeDataPathStatsTotalLens = lens nodeDataPathTotal (\x y -> x {nodeDataPathTotal = y})

nodeDataPathStatsTypeLens :: Lens' NodeDataPathStats (Maybe Text)
nodeDataPathStatsTypeLens = lens nodeDataPathType (\x y -> x {nodeDataPathType = y})

nodeDataPathStatsDeviceLens :: Lens' NodeDataPathStats (Maybe Text)
nodeDataPathStatsDeviceLens = lens nodeDataPathDevice (\x y -> x {nodeDataPathDevice = y})

nodeDataPathStatsMountLens :: Lens' NodeDataPathStats Text
nodeDataPathStatsMountLens = lens nodeDataPathMount (\x y -> x {nodeDataPathMount = y})

nodeDataPathStatsPathLens :: Lens' NodeDataPathStats Text
nodeDataPathStatsPathLens = lens nodeDataPathPath (\x y -> x {nodeDataPathPath = y})

data NodeFSTotalStats = NodeFSTotalStats
  { nodeFSTotalDiskServiceTime :: Maybe Double,
    nodeFSTotalDiskQueue :: Maybe Double,
    nodeFSTotalIOSize :: Maybe Bytes,
    nodeFSTotalWriteSize :: Maybe Bytes,
    nodeFSTotalReadSize :: Maybe Bytes,
    nodeFSTotalIOOps :: Maybe Int,
    nodeFSTotalWrites :: Maybe Int,
    nodeFSTotalReads :: Maybe Int,
    nodeFSTotalAvailable :: Bytes,
    nodeFSTotalFree :: Bytes,
    nodeFSTotalTotal :: Bytes
  }
  deriving stock (Eq, Show)

nodeFSTotalStatsDiskServiceTimeLens :: Lens' NodeFSTotalStats (Maybe Double)
nodeFSTotalStatsDiskServiceTimeLens = lens nodeFSTotalDiskServiceTime (\x y -> x {nodeFSTotalDiskServiceTime = y})

nodeFSTotalStatsDiskQueueLens :: Lens' NodeFSTotalStats (Maybe Double)
nodeFSTotalStatsDiskQueueLens = lens nodeFSTotalDiskQueue (\x y -> x {nodeFSTotalDiskQueue = y})

nodeFSTotalStatsIOSizeLens :: Lens' NodeFSTotalStats (Maybe Bytes)
nodeFSTotalStatsIOSizeLens = lens nodeFSTotalIOSize (\x y -> x {nodeFSTotalIOSize = y})

nodeFSTotalStatsWriteSizeLens :: Lens' NodeFSTotalStats (Maybe Bytes)
nodeFSTotalStatsWriteSizeLens = lens nodeFSTotalWriteSize (\x y -> x {nodeFSTotalWriteSize = y})

nodeFSTotalStatsReadSizeLens :: Lens' NodeFSTotalStats (Maybe Bytes)
nodeFSTotalStatsReadSizeLens = lens nodeFSTotalReadSize (\x y -> x {nodeFSTotalReadSize = y})

nodeFSTotalStatsIOOpsLens :: Lens' NodeFSTotalStats (Maybe Int)
nodeFSTotalStatsIOOpsLens = lens nodeFSTotalIOOps (\x y -> x {nodeFSTotalIOOps = y})

nodeFSTotalStatsWritesLens :: Lens' NodeFSTotalStats (Maybe Int)
nodeFSTotalStatsWritesLens = lens nodeFSTotalWrites (\x y -> x {nodeFSTotalWrites = y})

nodeFSTotalStatsReadsLens :: Lens' NodeFSTotalStats (Maybe Int)
nodeFSTotalStatsReadsLens = lens nodeFSTotalReads (\x y -> x {nodeFSTotalReads = y})

nodeFSTotalStatsAvailableLens :: Lens' NodeFSTotalStats Bytes
nodeFSTotalStatsAvailableLens = lens nodeFSTotalAvailable (\x y -> x {nodeFSTotalAvailable = y})

nodeFSTotalStatsFreeLens :: Lens' NodeFSTotalStats Bytes
nodeFSTotalStatsFreeLens = lens nodeFSTotalFree (\x y -> x {nodeFSTotalFree = y})

nodeFSTotalStatsTotalLens :: Lens' NodeFSTotalStats Bytes
nodeFSTotalStatsTotalLens = lens nodeFSTotalTotal (\x y -> x {nodeFSTotalTotal = y})

data NodeNetworkStats = NodeNetworkStats
  { nodeNetTCPOutRSTs :: Int,
    nodeNetTCPInErrs :: Int,
    nodeNetTCPAttemptFails :: Int,
    nodeNetTCPEstabResets :: Int,
    nodeNetTCPRetransSegs :: Int,
    nodeNetTCPOutSegs :: Int,
    nodeNetTCPInSegs :: Int,
    nodeNetTCPCurrEstab :: Int,
    nodeNetTCPPassiveOpens :: Int,
    nodeNetTCPActiveOpens :: Int
  }
  deriving stock (Eq, Show)

nodeNetworkStatsTCPOutRSTsLens :: Lens' NodeNetworkStats Int
nodeNetworkStatsTCPOutRSTsLens = lens nodeNetTCPOutRSTs (\x y -> x {nodeNetTCPOutRSTs = y})

nodeNetworkStatsTCPInErrsLens :: Lens' NodeNetworkStats Int
nodeNetworkStatsTCPInErrsLens = lens nodeNetTCPInErrs (\x y -> x {nodeNetTCPInErrs = y})

nodeNetworkStatsTCPAttemptFailsLens :: Lens' NodeNetworkStats Int
nodeNetworkStatsTCPAttemptFailsLens = lens nodeNetTCPAttemptFails (\x y -> x {nodeNetTCPAttemptFails = y})

nodeNetworkStatsTCPEstabResetsLens :: Lens' NodeNetworkStats Int
nodeNetworkStatsTCPEstabResetsLens = lens nodeNetTCPEstabResets (\x y -> x {nodeNetTCPEstabResets = y})

nodeNetworkStatsTCPRetransSegsLens :: Lens' NodeNetworkStats Int
nodeNetworkStatsTCPRetransSegsLens = lens nodeNetTCPRetransSegs (\x y -> x {nodeNetTCPRetransSegs = y})

nodeNetworkStatsTCPOutSegsLens :: Lens' NodeNetworkStats Int
nodeNetworkStatsTCPOutSegsLens = lens nodeNetTCPOutSegs (\x y -> x {nodeNetTCPOutSegs = y})

nodeNetworkStatsTCPInSegsLens :: Lens' NodeNetworkStats Int
nodeNetworkStatsTCPInSegsLens = lens nodeNetTCPInSegs (\x y -> x {nodeNetTCPInSegs = y})

nodeNetworkStatsTCPCurrEstabLens :: Lens' NodeNetworkStats Int
nodeNetworkStatsTCPCurrEstabLens = lens nodeNetTCPCurrEstab (\x y -> x {nodeNetTCPCurrEstab = y})

nodeNetworkStatsTCPPassiveOpensLens :: Lens' NodeNetworkStats Int
nodeNetworkStatsTCPPassiveOpensLens = lens nodeNetTCPPassiveOpens (\x y -> x {nodeNetTCPPassiveOpens = y})

nodeNetworkStatsTCPActiveOpensLens :: Lens' NodeNetworkStats Int
nodeNetworkStatsTCPActiveOpensLens = lens nodeNetTCPActiveOpens (\x y -> x {nodeNetTCPActiveOpens = y})

data NodeThreadPoolStats = NodeThreadPoolStats
  { nodeThreadPoolCompleted :: Int,
    nodeThreadPoolLargest :: Int,
    nodeThreadPoolRejected :: Int,
    nodeThreadPoolActive :: Int,
    nodeThreadPoolQueue :: Int,
    nodeThreadPoolThreads :: Int
  }
  deriving stock (Eq, Show)

nodeThreadPoolStatsCompletedLens :: Lens' NodeThreadPoolStats Int
nodeThreadPoolStatsCompletedLens = lens nodeThreadPoolCompleted (\x y -> x {nodeThreadPoolCompleted = y})

nodeThreadPoolStatsLargestLens :: Lens' NodeThreadPoolStats Int
nodeThreadPoolStatsLargestLens = lens nodeThreadPoolLargest (\x y -> x {nodeThreadPoolLargest = y})

nodeThreadPoolStatsRejectedLens :: Lens' NodeThreadPoolStats Int
nodeThreadPoolStatsRejectedLens = lens nodeThreadPoolRejected (\x y -> x {nodeThreadPoolRejected = y})

nodeThreadPoolStatsActiveLens :: Lens' NodeThreadPoolStats Int
nodeThreadPoolStatsActiveLens = lens nodeThreadPoolActive (\x y -> x {nodeThreadPoolActive = y})

nodeThreadPoolStatsQueueLens :: Lens' NodeThreadPoolStats Int
nodeThreadPoolStatsQueueLens = lens nodeThreadPoolQueue (\x y -> x {nodeThreadPoolQueue = y})

nodeThreadPoolStatsThreadsLens :: Lens' NodeThreadPoolStats Int
nodeThreadPoolStatsThreadsLens = lens nodeThreadPoolThreads (\x y -> x {nodeThreadPoolThreads = y})

data NodeJVMStats = NodeJVMStats
  { nodeJVMStatsMappedBufferPool :: JVMBufferPoolStats,
    nodeJVMStatsDirectBufferPool :: JVMBufferPoolStats,
    nodeJVMStatsGCOldCollector :: JVMGCStats,
    nodeJVMStatsGCYoungCollector :: JVMGCStats,
    nodeJVMStatsPeakThreadsCount :: Int,
    nodeJVMStatsThreadsCount :: Int,
    nodeJVMStatsOldPool :: JVMPoolStats,
    nodeJVMStatsSurvivorPool :: JVMPoolStats,
    nodeJVMStatsYoungPool :: JVMPoolStats,
    nodeJVMStatsNonHeapCommitted :: Bytes,
    nodeJVMStatsNonHeapUsed :: Bytes,
    nodeJVMStatsHeapMax :: Bytes,
    nodeJVMStatsHeapCommitted :: Bytes,
    nodeJVMStatsHeapUsedPercent :: Int,
    nodeJVMStatsHeapUsed :: Bytes,
    nodeJVMStatsUptime :: NominalDiffTime,
    nodeJVMStatsTimestamp :: UTCTime
  }
  deriving stock (Eq, Show)

nodeJVMStatsMappedBufferPoolLens :: Lens' NodeJVMStats JVMBufferPoolStats
nodeJVMStatsMappedBufferPoolLens = lens nodeJVMStatsMappedBufferPool (\x y -> x {nodeJVMStatsMappedBufferPool = y})

nodeJVMStatsDirectBufferPoolLens :: Lens' NodeJVMStats JVMBufferPoolStats
nodeJVMStatsDirectBufferPoolLens = lens nodeJVMStatsDirectBufferPool (\x y -> x {nodeJVMStatsDirectBufferPool = y})

nodeJVMStatsGCOldCollectorLens :: Lens' NodeJVMStats JVMGCStats
nodeJVMStatsGCOldCollectorLens = lens nodeJVMStatsGCOldCollector (\x y -> x {nodeJVMStatsGCOldCollector = y})

nodeJVMStatsGCYoungCollectorLens :: Lens' NodeJVMStats JVMGCStats
nodeJVMStatsGCYoungCollectorLens = lens nodeJVMStatsGCYoungCollector (\x y -> x {nodeJVMStatsGCYoungCollector = y})

nodeJVMStatsPeakThreadsCountLens :: Lens' NodeJVMStats Int
nodeJVMStatsPeakThreadsCountLens = lens nodeJVMStatsPeakThreadsCount (\x y -> x {nodeJVMStatsPeakThreadsCount = y})

nodeJVMStatsThreadsCountLens :: Lens' NodeJVMStats Int
nodeJVMStatsThreadsCountLens = lens nodeJVMStatsThreadsCount (\x y -> x {nodeJVMStatsThreadsCount = y})

nodeJVMStatsOldPoolLens :: Lens' NodeJVMStats JVMPoolStats
nodeJVMStatsOldPoolLens = lens nodeJVMStatsOldPool (\x y -> x {nodeJVMStatsOldPool = y})

nodeJVMStatsSurvivorPoolLens :: Lens' NodeJVMStats JVMPoolStats
nodeJVMStatsSurvivorPoolLens = lens nodeJVMStatsSurvivorPool (\x y -> x {nodeJVMStatsSurvivorPool = y})

nodeJVMStatsYoungPoolLens :: Lens' NodeJVMStats JVMPoolStats
nodeJVMStatsYoungPoolLens = lens nodeJVMStatsYoungPool (\x y -> x {nodeJVMStatsYoungPool = y})

nodeJVMStatsNonHeapCommittedLens :: Lens' NodeJVMStats Bytes
nodeJVMStatsNonHeapCommittedLens = lens nodeJVMStatsNonHeapCommitted (\x y -> x {nodeJVMStatsNonHeapCommitted = y})

nodeJVMStatsNonHeapUsedLens :: Lens' NodeJVMStats Bytes
nodeJVMStatsNonHeapUsedLens = lens nodeJVMStatsNonHeapUsed (\x y -> x {nodeJVMStatsNonHeapUsed = y})

nodeJVMStatsHeapMaxLens :: Lens' NodeJVMStats Bytes
nodeJVMStatsHeapMaxLens = lens nodeJVMStatsHeapMax (\x y -> x {nodeJVMStatsHeapMax = y})

nodeJVMStatsHeapCommittedLens :: Lens' NodeJVMStats Bytes
nodeJVMStatsHeapCommittedLens = lens nodeJVMStatsHeapCommitted (\x y -> x {nodeJVMStatsHeapCommitted = y})

nodeJVMStatsHeapUsedPercentLens :: Lens' NodeJVMStats Int
nodeJVMStatsHeapUsedPercentLens = lens nodeJVMStatsHeapUsedPercent (\x y -> x {nodeJVMStatsHeapUsedPercent = y})

nodeJVMStatsHeapUsedLens :: Lens' NodeJVMStats Bytes
nodeJVMStatsHeapUsedLens = lens nodeJVMStatsHeapUsed (\x y -> x {nodeJVMStatsHeapUsed = y})

nodeJVMStatsUptimeLens :: Lens' NodeJVMStats NominalDiffTime
nodeJVMStatsUptimeLens = lens nodeJVMStatsUptime (\x y -> x {nodeJVMStatsUptime = y})

nodeJVMStatsTimestampLens :: Lens' NodeJVMStats UTCTime
nodeJVMStatsTimestampLens = lens nodeJVMStatsTimestamp (\x y -> x {nodeJVMStatsTimestamp = y})

data JVMBufferPoolStats = JVMBufferPoolStats
  { jvmBufferPoolStatsTotalCapacity :: Bytes,
    jvmBufferPoolStatsUsed :: Bytes,
    jvmBufferPoolStatsCount :: Int
  }
  deriving stock (Eq, Show)

jvmBufferPoolStatsTotalCapacityLens :: Lens' JVMBufferPoolStats Bytes
jvmBufferPoolStatsTotalCapacityLens = lens jvmBufferPoolStatsTotalCapacity (\x y -> x {jvmBufferPoolStatsTotalCapacity = y})

jvmBufferPoolStatsUsedLens :: Lens' JVMBufferPoolStats Bytes
jvmBufferPoolStatsUsedLens = lens jvmBufferPoolStatsUsed (\x y -> x {jvmBufferPoolStatsUsed = y})

jvmBufferPoolStatsCountLens :: Lens' JVMBufferPoolStats Int
jvmBufferPoolStatsCountLens = lens jvmBufferPoolStatsCount (\x y -> x {jvmBufferPoolStatsCount = y})

data JVMGCStats = JVMGCStats
  { jvmGCStatsCollectionTime :: NominalDiffTime,
    jvmGCStatsCollectionCount :: Int
  }
  deriving stock (Eq, Show)

jvmGCStatsCollectionTimeLens :: Lens' JVMGCStats NominalDiffTime
jvmGCStatsCollectionTimeLens = lens jvmGCStatsCollectionTime (\x y -> x {jvmGCStatsCollectionTime = y})

jvmGCStatsCollectionCountLens :: Lens' JVMGCStats Int
jvmGCStatsCollectionCountLens = lens jvmGCStatsCollectionCount (\x y -> x {jvmGCStatsCollectionCount = y})

data JVMPoolStats = JVMPoolStats
  { jvmPoolStatsPeakMax :: Bytes,
    jvmPoolStatsPeakUsed :: Bytes,
    jvmPoolStatsMax :: Bytes,
    jvmPoolStatsUsed :: Bytes
  }
  deriving stock (Eq, Show)

jvmPoolStatsPeakMaxLens :: Lens' JVMPoolStats Bytes
jvmPoolStatsPeakMaxLens = lens jvmPoolStatsPeakMax (\x y -> x {jvmPoolStatsPeakMax = y})

jvmPoolStatsPeakUsedLens :: Lens' JVMPoolStats Bytes
jvmPoolStatsPeakUsedLens = lens jvmPoolStatsPeakUsed (\x y -> x {jvmPoolStatsPeakUsed = y})

jvmPoolStatsMaxLens :: Lens' JVMPoolStats Bytes
jvmPoolStatsMaxLens = lens jvmPoolStatsMax (\x y -> x {jvmPoolStatsMax = y})

jvmPoolStatsUsedLens :: Lens' JVMPoolStats Bytes
jvmPoolStatsUsedLens = lens jvmPoolStatsUsed (\x y -> x {jvmPoolStatsUsed = y})

data NodeProcessStats = NodeProcessStats
  { nodeProcessTimestamp :: UTCTime,
    nodeProcessOpenFDs :: Int,
    nodeProcessMaxFDs :: Int,
    nodeProcessCPUPercent :: Int,
    nodeProcessCPUTotal :: NominalDiffTime,
    nodeProcessMemTotalVirtual :: Bytes
  }
  deriving stock (Eq, Show)

nodeProcessStatsTimestampLens :: Lens' NodeProcessStats UTCTime
nodeProcessStatsTimestampLens = lens nodeProcessTimestamp (\x y -> x {nodeProcessTimestamp = y})

nodeProcessStatsOpenFDsLens :: Lens' NodeProcessStats Int
nodeProcessStatsOpenFDsLens = lens nodeProcessOpenFDs (\x y -> x {nodeProcessOpenFDs = y})

nodeProcessStatsMaxFDsLens :: Lens' NodeProcessStats Int
nodeProcessStatsMaxFDsLens = lens nodeProcessMaxFDs (\x y -> x {nodeProcessMaxFDs = y})

nodeProcessStatsCPUPercentLens :: Lens' NodeProcessStats Int
nodeProcessStatsCPUPercentLens = lens nodeProcessCPUPercent (\x y -> x {nodeProcessCPUPercent = y})

nodeProcessStatsCPUTotalLens :: Lens' NodeProcessStats NominalDiffTime
nodeProcessStatsCPUTotalLens = lens nodeProcessCPUTotal (\x y -> x {nodeProcessCPUTotal = y})

nodeProcessStatsMemTotalVirtualLens :: Lens' NodeProcessStats Bytes
nodeProcessStatsMemTotalVirtualLens = lens nodeProcessMemTotalVirtual (\x y -> x {nodeProcessMemTotalVirtual = y})

data NodeOSStats = NodeOSStats
  { nodeOSTimestamp :: UTCTime,
    nodeOSCPUPercent :: Int,
    nodeOSLoad :: Maybe LoadAvgs,
    nodeOSMemTotal :: Bytes,
    nodeOSMemFree :: Bytes,
    nodeOSMemFreePercent :: Int,
    nodeOSMemUsed :: Bytes,
    nodeOSMemUsedPercent :: Int,
    nodeOSSwapTotal :: Bytes,
    nodeOSSwapFree :: Bytes,
    nodeOSSwapUsed :: Bytes
  }
  deriving stock (Eq, Show)

nodeOSStatsTimestampLens :: Lens' NodeOSStats UTCTime
nodeOSStatsTimestampLens = lens nodeOSTimestamp (\x y -> x {nodeOSTimestamp = y})

nodeOSStatsCPUPercentLens :: Lens' NodeOSStats Int
nodeOSStatsCPUPercentLens = lens nodeOSCPUPercent (\x y -> x {nodeOSCPUPercent = y})

nodeOSStatsLoadLens :: Lens' NodeOSStats (Maybe LoadAvgs)
nodeOSStatsLoadLens = lens nodeOSLoad (\x y -> x {nodeOSLoad = y})

nodeOSStatsMemTotalLens :: Lens' NodeOSStats Bytes
nodeOSStatsMemTotalLens = lens nodeOSMemTotal (\x y -> x {nodeOSMemTotal = y})

nodeOSStatsMemFreeLens :: Lens' NodeOSStats Bytes
nodeOSStatsMemFreeLens = lens nodeOSMemFree (\x y -> x {nodeOSMemFree = y})

nodeOSStatsMemFreePercentLens :: Lens' NodeOSStats Int
nodeOSStatsMemFreePercentLens = lens nodeOSMemFreePercent (\x y -> x {nodeOSMemFreePercent = y})

nodeOSStatsMemUsedLens :: Lens' NodeOSStats Bytes
nodeOSStatsMemUsedLens = lens nodeOSMemUsed (\x y -> x {nodeOSMemUsed = y})

nodeOSStatsMemUsedPercentLens :: Lens' NodeOSStats Int
nodeOSStatsMemUsedPercentLens = lens nodeOSMemUsedPercent (\x y -> x {nodeOSMemUsedPercent = y})

nodeOSStatsSwapTotalLens :: Lens' NodeOSStats Bytes
nodeOSStatsSwapTotalLens = lens nodeOSSwapTotal (\x y -> x {nodeOSSwapTotal = y})

nodeOSStatsSwapFreeLens :: Lens' NodeOSStats Bytes
nodeOSStatsSwapFreeLens = lens nodeOSSwapFree (\x y -> x {nodeOSSwapFree = y})

nodeOSStatsSwapUsedLens :: Lens' NodeOSStats Bytes
nodeOSStatsSwapUsedLens = lens nodeOSSwapUsed (\x y -> x {nodeOSSwapUsed = y})

data LoadAvgs = LoadAvgs
  { loadAvg1Min :: Double,
    loadAvg5Min :: Double,
    loadAvg15Min :: Double
  }
  deriving stock (Eq, Show)

loadAvgs1MinLens :: Lens' LoadAvgs Double
loadAvgs1MinLens = lens loadAvg1Min (\x y -> x {loadAvg1Min = y})

loadAvgs5MinLens :: Lens' LoadAvgs Double
loadAvgs5MinLens = lens loadAvg5Min (\x y -> x {loadAvg5Min = y})

loadAvgs15MinLens :: Lens' LoadAvgs Double
loadAvgs15MinLens = lens loadAvg15Min (\x y -> x {loadAvg15Min = y})

data NodeIndicesStats = NodeIndicesStats
  { nodeIndicesStatsRecoveryThrottleTime :: Maybe NominalDiffTime,
    nodeIndicesStatsRecoveryCurrentAsTarget :: Maybe Int,
    nodeIndicesStatsRecoveryCurrentAsSource :: Maybe Int,
    nodeIndicesStatsQueryCacheMisses :: Maybe Int,
    nodeIndicesStatsQueryCacheHits :: Maybe Int,
    nodeIndicesStatsQueryCacheEvictions :: Maybe Int,
    nodeIndicesStatsQueryCacheSize :: Maybe Bytes,
    nodeIndicesStatsSuggestCurrent :: Maybe Int,
    nodeIndicesStatsSuggestTime :: Maybe NominalDiffTime,
    nodeIndicesStatsSuggestTotal :: Maybe Int,
    nodeIndicesStatsTranslogSize :: Bytes,
    nodeIndicesStatsTranslogOps :: Int,
    nodeIndicesStatsSegFixedBitSetMemory :: Maybe Bytes,
    nodeIndicesStatsSegVersionMapMemory :: Bytes,
    nodeIndicesStatsSegIndexWriterMaxMemory :: Maybe Bytes,
    nodeIndicesStatsSegIndexWriterMemory :: Bytes,
    nodeIndicesStatsSegMemory :: Bytes,
    nodeIndicesStatsSegCount :: Int,
    nodeIndicesStatsCompletionSize :: Bytes,
    nodeIndicesStatsPercolateQueries :: Maybe Int,
    nodeIndicesStatsPercolateMemory :: Maybe Bytes,
    nodeIndicesStatsPercolateCurrent :: Maybe Int,
    nodeIndicesStatsPercolateTime :: Maybe NominalDiffTime,
    nodeIndicesStatsPercolateTotal :: Maybe Int,
    nodeIndicesStatsFieldDataEvictions :: Int,
    nodeIndicesStatsFieldDataMemory :: Bytes,
    nodeIndicesStatsWarmerTotalTime :: NominalDiffTime,
    nodeIndicesStatsWarmerTotal :: Int,
    nodeIndicesStatsWarmerCurrent :: Int,
    nodeIndicesStatsFlushTotalTime :: NominalDiffTime,
    nodeIndicesStatsFlushTotal :: Int,
    nodeIndicesStatsRefreshTotalTime :: NominalDiffTime,
    nodeIndicesStatsRefreshTotal :: Int,
    nodeIndicesStatsMergesTotalSize :: Bytes,
    nodeIndicesStatsMergesTotalDocs :: Int,
    nodeIndicesStatsMergesTotalTime :: NominalDiffTime,
    nodeIndicesStatsMergesTotal :: Int,
    nodeIndicesStatsMergesCurrentSize :: Bytes,
    nodeIndicesStatsMergesCurrentDocs :: Int,
    nodeIndicesStatsMergesCurrent :: Int,
    nodeIndicesStatsSearchFetchCurrent :: Int,
    nodeIndicesStatsSearchFetchTime :: NominalDiffTime,
    nodeIndicesStatsSearchFetchTotal :: Int,
    nodeIndicesStatsSearchQueryCurrent :: Int,
    nodeIndicesStatsSearchQueryTime :: NominalDiffTime,
    nodeIndicesStatsSearchQueryTotal :: Int,
    nodeIndicesStatsSearchOpenContexts :: Int,
    nodeIndicesStatsGetCurrent :: Int,
    nodeIndicesStatsGetMissingTime :: NominalDiffTime,
    nodeIndicesStatsGetMissingTotal :: Int,
    nodeIndicesStatsGetExistsTime :: NominalDiffTime,
    nodeIndicesStatsGetExistsTotal :: Int,
    nodeIndicesStatsGetTime :: NominalDiffTime,
    nodeIndicesStatsGetTotal :: Int,
    nodeIndicesStatsIndexingThrottleTime :: Maybe NominalDiffTime,
    nodeIndicesStatsIndexingIsThrottled :: Maybe Bool,
    nodeIndicesStatsIndexingNoopUpdateTotal :: Maybe Int,
    nodeIndicesStatsIndexingDeleteCurrent :: Int,
    nodeIndicesStatsIndexingDeleteTime :: NominalDiffTime,
    nodeIndicesStatsIndexingDeleteTotal :: Int,
    nodeIndicesStatsIndexingIndexCurrent :: Int,
    nodeIndicesStatsIndexingIndexTime :: NominalDiffTime,
    nodeIndicesStatsIndexingTotal :: Int,
    nodeIndicesStatsStoreThrottleTime :: Maybe NominalDiffTime,
    nodeIndicesStatsStoreSize :: Bytes,
    nodeIndicesStatsDocsDeleted :: Int,
    nodeIndicesStatsDocsCount :: Int
  }
  deriving stock (Eq, Show)

nodeIndicesStatsRecoveryThrottleTimeLens :: Lens' NodeIndicesStats (Maybe NominalDiffTime)
nodeIndicesStatsRecoveryThrottleTimeLens = lens nodeIndicesStatsRecoveryThrottleTime (\x y -> x {nodeIndicesStatsRecoveryThrottleTime = y})

nodeIndicesStatsRecoveryCurrentAsTargetLens :: Lens' NodeIndicesStats (Maybe Int)
nodeIndicesStatsRecoveryCurrentAsTargetLens = lens nodeIndicesStatsRecoveryCurrentAsTarget (\x y -> x {nodeIndicesStatsRecoveryCurrentAsTarget = y})

nodeIndicesStatsRecoveryCurrentAsSourceLens :: Lens' NodeIndicesStats (Maybe Int)
nodeIndicesStatsRecoveryCurrentAsSourceLens = lens nodeIndicesStatsRecoveryCurrentAsSource (\x y -> x {nodeIndicesStatsRecoveryCurrentAsSource = y})

nodeIndicesStatsQueryCacheMissesLens :: Lens' NodeIndicesStats (Maybe Int)
nodeIndicesStatsQueryCacheMissesLens = lens nodeIndicesStatsQueryCacheMisses (\x y -> x {nodeIndicesStatsQueryCacheMisses = y})

nodeIndicesStatsQueryCacheHitsLens :: Lens' NodeIndicesStats (Maybe Int)
nodeIndicesStatsQueryCacheHitsLens = lens nodeIndicesStatsQueryCacheHits (\x y -> x {nodeIndicesStatsQueryCacheHits = y})

nodeIndicesStatsQueryCacheEvictionsLens :: Lens' NodeIndicesStats (Maybe Int)
nodeIndicesStatsQueryCacheEvictionsLens = lens nodeIndicesStatsQueryCacheEvictions (\x y -> x {nodeIndicesStatsQueryCacheEvictions = y})

nodeIndicesStatsQueryCacheSizeLens :: Lens' NodeIndicesStats (Maybe Bytes)
nodeIndicesStatsQueryCacheSizeLens = lens nodeIndicesStatsQueryCacheSize (\x y -> x {nodeIndicesStatsQueryCacheSize = y})

nodeIndicesStatsSuggestCurrentLens :: Lens' NodeIndicesStats (Maybe Int)
nodeIndicesStatsSuggestCurrentLens = lens nodeIndicesStatsSuggestCurrent (\x y -> x {nodeIndicesStatsSuggestCurrent = y})

nodeIndicesStatsSuggestTimeLens :: Lens' NodeIndicesStats (Maybe NominalDiffTime)
nodeIndicesStatsSuggestTimeLens = lens nodeIndicesStatsSuggestTime (\x y -> x {nodeIndicesStatsSuggestTime = y})

nodeIndicesStatsSuggestTotalLens :: Lens' NodeIndicesStats (Maybe Int)
nodeIndicesStatsSuggestTotalLens = lens nodeIndicesStatsSuggestTotal (\x y -> x {nodeIndicesStatsSuggestTotal = y})

nodeIndicesStatsTranslogSizeLens :: Lens' NodeIndicesStats Bytes
nodeIndicesStatsTranslogSizeLens = lens nodeIndicesStatsTranslogSize (\x y -> x {nodeIndicesStatsTranslogSize = y})

nodeIndicesStatsTranslogOpsLens :: Lens' NodeIndicesStats Int
nodeIndicesStatsTranslogOpsLens = lens nodeIndicesStatsTranslogOps (\x y -> x {nodeIndicesStatsTranslogOps = y})

nodeIndicesStatsSegFixedBitSetMemoryLens :: Lens' NodeIndicesStats (Maybe Bytes)
nodeIndicesStatsSegFixedBitSetMemoryLens = lens nodeIndicesStatsSegFixedBitSetMemory (\x y -> x {nodeIndicesStatsSegFixedBitSetMemory = y})

nodeIndicesStatsSegVersionMapMemoryLens :: Lens' NodeIndicesStats Bytes
nodeIndicesStatsSegVersionMapMemoryLens = lens nodeIndicesStatsSegVersionMapMemory (\x y -> x {nodeIndicesStatsSegVersionMapMemory = y})

nodeIndicesStatsSegIndexWriterMaxMemoryLens :: Lens' NodeIndicesStats (Maybe Bytes)
nodeIndicesStatsSegIndexWriterMaxMemoryLens = lens nodeIndicesStatsSegIndexWriterMaxMemory (\x y -> x {nodeIndicesStatsSegIndexWriterMaxMemory = y})

nodeIndicesStatsSegIndexWriterMemoryLens :: Lens' NodeIndicesStats Bytes
nodeIndicesStatsSegIndexWriterMemoryLens = lens nodeIndicesStatsSegIndexWriterMemory (\x y -> x {nodeIndicesStatsSegIndexWriterMemory = y})

nodeIndicesStatsSegMemoryLens :: Lens' NodeIndicesStats Bytes
nodeIndicesStatsSegMemoryLens = lens nodeIndicesStatsSegMemory (\x y -> x {nodeIndicesStatsSegMemory = y})

nodeIndicesStatsSegCountLens :: Lens' NodeIndicesStats Int
nodeIndicesStatsSegCountLens = lens nodeIndicesStatsSegCount (\x y -> x {nodeIndicesStatsSegCount = y})

nodeIndicesStatsCompletionSizeLens :: Lens' NodeIndicesStats Bytes
nodeIndicesStatsCompletionSizeLens = lens nodeIndicesStatsCompletionSize (\x y -> x {nodeIndicesStatsCompletionSize = y})

nodeIndicesStatsPercolateQueriesLens :: Lens' NodeIndicesStats (Maybe Int)
nodeIndicesStatsPercolateQueriesLens = lens nodeIndicesStatsPercolateQueries (\x y -> x {nodeIndicesStatsPercolateQueries = y})

nodeIndicesStatsPercolateMemoryLens :: Lens' NodeIndicesStats (Maybe Bytes)
nodeIndicesStatsPercolateMemoryLens = lens nodeIndicesStatsPercolateMemory (\x y -> x {nodeIndicesStatsPercolateMemory = y})

nodeIndicesStatsPercolateCurrentLens :: Lens' NodeIndicesStats (Maybe Int)
nodeIndicesStatsPercolateCurrentLens = lens nodeIndicesStatsPercolateCurrent (\x y -> x {nodeIndicesStatsPercolateCurrent = y})

nodeIndicesStatsPercolateTimeLens :: Lens' NodeIndicesStats (Maybe NominalDiffTime)
nodeIndicesStatsPercolateTimeLens = lens nodeIndicesStatsPercolateTime (\x y -> x {nodeIndicesStatsPercolateTime = y})

nodeIndicesStatsPercolateTotalLens :: Lens' NodeIndicesStats (Maybe Int)
nodeIndicesStatsPercolateTotalLens = lens nodeIndicesStatsPercolateTotal (\x y -> x {nodeIndicesStatsPercolateTotal = y})

nodeIndicesStatsFieldDataEvictionsLens :: Lens' NodeIndicesStats Int
nodeIndicesStatsFieldDataEvictionsLens = lens nodeIndicesStatsFieldDataEvictions (\x y -> x {nodeIndicesStatsFieldDataEvictions = y})

nodeIndicesStatsFieldDataMemoryLens :: Lens' NodeIndicesStats Bytes
nodeIndicesStatsFieldDataMemoryLens = lens nodeIndicesStatsFieldDataMemory (\x y -> x {nodeIndicesStatsFieldDataMemory = y})

nodeIndicesStatsWarmerTotalTimeLens :: Lens' NodeIndicesStats NominalDiffTime
nodeIndicesStatsWarmerTotalTimeLens = lens nodeIndicesStatsWarmerTotalTime (\x y -> x {nodeIndicesStatsWarmerTotalTime = y})

nodeIndicesStatsWarmerTotalLens :: Lens' NodeIndicesStats Int
nodeIndicesStatsWarmerTotalLens = lens nodeIndicesStatsWarmerTotal (\x y -> x {nodeIndicesStatsWarmerTotal = y})

nodeIndicesStatsWarmerCurrentLens :: Lens' NodeIndicesStats Int
nodeIndicesStatsWarmerCurrentLens = lens nodeIndicesStatsWarmerCurrent (\x y -> x {nodeIndicesStatsWarmerCurrent = y})

nodeIndicesStatsFlushTotalTimeLens :: Lens' NodeIndicesStats NominalDiffTime
nodeIndicesStatsFlushTotalTimeLens = lens nodeIndicesStatsFlushTotalTime (\x y -> x {nodeIndicesStatsFlushTotalTime = y})

nodeIndicesStatsFlushTotalLens :: Lens' NodeIndicesStats Int
nodeIndicesStatsFlushTotalLens = lens nodeIndicesStatsFlushTotal (\x y -> x {nodeIndicesStatsFlushTotal = y})

nodeIndicesStatsRefreshTotalTimeLens :: Lens' NodeIndicesStats NominalDiffTime
nodeIndicesStatsRefreshTotalTimeLens = lens nodeIndicesStatsRefreshTotalTime (\x y -> x {nodeIndicesStatsRefreshTotalTime = y})

nodeIndicesStatsRefreshTotalLens :: Lens' NodeIndicesStats Int
nodeIndicesStatsRefreshTotalLens = lens nodeIndicesStatsRefreshTotal (\x y -> x {nodeIndicesStatsRefreshTotal = y})

nodeIndicesStatsMergesTotalSizeLens :: Lens' NodeIndicesStats Bytes
nodeIndicesStatsMergesTotalSizeLens = lens nodeIndicesStatsMergesTotalSize (\x y -> x {nodeIndicesStatsMergesTotalSize = y})

nodeIndicesStatsMergesTotalDocsLens :: Lens' NodeIndicesStats Int
nodeIndicesStatsMergesTotalDocsLens = lens nodeIndicesStatsMergesTotalDocs (\x y -> x {nodeIndicesStatsMergesTotalDocs = y})

nodeIndicesStatsMergesTotalTimeLens :: Lens' NodeIndicesStats NominalDiffTime
nodeIndicesStatsMergesTotalTimeLens = lens nodeIndicesStatsMergesTotalTime (\x y -> x {nodeIndicesStatsMergesTotalTime = y})

nodeIndicesStatsMergesTotalLens :: Lens' NodeIndicesStats Int
nodeIndicesStatsMergesTotalLens = lens nodeIndicesStatsMergesTotal (\x y -> x {nodeIndicesStatsMergesTotal = y})

nodeIndicesStatsMergesCurrentSizeLens :: Lens' NodeIndicesStats Bytes
nodeIndicesStatsMergesCurrentSizeLens = lens nodeIndicesStatsMergesCurrentSize (\x y -> x {nodeIndicesStatsMergesCurrentSize = y})

nodeIndicesStatsMergesCurrentDocsLens :: Lens' NodeIndicesStats Int
nodeIndicesStatsMergesCurrentDocsLens = lens nodeIndicesStatsMergesCurrentDocs (\x y -> x {nodeIndicesStatsMergesCurrentDocs = y})

nodeIndicesStatsMergesCurrentLens :: Lens' NodeIndicesStats Int
nodeIndicesStatsMergesCurrentLens = lens nodeIndicesStatsMergesCurrent (\x y -> x {nodeIndicesStatsMergesCurrent = y})

nodeIndicesStatsSearchFetchCurrentLens :: Lens' NodeIndicesStats Int
nodeIndicesStatsSearchFetchCurrentLens = lens nodeIndicesStatsSearchFetchCurrent (\x y -> x {nodeIndicesStatsSearchFetchCurrent = y})

nodeIndicesStatsSearchFetchTimeLens :: Lens' NodeIndicesStats NominalDiffTime
nodeIndicesStatsSearchFetchTimeLens = lens nodeIndicesStatsSearchFetchTime (\x y -> x {nodeIndicesStatsSearchFetchTime = y})

nodeIndicesStatsSearchFetchTotalLens :: Lens' NodeIndicesStats Int
nodeIndicesStatsSearchFetchTotalLens = lens nodeIndicesStatsSearchFetchTotal (\x y -> x {nodeIndicesStatsSearchFetchTotal = y})

nodeIndicesStatsSearchQueryCurrentLens :: Lens' NodeIndicesStats Int
nodeIndicesStatsSearchQueryCurrentLens = lens nodeIndicesStatsSearchQueryCurrent (\x y -> x {nodeIndicesStatsSearchQueryCurrent = y})

nodeIndicesStatsSearchQueryTimeLens :: Lens' NodeIndicesStats NominalDiffTime
nodeIndicesStatsSearchQueryTimeLens = lens nodeIndicesStatsSearchQueryTime (\x y -> x {nodeIndicesStatsSearchQueryTime = y})

nodeIndicesStatsSearchQueryTotalLens :: Lens' NodeIndicesStats Int
nodeIndicesStatsSearchQueryTotalLens = lens nodeIndicesStatsSearchQueryTotal (\x y -> x {nodeIndicesStatsSearchQueryTotal = y})

nodeIndicesStatsSearchOpenContextsLens :: Lens' NodeIndicesStats Int
nodeIndicesStatsSearchOpenContextsLens = lens nodeIndicesStatsSearchOpenContexts (\x y -> x {nodeIndicesStatsSearchOpenContexts = y})

nodeIndicesStatsGetCurrentLens :: Lens' NodeIndicesStats Int
nodeIndicesStatsGetCurrentLens = lens nodeIndicesStatsGetCurrent (\x y -> x {nodeIndicesStatsGetCurrent = y})

nodeIndicesStatsGetMissingTimeLens :: Lens' NodeIndicesStats NominalDiffTime
nodeIndicesStatsGetMissingTimeLens = lens nodeIndicesStatsGetMissingTime (\x y -> x {nodeIndicesStatsGetMissingTime = y})

nodeIndicesStatsGetMissingTotalLens :: Lens' NodeIndicesStats Int
nodeIndicesStatsGetMissingTotalLens = lens nodeIndicesStatsGetMissingTotal (\x y -> x {nodeIndicesStatsGetMissingTotal = y})

nodeIndicesStatsGetExistsTimeLens :: Lens' NodeIndicesStats NominalDiffTime
nodeIndicesStatsGetExistsTimeLens = lens nodeIndicesStatsGetExistsTime (\x y -> x {nodeIndicesStatsGetExistsTime = y})

nodeIndicesStatsGetExistsTotalLens :: Lens' NodeIndicesStats Int
nodeIndicesStatsGetExistsTotalLens = lens nodeIndicesStatsGetExistsTotal (\x y -> x {nodeIndicesStatsGetExistsTotal = y})

nodeIndicesStatsGetTimeLens :: Lens' NodeIndicesStats NominalDiffTime
nodeIndicesStatsGetTimeLens = lens nodeIndicesStatsGetTime (\x y -> x {nodeIndicesStatsGetTime = y})

nodeIndicesStatsGetTotalLens :: Lens' NodeIndicesStats Int
nodeIndicesStatsGetTotalLens = lens nodeIndicesStatsGetTotal (\x y -> x {nodeIndicesStatsGetTotal = y})

nodeIndicesStatsIndexingThrottleTimeLens :: Lens' NodeIndicesStats (Maybe NominalDiffTime)
nodeIndicesStatsIndexingThrottleTimeLens = lens nodeIndicesStatsIndexingThrottleTime (\x y -> x {nodeIndicesStatsIndexingThrottleTime = y})

nodeIndicesStatsIndexingIsThrottledLens :: Lens' NodeIndicesStats (Maybe Bool)
nodeIndicesStatsIndexingIsThrottledLens = lens nodeIndicesStatsIndexingIsThrottled (\x y -> x {nodeIndicesStatsIndexingIsThrottled = y})

nodeIndicesStatsIndexingNoopUpdateTotalLens :: Lens' NodeIndicesStats (Maybe Int)
nodeIndicesStatsIndexingNoopUpdateTotalLens = lens nodeIndicesStatsIndexingNoopUpdateTotal (\x y -> x {nodeIndicesStatsIndexingNoopUpdateTotal = y})

nodeIndicesStatsIndexingDeleteCurrentLens :: Lens' NodeIndicesStats Int
nodeIndicesStatsIndexingDeleteCurrentLens = lens nodeIndicesStatsIndexingDeleteCurrent (\x y -> x {nodeIndicesStatsIndexingDeleteCurrent = y})

nodeIndicesStatsIndexingDeleteTimeLens :: Lens' NodeIndicesStats NominalDiffTime
nodeIndicesStatsIndexingDeleteTimeLens = lens nodeIndicesStatsIndexingDeleteTime (\x y -> x {nodeIndicesStatsIndexingDeleteTime = y})

nodeIndicesStatsIndexingDeleteTotalLens :: Lens' NodeIndicesStats Int
nodeIndicesStatsIndexingDeleteTotalLens = lens nodeIndicesStatsIndexingDeleteTotal (\x y -> x {nodeIndicesStatsIndexingDeleteTotal = y})

nodeIndicesStatsIndexingIndexCurrentLens :: Lens' NodeIndicesStats Int
nodeIndicesStatsIndexingIndexCurrentLens = lens nodeIndicesStatsIndexingIndexCurrent (\x y -> x {nodeIndicesStatsIndexingIndexCurrent = y})

nodeIndicesStatsIndexingIndexTimeLens :: Lens' NodeIndicesStats NominalDiffTime
nodeIndicesStatsIndexingIndexTimeLens = lens nodeIndicesStatsIndexingIndexTime (\x y -> x {nodeIndicesStatsIndexingIndexTime = y})

nodeIndicesStatsIndexingTotalLens :: Lens' NodeIndicesStats Int
nodeIndicesStatsIndexingTotalLens = lens nodeIndicesStatsIndexingTotal (\x y -> x {nodeIndicesStatsIndexingTotal = y})

nodeIndicesStatsStoreThrottleTimeLens :: Lens' NodeIndicesStats (Maybe NominalDiffTime)
nodeIndicesStatsStoreThrottleTimeLens = lens nodeIndicesStatsStoreThrottleTime (\x y -> x {nodeIndicesStatsStoreThrottleTime = y})

nodeIndicesStatsStoreSizeLens :: Lens' NodeIndicesStats Bytes
nodeIndicesStatsStoreSizeLens = lens nodeIndicesStatsStoreSize (\x y -> x {nodeIndicesStatsStoreSize = y})

nodeIndicesStatsDocsDeletedLens :: Lens' NodeIndicesStats Int
nodeIndicesStatsDocsDeletedLens = lens nodeIndicesStatsDocsDeleted (\x y -> x {nodeIndicesStatsDocsDeleted = y})

nodeIndicesStatsDocsCountLens :: Lens' NodeIndicesStats Int
nodeIndicesStatsDocsCountLens = lens nodeIndicesStatsDocsCount (\x y -> x {nodeIndicesStatsDocsCount = y})

-- | A quirky address format used throughout Elasticsearch. An example
-- would be inet[/1.1.1.1:9200]. inet may be a placeholder for a
-- <https://en.wikipedia.org/wiki/Fully_qualified_domain_name FQDN>.
newtype EsAddress = EsAddress {esAddress :: Text}
  deriving newtype (Eq, Ord, Show, FromJSON)

-- | Typically a 7 character hex string.
newtype BuildHash = BuildHash {buildHash :: Text}
  deriving newtype (Eq, Ord, Show, FromJSON, ToJSON)

newtype PluginName = PluginName {pluginName :: Text}
  deriving newtype (Eq, Ord, Show, FromJSON)

data NodeInfo = NodeInfo
  { nodeInfoHTTPAddress :: Maybe EsAddress,
    nodeInfoBuild :: BuildHash,
    nodeInfoESVersion :: VersionNumber,
    nodeInfoIP :: Server,
    nodeInfoHost :: Server,
    nodeInfoTransportAddress :: EsAddress,
    nodeInfoName :: NodeName,
    nodeInfoFullId :: FullNodeId,
    nodeInfoPlugins :: [NodePluginInfo],
    nodeInfoHTTP :: NodeHTTPInfo,
    nodeInfoTransport :: NodeTransportInfo,
    nodeInfoNetwork :: Maybe NodeNetworkInfo,
    nodeInfoThreadPool :: Map Text NodeThreadPoolInfo,
    nodeInfoJVM :: NodeJVMInfo,
    nodeInfoProcess :: NodeProcessInfo,
    nodeInfoOS :: NodeOSInfo,
    -- | The members of the settings objects are not consistent,
    -- dependent on plugins, etc.
    nodeInfoSettings :: Object
  }
  deriving stock (Eq, Show)

nodeInfoHTTPAddressLens :: Lens' NodeInfo (Maybe EsAddress)
nodeInfoHTTPAddressLens = lens nodeInfoHTTPAddress (\x y -> x {nodeInfoHTTPAddress = y})

nodeInfoBuildLens :: Lens' NodeInfo BuildHash
nodeInfoBuildLens = lens nodeInfoBuild (\x y -> x {nodeInfoBuild = y})

nodeInfoESVersionLens :: Lens' NodeInfo VersionNumber
nodeInfoESVersionLens = lens nodeInfoESVersion (\x y -> x {nodeInfoESVersion = y})

nodeInfoIPLens :: Lens' NodeInfo Server
nodeInfoIPLens = lens nodeInfoIP (\x y -> x {nodeInfoIP = y})

nodeInfoHostLens :: Lens' NodeInfo Server
nodeInfoHostLens = lens nodeInfoHost (\x y -> x {nodeInfoHost = y})

nodeInfoTransportAddressLens :: Lens' NodeInfo EsAddress
nodeInfoTransportAddressLens = lens nodeInfoTransportAddress (\x y -> x {nodeInfoTransportAddress = y})

nodeInfoNameLens :: Lens' NodeInfo NodeName
nodeInfoNameLens = lens nodeInfoName (\x y -> x {nodeInfoName = y})

nodeInfoFullIdLens :: Lens' NodeInfo FullNodeId
nodeInfoFullIdLens = lens nodeInfoFullId (\x y -> x {nodeInfoFullId = y})

nodeInfoPluginsLens :: Lens' NodeInfo [NodePluginInfo]
nodeInfoPluginsLens = lens nodeInfoPlugins (\x y -> x {nodeInfoPlugins = y})

nodeInfoHTTPLens :: Lens' NodeInfo NodeHTTPInfo
nodeInfoHTTPLens = lens nodeInfoHTTP (\x y -> x {nodeInfoHTTP = y})

nodeInfoTransportLens :: Lens' NodeInfo NodeTransportInfo
nodeInfoTransportLens = lens nodeInfoTransport (\x y -> x {nodeInfoTransport = y})

nodeInfoNetworkLens :: Lens' NodeInfo (Maybe NodeNetworkInfo)
nodeInfoNetworkLens = lens nodeInfoNetwork (\x y -> x {nodeInfoNetwork = y})

nodeInfoThreadPoolLens :: Lens' NodeInfo (Map Text NodeThreadPoolInfo)
nodeInfoThreadPoolLens = lens nodeInfoThreadPool (\x y -> x {nodeInfoThreadPool = y})

nodeInfoJVMLens :: Lens' NodeInfo NodeJVMInfo
nodeInfoJVMLens = lens nodeInfoJVM (\x y -> x {nodeInfoJVM = y})

nodeInfoProcessLens :: Lens' NodeInfo NodeProcessInfo
nodeInfoProcessLens = lens nodeInfoProcess (\x y -> x {nodeInfoProcess = y})

nodeInfoOSLens :: Lens' NodeInfo NodeOSInfo
nodeInfoOSLens = lens nodeInfoOS (\x y -> x {nodeInfoOS = y})

nodeInfoSettingsLens :: Lens' NodeInfo Object
nodeInfoSettingsLens = lens nodeInfoSettings (\x y -> x {nodeInfoSettings = y})

data NodePluginInfo = NodePluginInfo
  { -- | Is this a site plugin?
    nodePluginSite :: Maybe Bool,
    -- | Is this plugin running on the JVM
    nodePluginJVM :: Maybe Bool,
    nodePluginDescription :: Text,
    nodePluginVersion :: MaybeNA VersionNumber,
    nodePluginName :: PluginName
  }
  deriving stock (Eq, Show)

nodePluginSiteLens :: Lens' NodePluginInfo (Maybe Bool)
nodePluginSiteLens = lens nodePluginSite (\x y -> x {nodePluginSite = y})

-- \| Is this plugin running on the JVM
nodePluginInfoJVMLens :: Lens' NodePluginInfo (Maybe Bool)
nodePluginInfoJVMLens = lens nodePluginJVM (\x y -> x {nodePluginJVM = y})

nodePluginInfoDescriptionLens :: Lens' NodePluginInfo Text
nodePluginInfoDescriptionLens = lens nodePluginDescription (\x y -> x {nodePluginDescription = y})

nodePluginInfoVersionLens :: Lens' NodePluginInfo (MaybeNA VersionNumber)
nodePluginInfoVersionLens = lens nodePluginVersion (\x y -> x {nodePluginVersion = y})

nodePluginInfoNameLens :: Lens' NodePluginInfo PluginName
nodePluginInfoNameLens = lens nodePluginName (\x y -> x {nodePluginName = y})

data NodeHTTPInfo = NodeHTTPInfo
  { nodeHTTPMaxContentLength :: Bytes,
    nodeHTTPpublishAddress :: EsAddress,
    nodeHTTPbound_address :: [EsAddress]
  }
  deriving stock (Eq, Show)

nodeHTTPInfoMaxContentLengthLens :: Lens' NodeHTTPInfo Bytes
nodeHTTPInfoMaxContentLengthLens = lens nodeHTTPMaxContentLength (\x y -> x {nodeHTTPMaxContentLength = y})

nodeHTTPInfopublishAddressLens :: Lens' NodeHTTPInfo EsAddress
nodeHTTPInfopublishAddressLens = lens nodeHTTPpublishAddress (\x y -> x {nodeHTTPpublishAddress = y})

nodeHTTPInfoBoundAddressesLens :: Lens' NodeHTTPInfo [EsAddress]
nodeHTTPInfoBoundAddressesLens = lens nodeHTTPbound_address (\x y -> x {nodeHTTPbound_address = y})

data NodeTransportInfo = NodeTransportInfo
  { nodeTransportProfiles :: [BoundTransportAddress],
    nodeTransportPublishAddress :: EsAddress,
    nodeTransportBoundAddress :: [EsAddress]
  }
  deriving stock (Eq, Show)

nodeTransportInfoProfilesLens :: Lens' NodeTransportInfo [BoundTransportAddress]
nodeTransportInfoProfilesLens = lens nodeTransportProfiles (\x y -> x {nodeTransportProfiles = y})

nodeTransportInfoPublishAddressLens :: Lens' NodeTransportInfo EsAddress
nodeTransportInfoPublishAddressLens = lens nodeTransportPublishAddress (\x y -> x {nodeTransportPublishAddress = y})

nodeTransportInfoBoundAddressLens :: Lens' NodeTransportInfo [EsAddress]
nodeTransportInfoBoundAddressLens = lens nodeTransportBoundAddress (\x y -> x {nodeTransportBoundAddress = y})

data BoundTransportAddress = BoundTransportAddress
  { publishAddress :: EsAddress,
    boundAddress :: [EsAddress]
  }
  deriving stock (Eq, Show)

boundTransportAddressPublishAddressLens :: Lens' BoundTransportAddress EsAddress
boundTransportAddressPublishAddressLens = lens publishAddress (\x y -> x {publishAddress = y})

boundTransportAddressBoundAddressesLens :: Lens' BoundTransportAddress [EsAddress]
boundTransportAddressBoundAddressesLens = lens boundAddress (\x y -> x {boundAddress = y})

data NodeNetworkInfo = NodeNetworkInfo
  { nodeNetworkPrimaryInterface :: NodeNetworkInterface,
    nodeNetworkRefreshInterval :: NominalDiffTime
  }
  deriving stock (Eq, Show)

nodeNetworkInfoPrimaryInterfaceLens :: Lens' NodeNetworkInfo NodeNetworkInterface
nodeNetworkInfoPrimaryInterfaceLens = lens nodeNetworkPrimaryInterface (\x y -> x {nodeNetworkPrimaryInterface = y})

nodeNetworkInfoRefreshIntervalLens :: Lens' NodeNetworkInfo NominalDiffTime
nodeNetworkInfoRefreshIntervalLens = lens nodeNetworkRefreshInterval (\x y -> x {nodeNetworkRefreshInterval = y})

newtype MacAddress = MacAddress {macAddress :: Text}
  deriving newtype (Eq, Ord, Show, FromJSON)

newtype NetworkInterfaceName = NetworkInterfaceName {networkInterfaceName :: Text}
  deriving newtype (Eq, Ord, Show, FromJSON)

data NodeNetworkInterface = NodeNetworkInterface
  { nodeNetIfaceMacAddress :: MacAddress,
    nodeNetIfaceName :: NetworkInterfaceName,
    nodeNetIfaceAddress :: Server
  }
  deriving stock (Eq, Show)

nodeNetworkInterfaceMacAddressLens :: Lens' NodeNetworkInterface MacAddress
nodeNetworkInterfaceMacAddressLens = lens nodeNetIfaceMacAddress (\x y -> x {nodeNetIfaceMacAddress = y})

nodeNetworkInterfaceNameLens :: Lens' NodeNetworkInterface NetworkInterfaceName
nodeNetworkInterfaceNameLens = lens nodeNetIfaceName (\x y -> x {nodeNetIfaceName = y})

nodeNetworkInterfaceAddressLens :: Lens' NodeNetworkInterface Server
nodeNetworkInterfaceAddressLens = lens nodeNetIfaceAddress (\x y -> x {nodeNetIfaceAddress = y})

data ThreadPool = ThreadPool
  { nodeThreadPoolName :: Text,
    nodeThreadPoolInfo :: NodeThreadPoolInfo
  }
  deriving stock (Eq, Show)

threadPoolNodeThreadPoolNameLens :: Lens' ThreadPool Text
threadPoolNodeThreadPoolNameLens = lens nodeThreadPoolName (\x y -> x {nodeThreadPoolName = y})

threadPoolNodeThreadPoolInfoLens :: Lens' ThreadPool NodeThreadPoolInfo
threadPoolNodeThreadPoolInfoLens = lens nodeThreadPoolInfo (\x y -> x {nodeThreadPoolInfo = y})

data NodeThreadPoolInfo = NodeThreadPoolInfo
  { nodeThreadPoolQueueSize :: ThreadPoolSize,
    nodeThreadPoolKeepalive :: Maybe NominalDiffTime,
    nodeThreadPoolMin :: Maybe Int,
    nodeThreadPoolMax :: Maybe Int,
    nodeThreadPoolType :: ThreadPoolType
  }
  deriving stock (Eq, Show)

nodeThreadPoolInfoQueueSizeLens :: Lens' NodeThreadPoolInfo ThreadPoolSize
nodeThreadPoolInfoQueueSizeLens = lens nodeThreadPoolQueueSize (\x y -> x {nodeThreadPoolQueueSize = y})

nodeThreadPoolInfoKeepaliveLens :: Lens' NodeThreadPoolInfo (Maybe NominalDiffTime)
nodeThreadPoolInfoKeepaliveLens = lens nodeThreadPoolKeepalive (\x y -> x {nodeThreadPoolKeepalive = y})

nodeThreadPoolInfoMinLens :: Lens' NodeThreadPoolInfo (Maybe Int)
nodeThreadPoolInfoMinLens = lens nodeThreadPoolMin (\x y -> x {nodeThreadPoolMin = y})

nodeThreadPoolInfoMaxLens :: Lens' NodeThreadPoolInfo (Maybe Int)
nodeThreadPoolInfoMaxLens = lens nodeThreadPoolMax (\x y -> x {nodeThreadPoolMax = y})

nodeThreadPoolInfoTypeLens :: Lens' NodeThreadPoolInfo ThreadPoolType
nodeThreadPoolInfoTypeLens = lens nodeThreadPoolType (\x y -> x {nodeThreadPoolType = y})

data ThreadPoolSize
  = ThreadPoolBounded Int
  | ThreadPoolUnbounded
  deriving stock (Eq, Show)

threadPoolSizeBoundedPrism :: Prism' ThreadPoolSize Int
threadPoolSizeBoundedPrism = prism ThreadPoolBounded extract
  where
    extract s =
      case s of
        ThreadPoolBounded x -> Right x
        _ -> Left s

data ThreadPoolType
  = ThreadPoolScaling
  | ThreadPoolFixed
  | ThreadPoolCached
  | ThreadPoolFixedAutoQueueSize
  deriving stock (Eq, Show)

data NodeJVMInfo = NodeJVMInfo
  { nodeJVMInfoMemoryPools :: [JVMMemoryPool],
    nodeJVMInfoMemoryPoolsGCCollectors :: [JVMGCCollector],
    nodeJVMInfoMemoryInfo :: JVMMemoryInfo,
    nodeJVMInfoStartTime :: UTCTime,
    nodeJVMInfoVMVendor :: Text,
    -- | JVM doesn't seme to follow normal version conventions
    nodeJVMVMVersion :: VersionNumber,
    nodeJVMVMName :: Text,
    nodeJVMVersion :: JVMVersion,
    nodeJVMPID :: PID
  }
  deriving stock (Eq, Show)

nodeJVMInfoMemoryPoolsLens :: Lens' NodeJVMInfo [JVMMemoryPool]
nodeJVMInfoMemoryPoolsLens = lens nodeJVMInfoMemoryPools (\x y -> x {nodeJVMInfoMemoryPools = y})

nodeJVMInfoMemoryPoolsGCCollectorsLens :: Lens' NodeJVMInfo [JVMGCCollector]
nodeJVMInfoMemoryPoolsGCCollectorsLens = lens nodeJVMInfoMemoryPoolsGCCollectors (\x y -> x {nodeJVMInfoMemoryPoolsGCCollectors = y})

nodeJVMInfoMemoryInfoLens :: Lens' NodeJVMInfo JVMMemoryInfo
nodeJVMInfoMemoryInfoLens = lens nodeJVMInfoMemoryInfo (\x y -> x {nodeJVMInfoMemoryInfo = y})

nodeJVMInfoStartTimeLens :: Lens' NodeJVMInfo UTCTime
nodeJVMInfoStartTimeLens = lens nodeJVMInfoStartTime (\x y -> x {nodeJVMInfoStartTime = y})

nodeJVMInfoVMVendorLens :: Lens' NodeJVMInfo Text
nodeJVMInfoVMVendorLens = lens nodeJVMInfoVMVendor (\x y -> x {nodeJVMInfoVMVendor = y})

nodeJVMInfoVMVersionLens :: Lens' NodeJVMInfo VersionNumber
nodeJVMInfoVMVersionLens = lens nodeJVMVMVersion (\x y -> x {nodeJVMVMVersion = y})

nodeJVMInfoVMNameLens :: Lens' NodeJVMInfo Text
nodeJVMInfoVMNameLens = lens nodeJVMVMName (\x y -> x {nodeJVMVMName = y})

nodeJVMInfoVersionLens :: Lens' NodeJVMInfo JVMVersion
nodeJVMInfoVersionLens = lens nodeJVMVersion (\x y -> x {nodeJVMVersion = y})

nodeJVMInfoPIDLens :: Lens' NodeJVMInfo PID
nodeJVMInfoPIDLens = lens nodeJVMPID (\x y -> x {nodeJVMPID = y})

-- | We cannot parse JVM version numbers and we're not going to try.
newtype JVMVersion = JVMVersion {unJVMVersion :: Text}
  deriving stock (Eq, Show)

instance FromJSON JVMVersion where
  parseJSON = withText "JVMVersion" (pure . JVMVersion)

data JVMMemoryInfo = JVMMemoryInfo
  { jvmMemoryInfoDirectMax :: Bytes,
    jvmMemoryInfoNonHeapMax :: Bytes,
    jvmMemoryInfoNonHeapInit :: Bytes,
    jvmMemoryInfoHeapMax :: Bytes,
    jvmMemoryInfoHeapInit :: Bytes
  }
  deriving stock (Eq, Show)

jvmMemoryInfoDirectMaxLens :: Lens' JVMMemoryInfo Bytes
jvmMemoryInfoDirectMaxLens = lens jvmMemoryInfoDirectMax (\x y -> x {jvmMemoryInfoDirectMax = y})

jvmMemoryInfoNonHeapMaxLens :: Lens' JVMMemoryInfo Bytes
jvmMemoryInfoNonHeapMaxLens = lens jvmMemoryInfoNonHeapMax (\x y -> x {jvmMemoryInfoNonHeapMax = y})

jvmMemoryInfoNonHeapInitLens :: Lens' JVMMemoryInfo Bytes
jvmMemoryInfoNonHeapInitLens = lens jvmMemoryInfoNonHeapInit (\x y -> x {jvmMemoryInfoNonHeapInit = y})

jvmMemoryInfoHeapMaxLens :: Lens' JVMMemoryInfo Bytes
jvmMemoryInfoHeapMaxLens = lens jvmMemoryInfoHeapMax (\x y -> x {jvmMemoryInfoHeapMax = y})

jvmMemoryInfoHeapInitLens :: Lens' JVMMemoryInfo Bytes
jvmMemoryInfoHeapInitLens = lens jvmMemoryInfoHeapInit (\x y -> x {jvmMemoryInfoHeapInit = y})

newtype JVMMemoryPool = JVMMemoryPool
  { jvmMemoryPool :: Text
  }
  deriving newtype (Eq, Show, FromJSON)

newtype JVMGCCollector = JVMGCCollector
  { jvmGCCollector :: Text
  }
  deriving newtype (Eq, Show, FromJSON)

newtype PID = PID
  { pid :: Int
  }
  deriving newtype (Eq, Show, FromJSON)

data NodeOSInfo = NodeOSInfo
  { nodeOSRefreshInterval :: NominalDiffTime,
    nodeOSName :: Text,
    nodeOSArch :: Text,
    nodeOSVersion :: Text, -- semver breaks on "5.10.60.1-microsoft-standard-WSL2"
    nodeOSAvailableProcessors :: Int,
    nodeOSAllocatedProcessors :: Int
  }
  deriving stock (Eq, Show)

nodeOSInfoRefreshIntervalLens :: Lens' NodeOSInfo NominalDiffTime
nodeOSInfoRefreshIntervalLens = lens nodeOSRefreshInterval (\x y -> x {nodeOSRefreshInterval = y})

nodeOSInfoNameLens :: Lens' NodeOSInfo Text
nodeOSInfoNameLens = lens nodeOSName (\x y -> x {nodeOSName = y})

nodeOSInfoArchLens :: Lens' NodeOSInfo Text
nodeOSInfoArchLens = lens nodeOSArch (\x y -> x {nodeOSArch = y})

nodeOSInfoVersionLens :: Lens' NodeOSInfo Text
nodeOSInfoVersionLens = lens nodeOSVersion (\x y -> x {nodeOSVersion = y})

nodeOSInfoAvailableProcessorsLens :: Lens' NodeOSInfo Int
nodeOSInfoAvailableProcessorsLens = lens nodeOSAvailableProcessors (\x y -> x {nodeOSAvailableProcessors = y})

nodeOSInfoAllocatedProcessorsLens :: Lens' NodeOSInfo Int
nodeOSInfoAllocatedProcessorsLens = lens nodeOSAllocatedProcessors (\x y -> x {nodeOSAllocatedProcessors = y})

data CPUInfo = CPUInfo
  { cpuCacheSize :: Bytes,
    cpuCoresPerSocket :: Int,
    cpuTotalSockets :: Int,
    cpuTotalCores :: Int,
    cpuMHZ :: Int,
    cpuModel :: Text,
    cpuVendor :: Text
  }
  deriving stock (Eq, Show)

cpuInfoCacheSizeLens :: Lens' CPUInfo Bytes
cpuInfoCacheSizeLens = lens cpuCacheSize (\x y -> x {cpuCacheSize = y})

cpuInfoCoresPerSocketLens :: Lens' CPUInfo Int
cpuInfoCoresPerSocketLens = lens cpuCoresPerSocket (\x y -> x {cpuCoresPerSocket = y})

cpuInfoTotalSocketsLens :: Lens' CPUInfo Int
cpuInfoTotalSocketsLens = lens cpuTotalSockets (\x y -> x {cpuTotalSockets = y})

cpuInfoTotalCoresLens :: Lens' CPUInfo Int
cpuInfoTotalCoresLens = lens cpuTotalCores (\x y -> x {cpuTotalCores = y})

cpuInfoMHZLens :: Lens' CPUInfo Int
cpuInfoMHZLens = lens cpuMHZ (\x y -> x {cpuMHZ = y})

cpuInfoModelLens :: Lens' CPUInfo Text
cpuInfoModelLens = lens cpuModel (\x y -> x {cpuModel = y})

cpuInfoVendorLens :: Lens' CPUInfo Text
cpuInfoVendorLens = lens cpuVendor (\x y -> x {cpuVendor = y})

data NodeProcessInfo = NodeProcessInfo
  { -- | See <https://www.elastic.co/guide/en/elasticsearch/reference/current/setup-configuration.html>
    nodeProcessMLockAll :: Bool,
    nodeProcessMaxFileDescriptors :: Maybe Int,
    nodeProcessId :: PID,
    nodeProcessRefreshInterval :: NominalDiffTime
  }
  deriving stock (Eq, Show)

nodeProcessInfoMLockAllLens :: Lens' NodeProcessInfo Bool
nodeProcessInfoMLockAllLens = lens nodeProcessMLockAll (\x y -> x {nodeProcessMLockAll = y})

nodeProcessInfoMaxFileDescriptorsLens :: Lens' NodeProcessInfo (Maybe Int)
nodeProcessInfoMaxFileDescriptorsLens = lens nodeProcessMaxFileDescriptors (\x y -> x {nodeProcessMaxFileDescriptors = y})

nodeProcessInfoIdLens :: Lens' NodeProcessInfo PID
nodeProcessInfoIdLens = lens nodeProcessId (\x y -> x {nodeProcessId = y})

nodeProcessInfoRefreshIntervalLens :: Lens' NodeProcessInfo NominalDiffTime
nodeProcessInfoRefreshIntervalLens = lens nodeProcessRefreshInterval (\x y -> x {nodeProcessRefreshInterval = y})

instance FromJSON NodesInfo where
  parseJSON = withObject "NodesInfo" parse
    where
      parse o = do
        nodes <- o .: "nodes"
        infos <- forM (HM.toList nodes) $ \(fullNID, v) -> do
          node <- parseJSON v
          parseNodeInfo (FullNodeId fullNID) node
        cn <- o .: "cluster_name"
        return (NodesInfo infos cn)

instance FromJSON NodesStats where
  parseJSON = withObject "NodesStats" parse
    where
      parse o = do
        nodes <- o .: "nodes"
        stats <- forM (HM.toList nodes) $ \(fullNID, v) -> do
          node <- parseJSON v
          parseNodeStats (FullNodeId fullNID) node
        cn <- o .: "cluster_name"
        return (NodesStats stats cn)

instance FromJSON NodeBreakerStats where
  parseJSON = withObject "NodeBreakerStats" parse
    where
      parse o =
        NodeBreakerStats
          <$> o
            .: "tripped"
          <*> o
            .: "overhead"
          <*> o
            .: "estimated_size_in_bytes"
          <*> o
            .: "limit_size_in_bytes"

instance FromJSON NodeHTTPStats where
  parseJSON = withObject "NodeHTTPStats" parse
    where
      parse o =
        NodeHTTPStats
          <$> o
            .: "total_opened"
          <*> o
            .: "current_open"

instance FromJSON NodeTransportStats where
  parseJSON = withObject "NodeTransportStats" parse
    where
      parse o =
        NodeTransportStats
          <$> o
            .: "tx_size_in_bytes"
          <*> o
            .: "tx_count"
          <*> o
            .: "rx_size_in_bytes"
          <*> o
            .: "rx_count"
          <*> o
            .: "server_open"

instance FromJSON NodeFSStats where
  parseJSON = withObject "NodeFSStats" parse
    where
      parse o =
        NodeFSStats
          <$> o
            .: "data"
          <*> o
            .: "total"
          <*> (posixMS <$> o .: "timestamp")

instance FromJSON NodeDataPathStats where
  parseJSON = withObject "NodeDataPathStats" parse
    where
      parse o =
        NodeDataPathStats
          <$> (fmap unStringlyTypedDouble <$> o .:? "disk_service_time")
          <*> (fmap unStringlyTypedDouble <$> o .:? "disk_queue")
          <*> o
            .:? "disk_io_size_in_bytes"
          <*> o
            .:? "disk_write_size_in_bytes"
          <*> o
            .:? "disk_read_size_in_bytes"
          <*> o
            .:? "disk_io_op"
          <*> o
            .:? "disk_writes"
          <*> o
            .:? "disk_reads"
          <*> o
            .: "available_in_bytes"
          <*> o
            .: "free_in_bytes"
          <*> o
            .: "total_in_bytes"
          <*> o
            .:? "type"
          <*> o
            .:? "dev"
          <*> o
            .: "mount"
          <*> o
            .: "path"

instance FromJSON NodeFSTotalStats where
  parseJSON = withObject "NodeFSTotalStats" parse
    where
      parse o =
        NodeFSTotalStats
          <$> (fmap unStringlyTypedDouble <$> o .:? "disk_service_time")
          <*> (fmap unStringlyTypedDouble <$> o .:? "disk_queue")
          <*> o
            .:? "disk_io_size_in_bytes"
          <*> o
            .:? "disk_write_size_in_bytes"
          <*> o
            .:? "disk_read_size_in_bytes"
          <*> o
            .:? "disk_io_op"
          <*> o
            .:? "disk_writes"
          <*> o
            .:? "disk_reads"
          <*> o
            .: "available_in_bytes"
          <*> o
            .: "free_in_bytes"
          <*> o
            .: "total_in_bytes"

instance FromJSON NodeNetworkStats where
  parseJSON = withObject "NodeNetworkStats" parse
    where
      parse o = do
        tcp <- o .: "tcp"
        NodeNetworkStats
          <$> tcp
            .: "out_rsts"
          <*> tcp
            .: "in_errs"
          <*> tcp
            .: "attempt_fails"
          <*> tcp
            .: "estab_resets"
          <*> tcp
            .: "retrans_segs"
          <*> tcp
            .: "out_segs"
          <*> tcp
            .: "in_segs"
          <*> tcp
            .: "curr_estab"
          <*> tcp
            .: "passive_opens"
          <*> tcp
            .: "active_opens"

instance FromJSON NodeThreadPoolStats where
  parseJSON = withObject "NodeThreadPoolStats" parse
    where
      parse o =
        NodeThreadPoolStats
          <$> o
            .: "completed"
          <*> o
            .: "largest"
          <*> o
            .: "rejected"
          <*> o
            .: "active"
          <*> o
            .: "queue"
          <*> o
            .: "threads"

instance FromJSON NodeJVMStats where
  parseJSON = withObject "NodeJVMStats" parse
    where
      parse o = do
        bufferPools <- o .: "buffer_pools"
        mapped <- bufferPools .: "mapped"
        direct <- bufferPools .: "direct"
        gc <- o .: "gc"
        collectors <- gc .: "collectors"
        oldC <- collectors .: "old"
        youngC <- collectors .: "young"
        threads <- o .: "threads"
        mem <- o .: "mem"
        pools <- mem .: "pools"
        oldM <- pools .: "old"
        survivorM <- pools .: "survivor"
        youngM <- pools .: "young"
        NodeJVMStats
          <$> pure mapped
          <*> pure direct
          <*> pure oldC
          <*> pure youngC
          <*> threads
            .: "peak_count"
          <*> threads
            .: "count"
          <*> pure oldM
          <*> pure survivorM
          <*> pure youngM
          <*> mem
            .: "non_heap_committed_in_bytes"
          <*> mem
            .: "non_heap_used_in_bytes"
          <*> mem
            .: "heap_max_in_bytes"
          <*> mem
            .: "heap_committed_in_bytes"
          <*> mem
            .: "heap_used_percent"
          <*> mem
            .: "heap_used_in_bytes"
          <*> (unMS <$> o .: "uptime_in_millis")
          <*> (posixMS <$> o .: "timestamp")

instance FromJSON JVMBufferPoolStats where
  parseJSON = withObject "JVMBufferPoolStats" parse
    where
      parse o =
        JVMBufferPoolStats
          <$> o
            .: "total_capacity_in_bytes"
          <*> o
            .: "used_in_bytes"
          <*> o
            .: "count"

instance FromJSON JVMGCStats where
  parseJSON = withObject "JVMGCStats" parse
    where
      parse o =
        JVMGCStats
          <$> (unMS <$> o .: "collection_time_in_millis")
          <*> o
            .: "collection_count"

instance FromJSON JVMPoolStats where
  parseJSON = withObject "JVMPoolStats" parse
    where
      parse o =
        JVMPoolStats
          <$> o
            .: "peak_max_in_bytes"
          <*> o
            .: "peak_used_in_bytes"
          <*> o
            .: "max_in_bytes"
          <*> o
            .: "used_in_bytes"

instance FromJSON NodeProcessStats where
  parseJSON = withObject "NodeProcessStats" parse
    where
      parse o = do
        mem <- o .: "mem"
        cpu <- o .: "cpu"
        NodeProcessStats
          <$> (posixMS <$> o .: "timestamp")
          <*> o
            .: "open_file_descriptors"
          <*> o
            .: "max_file_descriptors"
          <*> cpu
            .: "percent"
          <*> (unMS <$> cpu .: "total_in_millis")
          <*> mem
            .: "total_virtual_in_bytes"

instance FromJSON NodeOSStats where
  parseJSON = withObject "NodeOSStats" parse
    where
      parse o = do
        swap <- o .: "swap"
        mem <- o .: "mem"
        cpu <- o .: "cpu"
        load <- o .:? "load_average"
        NodeOSStats
          <$> (posixMS <$> o .: "timestamp")
          <*> cpu
            .: "percent"
          <*> pure load
          <*> mem
            .: "total_in_bytes"
          <*> mem
            .: "free_in_bytes"
          <*> mem
            .: "free_percent"
          <*> mem
            .: "used_in_bytes"
          <*> mem
            .: "used_percent"
          <*> swap
            .: "total_in_bytes"
          <*> swap
            .: "free_in_bytes"
          <*> swap
            .: "used_in_bytes"

instance FromJSON LoadAvgs where
  parseJSON = withArray "LoadAvgs" parse
    where
      parse v = case V.toList v of
        [one, five, fifteen] ->
          LoadAvgs
            <$> parseJSON one
            <*> parseJSON five
            <*> parseJSON fifteen
        _ -> fail "Expecting a triple of Doubles"

instance FromJSON NodeIndicesStats where
  parseJSON = withObject "NodeIndicesStats" parse
    where
      parse o = do
        let (.::) mv k = case mv of
              Just v -> Just <$> v .: k
              Nothing -> pure Nothing
        mRecovery <- o .:? "recovery"
        mQueryCache <- o .:? "query_cache"
        mSuggest <- o .:? "suggest"
        translog <- o .: "translog"
        segments <- o .: "segments"
        completion <- o .: "completion"
        mPercolate <- o .:? "percolate"
        fielddata <- o .: "fielddata"
        warmer <- o .: "warmer"
        flush <- o .: "flush"
        refresh <- o .: "refresh"
        merges <- o .: "merges"
        search <- o .: "search"
        getStats <- o .: "get"
        indexing <- o .: "indexing"
        store <- o .: "store"
        docs <- o .: "docs"
        NodeIndicesStats
          <$> (fmap unMS <$> mRecovery .:: "throttle_time_in_millis")
          <*> mRecovery
            .:: "current_as_target"
          <*> mRecovery
            .:: "current_as_source"
          <*> mQueryCache
            .:: "miss_count"
          <*> mQueryCache
            .:: "hit_count"
          <*> mQueryCache
            .:: "evictions"
          <*> mQueryCache
            .:: "memory_size_in_bytes"
          <*> mSuggest
            .:: "current"
          <*> (fmap unMS <$> mSuggest .:: "time_in_millis")
          <*> mSuggest
            .:: "total"
          <*> translog
            .: "size_in_bytes"
          <*> translog
            .: "operations"
          <*> segments
            .:? "fixed_bit_set_memory_in_bytes"
          <*> segments
            .: "version_map_memory_in_bytes"
          <*> segments
            .:? "index_writer_max_memory_in_bytes"
          <*> segments
            .: "index_writer_memory_in_bytes"
          <*> segments
            .: "memory_in_bytes"
          <*> segments
            .: "count"
          <*> completion
            .: "size_in_bytes"
          <*> mPercolate
            .:: "queries"
          <*> mPercolate
            .:: "memory_size_in_bytes"
          <*> mPercolate
            .:: "current"
          <*> (fmap unMS <$> mPercolate .:: "time_in_millis")
          <*> mPercolate
            .:: "total"
          <*> fielddata
            .: "evictions"
          <*> fielddata
            .: "memory_size_in_bytes"
          <*> (unMS <$> warmer .: "total_time_in_millis")
          <*> warmer
            .: "total"
          <*> warmer
            .: "current"
          <*> (unMS <$> flush .: "total_time_in_millis")
          <*> flush
            .: "total"
          <*> (unMS <$> refresh .: "total_time_in_millis")
          <*> refresh
            .: "total"
          <*> merges
            .: "total_size_in_bytes"
          <*> merges
            .: "total_docs"
          <*> (unMS <$> merges .: "total_time_in_millis")
          <*> merges
            .: "total"
          <*> merges
            .: "current_size_in_bytes"
          <*> merges
            .: "current_docs"
          <*> merges
            .: "current"
          <*> search
            .: "fetch_current"
          <*> (unMS <$> search .: "fetch_time_in_millis")
          <*> search
            .: "fetch_total"
          <*> search
            .: "query_current"
          <*> (unMS <$> search .: "query_time_in_millis")
          <*> search
            .: "query_total"
          <*> search
            .: "open_contexts"
          <*> getStats
            .: "current"
          <*> (unMS <$> getStats .: "missing_time_in_millis")
          <*> getStats
            .: "missing_total"
          <*> (unMS <$> getStats .: "exists_time_in_millis")
          <*> getStats
            .: "exists_total"
          <*> (unMS <$> getStats .: "time_in_millis")
          <*> getStats
            .: "total"
          <*> (fmap unMS <$> indexing .:? "throttle_time_in_millis")
          <*> indexing
            .:? "is_throttled"
          <*> indexing
            .:? "noop_update_total"
          <*> indexing
            .: "delete_current"
          <*> (unMS <$> indexing .: "delete_time_in_millis")
          <*> indexing
            .: "delete_total"
          <*> indexing
            .: "index_current"
          <*> (unMS <$> indexing .: "index_time_in_millis")
          <*> indexing
            .: "index_total"
          <*> (fmap unMS <$> store .:? "throttle_time_in_millis")
          <*> store
            .: "size_in_bytes"
          <*> docs
            .: "deleted"
          <*> docs
            .: "count"

instance FromJSON NodeBreakersStats where
  parseJSON = withObject "NodeBreakersStats" parse
    where
      parse o =
        NodeBreakersStats
          <$> o
            .: "parent"
          <*> o
            .: "request"
          <*> o
            .: "fielddata"

parseNodeStats :: FullNodeId -> Object -> Parser NodeStats
parseNodeStats fnid o =
  NodeStats
    <$> o
      .: "name"
    <*> pure fnid
    <*> o
      .:? "breakers"
    <*> o
      .: "http"
    <*> o
      .: "transport"
    <*> o
      .: "fs"
    <*> o
      .:? "network"
    <*> o
      .: "thread_pool"
    <*> o
      .: "jvm"
    <*> o
      .: "process"
    <*> o
      .: "os"
    <*> o
      .: "indices"

parseNodeInfo :: FullNodeId -> Object -> Parser NodeInfo
parseNodeInfo nid o =
  NodeInfo
    <$> o
      .:? "http_address"
    <*> o
      .: "build_hash"
    <*> o
      .: "version"
    <*> o
      .: "ip"
    <*> o
      .: "host"
    <*> o
      .: "transport_address"
    <*> o
      .: "name"
    <*> pure nid
    <*> o
      .: "plugins"
    <*> o
      .: "http"
    <*> o
      .: "transport"
    <*> o
      .:? "network"
    <*> o
      .: "thread_pool"
    <*> o
      .: "jvm"
    <*> o
      .: "process"
    <*> o
      .: "os"
    <*> o
      .: "settings"

instance FromJSON NodePluginInfo where
  parseJSON = withObject "NodePluginInfo" parse
    where
      parse o =
        NodePluginInfo
          <$> o
            .:? "site"
          <*> o
            .:? "jvm"
          <*> o
            .: "description"
          <*> o
            .: "version"
          <*> o
            .: "name"

instance FromJSON NodeHTTPInfo where
  parseJSON = withObject "NodeHTTPInfo" parse
    where
      parse o =
        NodeHTTPInfo
          <$> o
            .: "max_content_length_in_bytes"
          <*> o
            .: "publish_address"
          <*> o
            .: "bound_address"

instance FromJSON BoundTransportAddress where
  parseJSON = withObject "BoundTransportAddress" parse
    where
      parse o =
        BoundTransportAddress
          <$> o
            .: "publish_address"
          <*> o
            .: "bound_address"

instance FromJSON NodeOSInfo where
  parseJSON = withObject "NodeOSInfo" parse
    where
      parse o =
        NodeOSInfo
          <$> (unMS <$> o .: "refresh_interval_in_millis")
          <*> o
            .: "name"
          <*> o
            .: "arch"
          <*> o
            .: "version"
          <*> o
            .: "available_processors"
          <*> o
            .: "allocated_processors"

instance FromJSON CPUInfo where
  parseJSON = withObject "CPUInfo" parse
    where
      parse o =
        CPUInfo
          <$> o
            .: "cache_size_in_bytes"
          <*> o
            .: "cores_per_socket"
          <*> o
            .: "total_sockets"
          <*> o
            .: "total_cores"
          <*> o
            .: "mhz"
          <*> o
            .: "model"
          <*> o
            .: "vendor"

instance FromJSON NodeProcessInfo where
  parseJSON = withObject "NodeProcessInfo" parse
    where
      parse o =
        NodeProcessInfo
          <$> o
            .: "mlockall"
          <*> o
            .:? "max_file_descriptors"
          <*> o
            .: "id"
          <*> (unMS <$> o .: "refresh_interval_in_millis")

instance FromJSON NodeJVMInfo where
  parseJSON = withObject "NodeJVMInfo" parse
    where
      parse o =
        NodeJVMInfo
          <$> o
            .: "memory_pools"
          <*> o
            .: "gc_collectors"
          <*> o
            .: "mem"
          <*> (posixMS <$> o .: "start_time_in_millis")
          <*> o
            .: "vm_vendor"
          <*> o
            .: "vm_version"
          <*> o
            .: "vm_name"
          <*> o
            .: "version"
          <*> o
            .: "pid"

instance FromJSON JVMMemoryInfo where
  parseJSON = withObject "JVMMemoryInfo" parse
    where
      parse o =
        JVMMemoryInfo
          <$> o
            .: "direct_max_in_bytes"
          <*> o
            .: "non_heap_max_in_bytes"
          <*> o
            .: "non_heap_init_in_bytes"
          <*> o
            .: "heap_max_in_bytes"
          <*> o
            .: "heap_init_in_bytes"

instance FromJSON NodeThreadPoolInfo where
  parseJSON = withObject "NodeThreadPoolInfo" parse
    where
      parse o = do
        ka <- maybe (return Nothing) (fmap Just . parseStringInterval) =<< o .:? "keep_alive"
        NodeThreadPoolInfo
          <$> (parseJSON . unStringlyTypeJSON =<< o .: "queue_size")
          <*> pure ka
          <*> o
            .:? "min"
          <*> o
            .:? "max"
          <*> o
            .: "type"

instance FromJSON ThreadPoolSize where
  parseJSON v = parseAsNumber v <|> parseAsString v
    where
      parseAsNumber = parseAsInt <=< parseJSON
      parseAsInt (-1) = return ThreadPoolUnbounded
      parseAsInt n
        | n >= 0 = return (ThreadPoolBounded n)
        | otherwise = fail "Thread pool size must be >= -1."
      parseAsString = withText "ThreadPoolSize" $ \t ->
        case first (readMay . T.unpack) (T.span isNumber t) of
          (Just n, "k") -> return (ThreadPoolBounded (n * 1000))
          (Just n, "") -> return (ThreadPoolBounded n)
          _ -> fail ("Invalid thread pool size " <> T.unpack t)

instance FromJSON ThreadPoolType where
  parseJSON = withText "ThreadPoolType" parse
    where
      parse "scaling" = return ThreadPoolScaling
      parse "fixed" = return ThreadPoolFixed
      parse "cached" = return ThreadPoolCached
      parse "fixed_auto_queue_size" = return ThreadPoolFixedAutoQueueSize
      parse e = fail ("Unexpected thread pool type" <> T.unpack e)

instance FromJSON NodeTransportInfo where
  parseJSON = withObject "NodeTransportInfo" parse
    where
      parse o =
        NodeTransportInfo
          <$> (maybe (return mempty) parseProfiles =<< o .:? "profiles")
          <*> o
            .: "publish_address"
          <*> o
            .: "bound_address"
      parseProfiles (Object o) | X.null o = return []
      parseProfiles v@(Array _) = parseJSON v
      parseProfiles Null = return []
      parseProfiles _ = fail "Could not parse profiles"

instance FromJSON NodeNetworkInfo where
  parseJSON = withObject "NodeNetworkInfo" parse
    where
      parse o =
        NodeNetworkInfo
          <$> o
            .: "primary_interface"
          <*> (unMS <$> o .: "refresh_interval_in_millis")

instance FromJSON NodeNetworkInterface where
  parseJSON = withObject "NodeNetworkInterface" parse
    where
      parse o =
        NodeNetworkInterface
          <$> o
            .: "mac_address"
          <*> o
            .: "name"
          <*> o
            .: "address"

data InitialShardCount
  = QuorumShards
  | QuorumMinus1Shards
  | FullShards
  | FullMinus1Shards
  | ExplicitShards Int
  deriving stock (Eq, Show, Generic)

initialShardCountExplicitShardsPrism :: Prism' InitialShardCount Int
initialShardCountExplicitShardsPrism = prism ExplicitShards extract
  where
    extract s =
      case s of
        ExplicitShards x -> Right x
        _ -> Left s

instance FromJSON InitialShardCount where
  parseJSON v =
    withText "InitialShardCount" parseText v
      <|> ExplicitShards
      <$> parseJSON v
    where
      parseText "quorum" = pure QuorumShards
      parseText "quorum-1" = pure QuorumMinus1Shards
      parseText "full" = pure FullShards
      parseText "full-1" = pure FullMinus1Shards
      parseText _ = mzero

instance ToJSON InitialShardCount where
  toJSON QuorumShards = String "quorum"
  toJSON QuorumMinus1Shards = String "quorum-1"
  toJSON FullShards = String "full"
  toJSON FullMinus1Shards = String "full-1"
  toJSON (ExplicitShards x) = toJSON x

newtype ShardsResult = ShardsResult
  { srShards :: ShardResult
  }
  deriving stock (Eq, Show)

instance FromJSON ShardsResult where
  parseJSON =
    withObject "ShardsResult" $ \v ->
      ShardsResult
        <$> v
          .: "_shards"

shardsResultShardsLens :: Lens' ShardsResult ShardResult
shardsResultShardsLens = lens srShards (\x y -> x {srShards = y})

data ShardResult = ShardResult
  { shardTotal :: Int,
    shardsSuccessful :: Int,
    shardsSkipped :: Int,
    shardsFailed :: Int
  }
  deriving stock (Eq, Show)

instance FromJSON ShardResult where
  parseJSON =
    withObject "ShardResult" $ \v ->
      ShardResult
        <$> v .:? "total" .!= 0
        <*> v .:? "successful" .!= 0
        <*> v .:? "skipped" .!= 0
        <*> v .:? "failed" .!= 0

instance ToJSON ShardResult where
  toJSON ShardResult {..} =
    object
      [ "total" .= shardTotal,
        "successful" .= shardsSuccessful,
        "skipped" .= shardsSkipped,
        "failed" .= shardsFailed
      ]

shardResultTotalLens :: Lens' ShardResult Int
shardResultTotalLens = lens shardTotal (\x y -> x {shardTotal = y})

shardsResultSuccessfulLens :: Lens' ShardResult Int
shardsResultSuccessfulLens = lens shardsSuccessful (\x y -> x {shardsSuccessful = y})

shardsResultResultSkippedLens :: Lens' ShardResult Int
shardsResultResultSkippedLens = lens shardsSkipped (\x y -> x {shardsSkipped = y})

shardsResultFailedLens :: Lens' ShardResult Int
shardsResultFailedLens = lens shardsFailed (\x y -> x {shardsFailed = y})

-- | 'Version' is embedded in 'Status'
data Version = Version
  { number :: VersionNumber,
    build_hash :: BuildHash,
    build_date :: UTCTime,
    build_snapshot :: Bool,
    lucene_version :: VersionNumber
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Version where
  toJSON Version {..} =
    object
      [ "number" .= number,
        "build_hash" .= build_hash,
        "build_date" .= build_date,
        "build_snapshot" .= build_snapshot,
        "lucene_version" .= lucene_version
      ]

instance FromJSON Version where
  parseJSON = withObject "Version" parse
    where
      parse o =
        Version
          <$> o
            .: "number"
          <*> o
            .: "build_hash"
          <*> o
            .: "build_date"
          <*> o
            .: "build_snapshot"
          <*> o
            .: "lucene_version"

versionNumberLens :: Lens' Version VersionNumber
versionNumberLens = lens number (\x y -> x {number = y})

versionBuildHashLens :: Lens' Version BuildHash
versionBuildHashLens = lens build_hash (\x y -> x {build_hash = y})

versionBuildDateLens :: Lens' Version UTCTime
versionBuildDateLens = lens build_date (\x y -> x {build_date = y})

versionBuildSnapshotLens :: Lens' Version Bool
versionBuildSnapshotLens = lens build_snapshot (\x y -> x {build_snapshot = y})

versionLuceneVersionLens :: Lens' Version VersionNumber
versionLuceneVersionLens = lens lucene_version (\x y -> x {lucene_version = y})

-- | Traditional software versioning number
newtype VersionNumber = VersionNumber
  {versionNumber :: Versions.Version}
  deriving stock (Eq, Ord, Show)

instance ToJSON VersionNumber where
  toJSON = toJSON . Versions.prettyVer . versionNumber

instance FromJSON VersionNumber where
  parseJSON = withText "VersionNumber" parse
    where
      parse t =
        case Versions.version t of
          (Left err) -> fail $ show err
          (Right v) -> return (VersionNumber v)

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
        <$> v
          .: "cluster_name"
        <*> v
          .: "status"
        <*> v
          .: "timed_out"
        <*> v
          .: "number_of_nodes"
        <*> v
          .: "number_of_data_nodes"
        <*> v
          .: "active_primary_shards"
        <*> v
          .: "active_shards"
        <*> v
          .: "relocating_shards"
        <*> v
          .: "initializing_shards"
        <*> v
          .: "unassigned_shards"
        <*> v
          .: "delayed_unassigned_shards"
        <*> v
          .: "number_of_pending_tasks"
        <*> v
          .: "number_of_in_flight_fetch"
        <*> v
          .: "task_max_waiting_in_queue_millis"
        <*> v
          .: "active_shards_percent_as_number"

healthStatusClusterNameLens :: Lens' HealthStatus Text
healthStatusClusterNameLens = lens healthStatusClusterName (\x y -> x {healthStatusClusterName = y})

healthStatusStatusLens :: Lens' HealthStatus Text
healthStatusStatusLens = lens healthStatusStatus (\x y -> x {healthStatusStatus = y})

healthStatusTimedOutLens :: Lens' HealthStatus Bool
healthStatusTimedOutLens = lens healthStatusTimedOut (\x y -> x {healthStatusTimedOut = y})

healthStatusNumberOfNodesLens :: Lens' HealthStatus Int
healthStatusNumberOfNodesLens = lens healthStatusNumberOfNodes (\x y -> x {healthStatusNumberOfNodes = y})

healthStatusNumberOfDataNodesLens :: Lens' HealthStatus Int
healthStatusNumberOfDataNodesLens = lens healthStatusNumberOfDataNodes (\x y -> x {healthStatusNumberOfDataNodes = y})

healthStatusActivePrimaryShardsLens :: Lens' HealthStatus Int
healthStatusActivePrimaryShardsLens = lens healthStatusActivePrimaryShards (\x y -> x {healthStatusActivePrimaryShards = y})

healthStatusActiveShardsLens :: Lens' HealthStatus Int
healthStatusActiveShardsLens = lens healthStatusActiveShards (\x y -> x {healthStatusActiveShards = y})

healthStatusRelocatingShardsLens :: Lens' HealthStatus Int
healthStatusRelocatingShardsLens = lens healthStatusRelocatingShards (\x y -> x {healthStatusRelocatingShards = y})

healthStatusInitializingShardsLens :: Lens' HealthStatus Int
healthStatusInitializingShardsLens = lens healthStatusInitializingShards (\x y -> x {healthStatusInitializingShards = y})

healthStatusUnassignedShardsLens :: Lens' HealthStatus Int
healthStatusUnassignedShardsLens = lens healthStatusUnassignedShards (\x y -> x {healthStatusUnassignedShards = y})

healthStatusDelayedUnassignedShardsLens :: Lens' HealthStatus Int
healthStatusDelayedUnassignedShardsLens = lens healthStatusDelayedUnassignedShards (\x y -> x {healthStatusDelayedUnassignedShards = y})

healthStatusNumberOfPendingTasksLens :: Lens' HealthStatus Int
healthStatusNumberOfPendingTasksLens = lens healthStatusNumberOfPendingTasks (\x y -> x {healthStatusNumberOfPendingTasks = y})

healthStatusNumberOfInFlightFetchLens :: Lens' HealthStatus Int
healthStatusNumberOfInFlightFetchLens = lens healthStatusNumberOfInFlightFetch (\x y -> x {healthStatusNumberOfInFlightFetch = y})

healthStatusTaskMaxWaitingInQueueMillisLens :: Lens' HealthStatus Int
healthStatusTaskMaxWaitingInQueueMillisLens = lens healthStatusTaskMaxWaitingInQueueMillis (\x y -> x {healthStatusTaskMaxWaitingInQueueMillis = y})

healthStatusActiveShardsPercentAsNumberLens :: Lens' HealthStatus Float
healthStatusActiveShardsPercentAsNumberLens = lens healthStatusActiveShardsPercentAsNumber (\x y -> x {healthStatusActiveShardsPercentAsNumber = y})

data IndexedDocument = IndexedDocument
  { idxDocIndex :: Text,
    idxDocType :: Maybe Text,
    idxDocId :: Text,
    idxDocVersion :: Int,
    idxDocResult :: Text,
    idxDocShards :: ShardResult,
    idxDocSeqNo :: Int,
    idxDocPrimaryTerm :: Int
  }
  deriving stock (Eq, Show)

{-# DEPRECATED idxDocType "deprecated since ElasticSearch 6.0" #-}

instance FromJSON IndexedDocument where
  parseJSON =
    withObject "IndexedDocument" $ \v ->
      IndexedDocument
        <$> v
          .: "_index"
        <*> v
          .:? "_type"
        <*> v
          .: "_id"
        <*> v
          .: "_version"
        <*> v
          .: "result"
        <*> v
          .: "_shards"
        <*> v
          .: "_seq_no"
        <*> v
          .: "_primary_term"

indexedDocumentIndexLens :: Lens' IndexedDocument Text
indexedDocumentIndexLens = lens idxDocIndex (\x y -> x {idxDocIndex = y})

indexedDocumentTypeLens :: Lens' IndexedDocument (Maybe Text)
indexedDocumentTypeLens = lens idxDocType (\x y -> x {idxDocType = y})

indexedDocumentIdLens :: Lens' IndexedDocument Text
indexedDocumentIdLens = lens idxDocId (\x y -> x {idxDocId = y})

indexedDocumentVersionLens :: Lens' IndexedDocument Int
indexedDocumentVersionLens = lens idxDocVersion (\x y -> x {idxDocVersion = y})

indexedDocumentResultLens :: Lens' IndexedDocument Text
indexedDocumentResultLens = lens idxDocResult (\x y -> x {idxDocResult = y})

indexedDocumentShardsLens :: Lens' IndexedDocument ShardResult
indexedDocumentShardsLens = lens idxDocShards (\x y -> x {idxDocShards = y})

indexedDocumentSeqNoLens :: Lens' IndexedDocument Int
indexedDocumentSeqNoLens = lens idxDocSeqNo (\x y -> x {idxDocSeqNo = y})

indexedDocumentPrimaryTermLens :: Lens' IndexedDocument Int
indexedDocumentPrimaryTermLens = lens idxDocPrimaryTerm (\x y -> x {idxDocPrimaryTerm = y})

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
        <$> v
          .: "took"
        <*> v
          .: "timed_out"
        <*> v
          .: "total"
        <*> v
          .: "deleted"
        <*> v
          .: "batches"
        <*> v
          .: "version_conflicts"
        <*> v
          .: "noops"
        <*> v
          .: "retries"
        <*> v
          .: "throttled_millis"
        <*> v
          .: "requests_per_second"
        <*> v
          .: "throttled_until_millis"
        <*> v
          .: "failures"

deletedDocumentsTookLens :: Lens' DeletedDocuments Int
deletedDocumentsTookLens = lens delDocsTook (\x y -> x {delDocsTook = y})

deletedDocumentsTimedOutLens :: Lens' DeletedDocuments Bool
deletedDocumentsTimedOutLens = lens delDocsTimedOut (\x y -> x {delDocsTimedOut = y})

deletedDocumentsTotalLens :: Lens' DeletedDocuments Int
deletedDocumentsTotalLens = lens delDocsTotal (\x y -> x {delDocsTotal = y})

deletedDocumentsDeletedLens :: Lens' DeletedDocuments Int
deletedDocumentsDeletedLens = lens delDocsDeleted (\x y -> x {delDocsDeleted = y})

deletedDocumentsBatchesLens :: Lens' DeletedDocuments Int
deletedDocumentsBatchesLens = lens delDocsBatches (\x y -> x {delDocsBatches = y})

deletedDocumentsVersionConflictsLens :: Lens' DeletedDocuments Int
deletedDocumentsVersionConflictsLens = lens delDocsVersionConflicts (\x y -> x {delDocsVersionConflicts = y})

deletedDocumentsNoopsLens :: Lens' DeletedDocuments Int
deletedDocumentsNoopsLens = lens delDocsNoops (\x y -> x {delDocsNoops = y})

deletedDocumentsRetriesLens :: Lens' DeletedDocuments DeletedDocumentsRetries
deletedDocumentsRetriesLens = lens delDocsRetries (\x y -> x {delDocsRetries = y})

deletedDocumentsThrottledMillisLens :: Lens' DeletedDocuments Int
deletedDocumentsThrottledMillisLens = lens delDocsThrottledMillis (\x y -> x {delDocsThrottledMillis = y})

deletedDocumentsRequestsPerSecondLens :: Lens' DeletedDocuments Float
deletedDocumentsRequestsPerSecondLens = lens delDocsRequestsPerSecond (\x y -> x {delDocsRequestsPerSecond = y})

deletedDocumentsThrottledUntilMillisLens :: Lens' DeletedDocuments Int
deletedDocumentsThrottledUntilMillisLens = lens delDocsThrottledUntilMillis (\x y -> x {delDocsThrottledUntilMillis = y})

deletedDocumentsFailuresLens :: Lens' DeletedDocuments [Value]
deletedDocumentsFailuresLens = lens delDocsFailures (\x y -> x {delDocsFailures = y})

data DeletedDocumentsRetries = DeletedDocumentsRetries
  { delDocsRetriesBulk :: Int,
    delDocsRetriesSearch :: Int
  }
  deriving stock (Eq, Show)

instance FromJSON DeletedDocumentsRetries where
  parseJSON =
    withObject "DeletedDocumentsRetries" $ \v ->
      DeletedDocumentsRetries
        <$> v
          .: "bulk"
        <*> v
          .: "search"

deletedDocumentsRetriesBulkLens :: Lens' DeletedDocumentsRetries Int
deletedDocumentsRetriesBulkLens = lens delDocsRetriesBulk (\x y -> x {delDocsRetriesBulk = y})

deletedDocumentsRetriesSearchLens :: Lens' DeletedDocumentsRetries Int
deletedDocumentsRetriesSearchLens = lens delDocsRetriesSearch (\x y -> x {delDocsRetriesSearch = y})
