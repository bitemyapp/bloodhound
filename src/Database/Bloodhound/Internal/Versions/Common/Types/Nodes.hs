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

newtype NodeAttrName = NodeAttrName Text deriving stock (Eq, Ord, Show)

-- | 'NodeSelection' is used for most cluster APIs. See <https://www.elastic.co/guide/en/elasticsearch/reference/current/cluster.html#cluster-nodes here> for more details.
data NodeSelection
  = -- | Whatever node receives this request
    LocalNode
  | NodeList (NonEmpty NodeSelector)
  | AllNodes
  deriving stock (Eq, Show)

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

data NodesStats = NodesStats
  { nodesStats :: [NodeStats],
    nodesStatsClusterName :: ClusterName
  }
  deriving stock (Eq, Show)

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

data NodeBreakersStats = NodeBreakersStats
  { nodeStatsParentBreaker :: NodeBreakerStats,
    nodeStatsRequestBreaker :: NodeBreakerStats,
    nodeStatsFieldDataBreaker :: NodeBreakerStats
  }
  deriving stock (Eq, Show)

data NodeBreakerStats = NodeBreakerStats
  { nodeBreakersTripped :: Int,
    nodeBreakersOverhead :: Double,
    nodeBreakersEstSize :: Bytes,
    nodeBreakersLimitSize :: Bytes
  }
  deriving stock (Eq, Show)

data NodeHTTPStats = NodeHTTPStats
  { nodeHTTPTotalOpened :: Int,
    nodeHTTPCurrentOpen :: Int
  }
  deriving stock (Eq, Show)

data NodeTransportStats = NodeTransportStats
  { nodeTransportTXSize :: Bytes,
    nodeTransportCount :: Int,
    nodeTransportRXSize :: Bytes,
    nodeTransportRXCount :: Int,
    nodeTransportServerOpen :: Int
  }
  deriving stock (Eq, Show)

data NodeFSStats = NodeFSStats
  { nodeFSDataPaths :: [NodeDataPathStats],
    nodeFSTotal :: NodeFSTotalStats,
    nodeFSTimestamp :: UTCTime
  }
  deriving stock (Eq, Show)

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

data NodeThreadPoolStats = NodeThreadPoolStats
  { nodeThreadPoolCompleted :: Int,
    nodeThreadPoolLargest :: Int,
    nodeThreadPoolRejected :: Int,
    nodeThreadPoolActive :: Int,
    nodeThreadPoolQueue :: Int,
    nodeThreadPoolThreads :: Int
  }
  deriving stock (Eq, Show)

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

data JVMBufferPoolStats = JVMBufferPoolStats
  { jvmBufferPoolStatsTotalCapacity :: Bytes,
    jvmBufferPoolStatsUsed :: Bytes,
    jvmBufferPoolStatsCount :: Int
  }
  deriving stock (Eq, Show)

data JVMGCStats = JVMGCStats
  { jvmGCStatsCollectionTime :: NominalDiffTime,
    jvmGCStatsCollectionCount :: Int
  }
  deriving stock (Eq, Show)

data JVMPoolStats = JVMPoolStats
  { jvmPoolStatsPeakMax :: Bytes,
    jvmPoolStatsPeakUsed :: Bytes,
    jvmPoolStatsMax :: Bytes,
    jvmPoolStatsUsed :: Bytes
  }
  deriving stock (Eq, Show)

data NodeProcessStats = NodeProcessStats
  { nodeProcessTimestamp :: UTCTime,
    nodeProcessOpenFDs :: Int,
    nodeProcessMaxFDs :: Int,
    nodeProcessCPUPercent :: Int,
    nodeProcessCPUTotal :: NominalDiffTime,
    nodeProcessMemTotalVirtual :: Bytes
  }
  deriving stock (Eq, Show)

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

data LoadAvgs = LoadAvgs
  { loadAvg1Min :: Double,
    loadAvg5Min :: Double,
    loadAvg15Min :: Double
  }
  deriving stock (Eq, Show)

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

data NodeHTTPInfo = NodeHTTPInfo
  { nodeHTTPMaxContentLength :: Bytes,
    nodeHTTPpublishAddress :: EsAddress,
    nodeHTTPbound_address :: [EsAddress]
  }
  deriving stock (Eq, Show)

data NodeTransportInfo = NodeTransportInfo
  { nodeTransportProfiles :: [BoundTransportAddress],
    nodeTransportPublishAddress :: EsAddress,
    nodeTransportBoundAddress :: [EsAddress]
  }
  deriving stock (Eq, Show)

data BoundTransportAddress = BoundTransportAddress
  { publishAddress :: EsAddress,
    boundAddress :: [EsAddress]
  }
  deriving stock (Eq, Show)

data NodeNetworkInfo = NodeNetworkInfo
  { nodeNetworkPrimaryInterface :: NodeNetworkInterface,
    nodeNetworkRefreshInterval :: NominalDiffTime
  }
  deriving stock (Eq, Show)

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

data ThreadPool = ThreadPool
  { nodeThreadPoolName :: Text,
    nodeThreadPoolInfo :: NodeThreadPoolInfo
  }
  deriving stock (Eq, Show)

data NodeThreadPoolInfo = NodeThreadPoolInfo
  { nodeThreadPoolQueueSize :: ThreadPoolSize,
    nodeThreadPoolKeepalive :: Maybe NominalDiffTime,
    nodeThreadPoolMin :: Maybe Int,
    nodeThreadPoolMax :: Maybe Int,
    nodeThreadPoolType :: ThreadPoolType
  }
  deriving stock (Eq, Show)

data ThreadPoolSize
  = ThreadPoolBounded Int
  | ThreadPoolUnbounded
  deriving stock (Eq, Show)

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
