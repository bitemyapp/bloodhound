{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Bloodhound.Client.Cluster where

import Control.Monad.Catch
import Control.Monad.Except
import qualified Data.Aeson.Key as X
import qualified Data.Aeson.KeyMap as X
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.HashMap.Strict as HM
import Data.Map.Strict (Map)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Traversable as DT
import qualified Data.Vector as V
import qualified Data.Versions as Versions
import Database.Bloodhound.Internal.Client.BHRequest
import Database.Bloodhound.Internal.Client.Doc
import Database.Bloodhound.Internal.Utils.Imports
import Database.Bloodhound.Internal.Utils.StringlyTyped
import Database.Bloodhound.Internal.Versions.Common.Types.Analysis
import Database.Bloodhound.Internal.Versions.Common.Types.Newtypes
import Database.Bloodhound.Internal.Versions.Common.Types.Query
import GHC.Generics
import Network.HTTP.Client
import qualified Network.URI as URI
import Text.Read (Read (..))
import qualified Text.Read as TR

-- | Common environment for Elasticsearch calls. Connections will be
--    pipelined according to the provided HTTP connection manager.
data BHEnv = BHEnv
  { bhServer :: Server,
    bhManager :: Manager,
    -- | Low-level hook that is run before every request is sent. Used to implement custom authentication strategies. Defaults to 'return' with 'mkBHEnv'.
    bhRequestHook :: Request -> IO Request
  }

-- | All API calls to Elasticsearch operate within
--    MonadBH
--    . The idea is that it can be easily embedded in your
--    own monad transformer stack. A default instance for a ReaderT and
--    alias 'BH' is provided for the simple case.
class (Functor m, Applicative m, MonadIO m, MonadCatch m) => MonadBH m where
  dispatch :: BHRequest contextualized body -> m (BHResponse contextualized body)
  tryEsError :: m a -> m (Either EsError a)
  throwEsError :: EsError -> m a

-- | Create a 'BHEnv' with all optional fields defaulted. HTTP hook
-- will be a noop. You can use the exported fields to customize
-- it further, e.g.:
--
-- >> (mkBHEnv myServer myManager) { bhRequestHook = customHook }
mkBHEnv :: Server -> Manager -> BHEnv
mkBHEnv s m = BHEnv s m return

newtype BH m a = BH
  { unBH :: ReaderT BHEnv (ExceptT EsError m) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadIO,
      MonadState s,
      MonadWriter w,
      Alternative,
      MonadPlus,
      MonadFix,
      MonadThrow,
      MonadCatch,
      MonadFail,
      MonadMask
    )

instance MonadTrans BH where
  lift = BH . lift . lift

instance (MonadReader r m) => MonadReader r (BH m) where
  ask = lift ask
  local f (BH (ReaderT m)) = BH $
    ReaderT $ \r ->
      local f (m r)

instance (Functor m, Applicative m, MonadIO m, MonadCatch m, MonadThrow m) => MonadBH (BH m) where
  dispatch request = BH $ do
    env <- ask @BHEnv
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
  tryEsError = try
  throwEsError = throwM

tryPerformBHRequest ::
  (MonadBH m, MonadThrow m, ParseBHResponse contextualized) =>
  BHRequest contextualized a ->
  m (ParsedEsResponse a)
tryPerformBHRequest req = dispatch req >>= either throwM return . bhRequestParser req

performBHRequest ::
  (MonadBH m, MonadThrow m, ParseBHResponse contextualized) =>
  BHRequest contextualized a ->
  m a
performBHRequest req = tryPerformBHRequest req >>= either throwEsError return

emptyBody :: L.ByteString
emptyBody = L.pack ""

parseUrl' :: (MonadThrow m) => Text -> m Request
parseUrl' t = parseRequest (URI.escapeURIString URI.isAllowedInURI (T.unpack t))

runBH :: BHEnv -> BH m a -> m (Either EsError a)
runBH e f = runExceptT $ runReaderT (unBH f) e

-- | 'Version' is embedded in 'Status'
data Version = Version
  { number :: VersionNumber,
    build_hash :: BuildHash,
    build_date :: UTCTime,
    build_snapshot :: Bool,
    lucene_version :: VersionNumber
  }
  deriving (Eq, Show, Generic)

-- | Traditional software versioning number
newtype VersionNumber = VersionNumber
  {versionNumber :: Versions.Version}
  deriving (Eq, Ord, Show)

-- | 'Status' is a data type for describing the JSON body returned by
--    Elasticsearch when you query its status. This was deprecated in 1.2.0.
--
--   <http://www.elastic.co/guide/en/elasticsearch/reference/current/indices-status.html#indices-status>
data Status = Status
  { name :: Text,
    cluster_name :: Text,
    cluster_uuid :: Text,
    version :: Version,
    tagline :: Text
  }
  deriving (Eq, Show)

instance FromJSON Status where
  parseJSON (Object v) =
    Status
      <$> v
        .: "name"
      <*> v
        .: "cluster_name"
      <*> v
        .: "cluster_uuid"
      <*> v
        .: "version"
      <*> v
        .: "tagline"
  parseJSON _ = empty

-- | 'IndexSettings' is used to configure the shards and replicas when
--    you create an Elasticsearch Index.
--
--   <http://www.elastic.co/guide/en/elasticsearch/reference/current/indices-create-index.html>
data IndexSettings = IndexSettings
  { indexShards :: ShardCount,
    indexReplicas :: ReplicaCount,
    indexMappingsLimits :: IndexMappingsLimits
  }
  deriving (Eq, Show, Generic)

instance ToJSON IndexSettings where
  toJSON (IndexSettings s r l) =
    object
      [ "settings"
          .= object
            [ "index"
                .= object ["number_of_shards" .= s, "number_of_replicas" .= r, "mapping" .= l]
            ]
      ]

instance FromJSON IndexSettings where
  parseJSON = withObject "IndexSettings" parse
    where
      parse o = do
        s <- o .: "settings"
        i <- s .: "index"
        IndexSettings
          <$> i
            .: "number_of_shards"
          <*> i
            .: "number_of_replicas"
          <*> i
            .:? "mapping"
            .!= defaultIndexMappingsLimits

-- | 'defaultIndexSettings' is an 'IndexSettings' with 3 shards and
--    2 replicas.
defaultIndexSettings :: IndexSettings
defaultIndexSettings = IndexSettings (ShardCount 3) (ReplicaCount 2) defaultIndexMappingsLimits

-- defaultIndexSettings is exported by Database.Bloodhound as well
-- no trailing slashes in servers, library handles building the path.

-- | 'IndexMappingsLimits is used to configure index's limits.
--   <https://www.elastic.co/guide/en/elasticsearch/reference/master/mapping-settings-limit.html>
data IndexMappingsLimits = IndexMappingsLimits
  { indexMappingsLimitDepth :: Maybe Int,
    indexMappingsLimitNestedFields :: Maybe Int,
    indexMappingsLimitNestedObjects :: Maybe Int,
    indexMappingsLimitFieldNameLength :: Maybe Int
  }
  deriving (Eq, Show, Generic)

instance ToJSON IndexMappingsLimits where
  toJSON (IndexMappingsLimits d f o n) =
    object $
      mapMaybe
        go
        [ ("depth.limit", d),
          ("nested_fields.limit", f),
          ("nested_objects.limit", o),
          ("field_name_length.limit", n)
        ]
    where
      go (name, value) = (name .=) <$> value

instance FromJSON IndexMappingsLimits where
  parseJSON = withObject "IndexMappingsLimits" $ \o ->
    IndexMappingsLimits
      <$> o .:?? "depth"
      <*> o .:?? "nested_fields"
      <*> o .:?? "nested_objects"
      <*> o .:?? "field_name_length"
    where
      o .:?? name = optional $ do
        f <- o .: name
        f .: "limit"

defaultIndexMappingsLimits :: IndexMappingsLimits
defaultIndexMappingsLimits = IndexMappingsLimits Nothing Nothing Nothing Nothing

-- | 'ForceMergeIndexSettings' is used to configure index optimization. See
--    <https://www.elastic.co/guide/en/elasticsearch/reference/current/indices-forcemerge.html>
--    for more info.
data ForceMergeIndexSettings = ForceMergeIndexSettings
  { -- | Number of segments to optimize to. 1 will fully optimize the index. If omitted, the default behavior is to only optimize if the server deems it necessary.
    maxNumSegments :: Maybe Int,
    -- | Should the optimize process only expunge segments with deletes in them? If the purpose of the optimization is to free disk space, this should be set to True.
    onlyExpungeDeletes :: Bool,
    -- | Should a flush be performed after the optimize.
    flushAfterOptimize :: Bool
  }
  deriving (Eq, Show)

-- | 'defaultForceMergeIndexSettings' implements the default settings that
--    Elasticsearch uses for index optimization. 'maxNumSegments' is Nothing,
--    'onlyExpungeDeletes' is False, and flushAfterOptimize is True.
defaultForceMergeIndexSettings :: ForceMergeIndexSettings
defaultForceMergeIndexSettings = ForceMergeIndexSettings Nothing False True

-- | 'UpdatableIndexSetting' are settings which may be updated after an index is created.
--
--   <https://www.elastic.co/guide/en/elasticsearch/reference/current/indices-update-settings.html>
data UpdatableIndexSetting
  = -- | The number of replicas each shard has.
    NumberOfReplicas ReplicaCount
  | AutoExpandReplicas ReplicaBounds
  | -- | Set to True to have the index read only. False to allow writes and metadata changes.
    BlocksReadOnly Bool
  | -- | Set to True to disable read operations against the index.
    BlocksRead Bool
  | -- | Set to True to disable write operations against the index.
    BlocksWrite Bool
  | -- | Set to True to disable metadata operations against the index.
    BlocksMetaData Bool
  | -- | The async refresh interval of a shard
    RefreshInterval NominalDiffTime
  | IndexConcurrency Int
  | FailOnMergeFailure Bool
  | -- | When to flush on operations.
    TranslogFlushThresholdOps Int
  | -- | When to flush based on translog (bytes) size.
    TranslogFlushThresholdSize Bytes
  | -- | When to flush based on a period of not flushing.
    TranslogFlushThresholdPeriod NominalDiffTime
  | -- | Disables flushing. Note, should be set for a short interval and then enabled.
    TranslogDisableFlush Bool
  | -- | The maximum size of filter cache (per segment in shard).
    CacheFilterMaxSize (Maybe Bytes)
  | -- | The expire after access time for filter cache.
    CacheFilterExpire (Maybe NominalDiffTime)
  | -- | The gateway snapshot interval (only applies to shared gateways).
    GatewaySnapshotInterval NominalDiffTime
  | -- | A node matching any rule will be allowed to host shards from the index.
    RoutingAllocationInclude (NonEmpty NodeAttrFilter)
  | -- | A node matching any rule will NOT be allowed to host shards from the index.
    RoutingAllocationExclude (NonEmpty NodeAttrFilter)
  | -- | Only nodes matching all rules will be allowed to host shards from the index.
    RoutingAllocationRequire (NonEmpty NodeAttrFilter)
  | -- | Enables shard allocation for a specific index.
    RoutingAllocationEnable AllocationPolicy
  | -- | Controls the total number of shards (replicas and primaries) allowed to be allocated on a single node.
    RoutingAllocationShardsPerNode ShardCount
  | -- | When using local gateway a particular shard is recovered only if there can be allocated quorum shards in the cluster.
    RecoveryInitialShards InitialShardCount
  | GCDeletes NominalDiffTime
  | -- | Disables temporarily the purge of expired docs.
    TTLDisablePurge Bool
  | TranslogFSType FSType
  | CompressionSetting Compression
  | IndexCompoundFormat CompoundFormat
  | IndexCompoundOnFlush Bool
  | WarmerEnabled Bool
  | MappingTotalFieldsLimit Int
  | -- | Analysis is not a dynamic setting and can only be performed on a closed index.
    AnalysisSetting Analysis
  | -- | Sets a delay to the allocation of replica shards which become unassigned because a node has left, giving them chance to return. See <https://www.elastic.co/guide/en/elasticsearch/reference/5.6/delayed-allocation.html>
    UnassignedNodeLeftDelayedTimeout NominalDiffTime
  deriving (Eq, Show, Generic)

attrFilterJSON :: NonEmpty NodeAttrFilter -> Value
attrFilterJSON fs =
  object
    [ fromText n .= T.intercalate "," (toList vs)
      | NodeAttrFilter (NodeAttrName n) vs <- toList fs
    ]

parseAttrFilter :: Value -> Parser (NonEmpty NodeAttrFilter)
parseAttrFilter = withObject "NonEmpty NodeAttrFilter" parse
  where
    parse o = case X.toList o of
      [] -> fail "Expected non-empty list of NodeAttrFilters"
      x : xs -> DT.mapM (uncurry parse') (x :| xs)
    parse' n = withText "Text" $ \t ->
      case T.splitOn "," t of
        fv : fvs -> return (NodeAttrFilter (NodeAttrName $ toText n) (fv :| fvs))
        [] -> fail "Expected non-empty list of filter values"

instance ToJSON UpdatableIndexSetting where
  toJSON (NumberOfReplicas x) = oPath ("index" :| ["number_of_replicas"]) x
  toJSON (AutoExpandReplicas x) = oPath ("index" :| ["auto_expand_replicas"]) x
  toJSON (RefreshInterval x) = oPath ("index" :| ["refresh_interval"]) (NominalDiffTimeJSON x)
  toJSON (IndexConcurrency x) = oPath ("index" :| ["concurrency"]) x
  toJSON (FailOnMergeFailure x) = oPath ("index" :| ["fail_on_merge_failure"]) x
  toJSON (TranslogFlushThresholdOps x) = oPath ("index" :| ["translog", "flush_threshold_ops"]) x
  toJSON (TranslogFlushThresholdSize x) = oPath ("index" :| ["translog", "flush_threshold_size"]) x
  toJSON (TranslogFlushThresholdPeriod x) = oPath ("index" :| ["translog", "flush_threshold_period"]) (NominalDiffTimeJSON x)
  toJSON (TranslogDisableFlush x) = oPath ("index" :| ["translog", "disable_flush"]) x
  toJSON (CacheFilterMaxSize x) = oPath ("index" :| ["cache", "filter", "max_size"]) x
  toJSON (CacheFilterExpire x) = oPath ("index" :| ["cache", "filter", "expire"]) (NominalDiffTimeJSON <$> x)
  toJSON (GatewaySnapshotInterval x) = oPath ("index" :| ["gateway", "snapshot_interval"]) (NominalDiffTimeJSON x)
  toJSON (RoutingAllocationInclude fs) = oPath ("index" :| ["routing", "allocation", "include"]) (attrFilterJSON fs)
  toJSON (RoutingAllocationExclude fs) = oPath ("index" :| ["routing", "allocation", "exclude"]) (attrFilterJSON fs)
  toJSON (RoutingAllocationRequire fs) = oPath ("index" :| ["routing", "allocation", "require"]) (attrFilterJSON fs)
  toJSON (RoutingAllocationEnable x) = oPath ("index" :| ["routing", "allocation", "enable"]) x
  toJSON (RoutingAllocationShardsPerNode x) = oPath ("index" :| ["routing", "allocation", "total_shards_per_node"]) x
  toJSON (RecoveryInitialShards x) = oPath ("index" :| ["recovery", "initial_shards"]) x
  toJSON (GCDeletes x) = oPath ("index" :| ["gc_deletes"]) (NominalDiffTimeJSON x)
  toJSON (TTLDisablePurge x) = oPath ("index" :| ["ttl", "disable_purge"]) x
  toJSON (TranslogFSType x) = oPath ("index" :| ["translog", "fs", "type"]) x
  toJSON (CompressionSetting x) = oPath ("index" :| ["codec"]) x
  toJSON (IndexCompoundFormat x) = oPath ("index" :| ["compound_format"]) x
  toJSON (IndexCompoundOnFlush x) = oPath ("index" :| ["compound_on_flush"]) x
  toJSON (WarmerEnabled x) = oPath ("index" :| ["warmer", "enabled"]) x
  toJSON (BlocksReadOnly x) = oPath ("blocks" :| ["read_only"]) x
  toJSON (BlocksRead x) = oPath ("blocks" :| ["read"]) x
  toJSON (BlocksWrite x) = oPath ("blocks" :| ["write"]) x
  toJSON (BlocksMetaData x) = oPath ("blocks" :| ["metadata"]) x
  toJSON (MappingTotalFieldsLimit x) = oPath ("index" :| ["mapping", "total_fields", "limit"]) x
  toJSON (AnalysisSetting x) = oPath ("index" :| ["analysis"]) x
  toJSON (UnassignedNodeLeftDelayedTimeout x) = oPath ("index" :| ["unassigned", "node_left", "delayed_timeout"]) (NominalDiffTimeJSON x)

instance FromJSON UpdatableIndexSetting where
  parseJSON = withObject "UpdatableIndexSetting" parse
    where
      parse o =
        numberOfReplicas
          `taggedAt` ["index", "number_of_replicas"]
          <|> autoExpandReplicas
            `taggedAt` ["index", "auto_expand_replicas"]
          <|> refreshInterval
            `taggedAt` ["index", "refresh_interval"]
          <|> indexConcurrency
            `taggedAt` ["index", "concurrency"]
          <|> failOnMergeFailure
            `taggedAt` ["index", "fail_on_merge_failure"]
          <|> translogFlushThresholdOps
            `taggedAt` ["index", "translog", "flush_threshold_ops"]
          <|> translogFlushThresholdSize
            `taggedAt` ["index", "translog", "flush_threshold_size"]
          <|> translogFlushThresholdPeriod
            `taggedAt` ["index", "translog", "flush_threshold_period"]
          <|> translogDisableFlush
            `taggedAt` ["index", "translog", "disable_flush"]
          <|> cacheFilterMaxSize
            `taggedAt` ["index", "cache", "filter", "max_size"]
          <|> cacheFilterExpire
            `taggedAt` ["index", "cache", "filter", "expire"]
          <|> gatewaySnapshotInterval
            `taggedAt` ["index", "gateway", "snapshot_interval"]
          <|> routingAllocationInclude
            `taggedAt` ["index", "routing", "allocation", "include"]
          <|> routingAllocationExclude
            `taggedAt` ["index", "routing", "allocation", "exclude"]
          <|> routingAllocationRequire
            `taggedAt` ["index", "routing", "allocation", "require"]
          <|> routingAllocationEnable
            `taggedAt` ["index", "routing", "allocation", "enable"]
          <|> routingAllocationShardsPerNode
            `taggedAt` ["index", "routing", "allocation", "total_shards_per_node"]
          <|> recoveryInitialShards
            `taggedAt` ["index", "recovery", "initial_shards"]
          <|> gcDeletes
            `taggedAt` ["index", "gc_deletes"]
          <|> ttlDisablePurge
            `taggedAt` ["index", "ttl", "disable_purge"]
          <|> translogFSType
            `taggedAt` ["index", "translog", "fs", "type"]
          <|> compressionSetting
            `taggedAt` ["index", "codec"]
          <|> compoundFormat
            `taggedAt` ["index", "compound_format"]
          <|> compoundOnFlush
            `taggedAt` ["index", "compound_on_flush"]
          <|> warmerEnabled
            `taggedAt` ["index", "warmer", "enabled"]
          <|> blocksReadOnly
            `taggedAt` ["blocks", "read_only"]
          <|> blocksRead
            `taggedAt` ["blocks", "read"]
          <|> blocksWrite
            `taggedAt` ["blocks", "write"]
          <|> blocksMetaData
            `taggedAt` ["blocks", "metadata"]
          <|> mappingTotalFieldsLimit
            `taggedAt` ["index", "mapping", "total_fields", "limit"]
          <|> analysisSetting
            `taggedAt` ["index", "analysis"]
          <|> unassignedNodeLeftDelayedTimeout
            `taggedAt` ["index", "unassigned", "node_left", "delayed_timeout"]
        where
          taggedAt :: (FromJSON a) => (a -> Parser b) -> [Key] -> Parser b
          taggedAt f ks = taggedAt' f (Object o) ks
      taggedAt' f v [] =
        f =<< (parseJSON v <|> parseJSON (unStringlyTypeJSON v))
      taggedAt' f v (k : ks) =
        withObject
          "Object"
          ( \o -> do
              v' <- o .: k
              taggedAt' f v' ks
          )
          v
      numberOfReplicas = pure . NumberOfReplicas
      autoExpandReplicas = pure . AutoExpandReplicas
      refreshInterval = pure . RefreshInterval . ndtJSON
      indexConcurrency = pure . IndexConcurrency
      failOnMergeFailure = pure . FailOnMergeFailure
      translogFlushThresholdOps = pure . TranslogFlushThresholdOps
      translogFlushThresholdSize = pure . TranslogFlushThresholdSize
      translogFlushThresholdPeriod = pure . TranslogFlushThresholdPeriod . ndtJSON
      translogDisableFlush = pure . TranslogDisableFlush
      cacheFilterMaxSize = pure . CacheFilterMaxSize
      cacheFilterExpire = pure . CacheFilterExpire . fmap ndtJSON
      gatewaySnapshotInterval = pure . GatewaySnapshotInterval . ndtJSON
      routingAllocationInclude = fmap RoutingAllocationInclude . parseAttrFilter
      routingAllocationExclude = fmap RoutingAllocationExclude . parseAttrFilter
      routingAllocationRequire = fmap RoutingAllocationRequire . parseAttrFilter
      routingAllocationEnable = pure . RoutingAllocationEnable
      routingAllocationShardsPerNode = pure . RoutingAllocationShardsPerNode
      recoveryInitialShards = pure . RecoveryInitialShards
      gcDeletes = pure . GCDeletes . ndtJSON
      ttlDisablePurge = pure . TTLDisablePurge
      translogFSType = pure . TranslogFSType
      compressionSetting = pure . CompressionSetting
      compoundFormat = pure . IndexCompoundFormat
      compoundOnFlush = pure . IndexCompoundOnFlush
      warmerEnabled = pure . WarmerEnabled
      blocksReadOnly = pure . BlocksReadOnly
      blocksRead = pure . BlocksRead
      blocksWrite = pure . BlocksWrite
      blocksMetaData = pure . BlocksMetaData
      mappingTotalFieldsLimit = pure . MappingTotalFieldsLimit
      analysisSetting = pure . AnalysisSetting
      unassignedNodeLeftDelayedTimeout = pure . UnassignedNodeLeftDelayedTimeout . ndtJSON

data ReplicaBounds
  = ReplicasBounded Int Int
  | ReplicasLowerBounded Int
  | ReplicasUnbounded
  deriving (Eq, Show)

instance ToJSON ReplicaBounds where
  toJSON (ReplicasBounded a b) = String (showText a <> "-" <> showText b)
  toJSON (ReplicasLowerBounded a) = String (showText a <> "-all")
  toJSON ReplicasUnbounded = Bool False

instance FromJSON ReplicaBounds where
  parseJSON v =
    withText "ReplicaBounds" parseText v
      <|> withBool "ReplicaBounds" parseBool v
    where
      parseText t = case T.splitOn "-" t of
        [a, "all"] -> ReplicasLowerBounded <$> parseReadText a
        [a, b] ->
          ReplicasBounded
            <$> parseReadText a
            <*> parseReadText b
        _ -> fail ("Could not parse ReplicaBounds: " <> show t)
      parseBool False = pure ReplicasUnbounded
      parseBool _ = fail "ReplicasUnbounded cannot be represented with True"

data Compression
  = -- | Compress with LZ4
    CompressionDefault
  | -- | Compress with DEFLATE. Elastic
    --   <https://www.elastic.co/blog/elasticsearch-storage-the-true-story-2.0 blogs>
    --   that this can reduce disk use by 15%-25%.
    CompressionBest
  deriving (Eq, Show, Generic)

instance ToJSON Compression where
  toJSON x = case x of
    CompressionDefault -> toJSON ("default" :: Text)
    CompressionBest -> toJSON ("best_compression" :: Text)

instance FromJSON Compression where
  parseJSON = withText "Compression" $ \t -> case t of
    "default" -> return CompressionDefault
    "best_compression" -> return CompressionBest
    _ -> fail "invalid compression codec"

-- | A measure of bytes used for various configurations. You may want
-- to use smart constructors like 'gigabytes' for larger values.
--
-- >>> gigabytes 9
-- Bytes 9000000000
--
-- >>> megabytes 9
-- Bytes 9000000
--
-- >>> kilobytes 9
-- Bytes 9000
newtype Bytes
  = Bytes Int
  deriving (Eq, Show, Generic, Ord, ToJSON, FromJSON)

gigabytes :: Int -> Bytes
gigabytes n = megabytes (1000 * n)

megabytes :: Int -> Bytes
megabytes n = kilobytes (1000 * n)

kilobytes :: Int -> Bytes
kilobytes n = Bytes (1000 * n)

data FSType
  = FSSimple
  | FSBuffered
  deriving (Eq, Show, Generic)

instance ToJSON FSType where
  toJSON FSSimple = "simple"
  toJSON FSBuffered = "buffered"

instance FromJSON FSType where
  parseJSON = withText "FSType" parse
    where
      parse "simple" = pure FSSimple
      parse "buffered" = pure FSBuffered
      parse t = fail ("Invalid FSType: " <> show t)

data InitialShardCount
  = QuorumShards
  | QuorumMinus1Shards
  | FullShards
  | FullMinus1Shards
  | ExplicitShards Int
  deriving (Eq, Show, Generic)

instance FromJSON InitialShardCount where
  parseJSON v =
    withText "InitialShardCount" parseText v
      <|> ExplicitShards <$> parseJSON v
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

data NodeAttrFilter = NodeAttrFilter
  { nodeAttrFilterName :: NodeAttrName,
    nodeAttrFilterValues :: NonEmpty Text
  }
  deriving (Eq, Ord, Show)

newtype NodeAttrName = NodeAttrName Text deriving (Eq, Ord, Show)

data CompoundFormat
  = CompoundFileFormat Bool
  | -- | percentage between 0 and 1 where 0 is false, 1 is true
    MergeSegmentVsTotalIndex Double
  deriving (Eq, Show, Generic)

instance ToJSON CompoundFormat where
  toJSON (CompoundFileFormat x) = Bool x
  toJSON (MergeSegmentVsTotalIndex x) = toJSON x

instance FromJSON CompoundFormat where
  parseJSON v =
    CompoundFileFormat <$> parseJSON v
      <|> MergeSegmentVsTotalIndex <$> parseJSON v

newtype NominalDiffTimeJSON = NominalDiffTimeJSON {ndtJSON :: NominalDiffTime}

instance ToJSON NominalDiffTimeJSON where
  toJSON (NominalDiffTimeJSON t) = String (showText (round t :: Integer) <> "s")

instance FromJSON NominalDiffTimeJSON where
  parseJSON = withText "NominalDiffTime" parse
    where
      parse t = case T.takeEnd 1 t of
        "s" -> NominalDiffTimeJSON . fromInteger <$> parseReadText (T.dropEnd 1 t)
        _ -> fail "Invalid or missing NominalDiffTime unit (expected s)"

data IndexSettingsSummary = IndexSettingsSummary
  { sSummaryIndexName :: IndexName,
    sSummaryFixedSettings :: IndexSettings,
    sSummaryUpdateable :: [UpdatableIndexSetting]
  }
  deriving (Eq, Show)

parseSettings :: Object -> Parser [UpdatableIndexSetting]
parseSettings o = do
  o' <- o .: "index"
  -- slice the index object into singleton hashmaps and try to parse each
  parses <- forM (HM.toList o') $ \(k, v) -> do
    -- blocks are now nested into the "index" key, which is not how they're serialized
    let atRoot = Object (X.singleton k v)
    let atIndex = Object (X.singleton "index" atRoot)
    optional (parseJSON atRoot <|> parseJSON atIndex)
  return (catMaybes parses)

instance FromJSON IndexSettingsSummary where
  parseJSON = withObject "IndexSettingsSummary" parse
    where
      parse o = case X.toList o of
        [(ixn, v@(Object o'))] ->
          IndexSettingsSummary
            <$> parseJSON (toJSON ixn)
            <*> parseJSON v
            <*> (fmap (filter (not . redundant)) . parseSettings =<< o' .: "settings")
        _ -> fail "Expected single-key object with index name"
      redundant (NumberOfReplicas _) = True
      redundant _ = False

-- | 'OpenCloseIndex' is a sum type for opening and closing indices.
--
--   <http://www.elastic.co/guide/en/elasticsearch/reference/current/indices-open-close.html>
data OpenCloseIndex = OpenIndex | CloseIndex deriving (Eq, Show)

data FieldType
  = GeoPointType
  | GeoShapeType
  | FloatType
  | IntegerType
  | LongType
  | ShortType
  | ByteType
  deriving (Eq, Show)

newtype FieldDefinition = FieldDefinition
  { fieldType :: FieldType
  }
  deriving (Eq, Show)

-- | An 'IndexTemplate' defines a template that will automatically be
--    applied to new indices created. The templates include both
--    'IndexSettings' and mappings, and a simple 'IndexPattern' that
--    controls if the template will be applied to the index created.
--    Specify mappings as follows: @[toJSON TweetMapping, ...]@
--
--    https://www.elastic.co/guide/en/elasticsearch/reference/1.7/indices-templates.html
data IndexTemplate = IndexTemplate
  { templatePatterns :: [IndexPattern],
    templateSettings :: Maybe IndexSettings,
    templateMappings :: Value
  }

instance ToJSON IndexTemplate where
  toJSON (IndexTemplate p s m) =
    merge
      ( object
          [ "index_patterns" .= p,
            "mappings" .= m
          ]
      )
      (toJSON s)
    where
      merge (Object o1) (Object o2) = toJSON $ X.union o1 o2
      merge o Null = o
      merge _ _ = undefined

data MappingField = MappingField
  { mappingFieldName :: FieldName,
    fieldDefinition :: FieldDefinition
  }
  deriving (Eq, Show)

-- | Support for type reification of 'Mapping's is currently incomplete, for
--    now the mapping API verbiage expects a 'ToJSON'able blob.
--
--    Indexes have mappings, mappings are schemas for the documents contained
--    in the index. I'd recommend having only one mapping per index, always
--    having a mapping, and keeping different kinds of documents separated
--    if possible.
newtype Mapping = Mapping {mappingFields :: [MappingField]}
  deriving (Eq, Show)

data UpsertActionMetadata
  = UA_RetryOnConflict Int
  | UA_Version Int
  deriving (Eq, Show)

buildUpsertActionMetadata :: UpsertActionMetadata -> Pair
buildUpsertActionMetadata (UA_RetryOnConflict i) = "retry_on_conflict" .= i
buildUpsertActionMetadata (UA_Version i) = "_version" .= i

data UpsertPayload
  = UpsertDoc Value
  | UpsertScript Bool Script Value
  deriving (Eq, Show)

data AllocationPolicy
  = -- | Allows shard allocation for all shards.
    AllocAll
  | -- | Allows shard allocation only for primary shards.
    AllocPrimaries
  | -- | Allows shard allocation only for primary shards for new indices.
    AllocNewPrimaries
  | -- | No shard allocation is allowed
    AllocNone
  deriving (Eq, Show, Generic)

instance ToJSON AllocationPolicy where
  toJSON AllocAll = String "all"
  toJSON AllocPrimaries = String "primaries"
  toJSON AllocNewPrimaries = String "new_primaries"
  toJSON AllocNone = String "none"

instance FromJSON AllocationPolicy where
  parseJSON = withText "AllocationPolicy" parse
    where
      parse "all" = pure AllocAll
      parse "primaries" = pure AllocPrimaries
      parse "new_primaries" = pure AllocNewPrimaries
      parse "none" = pure AllocNone
      parse t = fail ("Invlaid AllocationPolicy: " <> show t)

-- | 'BulkOperation' is a sum type for expressing the four kinds of bulk
--    operation index, create, delete, and update. 'BulkIndex' behaves like an
--    "upsert", 'BulkCreate' will fail if a document already exists at the DocId.
--    Consult the <http://www.elastic.co/guide/en/elasticsearch/reference/current/docs-bulk.html#docs-bulk Bulk API documentation>
--    for further explanation.
--    Warning: Bulk operations suffixed with @Auto@ rely on Elasticsearch to
--    generate the id. Often, people use auto-generated identifiers when
--    Elasticsearch is the only place that their data is stored. Do not let
--    Elasticsearch be the only place your data is stored. It does not guarantee
--    durability, and it may silently discard data.
--    This <https://github.com/elastic/elasticsearch/issues/10708 issue> is
--    discussed further on github.
data BulkOperation
  = -- | Create the document, replacing it if it already exists.
    BulkIndex IndexName DocId Value
  | -- | Create a document with an autogenerated id.
    BulkIndexAuto IndexName Value
  | -- | Create a document with an autogenerated id. Use fast JSON encoding.
    BulkIndexEncodingAuto IndexName Encoding
  | -- | Create a document, failing if it already exists.
    BulkCreate IndexName DocId Value
  | -- | Create a document, failing if it already exists. Use fast JSON encoding.
    BulkCreateEncoding IndexName DocId Encoding
  | -- | Delete the document
    BulkDelete IndexName DocId
  | -- | Update the document, merging the new value with the existing one.
    BulkUpdate IndexName DocId Value
  | -- | Update the document if it already exists, otherwise insert it.
    BulkUpsert IndexName DocId UpsertPayload [UpsertActionMetadata]
  deriving (Eq, Show)

data IndexAlias = IndexAlias
  { srcIndex :: IndexName,
    indexAlias :: IndexAliasName
  }
  deriving (Eq, Show)

data IndexAliasAction
  = AddAlias IndexAlias IndexAliasCreate
  | RemoveAlias IndexAlias
  deriving (Eq, Show)

data IndexAliasCreate = IndexAliasCreate
  { aliasCreateRouting :: Maybe AliasRouting,
    aliasCreateFilter :: Maybe Filter
  }
  deriving (Eq, Show)

data AliasRouting
  = AllAliasRouting RoutingValue
  | GranularAliasRouting (Maybe SearchAliasRouting) (Maybe IndexAliasRouting)
  deriving (Eq, Show)

newtype SearchAliasRouting
  = SearchAliasRouting (NonEmpty RoutingValue)
  deriving (Eq, Show, Generic)

instance ToJSON SearchAliasRouting where
  toJSON (SearchAliasRouting rvs) = toJSON (T.intercalate "," (routingValue <$> toList rvs))

instance FromJSON SearchAliasRouting where
  parseJSON = withText "SearchAliasRouting" parse
    where
      parse t = SearchAliasRouting <$> parseNEJSON (String <$> T.splitOn "," t)

newtype IndexAliasRouting
  = IndexAliasRouting RoutingValue
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype RoutingValue = RoutingValue {routingValue :: Text}
  deriving (Eq, Show, ToJSON, FromJSON)

newtype IndexAliasesSummary = IndexAliasesSummary {indexAliasesSummary :: [IndexAliasSummary]}
  deriving (Eq, Show)

instance FromJSON IndexAliasesSummary where
  parseJSON = withObject "IndexAliasesSummary" parse
    where
      parse o = IndexAliasesSummary . mconcat <$> mapM (uncurry go) (X.toList o)
      go ixn = withObject "index aliases" $ \ia -> do
        indexName <- parseJSON $ toJSON ixn
        aliases <- ia .:? "aliases" .!= mempty
        forM (HM.toList aliases) $ \(aName, v) -> do
          let indexAlias = IndexAlias indexName (IndexAliasName aName)
          IndexAliasSummary indexAlias <$> parseJSON v

instance ToJSON IndexAliasAction where
  toJSON (AddAlias ia opts) = object ["add" .= (jsonObject ia <> jsonObject opts)]
  toJSON (RemoveAlias ia) = object ["remove" .= jsonObject ia]

instance ToJSON IndexAlias where
  toJSON IndexAlias {..} =
    object
      [ "index" .= srcIndex,
        "alias" .= indexAlias
      ]

instance ToJSON IndexAliasCreate where
  toJSON IndexAliasCreate {..} = Object (filterObj <> routingObj)
    where
      filterObj = maybe mempty (X.singleton "filter" . toJSON) aliasCreateFilter
      routingObj = jsonObject $ maybe (Object mempty) toJSON aliasCreateRouting

instance ToJSON AliasRouting where
  toJSON (AllAliasRouting v) = object ["routing" .= v]
  toJSON (GranularAliasRouting srch idx) = object (catMaybes prs)
    where
      prs =
        [ ("search_routing" .=) <$> srch,
          ("index_routing" .=) <$> idx
        ]

instance FromJSON AliasRouting where
  parseJSON = withObject "AliasRouting" parse
    where
      parse o = parseAll o <|> parseGranular o
      parseAll o = AllAliasRouting <$> o .: "routing"
      parseGranular o = do
        sr <- o .:? "search_routing"
        ir <- o .:? "index_routing"
        if isNothing sr && isNothing ir
          then fail "Both search_routing and index_routing can't be blank"
          else return (GranularAliasRouting sr ir)

instance FromJSON IndexAliasCreate where
  parseJSON v = withObject "IndexAliasCreate" parse v
    where
      parse o =
        IndexAliasCreate
          <$> optional (parseJSON v)
          <*> o .:? "filter"

-- | 'IndexAliasSummary' is a summary of an index alias configured for a server.
data IndexAliasSummary = IndexAliasSummary
  { indexAliasSummaryAlias :: IndexAlias,
    indexAliasSummaryCreate :: IndexAliasCreate
  }
  deriving (Eq, Show)

data JoinRelation
  = ParentDocument FieldName RelationName
  | ChildDocument FieldName RelationName DocId
  deriving (Show, Eq)

-- | 'IndexDocumentSettings' are special settings supplied when indexing
-- a document. For the best backwards compatiblity when new fields are
-- added, you should probably prefer to start with 'defaultIndexDocumentSettings'
data IndexDocumentSettings = IndexDocumentSettings
  { idsVersionControl :: VersionControl,
    idsJoinRelation :: Maybe JoinRelation
  }
  deriving (Eq, Show)

-- | Reasonable default settings. Chooses no version control and no parent.
defaultIndexDocumentSettings :: IndexDocumentSettings
defaultIndexDocumentSettings = IndexDocumentSettings NoVersionControl Nothing

-- | 'IndexSelection' is used for APIs which take a single index, a list of
--    indexes, or the special @_all@ index.

-- TODO: this does not fully support <https://www.elastic.co/guide/en/elasticsearch/reference/1.7/multi-index.html multi-index syntax>. It wouldn't be too hard to implement but you'd have to add the optional parameters (ignore_unavailable, allow_no_indices, expand_wildcards) to any APIs using it. Also would be a breaking API.
data IndexSelection
  = IndexList (NonEmpty IndexName)
  | AllIndexes
  deriving (Eq, Show)

-- | 'NodeSelection' is used for most cluster APIs. See <https://www.elastic.co/guide/en/elasticsearch/reference/current/cluster.html#cluster-nodes here> for more details.
data NodeSelection
  = -- | Whatever node receives this request
    LocalNode
  | NodeList (NonEmpty NodeSelector)
  | AllNodes
  deriving (Eq, Show)

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
  deriving (Eq, Show)

-- | 'TemplateName' is used to describe which template to query/create/delete
newtype TemplateName = TemplateName Text deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | 'IndexPattern' represents a pattern which is matched against index names
newtype IndexPattern = IndexPattern Text deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Username type used for HTTP Basic authentication. See 'basicAuthHook'.
newtype EsUsername = EsUsername {esUsername :: Text} deriving (Read, Show, Eq)

-- | Password type used for HTTP Basic authentication. See 'basicAuthHook'.
newtype EsPassword = EsPassword {esPassword :: Text} deriving (Read, Show, Eq)

data SnapshotRepoSelection
  = SnapshotRepoList (NonEmpty SnapshotRepoPattern)
  | AllSnapshotRepos
  deriving (Eq, Show)

-- | Either specifies an exact repo name or one with globs in it,
-- e.g. @RepoPattern "foo*"@ __NOTE__: Patterns are not supported on ES < 1.7
data SnapshotRepoPattern
  = ExactRepo SnapshotRepoName
  | RepoPattern Text
  deriving (Eq, Show)

-- | The unique name of a snapshot repository.
newtype SnapshotRepoName = SnapshotRepoName {snapshotRepoName :: Text}
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | A generic representation of a snapshot repo. This is what gets
-- sent to and parsed from the server. For repo types enabled by
-- plugins that aren't exported by this library, consider making a
-- custom type which implements 'SnapshotRepo'. If it is a common repo
-- type, consider submitting a pull request to have it included in the
-- library proper
data GenericSnapshotRepo = GenericSnapshotRepo
  { gSnapshotRepoName :: SnapshotRepoName,
    gSnapshotRepoType :: SnapshotRepoType,
    gSnapshotRepoSettings :: GenericSnapshotRepoSettings
  }
  deriving (Eq, Show)

instance SnapshotRepo GenericSnapshotRepo where
  toGSnapshotRepo = id
  fromGSnapshotRepo = Right

newtype SnapshotRepoType = SnapshotRepoType {snapshotRepoType :: Text}
  deriving (Eq, Ord, Show, ToJSON, FromJSON)

-- | Opaque representation of snapshot repo settings. Instances of
-- 'SnapshotRepo' will produce this.
newtype GenericSnapshotRepoSettings = GenericSnapshotRepoSettings {gSnapshotRepoSettingsObject :: Object}
  deriving (Eq, Show, ToJSON)

-- Regardless of whether you send strongly typed json, my version of
-- ES sends back stringly typed json in the settings, e.g. booleans
-- as strings, so we'll try to convert them.
instance FromJSON GenericSnapshotRepoSettings where
  parseJSON = fmap (GenericSnapshotRepoSettings . fmap unStringlyTypeJSON) . parseJSON

-- | The result of running 'verifySnapshotRepo'.
newtype SnapshotVerification = SnapshotVerification
  { snapshotNodeVerifications :: [SnapshotNodeVerification]
  }
  deriving (Eq, Show)

instance FromJSON SnapshotVerification where
  parseJSON = withObject "SnapshotVerification" parse
    where
      parse o = do
        o2 <- o .: "nodes"
        SnapshotVerification <$> mapM (uncurry parse') (HM.toList o2)
      parse' rawFullId = withObject "SnapshotNodeVerification" $ \o ->
        SnapshotNodeVerification (FullNodeId rawFullId) <$> o .: "name"

-- | A node that has verified a snapshot
data SnapshotNodeVerification = SnapshotNodeVerification
  { snvFullId :: FullNodeId,
    snvNodeName :: NodeName
  }
  deriving (Eq, Show)

-- | Unique, automatically-generated name assigned to nodes that are
-- usually returned in node-oriented APIs.
newtype FullNodeId = FullNodeId {fullNodeId :: Text}
  deriving (Eq, Ord, Show, FromJSON)

-- | A human-readable node name that is supplied by the user in the
-- node config or automatically generated by Elasticsearch.
newtype NodeName = NodeName {nodeName :: Text}
  deriving (Eq, Ord, Show, FromJSON)

newtype ClusterName = ClusterName {clusterName :: Text}
  deriving (Eq, Ord, Show, FromJSON)

data NodesInfo = NodesInfo
  { nodesInfo :: [NodeInfo],
    nodesClusterName :: ClusterName
  }
  deriving (Eq, Show)

data NodesStats = NodesStats
  { nodesStats :: [NodeStats],
    nodesStatsClusterName :: ClusterName
  }
  deriving (Eq, Show)

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
  deriving (Eq, Show)

data NodeBreakersStats = NodeBreakersStats
  { nodeStatsParentBreaker :: NodeBreakerStats,
    nodeStatsRequestBreaker :: NodeBreakerStats,
    nodeStatsFieldDataBreaker :: NodeBreakerStats
  }
  deriving (Eq, Show)

data NodeBreakerStats = NodeBreakerStats
  { nodeBreakersTripped :: Int,
    nodeBreakersOverhead :: Double,
    nodeBreakersEstSize :: Bytes,
    nodeBreakersLimitSize :: Bytes
  }
  deriving (Eq, Show)

data NodeHTTPStats = NodeHTTPStats
  { nodeHTTPTotalOpened :: Int,
    nodeHTTPCurrentOpen :: Int
  }
  deriving (Eq, Show)

data NodeTransportStats = NodeTransportStats
  { nodeTransportTXSize :: Bytes,
    nodeTransportCount :: Int,
    nodeTransportRXSize :: Bytes,
    nodeTransportRXCount :: Int,
    nodeTransportServerOpen :: Int
  }
  deriving (Eq, Show)

data NodeFSStats = NodeFSStats
  { nodeFSDataPaths :: [NodeDataPathStats],
    nodeFSTotal :: NodeFSTotalStats,
    nodeFSTimestamp :: UTCTime
  }
  deriving (Eq, Show)

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
  deriving (Eq, Show)

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
  deriving (Eq, Show)

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
  deriving (Eq, Show)

data NodeThreadPoolStats = NodeThreadPoolStats
  { nodeThreadPoolCompleted :: Int,
    nodeThreadPoolLargest :: Int,
    nodeThreadPoolRejected :: Int,
    nodeThreadPoolActive :: Int,
    nodeThreadPoolQueue :: Int,
    nodeThreadPoolThreads :: Int
  }
  deriving (Eq, Show)

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
  deriving (Eq, Show)

data JVMBufferPoolStats = JVMBufferPoolStats
  { jvmBufferPoolStatsTotalCapacity :: Bytes,
    jvmBufferPoolStatsUsed :: Bytes,
    jvmBufferPoolStatsCount :: Int
  }
  deriving (Eq, Show)

data JVMGCStats = JVMGCStats
  { jvmGCStatsCollectionTime :: NominalDiffTime,
    jvmGCStatsCollectionCount :: Int
  }
  deriving (Eq, Show)

data JVMPoolStats = JVMPoolStats
  { jvmPoolStatsPeakMax :: Bytes,
    jvmPoolStatsPeakUsed :: Bytes,
    jvmPoolStatsMax :: Bytes,
    jvmPoolStatsUsed :: Bytes
  }
  deriving (Eq, Show)

data NodeProcessStats = NodeProcessStats
  { nodeProcessTimestamp :: UTCTime,
    nodeProcessOpenFDs :: Int,
    nodeProcessMaxFDs :: Int,
    nodeProcessCPUPercent :: Int,
    nodeProcessCPUTotal :: NominalDiffTime,
    nodeProcessMemTotalVirtual :: Bytes
  }
  deriving (Eq, Show)

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
  deriving (Eq, Show)

data LoadAvgs = LoadAvgs
  { loadAvg1Min :: Double,
    loadAvg5Min :: Double,
    loadAvg15Min :: Double
  }
  deriving (Eq, Show)

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
  deriving (Eq, Show)

-- | A quirky address format used throughout Elasticsearch. An example
-- would be inet[/1.1.1.1:9200]. inet may be a placeholder for a
-- <https://en.wikipedia.org/wiki/Fully_qualified_domain_name FQDN>.
newtype EsAddress = EsAddress {esAddress :: Text}
  deriving (Eq, Ord, Show, FromJSON)

-- | Typically a 7 character hex string.
newtype BuildHash = BuildHash {buildHash :: Text}
  deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

newtype PluginName = PluginName {pluginName :: Text}
  deriving (Eq, Ord, Show, FromJSON)

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
  deriving (Eq, Show)

data NodePluginInfo = NodePluginInfo
  { -- | Is this a site plugin?
    nodePluginSite :: Maybe Bool,
    -- | Is this plugin running on the JVM
    nodePluginJVM :: Maybe Bool,
    nodePluginDescription :: Text,
    nodePluginVersion :: MaybeNA VersionNumber,
    nodePluginName :: PluginName
  }
  deriving (Eq, Show)

data NodeHTTPInfo = NodeHTTPInfo
  { nodeHTTPMaxContentLength :: Bytes,
    nodeHTTPpublishAddress :: EsAddress,
    nodeHTTPbound_address :: [EsAddress]
  }
  deriving (Eq, Show)

data NodeTransportInfo = NodeTransportInfo
  { nodeTransportProfiles :: [BoundTransportAddress],
    nodeTransportPublishAddress :: EsAddress,
    nodeTransportBoundAddress :: [EsAddress]
  }
  deriving (Eq, Show)

data BoundTransportAddress = BoundTransportAddress
  { publishAddress :: EsAddress,
    boundAddress :: [EsAddress]
  }
  deriving (Eq, Show)

data NodeNetworkInfo = NodeNetworkInfo
  { nodeNetworkPrimaryInterface :: NodeNetworkInterface,
    nodeNetworkRefreshInterval :: NominalDiffTime
  }
  deriving (Eq, Show)

newtype MacAddress = MacAddress {macAddress :: Text}
  deriving (Eq, Ord, Show, FromJSON)

newtype NetworkInterfaceName = NetworkInterfaceName {networkInterfaceName :: Text}
  deriving (Eq, Ord, Show, FromJSON)

data NodeNetworkInterface = NodeNetworkInterface
  { nodeNetIfaceMacAddress :: MacAddress,
    nodeNetIfaceName :: NetworkInterfaceName,
    nodeNetIfaceAddress :: Server
  }
  deriving (Eq, Show)

data ThreadPool = ThreadPool
  { nodeThreadPoolName :: Text,
    nodeThreadPoolInfo :: NodeThreadPoolInfo
  }
  deriving (Eq, Show)

data NodeThreadPoolInfo = NodeThreadPoolInfo
  { nodeThreadPoolQueueSize :: ThreadPoolSize,
    nodeThreadPoolKeepalive :: Maybe NominalDiffTime,
    nodeThreadPoolMin :: Maybe Int,
    nodeThreadPoolMax :: Maybe Int,
    nodeThreadPoolType :: ThreadPoolType
  }
  deriving (Eq, Show)

data ThreadPoolSize
  = ThreadPoolBounded Int
  | ThreadPoolUnbounded
  deriving (Eq, Show)

data ThreadPoolType
  = ThreadPoolScaling
  | ThreadPoolFixed
  | ThreadPoolCached
  | ThreadPoolFixedAutoQueueSize
  deriving (Eq, Show)

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
  deriving (Eq, Show)

-- | We cannot parse JVM version numbers and we're not going to try.
newtype JVMVersion = JVMVersion {unJVMVersion :: Text}
  deriving (Eq, Show)

instance FromJSON JVMVersion where
  parseJSON = withText "JVMVersion" (pure . JVMVersion)

data JVMMemoryInfo = JVMMemoryInfo
  { jvmMemoryInfoDirectMax :: Bytes,
    jvmMemoryInfoNonHeapMax :: Bytes,
    jvmMemoryInfoNonHeapInit :: Bytes,
    jvmMemoryInfoHeapMax :: Bytes,
    jvmMemoryInfoHeapInit :: Bytes
  }
  deriving (Eq, Show)

newtype JVMMemoryPool = JVMMemoryPool
  { jvmMemoryPool :: Text
  }
  deriving (Eq, Show, FromJSON)

newtype JVMGCCollector = JVMGCCollector
  { jvmGCCollector :: Text
  }
  deriving (Eq, Show, FromJSON)

newtype PID = PID
  { pid :: Int
  }
  deriving (Eq, Show, FromJSON)

data NodeOSInfo = NodeOSInfo
  { nodeOSRefreshInterval :: NominalDiffTime,
    nodeOSName :: Text,
    nodeOSArch :: Text,
    nodeOSVersion :: Text, -- semver breaks on "5.10.60.1-microsoft-standard-WSL2"
    nodeOSAvailableProcessors :: Int,
    nodeOSAllocatedProcessors :: Int
  }
  deriving (Eq, Show)

data CPUInfo = CPUInfo
  { cpuCacheSize :: Bytes,
    cpuCoresPerSocket :: Int,
    cpuTotalSockets :: Int,
    cpuTotalCores :: Int,
    cpuMHZ :: Int,
    cpuModel :: Text,
    cpuVendor :: Text
  }
  deriving (Eq, Show)

data NodeProcessInfo = NodeProcessInfo
  { -- | See <https://www.elastic.co/guide/en/elasticsearch/reference/current/setup-configuration.html>
    nodeProcessMLockAll :: Bool,
    nodeProcessMaxFileDescriptors :: Maybe Int,
    nodeProcessId :: PID,
    nodeProcessRefreshInterval :: NominalDiffTime
  }
  deriving (Eq, Show)

newtype ShardsResult = ShardsResult
  { shards :: ShardResult
  }
  deriving (Eq, Show)

instance FromJSON ShardsResult where
  parseJSON =
    withObject "ShardsResult" $ \v ->
      ShardsResult
        <$> v
          .: "_shards"

data ShardResult = ShardResult
  { shardTotal :: Int,
    shardsSuccessful :: Int,
    shardsSkipped :: Int,
    shardsFailed :: Int
  }
  deriving (Eq, Show)

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

data SnapshotState
  = SnapshotInit
  | SnapshotStarted
  | SnapshotSuccess
  | SnapshotFailed
  | SnapshotAborted
  | SnapshotMissing
  | SnapshotWaiting
  deriving (Eq, Show)

instance FromJSON SnapshotState where
  parseJSON = withText "SnapshotState" parse
    where
      parse "INIT" = return SnapshotInit
      parse "STARTED" = return SnapshotStarted
      parse "SUCCESS" = return SnapshotSuccess
      parse "FAILED" = return SnapshotFailed
      parse "ABORTED" = return SnapshotAborted
      parse "MISSING" = return SnapshotMissing
      parse "WAITING" = return SnapshotWaiting
      parse t = fail ("Invalid snapshot state " <> T.unpack t)

data SnapshotRestoreSettings = SnapshotRestoreSettings
  { -- | Should the API call return immediately after initializing
    -- the restore or wait until completed? Note that if this is
    -- enabled, it could wait a long time, so you should adjust your
    -- 'ManagerSettings' accordingly to set long timeouts or
    -- explicitly handle timeouts.
    snapRestoreWaitForCompletion :: Bool,
    -- | Nothing will restore all indices in the snapshot. Just [] is
    -- permissable and will essentially be a no-op restore.
    snapRestoreIndices :: Maybe IndexSelection,
    -- | If set to True, any indices that do not exist will be ignored
    -- during snapshot rather than failing the restore.
    snapRestoreIgnoreUnavailable :: Bool,
    -- | If set to false, will ignore any global state in the snapshot
    -- and will not restore it.
    snapRestoreIncludeGlobalState :: Bool,
    -- | A regex pattern for matching indices. Used with
    -- 'snapRestoreRenameReplacement', the restore can reference the
    -- matched index and create a new index name upon restore.
    snapRestoreRenamePattern :: Maybe RestoreRenamePattern,
    -- | Expression of how index renames should be constructed.
    snapRestoreRenameReplacement :: Maybe (NonEmpty RestoreRenameToken),
    -- | If some indices fail to restore, should the process proceed?
    snapRestorePartial :: Bool,
    -- | Should the restore also restore the aliases captured in the
    -- snapshot.
    snapRestoreIncludeAliases :: Bool,
    -- | Settings to apply during the restore process. __NOTE:__ This
    -- option is not supported in ES < 1.5 and should be set to
    -- Nothing in that case.
    snapRestoreIndexSettingsOverrides :: Maybe RestoreIndexSettings,
    -- | This type could be more rich but it isn't clear which
    -- settings are allowed to be ignored during restore, so we're
    -- going with including this feature in a basic form rather than
    -- omitting it. One example here would be
    -- "index.refresh_interval". Any setting specified here will
    -- revert back to the server default during the restore process.
    snapRestoreIgnoreIndexSettings :: Maybe (NonEmpty Text)
  }
  deriving (Eq, Show)

newtype SnapshotRepoUpdateSettings = SnapshotRepoUpdateSettings
  { -- | After creation/update, synchronously check that nodes can
    -- write to this repo. Defaults to True. You may use False if you
    -- need a faster response and plan on verifying manually later
    -- with 'verifySnapshotRepo'.
    repoUpdateVerify :: Bool
  }
  deriving (Eq, Show)

-- | Reasonable defaults for repo creation/update
--
-- * repoUpdateVerify True
defaultSnapshotRepoUpdateSettings :: SnapshotRepoUpdateSettings
defaultSnapshotRepoUpdateSettings = SnapshotRepoUpdateSettings True

-- | A filesystem-based snapshot repo that ships with
-- Elasticsearch. This is an instance of 'SnapshotRepo' so it can be
-- used with 'updateSnapshotRepo'
data FsSnapshotRepo = FsSnapshotRepo
  { fsrName :: SnapshotRepoName,
    fsrLocation :: FilePath,
    fsrCompressMetadata :: Bool,
    -- | Size by which to split large files during snapshotting.
    fsrChunkSize :: Maybe Bytes,
    -- | Throttle node restore rate. If not supplied, defaults to 40mb/sec
    fsrMaxRestoreBytesPerSec :: Maybe Bytes,
    -- | Throttle node snapshot rate. If not supplied, defaults to 40mb/sec
    fsrMaxSnapshotBytesPerSec :: Maybe Bytes
  }
  deriving (Eq, Show, Generic)

instance SnapshotRepo FsSnapshotRepo where
  toGSnapshotRepo FsSnapshotRepo {..} =
    GenericSnapshotRepo fsrName fsRepoType (GenericSnapshotRepoSettings settings)
    where
      settings =
        X.fromList $
          [ X.fromText "location" .= fsrLocation,
            X.fromText "compress" .= fsrCompressMetadata
          ]
            ++ optionalPairs
      optionalPairs =
        catMaybes
          [ ("chunk_size" .=) <$> fsrChunkSize,
            ("max_restore_bytes_per_sec" .=) <$> fsrMaxRestoreBytesPerSec,
            ("max_snapshot_bytes_per_sec" .=) <$> fsrMaxSnapshotBytesPerSec
          ]
  fromGSnapshotRepo GenericSnapshotRepo {..}
    | gSnapshotRepoType == fsRepoType = do
        let o = gSnapshotRepoSettingsObject gSnapshotRepoSettings
        parseRepo $
          FsSnapshotRepo gSnapshotRepoName
            <$> o
              .: "location"
            <*> o
              .:? "compress"
              .!= False
            <*> o
              .:? "chunk_size"
            <*> o
              .:? "max_restore_bytes_per_sec"
            <*> o
              .:? "max_snapshot_bytes_per_sec"
    | otherwise = Left (RepoTypeMismatch fsRepoType gSnapshotRepoType)

parseRepo :: Parser a -> Either SnapshotRepoConversionError a
parseRepo parser = case parseEither (const parser) () of
  Left e -> Left (OtherRepoConversionError (T.pack e))
  Right a -> Right a

fsRepoType :: SnapshotRepoType
fsRepoType = SnapshotRepoType "fs"

-- | Law: fromGSnapshotRepo (toGSnapshotRepo r) == Right r
class SnapshotRepo r where
  toGSnapshotRepo :: r -> GenericSnapshotRepo
  fromGSnapshotRepo :: GenericSnapshotRepo -> Either SnapshotRepoConversionError r

data SnapshotRepoConversionError
  = -- | Expected type and actual type
    RepoTypeMismatch SnapshotRepoType SnapshotRepoType
  | OtherRepoConversionError Text
  deriving (Show, Eq)

instance Exception SnapshotRepoConversionError

data SnapshotCreateSettings = SnapshotCreateSettings
  { -- | Should the API call return immediately after initializing
    -- the snapshot or wait until completed? Note that if this is
    -- enabled it could wait a long time, so you should adjust your
    -- 'ManagerSettings' accordingly to set long timeouts or
    -- explicitly handle timeouts.
    snapWaitForCompletion :: Bool,
    -- | Nothing will snapshot all indices. Just [] is permissable and
    -- will essentially be a no-op snapshot.
    snapIndices :: Maybe IndexSelection,
    -- | If set to True, any matched indices that don't exist will be
    -- ignored. Otherwise it will be an error and fail.
    snapIgnoreUnavailable :: Bool,
    snapIncludeGlobalState :: Bool,
    -- | If some indices failed to snapshot (e.g. if not all primary
    -- shards are available), should the process proceed?
    snapPartial :: Bool
  }
  deriving (Eq, Show)

-- | Reasonable defaults for snapshot creation
--
-- * snapWaitForCompletion False
-- * snapIndices Nothing
-- * snapIgnoreUnavailable False
-- * snapIncludeGlobalState True
-- * snapPartial False
defaultSnapshotCreateSettings :: SnapshotCreateSettings
defaultSnapshotCreateSettings =
  SnapshotCreateSettings
    { snapWaitForCompletion = False,
      snapIndices = Nothing,
      snapIgnoreUnavailable = False,
      snapIncludeGlobalState = True,
      snapPartial = False
    }

data SnapshotSelection
  = SnapshotList (NonEmpty SnapshotPattern)
  | AllSnapshots
  deriving (Eq, Show)

-- | Either specifies an exact snapshot name or one with globs in it,
-- e.g. @SnapPattern "foo*"@ __NOTE__: Patterns are not supported on
-- ES < 1.7
data SnapshotPattern
  = ExactSnap SnapshotName
  | SnapPattern Text
  deriving (Eq, Show)

-- | General information about the state of a snapshot. Has some
-- redundancies with 'SnapshotStatus'
data SnapshotInfo = SnapshotInfo
  { snapInfoShards :: ShardResult,
    snapInfoFailures :: [SnapshotShardFailure],
    snapInfoDuration :: NominalDiffTime,
    snapInfoEndTime :: UTCTime,
    snapInfoStartTime :: UTCTime,
    snapInfoState :: SnapshotState,
    snapInfoIndices :: [IndexName],
    snapInfoName :: SnapshotName
  }
  deriving (Eq, Show)

instance FromJSON SnapshotInfo where
  parseJSON = withObject "SnapshotInfo" parse
    where
      parse o =
        SnapshotInfo
          <$> o
            .: "shards"
          <*> o
            .: "failures"
          <*> (unMS <$> o .: "duration_in_millis")
          <*> (posixMS <$> o .: "end_time_in_millis")
          <*> (posixMS <$> o .: "start_time_in_millis")
          <*> o
            .: "state"
          <*> o
            .: "indices"
          <*> o
            .: "snapshot"

data SnapshotShardFailure = SnapshotShardFailure
  { snapShardFailureIndex :: IndexName,
    snapShardFailureNodeId :: Maybe NodeName, -- I'm not 100% sure this isn't actually 'FullNodeId'
    snapShardFailureReason :: Text,
    snapShardFailureShardId :: ShardId
  }
  deriving (Eq, Show)

instance FromJSON SnapshotShardFailure where
  parseJSON = withObject "SnapshotShardFailure" parse
    where
      parse o =
        SnapshotShardFailure
          <$> o
            .: "index"
          <*> o
            .:? "node_id"
          <*> o
            .: "reason"
          <*> o
            .: "shard_id"

-- | Regex-stype pattern, e.g. "index_(.+)" to match index names
newtype RestoreRenamePattern = RestoreRenamePattern {rrPattern :: Text}
  deriving (Eq, Show, Ord, ToJSON)

-- | A single token in a index renaming scheme for a restore. These
-- are concatenated into a string before being sent to
-- Elasticsearch. Check out these Java
-- <https://docs.oracle.com/javase/7/docs/api/java/util/regex/Matcher.html docs> to find out more if you're into that sort of thing.
data RestoreRenameToken
  = -- | Just a literal string of characters
    RRTLit Text
  | -- | Equivalent to $0. The entire matched pattern, not any subgroup
    RRSubWholeMatch
  | -- | A specific reference to a group number
    RRSubGroup RRGroupRefNum
  deriving (Eq, Show)

-- | A group number for regex matching. Only values from 1-9 are
-- supported. Construct with 'mkRRGroupRefNum'
newtype RRGroupRefNum = RRGroupRefNum {rrGroupRefNum :: Int}
  deriving (Eq, Ord, Show)

instance Bounded RRGroupRefNum where
  minBound = RRGroupRefNum 1
  maxBound = RRGroupRefNum 9

-- | Only allows valid group number references (1-9).
mkRRGroupRefNum :: Int -> Maybe RRGroupRefNum
mkRRGroupRefNum i
  | i >= rrGroupRefNum minBound
      && i <= rrGroupRefNum maxBound =
      Just $ RRGroupRefNum i
  | otherwise = Nothing

-- | Reasonable defaults for snapshot restores
--
-- * snapRestoreWaitForCompletion False
-- * snapRestoreIndices Nothing
-- * snapRestoreIgnoreUnavailable False
-- * snapRestoreIncludeGlobalState True
-- * snapRestoreRenamePattern Nothing
-- * snapRestoreRenameReplacement Nothing
-- * snapRestorePartial False
-- * snapRestoreIncludeAliases True
-- * snapRestoreIndexSettingsOverrides Nothing
-- * snapRestoreIgnoreIndexSettings Nothing
defaultSnapshotRestoreSettings :: SnapshotRestoreSettings
defaultSnapshotRestoreSettings =
  SnapshotRestoreSettings
    { snapRestoreWaitForCompletion = False,
      snapRestoreIndices = Nothing,
      snapRestoreIgnoreUnavailable = False,
      snapRestoreIncludeGlobalState = True,
      snapRestoreRenamePattern = Nothing,
      snapRestoreRenameReplacement = Nothing,
      snapRestorePartial = False,
      snapRestoreIncludeAliases = True,
      snapRestoreIndexSettingsOverrides = Nothing,
      snapRestoreIgnoreIndexSettings = Nothing
    }

-- | Index settings that can be overridden. The docs only mention you
-- can update number of replicas, but there may be more. You
-- definitely cannot override shard count.
newtype RestoreIndexSettings = RestoreIndexSettings
  { restoreOverrideReplicas :: Maybe ReplicaCount
  }
  deriving (Eq, Show)

instance ToJSON RestoreIndexSettings where
  toJSON RestoreIndexSettings {..} = object prs
    where
      prs = catMaybes [("index.number_of_replicas" .=) <$> restoreOverrideReplicas]

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

data TimeInterval
  = Weeks
  | Days
  | Hours
  | Minutes
  | Seconds
  deriving (Eq)

instance Show TimeInterval where
  show Weeks = "w"
  show Days = "d"
  show Hours = "h"
  show Minutes = "m"
  show Seconds = "s"

instance Read TimeInterval where
  readPrec = f =<< TR.get
    where
      f 'w' = return Weeks
      f 'd' = return Days
      f 'h' = return Hours
      f 'm' = return Minutes
      f 's' = return Seconds
      f _ = fail "TimeInterval expected one of w, d, h, m, s"

data Interval
  = Year
  | Quarter
  | Month
  | Week
  | Day
  | Hour
  | Minute
  | Second
  deriving (Eq, Show)

instance ToJSON Interval where
  toJSON Year = "year"
  toJSON Quarter = "quarter"
  toJSON Month = "month"
  toJSON Week = "week"
  toJSON Day = "day"
  toJSON Hour = "hour"
  toJSON Minute = "minute"
  toJSON Second = "second"

parseStringInterval :: (Monad m, MonadFail m) => String -> m NominalDiffTime
parseStringInterval s = case span isNumber s of
  ("", _) -> fail "Invalid interval"
  (nS, unitS) -> case (readMay nS, readMay unitS) of
    (Just n, Just unit) -> return (fromInteger (n * unitNDT unit))
    (Nothing, _) -> fail "Invalid interval number"
    (_, Nothing) -> fail "Invalid interval unit"
  where
    unitNDT Seconds = 1
    unitNDT Minutes = 60
    unitNDT Hours = 60 * 60
    unitNDT Days = 24 * 60 * 60
    unitNDT Weeks = 7 * 24 * 60 * 60

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

instance ToJSON VersionNumber where
  toJSON = toJSON . Versions.prettyVer . versionNumber

instance FromJSON VersionNumber where
  parseJSON = withText "VersionNumber" parse
    where
      parse t =
        case Versions.version t of
          (Left err) -> fail $ show err
          (Right v) -> return (VersionNumber v)

-- * Utils

jsonObject :: (ToJSON a) => a -> Object
jsonObject x =
  case toJSON x of
    Object o -> o
    e -> error $ "Expected Object, but got " <> show e
