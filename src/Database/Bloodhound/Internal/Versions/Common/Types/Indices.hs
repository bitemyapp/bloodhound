{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Bloodhound.Internal.Versions.Common.Types.Indices
  ( AliasRouting (..),
    AllocationPolicy (..),
    CompoundFormat (..),
    Compression (..),
    FSType (..),
    FieldDefinition (..),
    FieldType (..),
    ForceMergeIndexSettings (..),
    IndexAlias (..),
    IndexAliasAction (..),
    IndexAliasCreate (..),
    IndexAliasRouting (..),
    IndexAliasSummary (..),
    IndexAliasesSummary (..),
    IndexDocumentSettings (..),
    IndexMappingsLimits (..),
    IndexPattern (..),
    IndexSelection (..),
    IndexSettings (..),
    IndexSettingsSummary (..),
    IndexTemplate (..),
    JoinRelation (..),
    Mapping (..),
    MappingField (..),
    NominalDiffTimeJSON (..),
    OpenCloseIndex (..),
    ReplicaBounds (..),
    RoutingValue (..),
    SearchAliasRouting (..),
    Status (..),
    TemplateName (..),
    UpdatableIndexSetting (..),
    defaultForceMergeIndexSettings,
    defaultIndexDocumentSettings,
    defaultIndexMappingsLimits,
    defaultIndexSettings,

    -- * Optics
    nameLens,
    clusterNameLens,
    clusterUuidLens,
    versionLens,
    taglineLens,
    indexShardsLens,
    indexReplicasLens,
    indexMappingsLimitsLens,
    indexMappingsLimitDepthLens,
    indexMappingsLimitNestedFieldsLens,
    indexMappingsLimitNestedObjectsLens,
    indexMappingsLimitFieldNameLengthLens,
    maxNumSegmentsLens,
    onlyExpungeDeletesLens,
    flushAfterOptimizeLens,
    sSummaryIndexNameLens,
    sSummaryFixedSettingsLens,
    sSummaryUpdateableLens,
    fieldTypeLens,
    templatePatternsLens,
    templateSettingsLens,
    templateMappingsLens,
    mappingFieldNameLens,
    fieldDefinitionLens,
    mappingFieldsLens,
    srcIndexLens,
    indexAliasLens,
    aliasCreateRoutingLens,
    aliasCreateFilterLens,
    routingValueLens,
    indexAliasesSummaryLens,
    indexAliasSummaryAliasLens,
    indexAliasSummaryCreateLens,
    idsVersionControlLens,
    idsJoinRelationLens,
  )
where

import Control.Monad.Except
import qualified Data.Aeson.KeyMap as X
import qualified Data.HashMap.Strict as HM
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Traversable as DT
import Database.Bloodhound.Internal.Client.Doc
import Database.Bloodhound.Internal.Utils.Imports
import Database.Bloodhound.Internal.Utils.StringlyTyped
import Database.Bloodhound.Internal.Versions.Common.Types.Analysis
import Database.Bloodhound.Internal.Versions.Common.Types.Newtypes
import Database.Bloodhound.Internal.Versions.Common.Types.Nodes
import Database.Bloodhound.Internal.Versions.Common.Types.Query
import Database.Bloodhound.Internal.Versions.Common.Types.Units
import GHC.Generics

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
  deriving stock (Eq, Show)

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

nameLens :: Lens' Status Text
nameLens = lens name (\x y -> x {name = y})

clusterNameLens :: Lens' Status Text
clusterNameLens = lens cluster_name (\x y -> x {cluster_name = y})

clusterUuidLens :: Lens' Status Text
clusterUuidLens = lens cluster_uuid (\x y -> x {cluster_uuid = y})

versionLens :: Lens' Status Version
versionLens = lens version (\x y -> x {version = y})

taglineLens :: Lens' Status Text
taglineLens = lens tagline (\x y -> x {tagline = y})

-- | 'IndexSettings' is used to configure the shards and replicas when
--    you create an Elasticsearch Index.
--
--   <http://www.elastic.co/guide/en/elasticsearch/reference/current/indices-create-index.html>
data IndexSettings = IndexSettings
  { indexShards :: ShardCount,
    indexReplicas :: ReplicaCount,
    indexMappingsLimits :: IndexMappingsLimits
  }
  deriving stock (Eq, Show, Generic)

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

indexShardsLens :: Lens' IndexSettings ShardCount
indexShardsLens = lens indexShards (\x y -> x {indexShards = y})

indexReplicasLens :: Lens' IndexSettings ReplicaCount
indexReplicasLens = lens indexReplicas (\x y -> x {indexReplicas = y})

indexMappingsLimitsLens :: Lens' IndexSettings IndexMappingsLimits
indexMappingsLimitsLens = lens indexMappingsLimits (\x y -> x {indexMappingsLimits = y})

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
  deriving stock (Eq, Show, Generic)

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

indexMappingsLimitDepthLens :: Lens' IndexMappingsLimits (Maybe Int)
indexMappingsLimitDepthLens = lens indexMappingsLimitDepth (\x y -> x {indexMappingsLimitDepth = y})

indexMappingsLimitNestedFieldsLens :: Lens' IndexMappingsLimits (Maybe Int)
indexMappingsLimitNestedFieldsLens = lens indexMappingsLimitNestedFields (\x y -> x {indexMappingsLimitNestedFields = y})

indexMappingsLimitNestedObjectsLens :: Lens' IndexMappingsLimits (Maybe Int)
indexMappingsLimitNestedObjectsLens = lens indexMappingsLimitNestedObjects (\x y -> x {indexMappingsLimitNestedObjects = y})

indexMappingsLimitFieldNameLengthLens :: Lens' IndexMappingsLimits (Maybe Int)
indexMappingsLimitFieldNameLengthLens = lens indexMappingsLimitFieldNameLength (\x y -> x {indexMappingsLimitFieldNameLength = y})

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
  deriving stock (Eq, Show)

maxNumSegmentsLens :: Lens' ForceMergeIndexSettings (Maybe Int)
maxNumSegmentsLens = lens maxNumSegments (\x y -> x {maxNumSegments = y})

onlyExpungeDeletesLens :: Lens' ForceMergeIndexSettings Bool
onlyExpungeDeletesLens = lens onlyExpungeDeletes (\x y -> x {onlyExpungeDeletes = y})

flushAfterOptimizeLens :: Lens' ForceMergeIndexSettings Bool
flushAfterOptimizeLens = lens flushAfterOptimize (\x y -> x {flushAfterOptimize = y})

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
  deriving stock (Eq, Show, Generic)

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
  deriving stock (Eq, Show)

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
  deriving stock (Eq, Show, Generic)

instance ToJSON Compression where
  toJSON x = case x of
    CompressionDefault -> toJSON ("default" :: Text)
    CompressionBest -> toJSON ("best_compression" :: Text)

instance FromJSON Compression where
  parseJSON = withText "Compression" $ \t -> case t of
    "default" -> return CompressionDefault
    "best_compression" -> return CompressionBest
    _ -> fail "invalid compression codec"

data FSType
  = FSSimple
  | FSBuffered
  deriving stock (Eq, Show, Generic)

instance ToJSON FSType where
  toJSON FSSimple = "simple"
  toJSON FSBuffered = "buffered"

instance FromJSON FSType where
  parseJSON = withText "FSType" parse
    where
      parse "simple" = pure FSSimple
      parse "buffered" = pure FSBuffered
      parse t = fail ("Invalid FSType: " <> show t)

data CompoundFormat
  = CompoundFileFormat Bool
  | -- | percentage between 0 and 1 where 0 is false, 1 is true
    MergeSegmentVsTotalIndex Double
  deriving stock (Eq, Show, Generic)

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
  deriving stock (Eq, Show)

sSummaryIndexNameLens :: Lens' IndexSettingsSummary IndexName
sSummaryIndexNameLens = lens sSummaryIndexName (\x y -> x {sSummaryIndexName = y})

sSummaryFixedSettingsLens :: Lens' IndexSettingsSummary IndexSettings
sSummaryFixedSettingsLens = lens sSummaryFixedSettings (\x y -> x {sSummaryFixedSettings = y})

sSummaryUpdateableLens :: Lens' IndexSettingsSummary [UpdatableIndexSetting]
sSummaryUpdateableLens = lens sSummaryUpdateable (\x y -> x {sSummaryUpdateable = y})

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
data OpenCloseIndex = OpenIndex | CloseIndex deriving stock (Eq, Show)

data FieldType
  = GeoPointType
  | GeoShapeType
  | FloatType
  | IntegerType
  | LongType
  | ShortType
  | ByteType
  deriving stock (Eq, Show)

newtype FieldDefinition = FieldDefinition
  { fieldType :: FieldType
  }
  deriving stock (Eq, Show)

fieldTypeLens :: Lens' FieldDefinition FieldType
fieldTypeLens = lens fieldType (\x y -> x {fieldType = y})

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

templatePatternsLens :: Lens' IndexTemplate [IndexPattern]
templatePatternsLens = lens templatePatterns (\x y -> x {templatePatterns = y})

templateSettingsLens :: Lens' IndexTemplate (Maybe IndexSettings)
templateSettingsLens = lens templateSettings (\x y -> x {templateSettings = y})

templateMappingsLens :: Lens' IndexTemplate Value
templateMappingsLens = lens templateMappings (\x y -> x {templateMappings = y})

data MappingField = MappingField
  { mappingFieldName :: FieldName,
    fieldDefinition :: FieldDefinition
  }
  deriving stock (Eq, Show)

mappingFieldNameLens :: Lens' MappingField FieldName
mappingFieldNameLens = lens mappingFieldName (\x y -> x {mappingFieldName = y})

fieldDefinitionLens :: Lens' MappingField FieldDefinition
fieldDefinitionLens = lens fieldDefinition (\x y -> x {fieldDefinition = y})

-- | Support for type reification of 'Mapping's is currently incomplete, for
--    now the mapping API verbiage expects a 'ToJSON'able blob.
--
--    Indexes have mappings, mappings are schemas for the documents contained
--    in the index. I'd recommend having only one mapping per index, always
--    having a mapping, and keeping different kinds of documents separated
--    if possible.
newtype Mapping = Mapping {mappingFields :: [MappingField]}
  deriving stock (Eq, Show)

mappingFieldsLens :: Lens' Mapping [MappingField]
mappingFieldsLens = lens mappingFields (\x y -> x {mappingFields = y})

data AllocationPolicy
  = -- | Allows shard allocation for all shards.
    AllocAll
  | -- | Allows shard allocation only for primary shards.
    AllocPrimaries
  | -- | Allows shard allocation only for primary shards for new indices.
    AllocNewPrimaries
  | -- | No shard allocation is allowed
    AllocNone
  deriving stock (Eq, Show, Generic)

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

data IndexAlias = IndexAlias
  { srcIndex :: IndexName,
    indexAlias :: IndexAliasName
  }
  deriving stock (Eq, Show)

srcIndexLens :: Lens' IndexAlias IndexName
srcIndexLens = lens srcIndex (\x y -> x {srcIndex = y})

indexAliasLens :: Lens' IndexAlias IndexAliasName
indexAliasLens = lens indexAlias (\x y -> x {indexAlias = y})

data IndexAliasAction
  = AddAlias IndexAlias IndexAliasCreate
  | RemoveAlias IndexAlias
  deriving stock (Eq, Show)

data IndexAliasCreate = IndexAliasCreate
  { aliasCreateRouting :: Maybe AliasRouting,
    aliasCreateFilter :: Maybe Filter
  }
  deriving stock (Eq, Show)

aliasCreateRoutingLens :: Lens' IndexAliasCreate (Maybe AliasRouting)
aliasCreateRoutingLens = lens aliasCreateRouting (\x y -> x {aliasCreateRouting = y})

aliasCreateFilterLens :: Lens' IndexAliasCreate (Maybe Filter)
aliasCreateFilterLens = lens aliasCreateFilter (\x y -> x {aliasCreateFilter = y})

data AliasRouting
  = AllAliasRouting RoutingValue
  | GranularAliasRouting (Maybe SearchAliasRouting) (Maybe IndexAliasRouting)
  deriving stock (Eq, Show)

newtype SearchAliasRouting
  = SearchAliasRouting (NonEmpty RoutingValue)
  deriving stock (Eq, Show, Generic)

instance ToJSON SearchAliasRouting where
  toJSON (SearchAliasRouting rvs) = toJSON (T.intercalate "," (routingValue <$> toList rvs))

instance FromJSON SearchAliasRouting where
  parseJSON = withText "SearchAliasRouting" parse
    where
      parse t = SearchAliasRouting <$> parseNEJSON (String <$> T.splitOn "," t)

newtype IndexAliasRouting
  = IndexAliasRouting RoutingValue
  deriving newtype (Eq, Show, ToJSON, FromJSON)

newtype RoutingValue = RoutingValue {routingValue :: Text}
  deriving newtype (Eq, Show, ToJSON, FromJSON)

routingValueLens :: Lens' RoutingValue Text
routingValueLens = lens routingValue (\x y -> x {routingValue = y})

newtype IndexAliasesSummary = IndexAliasesSummary {indexAliasesSummary :: [IndexAliasSummary]}
  deriving stock (Eq, Show)

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

indexAliasesSummaryLens :: Lens' IndexAliasesSummary [IndexAliasSummary]
indexAliasesSummaryLens = lens indexAliasesSummary (\x y -> x {indexAliasesSummary = y})

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
  deriving stock (Eq, Show)

indexAliasSummaryAliasLens :: Lens' IndexAliasSummary IndexAlias
indexAliasSummaryAliasLens = lens indexAliasSummaryAlias (\x y -> x {indexAliasSummaryAlias = y})

indexAliasSummaryCreateLens :: Lens' IndexAliasSummary IndexAliasCreate
indexAliasSummaryCreateLens = lens indexAliasSummaryCreate (\x y -> x {indexAliasSummaryCreate = y})

data JoinRelation
  = ParentDocument FieldName RelationName
  | ChildDocument FieldName RelationName DocId
  deriving stock (Eq, Show)

-- | 'IndexDocumentSettings' are special settings supplied when indexing
-- a document. For the best backwards compatiblity when new fields are
-- added, you should probably prefer to start with 'defaultIndexDocumentSettings'
data IndexDocumentSettings = IndexDocumentSettings
  { idsVersionControl :: VersionControl,
    idsJoinRelation :: Maybe JoinRelation
  }
  deriving stock (Eq, Show)

idsVersionControlLens :: Lens' IndexDocumentSettings VersionControl
idsVersionControlLens = lens idsVersionControl (\x y -> x {idsVersionControl = y})

idsJoinRelationLens :: Lens' IndexDocumentSettings (Maybe JoinRelation)
idsJoinRelationLens = lens idsJoinRelation (\x y -> x {idsJoinRelation = y})

-- | Reasonable default settings. Chooses no version control and no parent.
defaultIndexDocumentSettings :: IndexDocumentSettings
defaultIndexDocumentSettings = IndexDocumentSettings NoVersionControl Nothing

-- | 'IndexSelection' is used for APIs which take a single index, a list of
--    indexes, or the special @_all@ index.

-- TODO: this does not fully support <https://www.elastic.co/guide/en/elasticsearch/reference/1.7/multi-index.html multi-index syntax>. It wouldn't be too hard to implement but you'd have to add the optional parameters (ignore_unavailable, allow_no_indices, expand_wildcards) to any APIs using it. Also would be a breaking API.
data IndexSelection
  = IndexList (NonEmpty IndexName)
  | AllIndexes
  deriving stock (Eq, Show)

-- | 'TemplateName' is used to describe which template to query/create/delete
newtype TemplateName = TemplateName Text deriving newtype (Eq, Show, ToJSON, FromJSON)

-- | 'IndexPattern' represents a pattern which is matched against index names
newtype IndexPattern = IndexPattern Text deriving newtype (Eq, Show, ToJSON, FromJSON)

-- * Utils

jsonObject :: (ToJSON a) => a -> Object
jsonObject x =
  case toJSON x of
    Object o -> o
    e -> error $ "Expected Object, but got " <> show e
