{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
-- {-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE UndecidableInstances       #-}

-------------------------------------------------------------------------------
-- |
-- Module : Database.Bloodhound.Types
-- Copyright : (C) 2014, 2015, 2016 Chris Allen
-- License : BSD-style (see the file LICENSE)
-- Maintainer : Chris Allen <cma@bitemyapp.com
-- Stability : provisional
-- Portability : DeriveGeneric, RecordWildCards
--
-- Data types for describing actions and data structures performed to interact
-- with Elasticsearch. The two main buckets your queries against Elasticsearch
-- will fall into are 'Query's and 'Filter's. 'Filter's are more like
-- traditional database constraints and often have preferable performance
-- properties. 'Query's support human-written textual queries, such as fuzzy
-- queries.
-------------------------------------------------------------------------------



module Database.V1.Bloodhound.Types
       ( defaultCache
       , defaultIndexSettings
       , defaultIndexDocumentSettings
       , mkSort
       , showText
       , unpackId
       , mkMatchQuery
       , mkMultiMatchQuery
       , mkBoolQuery
       , mkRangeQuery
       , mkQueryStringQuery
       , mkAggregations
       , mkTermsAggregation
       , mkTermsScriptAggregation
       , mkDateHistogram
       , mkCardinalityAggregation
       , mkDocVersion
       , docVersionNumber
       , toMissing
       , toTerms
       , toDateHistogram
       , toTopHits
       , omitNulls
       , BH(..)
       , runBH
       , BHEnv
       , bhServer
       , bhManager
       , bhRequestHook
       , mkBHEnv
       , MonadBH(..)
       , Version(..)
       , VersionNumber(..)
       , MaybeNA(..)
       , BuildHash(..)
       , Status(..)
       , Existence(..)
       , NullValue(..)
       , IndexSettings(..)
       , UpdatableIndexSetting(..)
       , IndexSettingsSummary(..)
       , AllocationPolicy(..)
       , ReplicaBounds(..)
       , Bytes(..)
       , gigabytes
       , megabytes
       , kilobytes
       , FSType(..)
       , InitialShardCount(..)
       , NodeAttrFilter(..)
       , NodeAttrName(..)
       , CompoundFormat(..)
       , IndexTemplate(..)
       , Server(..)
       , Reply
       , EsResult(..)
       , EsResultFound(..)
       , EsError(..)
       , EsProtocolException(..)
       , IndexAlias(..)
       , IndexAliasName(..)
       , IndexAliasAction(..)
       , IndexAliasCreate(..)
       , IndexAliasSummary(..)
       , IndexAliasesSummary(..)
       , AliasRouting(..)
       , SearchAliasRouting(..)
       , IndexAliasRouting(..)
       , RoutingValue(..)
       , DocVersion
       , ExternalDocVersion(..)
       , VersionControl(..)
       , DocumentParent(..)
       , IndexDocumentSettings(..)
       , Query(..)
       , Search(..)
       , SearchType(..)
       , SearchResult(..)
       , ScrollId(..)
       , SearchHits(..)
       , TrackSortScores
       , From(..)
       , Size(..)
       , Source(..)
       , PatternOrPatterns(..)
       , Include(..)
       , Exclude(..)
       , Pattern(..)
       , ShardResult(..)
       , Hit(..)
       , Filter(..)
       , Seminearring(..)
       , BoolMatch(..)
       , Term(..)
       , GeoPoint(..)
       , GeoBoundingBoxConstraint(..)
       , GeoBoundingBox(..)
       , GeoFilterType(..)
       , Distance(..)
       , DistanceUnit(..)
       , DistanceType(..)
       , DistanceRange(..)
       , OptimizeBbox(..)
       , LatLon(..)
       , RangeValue(..)
       , RangeExecution(..)
       , LessThan(..)
       , LessThanEq(..)
       , GreaterThan(..)
       , GreaterThanEq(..)
       , LessThanD(..)
       , LessThanEqD(..)
       , GreaterThanD(..)
       , GreaterThanEqD(..)
       , Regexp(..)
       , RegexpFlags(..)
       , RegexpFlag(..)
       , FieldName(..)
       , Script(..)
       , IndexName(..)
       , IndexSelection(..)
       , NodeSelection(..)
       , NodeSelector(..)
       , IndexOptimizationSettings(..)
       , defaultIndexOptimizationSettings
       , TemplateName(..)
       , TemplatePattern(..)
       , MappingName(..)
       , DocId(..)
       , CacheName(..)
       , CacheKey(..)
       , BulkOperation(..)
       , ReplicaCount(..)
       , ShardCount(..)
       , Sort
       , SortMode(..)
       , SortOrder(..)
       , SortSpec(..)
       , DefaultSort(..)
       , Missing(..)
       , OpenCloseIndex(..)
       , Method
       , Boost(..)
       , MatchQuery(..)
       , MultiMatchQuery(..)
       , BoolQuery(..)
       , BoostingQuery(..)
       , CommonTermsQuery(..)
       , DisMaxQuery(..)
       , FilteredQuery(..)
       , FuzzyLikeThisQuery(..)
       , FuzzyLikeFieldQuery(..)
       , FuzzyQuery(..)
       , HasChildQuery(..)
       , HasParentQuery(..)
       , IndicesQuery(..)
       , MoreLikeThisQuery(..)
       , MoreLikeThisFieldQuery(..)
       , NestedQuery(..)
       , PrefixQuery(..)
       , QueryStringQuery(..)
       , SimpleQueryStringQuery(..)
       , RangeQuery(..)
       , RegexpQuery(..)
       , QueryString(..)
       , TemplateQueryInline(..)
       , TemplateQueryKeyValuePairs(..)
       , BooleanOperator(..)
       , ZeroTermsQuery(..)
       , CutoffFrequency(..)
       , Analyzer(..)
       , MaxExpansions(..)
       , Lenient(..)
       , MatchQueryType(..)
       , MultiMatchQueryType(..)
       , Tiebreaker(..)
       , MinimumMatch(..)
       , DisableCoord(..)
       , CommonMinimumMatch(..)
       , MinimumMatchHighLow(..)
       , PrefixLength(..)
       , Fuzziness(..)
       , IgnoreTermFrequency(..)
       , MaxQueryTerms(..)
       , ScoreType(..)
       , Score
       , Cache
       , TypeName(..)
       , BoostTerms(..)
       , MaxWordLength(..)
       , MinWordLength(..)
       , MaxDocFrequency(..)
       , MinDocFrequency(..)
       , PhraseSlop(..)
       , StopWord(..)
       , QueryPath(..)
       , MinimumTermFrequency(..)
       , PercentMatch(..)
       , FieldDefinition(..)
       , MappingField(..)
       , Mapping(..)
       , AllowLeadingWildcard(..)
       , LowercaseExpanded(..)
       , GeneratePhraseQueries(..)
       , Locale(..)
       , AnalyzeWildcard(..)
       , EnablePositionIncrements(..)
       , SimpleQueryFlag(..)
       , FieldOrFields(..)
       , Monoid(..)
       , ToJSON(..)
       , Interval(..)
       , TimeInterval(..)
       , ExecutionHint(..)
       , CollectionMode(..)
       , TermOrder(..)
       , TermInclusion(..)
       , SnapshotRepoSelection(..)
       , GenericSnapshotRepo(..)
       , SnapshotRepo(..)
       , SnapshotRepoConversionError(..)
       , SnapshotRepoType(..)
       , GenericSnapshotRepoSettings(..)
       , SnapshotRepoUpdateSettings(..)
       , defaultSnapshotRepoUpdateSettings
       , SnapshotRepoName(..)
       , SnapshotRepoPattern(..)
       , SnapshotVerification(..)
       , SnapshotNodeVerification(..)
       , FullNodeId(..)
       , NodeName(..)
       , ClusterName(..)
       , NodesInfo(..)
       , NodesStats(..)
       , NodeStats(..)
       , NodeBreakersStats(..)
       , NodeBreakerStats(..)
       , NodeHTTPStats(..)
       , NodeTransportStats(..)
       , NodeFSStats(..)
       , NodeDataPathStats(..)
       , NodeFSTotalStats(..)
       , NodeNetworkStats(..)
       , NodeThreadPoolsStats(..)
       , NodeThreadPoolStats(..)
       , NodeJVMStats(..)
       , JVMBufferPoolStats(..)
       , JVMGCStats(..)
       , JVMPoolStats(..)
       , NodeProcessStats(..)
       , NodeOSStats(..)
       , LoadAvgs(..)
       , NodeIndicesStats(..)
       , EsAddress(..)
       , PluginName(..)
       , NodeInfo(..)
       , NodePluginInfo(..)
       , NodeHTTPInfo(..)
       , NodeTransportInfo(..)
       , BoundTransportAddress(..)
       , NodeNetworkInfo(..)
       , MacAddress(..)
       , NetworkInterfaceName(..)
       , NodeNetworkInterface(..)
       , NodeThreadPoolsInfo(..)
       , NodeThreadPoolInfo(..)
       , ThreadPoolSize(..)
       , ThreadPoolType(..)
       , NodeJVMInfo(..)
       , JVMMemoryPool(..)
       , JVMGCCollector(..)
       , JVMMemoryInfo(..)
       , PID(..)
       , NodeOSInfo(..)
       , CPUInfo(..)
       , NodeProcessInfo(..)
       , FsSnapshotRepo(..)
       , SnapshotCreateSettings(..)
       , defaultSnapshotCreateSettings
       , SnapshotSelection(..)
       , SnapshotPattern(..)
       , SnapshotInfo(..)
       , SnapshotShardFailure(..)
       , ShardId(..)
       , SnapshotName(..)
       , SnapshotState(..)
       , SnapshotRestoreSettings(..)
       , defaultSnapshotRestoreSettings
       , RestoreRenamePattern(..)
       , RestoreRenameToken(..)
       , RRGroupRefNum
       , rrGroupRefNum
       , mkRRGroupRefNum
       , RestoreIndexSettings(..)
       , Suggest(..)
       , SuggestType(..)
       , PhraseSuggester(..)
       , PhraseSuggesterHighlighter(..)
       , PhraseSuggesterCollate(..)
       , mkPhraseSuggester
       , SuggestOptions(..)
       , SuggestResponse(..)
       , NamedSuggestionResponse(..)
       , DirectGenerators(..)
       , mkDirectGenerators
       , DirectGeneratorSuggestModeTypes (..)

       , Aggregation(..)
       , Aggregations
       , AggregationResults
       , BucketValue(..)
       , Bucket(..)
       , BucketAggregation(..)
       , TermsAggregation(..)
       , MissingAggregation(..)
       , ValueCountAggregation(..)
       , FilterAggregation(..)
       , CardinalityAggregation(..)
       , DateHistogramAggregation(..)
       , DateRangeAggregation(..)
       , DateRangeAggRange(..)
       , DateMathExpr(..)
       , DateMathAnchor(..)
       , DateMathModifier(..)
       , DateMathUnit(..)
       , TopHitsAggregation(..)

       , Highlights(..)
       , FieldHighlight(..)
       , HighlightSettings(..)
       , PlainHighlight(..)
       , PostingsHighlight(..)
       , FastVectorHighlight(..)
       , CommonHighlight(..)
       , NonPostings(..)
       , HighlightEncoder(..)
       , HighlightTag(..)
       , HitHighlight

       , MissingResult(..)
       , TermsResult(..)
       , DateHistogramResult(..)
       , DateRangeResult(..)
       , TopHitResult(..)

       , EsUsername(..)
       , EsPassword(..)
         ) where

import           Control.Applicative                   as A
import           Control.Arrow                         (first)
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Reader                  (MonadReader (..),
                                                        ReaderT (..))
import           Control.Monad.State                   (MonadState)
import           Control.Monad.Writer                  (MonadWriter)
import           Data.Aeson
import           Data.Aeson.Types                      (Pair, Parser,
                                                        emptyObject,
                                                        parseEither, parseMaybe,
                                                        typeMismatch)
import qualified Data.ByteString.Lazy.Char8            as L
import           Data.Char
import           Data.Hashable                         (Hashable)
import qualified Data.HashMap.Strict                   as HM
import           Data.List                             (foldl', intercalate,
                                                        nub)
import           Data.List.NonEmpty                    (NonEmpty (..), toList)
import qualified Data.Map.Strict                       as M
import           Data.Maybe
import           Data.Scientific                       (Scientific)
import           Data.Semigroup
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import           Data.Time.Calendar
import           Data.Time.Clock                       (NominalDiffTime,
                                                        UTCTime)
import           Data.Time.Clock.POSIX
import qualified Data.Traversable                      as DT
import           Data.Typeable                         (Typeable)
import qualified Data.Vector                           as V
import qualified Data.Version                          as Vers
import           GHC.Enum
import           GHC.Generics                          (Generic)
import           Network.HTTP.Client
import qualified Network.HTTP.Types.Method             as NHTM
import qualified Text.ParserCombinators.ReadP          as RP
import qualified Text.Read                             as TR

import           Database.V1.Bloodhound.Types.Class
import           Database.V1.Bloodhound.Types.Internal

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Aeson
-- >>> import Database.V1.Bloodhound
-- >>> let testServer = (Server "http://localhost:9200")
-- >>> let testIndex = IndexName "twitter"
-- >>> let testMapping = MappingName "tweet"
-- >>> let defaultIndexSettings = IndexSettings (ShardCount 3) (ReplicaCount 2)

-- defaultIndexSettings is exported by Database.Bloodhound as well
-- no trailing slashes in servers, library handles building the path.

-- | Create a 'BHEnv' with all optional fields defaulted. HTTP hook
-- will be a noop. You can use the exported fields to customize it further, e.g.:
--
-- >> (mkBHEnv myServer myManager) { bhRequestHook = customHook }
mkBHEnv :: Server -> Manager -> BHEnv
mkBHEnv s m = BHEnv s m return

newtype BH m a = BH {
      unBH :: ReaderT BHEnv m a
    } deriving ( Functor
               , A.Applicative
               , Monad
               , MonadIO
               , MonadState s
               , MonadWriter w
               , MonadError e
               , Alternative
               , MonadPlus
               , MonadFix
               , MonadThrow
               , MonadCatch
               , MonadMask)

instance MonadTrans BH where
  lift = BH . lift

instance (MonadReader r m) => MonadReader r (BH m) where
    ask = lift ask
    local f (BH (ReaderT m)) = BH $ ReaderT $ \r ->
      local f (m r)

instance (Functor m, Applicative m, MonadIO m) => MonadBH (BH m) where
  getBHEnv = BH getBHEnv

runBH :: BHEnv -> BH m a -> m a
runBH e f = runReaderT (unBH f) e

{-| 'Version' is embedded in 'Status' -}
data Version = Version { number          :: VersionNumber
                       , build_hash      :: BuildHash
                       , build_timestamp :: UTCTime
                       , build_snapshot  :: Bool
                       , lucene_version  :: VersionNumber } deriving (Eq, Read, Show, Generic, Typeable)

-- | Traditional software versioning number
newtype VersionNumber = VersionNumber { versionNumber :: Vers.Version
                                      } deriving (Eq, Read, Show, Generic, Typeable, Ord)

{-| 'Status' is a data type for describing the JSON body returned by
    Elasticsearch when you query its status. This was deprecated in 1.2.0.

   <http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/indices-status.html#indices-status>
-}

data Status = Status { ok      :: Maybe Bool
                     , status  :: Int
                     , name    :: Text
                     , version :: Version
                     , tagline :: Text } deriving (Eq, Read, Show, Generic)

{-| 'IndexSettings' is used to configure the shards and replicas when you create
   an Elasticsearch Index.

   <http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/indices-create-index.html>
-}

data IndexSettings =
  IndexSettings { indexShards   :: ShardCount
                , indexReplicas :: ReplicaCount } deriving (Eq, Read, Show, Generic, Typeable)

{-| 'defaultIndexSettings' is an 'IndexSettings' with 3 shards and 2 replicas. -}
defaultIndexSettings :: IndexSettings
defaultIndexSettings =  IndexSettings (ShardCount 3) (ReplicaCount 2)


{-| 'IndexOptimizationSettings' is used to configure index optimization. See
    <https://www.elastic.co/guide/en/elasticsearch/reference/1.7/indices-optimize.html>
    for more info.
-}
data IndexOptimizationSettings =
  IndexOptimizationSettings { maxNumSegments     :: Maybe Int
                            -- ^ Number of segments to optimize to. 1 will fully optimize the index. If omitted, the default behavior is to only optimize if the server deems it necessary.
                            , onlyExpungeDeletes :: Bool
                            -- ^ Should the optimize process only expunge segments with deletes in them? If the purpose of the optimization is to free disk space, this should be set to True.
                            , flushAfterOptimize :: Bool
                            -- ^ Should a flush be performed after the optimize.
                            } deriving (Eq, Show, Generic, Typeable)


{-| 'defaultIndexOptimizationSettings' implements the default settings that
    ElasticSearch uses for index optimization. 'maxNumSegments' is Nothing,
    'onlyExpungeDeletes' is False, and flushAfterOptimize is True.
-}
defaultIndexOptimizationSettings :: IndexOptimizationSettings
defaultIndexOptimizationSettings = IndexOptimizationSettings Nothing False True

{-| 'UpdatableIndexSetting' are settings which may be updated after an index is created.

   <https://www.elastic.co/guide/en/elasticsearch/reference/current/indices-update-settings.html>
-}
data UpdatableIndexSetting = NumberOfReplicas ReplicaCount
                           -- ^ The number of replicas each shard has.
                           | AutoExpandReplicas ReplicaBounds
                           | BlocksReadOnly Bool
                           -- ^ Set to True to have the index read only. False to allow writes and metadata changes.
                           | BlocksRead Bool
                           -- ^ Set to True to disable read operations against the index.
                           | BlocksWrite Bool
                           -- ^ Set to True to disable write operations against the index.
                           | BlocksMetaData Bool
                           -- ^ Set to True to disable metadata operations against the index.
                           | RefreshInterval NominalDiffTime
                           -- ^ The async refresh interval of a shard
                           | IndexConcurrency Int
                           | FailOnMergeFailure Bool
                           | TranslogFlushThresholdOps Int
                           -- ^ When to flush on operations.
                           | TranslogFlushThresholdSize Bytes
                           -- ^ When to flush based on translog (bytes) size.
                           | TranslogFlushThresholdPeriod NominalDiffTime
                           -- ^ When to flush based on a period of not flushing.
                           | TranslogDisableFlush Bool
                           -- ^ Disables flushing. Note, should be set for a short interval and then enabled.
                           | CacheFilterMaxSize (Maybe Bytes)
                           -- ^ The maximum size of filter cache (per segment in shard).
                           | CacheFilterExpire (Maybe NominalDiffTime)
                           -- ^ The expire after access time for filter cache.
                           | GatewaySnapshotInterval NominalDiffTime
                           -- ^ The gateway snapshot interval (only applies to shared gateways).
                           | RoutingAllocationInclude (NonEmpty NodeAttrFilter)
                           -- ^ A node matching any rule will be allowed to host shards from the index.
                           | RoutingAllocationExclude (NonEmpty NodeAttrFilter)
                           -- ^ A node matching any rule will NOT be allowed to host shards from the index.
                           | RoutingAllocationRequire (NonEmpty NodeAttrFilter)
                           -- ^ Only nodes matching all rules will be allowed to host shards from the index.
                           | RoutingAllocationEnable AllocationPolicy
                           -- ^ Enables shard allocation for a specific index.
                           | RoutingAllocationShardsPerNode ShardCount
                           -- ^ Controls the total number of shards (replicas and primaries) allowed to be allocated on a single node.
                           | RecoveryInitialShards InitialShardCount
                           -- ^ When using local gateway a particular shard is recovered only if there can be allocated quorum shards in the cluster.
                           | GCDeletes NominalDiffTime
                           | TTLDisablePurge Bool
                           -- ^ Disables temporarily the purge of expired docs.
                           | TranslogFSType FSType
                           | IndexCompoundFormat CompoundFormat
                           | IndexCompoundOnFlush Bool
                           | WarmerEnabled Bool
                           deriving (Eq, Show, Generic, Typeable)

data AllocationPolicy = AllocAll
                      -- ^ Allows shard allocation for all shards.
                      | AllocPrimaries
                      -- ^ Allows shard allocation only for primary shards.
                      | AllocNewPrimaries
                      -- ^ Allows shard allocation only for primary shards for new indices.
                      | AllocNone
                      -- ^ No shard allocation is allowed
                      deriving (Eq, Read, Show, Generic, Typeable)

data ReplicaBounds = ReplicasBounded Int Int
                   | ReplicasLowerBounded Int
                   | ReplicasUnbounded
                   deriving (Eq, Read, Show, Generic, Typeable)

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
newtype Bytes = Bytes Int deriving (Eq, Read, Show, Generic, Typeable, Ord, ToJSON, FromJSON)

gigabytes :: Int -> Bytes
gigabytes n = megabytes (1000 * n)


megabytes :: Int -> Bytes
megabytes n = kilobytes (1000 * n)


kilobytes :: Int -> Bytes
kilobytes n = Bytes (1000 * n)


data FSType = FSSimple
            | FSBuffered deriving (Eq, Read, Show, Generic, Typeable, Ord)

data InitialShardCount = QuorumShards
                       | QuorumMinus1Shards
                       | FullShards
                       | FullMinus1Shards
                       | ExplicitShards Int
                       deriving (Eq, Read, Show, Generic, Typeable)

data NodeAttrFilter = NodeAttrFilter { nodeAttrFilterName   :: NodeAttrName
                                     , nodeAttrFilterValues :: NonEmpty Text}
                                     deriving (Eq, Read, Show, Generic, Ord, Typeable)

newtype NodeAttrName = NodeAttrName Text deriving (Eq, Read, Show, Ord, Generic, Typeable)

data CompoundFormat = CompoundFileFormat Bool
                    | MergeSegmentVsTotalIndex Double
                    -- ^ percentage between 0 and 1 where 0 is false, 1 is true
                    deriving (Eq, Read, Show, Generic, Typeable)

newtype NominalDiffTimeJSON = NominalDiffTimeJSON { ndtJSON ::  NominalDiffTime }

data IndexSettingsSummary = IndexSettingsSummary { sSummaryIndexName     :: IndexName
                                                 , sSummaryFixedSettings :: IndexSettings
                                                 , sSummaryUpdateable    :: [UpdatableIndexSetting]}
                                                 deriving (Eq, Show, Generic, Typeable)

{-| 'Reply' and 'Method' are type synonyms from 'Network.HTTP.Types.Method.Method' -}
type Reply = Network.HTTP.Client.Response L.ByteString
type Method = NHTM.Method

{-| 'OpenCloseIndex' is a sum type for opening and closing indices.

   <http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/indices-open-close.html>
-}
data OpenCloseIndex = OpenIndex | CloseIndex deriving (Eq, Read, Show, Generic, Typeable)

data FieldType = GeoPointType
               | GeoShapeType
               | FloatType
               | IntegerType
               | LongType
               | ShortType
               | ByteType deriving (Eq, Read, Show, Generic, Typeable)

data FieldDefinition =
  FieldDefinition { fieldType :: FieldType } deriving (Eq, Read, Show, Generic, Typeable)

{-| An 'IndexTemplate' defines a template that will automatically be
    applied to new indices created. The templates include both
    'IndexSettings' and mappings, and a simple 'TemplatePattern' that
    controls if the template will be applied to the index created.
    Specify mappings as follows: @[toJSON TweetMapping, ...]@

    https://www.elastic.co/guide/en/elasticsearch/reference/1.7/indices-templates.html
-}
data IndexTemplate =
  IndexTemplate { templatePattern  :: TemplatePattern
                , templateSettings :: Maybe IndexSettings
                , templateMappings :: [Value]
                }

data MappingField =
  MappingField   { mappingFieldName :: FieldName
                 , fieldDefinition  :: FieldDefinition } deriving (Eq, Read, Show, Generic, Typeable)

{-| Support for type reification of 'Mapping's is currently incomplete, for
    now the mapping API verbiage expects a 'ToJSON'able blob.

    Indexes have mappings, mappings are schemas for the documents contained in the
    index. I'd recommend having only one mapping per index, always having a mapping,
    and keeping different kinds of documents separated if possible.
-}
data Mapping = Mapping { typeName      :: TypeName
                       , mappingFields :: [MappingField] } deriving (Eq, Read, Show, Generic, Typeable)

{-| 'BulkOperation' is a sum type for expressing the four kinds of bulk
    operation index, create, delete, and update. 'BulkIndex' behaves like an
    "upsert", 'BulkCreate' will fail if a document already exists at the DocId.

   <http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/docs-bulk.html#docs-bulk>
-}
data BulkOperation =
    BulkIndex  IndexName MappingName DocId Value
  | BulkCreate IndexName MappingName DocId Value
  | BulkDelete IndexName MappingName DocId
  | BulkUpdate IndexName MappingName DocId Value deriving (Eq, Read, Show, Generic, Typeable)

{-| 'EsResult' describes the standard wrapper JSON document that you see in
    successful Elasticsearch lookups or lookups that couldn't find the document.
-}
data EsResult a = EsResult { _index      :: Text
                           , _type       :: Text
                           , _id         :: Text
                           , foundResult :: Maybe (EsResultFound a)} deriving (Eq, Read, Show, Generic, Typeable)

{-| 'EsResultFound' contains the document and its metadata inside of an
    'EsResult' when the document was successfully found.
-}
data EsResultFound a = EsResultFound {  _version :: DocVersion
                                     , _source   :: a } deriving (Eq, Read, Show, Generic, Typeable)

{-| 'EsError' is the generic type that will be returned when there was a
    problem. If you can't parse the expected response, its a good idea to
    try parsing this.
-}
data EsError = EsError { errorStatus  :: Int
                       , errorMessage :: Text } deriving (Eq, Read, Show, Generic, Typeable)

{-| 'EsProtocolException' will be thrown if Bloodhound cannot parse a response
returned by the ElasticSearch server. If you encounter this error, please
verify that your domain data types and FromJSON instances are working properly
(for example, the 'a' of '[Hit a]' in 'SearchResult.searchHits.hits'). If you're
sure that your mappings are correct, then this error may be an indication of an
incompatibility between Bloodhound and ElasticSearch. Please open a bug report
and be sure to include the exception body.
-}
data EsProtocolException = EsProtocolException { esProtoExBody :: L.ByteString }
                                               deriving (Eq, Read, Show, Generic, Typeable)

instance Exception EsProtocolException

data IndexAlias = IndexAlias { srcIndex   :: IndexName
                             , indexAlias :: IndexAliasName } deriving (Eq, Read, Show, Generic, Typeable)

newtype IndexAliasName = IndexAliasName { indexAliasName :: IndexName } deriving (Eq, Read, Show, Generic, ToJSON)

data IndexAliasAction = AddAlias IndexAlias IndexAliasCreate
                      | RemoveAlias IndexAlias deriving (Read, Show, Eq, Generic, Typeable)

data IndexAliasCreate = IndexAliasCreate { aliasCreateRouting :: Maybe AliasRouting
                                         , aliasCreateFilter  :: Maybe Filter}
                                         deriving (Read, Show, Eq, Generic, Typeable)

data AliasRouting = AllAliasRouting RoutingValue
                  | GranularAliasRouting (Maybe SearchAliasRouting) (Maybe IndexAliasRouting)
                  deriving (Read, Show, Eq, Generic, Typeable)

newtype SearchAliasRouting = SearchAliasRouting (NonEmpty RoutingValue) deriving (Read, Show, Eq, Generic, Typeable)

newtype IndexAliasRouting = IndexAliasRouting RoutingValue deriving (Read, Show, Eq, Generic, ToJSON, FromJSON, Typeable)

newtype RoutingValue = RoutingValue { routingValue :: Text } deriving (Read, Show, Eq, Generic, ToJSON, FromJSON, Typeable)

newtype IndexAliasesSummary = IndexAliasesSummary { indexAliasesSummary :: [IndexAliasSummary] } deriving (Read, Show, Eq, Generic, Typeable)

{-| 'IndexAliasSummary' is a summary of an index alias configured for a server. -}
data IndexAliasSummary = IndexAliasSummary { indexAliasSummaryAlias  :: IndexAlias
                                           , indexAliasSummaryCreate :: IndexAliasCreate} deriving (Read, Show, Eq, Generic, Typeable)

{-| 'DocVersion' is an integer version number for a document between 1
and 9.2e+18 used for <<https://www.elastic.co/guide/en/elasticsearch/guide/current/optimistic-concurrency-control.html optimistic concurrency control>>.
-}
newtype DocVersion = DocVersion {
      docVersionNumber :: Int
    } deriving (Eq, Read, Show, Generic, Ord, ToJSON)

-- | Smart constructor for in-range doc version
mkDocVersion :: Int -> Maybe DocVersion
mkDocVersion i
  | i >= (docVersionNumber minBound) && i <= (docVersionNumber maxBound) =
    Just $ DocVersion i
  | otherwise = Nothing


{-| 'ExternalDocVersion' is a convenience wrapper if your code uses its
own version numbers instead of ones from ES.
-}
newtype ExternalDocVersion = ExternalDocVersion DocVersion
    deriving (Eq, Read, Show, Generic, Ord, Bounded, Enum, ToJSON)

{-| 'VersionControl' is specified when indexing documents as a
optimistic concurrency control.
-}
data VersionControl = NoVersionControl
                    -- ^ Don't send a version. This is a pure overwrite.
                    | InternalVersion DocVersion
                    -- ^ Use the default ES versioning scheme. Only
                    -- index the document if the version is the same
                    -- as the one specified. Only applicable to
                    -- updates, as you should be getting Version from
                    -- a search result.
                    | ExternalGT ExternalDocVersion
                    -- ^ Use your own version numbering. Only index
                    -- the document if the version is strictly higher
                    -- OR the document doesn't exist. The given
                    -- version will be used as the new version number
                    -- for the stored document. N.B. All updates must
                    -- increment this number, meaning there is some
                    -- global, external ordering of updates.
                    | ExternalGTE ExternalDocVersion
                    -- ^ Use your own version numbering. Only index
                    -- the document if the version is equal or higher
                    -- than the stored version. Will succeed if there
                    -- is no existing document. The given version will
                    -- be used as the new version number for the
                    -- stored document. Use with care, as this could
                    -- result in data loss.
                    | ForceVersion ExternalDocVersion
                    -- ^ The document will always be indexed and the
                    -- given version will be the new version. This is
                    -- typically used for correcting errors. Use with
                    -- care, as this could result in data loss.
                    deriving (Read, Show, Eq, Generic, Ord)

{-| 'DocumentParent' is used to specify a parent document.
-}
newtype DocumentParent = DocumentParent DocId
  deriving (Eq, Read, Show, Generic, Typeable)

{-| 'IndexDocumentSettings' are special settings supplied when indexing
a document. For the best backwards compatiblity when new fields are
added, you should probably prefer to start with 'defaultIndexDocumentSettings'
-}
data IndexDocumentSettings =
  IndexDocumentSettings { idsVersionControl :: VersionControl
                        , idsParent         :: Maybe DocumentParent
                        } deriving (Eq, Read, Show, Generic, Typeable)

{-| Reasonable default settings. Chooses no version control and no parent.
-}
defaultIndexDocumentSettings :: IndexDocumentSettings
defaultIndexDocumentSettings = IndexDocumentSettings NoVersionControl Nothing

{-| 'Sort' is a synonym for a list of 'SortSpec's. Sort behavior is order
    dependent with later sorts acting as tie-breakers for earlier sorts.
-}
type Sort = [SortSpec]

{-| The two main kinds of 'SortSpec' are 'DefaultSortSpec' and
    'GeoDistanceSortSpec'. The latter takes a 'SortOrder', 'GeoPoint', and
    'DistanceUnit' to express "nearness" to a single geographical point as a
    sort specification.

<http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/search-request-sort.html#search-request-sort>
-}
data SortSpec = DefaultSortSpec DefaultSort
              | GeoDistanceSortSpec SortOrder GeoPoint DistanceUnit deriving (Eq, Read, Show, Generic, Typeable)

{-| 'DefaultSort' is usually the kind of 'SortSpec' you'll want. There's a
    'mkSort' convenience function for when you want to specify only the most
    common parameters.

<http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/search-request-sort.html#search-request-sort>
-}
data DefaultSort =
  DefaultSort { sortFieldName  :: FieldName
              , sortOrder      :: SortOrder
                                  -- default False
              , ignoreUnmapped :: Bool
              , sortMode       :: Maybe SortMode
              , missingSort    :: Maybe Missing
              , nestedFilter   :: Maybe Filter } deriving (Eq, Read, Show, Generic, Typeable)

{-| 'SortOrder' is 'Ascending' or 'Descending', as you might expect. These get
    encoded into "asc" or "desc" when turned into JSON.

<http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/search-request-sort.html#search-request-sort>
-}
data SortOrder = Ascending
               | Descending deriving (Eq, Read, Show, Generic, Typeable)

{-| 'Missing' prescribes how to handle missing fields. A missing field can be
    sorted last, first, or using a custom value as a substitute.

<http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/search-request-sort.html#_missing_values>
-}
data Missing = LastMissing
             | FirstMissing
             | CustomMissing Text deriving (Eq, Read, Show, Generic, Typeable)

{-| 'SortMode' prescribes how to handle sorting array/multi-valued fields.

http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/search-request-sort.html#_sort_mode_option
-}
data SortMode = SortMin
              | SortMax
              | SortSum
              | SortAvg deriving (Eq, Read, Show, Generic, Typeable)

{-| 'mkSort' defaults everything but the 'FieldName' and the 'SortOrder' so
    that you can concisely describe the usual kind of 'SortSpec's you want.
-}
mkSort :: FieldName -> SortOrder -> DefaultSort
mkSort fieldName sOrder = DefaultSort fieldName sOrder False Nothing Nothing Nothing

{-| 'Cache' is for telling ES whether it should cache a 'Filter' not.
    'Query's cannot be cached.
-}
type Cache   = Bool -- caching on/off
defaultCache :: Cache
defaultCache = False

{-| 'PrefixValue' is used in 'PrefixQuery' as the main query component.
-}
type PrefixValue = Text

{-| 'BooleanOperator' is the usual And/Or operators with an ES compatible
    JSON encoding baked in. Used all over the place.
-}
data BooleanOperator = And | Or deriving (Eq, Read, Show, Generic, Typeable)

{-| 'ShardCount' is part of 'IndexSettings'
-}
newtype ShardCount = ShardCount Int deriving (Eq, Read, Show, Generic, ToJSON, Typeable)

{-| 'ReplicaCount' is part of 'IndexSettings'
-}
newtype ReplicaCount = ReplicaCount Int deriving (Eq, Read, Show, Generic, ToJSON, Typeable)

{-| 'IndexName' is used to describe which index to query/create/delete
-}
newtype IndexName = IndexName Text deriving (Eq, Generic, Read, Show, ToJSON, FromJSON, Typeable)

{-| 'IndexSelection' is used for APIs which take a single index, a list of
    indexes, or the special @_all@ index.
-}
--TODO: this does not fully support <https://www.elastic.co/guide/en/elasticsearch/reference/1.7/multi-index.html multi-index syntax>. It wouldn't be too hard to implement but you'd have to add the optional parameters (ignore_unavailable, allow_no_indices, expand_wildcards) to any APIs using it. Also would be a breaking API.
data IndexSelection = IndexList (NonEmpty IndexName)
                    | AllIndexes deriving (Eq, Generic, Show, Typeable)

{-| 'NodeSelection' is used for most cluster APIs. See <https://www.elastic.co/guide/en/elasticsearch/reference/current/cluster.html#cluster-nodes here> for more details.
-}
data NodeSelection = LocalNode
                   -- ^ Whatever node receives this request
                   | NodeList (NonEmpty NodeSelector)
                   | AllNodes deriving (Eq, Generic, Show, Typeable)


-- | An exact match or pattern to identify a node. Note that All of
-- these options support wildcarding, so your node name, server, attr
-- name can all contain * characters to be a fuzzy match.
data NodeSelector = NodeByName NodeName
                  | NodeByFullNodeId FullNodeId
                  | NodeByHost Server
                  -- ^ e.g. 10.0.0.1 or even 10.0.0.*
                  | NodeByAttribute NodeAttrName Text
                  -- ^ NodeAttrName can be a pattern, e.g. rack*. The value can too.
                  deriving (Eq, Generic, Show, Typeable)

{-| 'TemplateName' is used to describe which template to query/create/delete
-}
newtype TemplateName = TemplateName Text deriving (Eq, Read, Show, Generic, ToJSON, FromJSON, Typeable)

{-| 'TemplatePattern' represents a pattern which is matched against index names
-}
newtype TemplatePattern = TemplatePattern Text deriving (Eq, Read, Show, Generic, ToJSON, FromJSON, Typeable)

{-| 'MappingName' is part of mappings which are how ES describes and schematizes
    the data in the indices.
-}
newtype MappingName = MappingName Text deriving (Eq, Generic, Read, Show, ToJSON, FromJSON, Typeable)

{-| 'DocId' is a generic wrapper value for expressing unique Document IDs.
    Can be set by the user or created by ES itself. Often used in client
    functions for poking at specific documents.
-}
newtype DocId = DocId Text deriving (Eq, Generic, Read, Show, ToJSON, FromJSON, Typeable)

{-| 'QueryString' is used to wrap query text bodies, be they human written or not.
-}
newtype QueryString = QueryString Text deriving (Eq, Generic, Read, Show, ToJSON, FromJSON, Typeable)

{-| 'FieldName' is used all over the place wherever a specific field within
     a document needs to be specified, usually in 'Query's or 'Filter's.
-}
newtype FieldName = FieldName Text deriving (Eq, Read, Show, Generic, ToJSON, FromJSON, Typeable)


{-| 'Script' is often used in place of 'FieldName' to specify more
complex ways of extracting a value from a document.
-}
newtype Script = Script { scriptText :: Text } deriving (Eq, Read, Show, Generic, Typeable)

{-| 'CacheName' is used in 'RegexpFilter' for describing the
    'CacheKey' keyed caching behavior.
-}
newtype CacheName = CacheName Text deriving (Eq, Read, Show, Generic, ToJSON, FromJSON, Typeable)

{-| 'CacheKey' is used in 'RegexpFilter' to key regex caching.
-}
newtype CacheKey =
  CacheKey Text deriving (Eq, Read, Show, Generic, ToJSON, FromJSON, Typeable)
newtype Existence =
  Existence Bool deriving (Eq, Read, Show, Generic, ToJSON, FromJSON, Typeable)
newtype NullValue =
  NullValue Bool deriving (Eq, Read, Show, Generic, ToJSON, FromJSON, Typeable)
newtype CutoffFrequency =
  CutoffFrequency Double deriving (Eq, Read, Show, Generic, ToJSON, FromJSON, Typeable)
newtype Analyzer =
  Analyzer Text deriving (Eq, Read, Show, Generic, ToJSON, FromJSON, Typeable)
newtype MaxExpansions =
  MaxExpansions Int deriving (Eq, Read, Show, Generic, ToJSON, FromJSON, Typeable)

{-| 'Lenient', if set to true, will cause format based failures to be
    ignored. I don't know what the bloody default is, Elasticsearch
    documentation didn't say what it was. Let me know if you figure it out.
-}
newtype Lenient =
  Lenient Bool deriving (Eq, Read, Show, Generic, ToJSON, FromJSON, Typeable)
newtype Tiebreaker =
  Tiebreaker Double deriving (Eq, Read, Show, Generic, ToJSON, FromJSON, Typeable)
newtype Boost =
  Boost Double deriving (Eq, Read, Show, Generic, ToJSON, FromJSON, Typeable)
newtype BoostTerms =
  BoostTerms Double deriving (Eq, Read, Show, Generic, ToJSON, FromJSON, Typeable)

{-| 'MinimumMatch' controls how many should clauses in the bool query should
     match. Can be an absolute value (2) or a percentage (30%) or a
     combination of both.
-}
newtype MinimumMatch =
  MinimumMatch Int deriving (Eq, Read, Show, Generic, ToJSON, FromJSON, Typeable)
newtype DisableCoord =
  DisableCoord Bool deriving (Eq, Read, Show, Generic, ToJSON, FromJSON, Typeable)
newtype IgnoreTermFrequency =
  IgnoreTermFrequency Bool deriving (Eq, Read, Show, Generic, ToJSON, FromJSON, Typeable)
newtype MinimumTermFrequency =
  MinimumTermFrequency Int deriving (Eq, Read, Show, Generic, ToJSON, FromJSON, Typeable)
newtype MaxQueryTerms =
  MaxQueryTerms Int deriving (Eq, Read, Show, Generic, ToJSON, FromJSON, Typeable)
newtype Fuzziness =
  Fuzziness Double deriving (Eq, Read, Show, Generic, ToJSON, FromJSON, Typeable)

{-| 'PrefixLength' is the prefix length used in queries, defaults to 0. -}
newtype PrefixLength =
  PrefixLength Int deriving (Eq, Read, Show, Generic, ToJSON, FromJSON, Typeable)
newtype TypeName =
  TypeName Text deriving (Eq, Read, Show, Generic, ToJSON, FromJSON, Typeable)
newtype PercentMatch =
  PercentMatch Double deriving (Eq, Read, Show, Generic, ToJSON, FromJSON, Typeable)
newtype StopWord =
  StopWord Text deriving (Eq, Read, Show, Generic, ToJSON, FromJSON, Typeable)
newtype QueryPath =
  QueryPath Text deriving (Eq, Read, Show, Generic, ToJSON, FromJSON, Typeable)

{-| Allowing a wildcard at the beginning of a word (eg "*ing") is particularly
    heavy, because all terms in the index need to be examined, just in case
    they match. Leading wildcards can be disabled by setting
    'AllowLeadingWildcard' to false. -}
newtype AllowLeadingWildcard =
  AllowLeadingWildcard     Bool deriving (Eq, Read, Show, Generic, ToJSON, FromJSON, Typeable)
newtype LowercaseExpanded =
  LowercaseExpanded        Bool deriving (Eq, Read, Show, Generic, ToJSON, FromJSON, Typeable)
newtype EnablePositionIncrements =
  EnablePositionIncrements Bool deriving (Eq, Read, Show, Generic, ToJSON, FromJSON, Typeable)

{-| By default, wildcard terms in a query are not analyzed.
    Setting 'AnalyzeWildcard' to true enables best-effort analysis.
-}
newtype AnalyzeWildcard = AnalyzeWildcard Bool deriving (Eq, Read, Show, Generic, ToJSON, FromJSON, Typeable)

{-| 'GeneratePhraseQueries' defaults to false.
-}
newtype GeneratePhraseQueries =
  GeneratePhraseQueries Bool deriving (Eq, Read, Show, Generic, ToJSON, FromJSON, Typeable)

{-| 'Locale' is used for string conversions - defaults to ROOT.
-}
newtype Locale        = Locale        Text deriving (Eq, Read, Show, Generic, ToJSON, FromJSON, Typeable)
newtype MaxWordLength = MaxWordLength Int  deriving (Eq, Read, Show, Generic, ToJSON, FromJSON, Typeable)
newtype MinWordLength = MinWordLength Int  deriving (Eq, Read, Show, Generic, ToJSON, FromJSON, Typeable)

{-| 'PhraseSlop' sets the default slop for phrases, 0 means exact
     phrase matches. Default is 0.
-}
newtype PhraseSlop      = PhraseSlop      Int deriving (Eq, Read, Show, Generic, ToJSON, FromJSON, Typeable)
newtype MinDocFrequency = MinDocFrequency Int deriving (Eq, Read, Show, Generic, ToJSON, FromJSON, Typeable)
newtype MaxDocFrequency = MaxDocFrequency Int deriving (Eq, Read, Show, Generic, ToJSON, FromJSON, Typeable)

-- | Newtype wrapper to parse ES's concerning tendency to in some APIs return a floating point number of milliseconds since epoch ಠ_ಠ
newtype POSIXMS = POSIXMS { posixMS :: UTCTime }

{-| 'unpackId' is a silly convenience function that gets used once.
-}
unpackId :: DocId -> Text
unpackId (DocId docId) = docId

type TrackSortScores = Bool
newtype From = From Int deriving (Eq, Read, Show, Generic, ToJSON)
newtype Size = Size Int deriving (Eq, Read, Show, Generic, ToJSON, FromJSON, Typeable)

data Search = Search { queryBody       :: Maybe Query
                     , filterBody      :: Maybe Filter
                     , sortBody        :: Maybe Sort
                     , aggBody         :: Maybe Aggregations
                     , highlight       :: Maybe Highlights
                       -- default False
                     , trackSortScores :: TrackSortScores
                     , from            :: From
                     , size            :: Size
                     , searchType      :: SearchType
                     , fields          :: Maybe [FieldName]
                     , source          :: Maybe Source
                     , suggestBody     :: Maybe Suggest -- ^ Only one Suggestion request / response per Search is supported.
                     } deriving (Eq, Read, Show, Generic, Typeable)

data SearchType = SearchTypeQueryThenFetch
                | SearchTypeDfsQueryThenFetch
                | SearchTypeCount
                | SearchTypeScan
                | SearchTypeQueryAndFetch
                | SearchTypeDfsQueryAndFetch
  deriving (Eq, Read, Show, Generic, Typeable)

data Source =
    NoSource
  | SourcePatterns PatternOrPatterns
  | SourceIncludeExclude Include Exclude
    deriving (Read, Show, Eq, Generic, Typeable)

data PatternOrPatterns = PopPattern   Pattern
                       | PopPatterns [Pattern] deriving (Eq, Read, Show, Generic, Typeable)

data Include = Include [Pattern] deriving (Eq, Read, Show, Generic, Typeable)
data Exclude = Exclude [Pattern] deriving (Eq, Read, Show, Generic, Typeable)

newtype Pattern = Pattern Text deriving (Eq, Read, Show, Generic, Typeable)

data Highlights = Highlights { globalsettings  :: Maybe HighlightSettings
                             , highlightFields :: [FieldHighlight]
                             } deriving (Read, Show, Eq, Generic, Typeable)

data FieldHighlight = FieldHighlight FieldName (Maybe HighlightSettings)
                      deriving (Read, Show, Eq, Generic, Typeable)


data HighlightSettings = Plain PlainHighlight
                       | Postings PostingsHighlight
                       | FastVector FastVectorHighlight
                         deriving (Read, Show, Eq, Generic, Typeable)
data PlainHighlight =
    PlainHighlight { plainCommon  :: Maybe CommonHighlight
                   , plainNonPost :: Maybe NonPostings } deriving (Read, Show, Eq, Generic, Typeable)

 -- This requires that index_options are set to 'offset' in the mapping.
data PostingsHighlight = PostingsHighlight (Maybe CommonHighlight) deriving (Read, Show, Eq, Generic, Typeable)

-- This requires that term_vector is set to 'with_positions_offsets' in the mapping.
data FastVectorHighlight =
    FastVectorHighlight { fvCommon          :: Maybe CommonHighlight
                        , fvNonPostSettings :: Maybe NonPostings
                        , boundaryChars     :: Maybe Text
                        , boundaryMaxScan   :: Maybe Int
                        , fragmentOffset    :: Maybe Int
                        , matchedFields     :: [Text]
                        , phraseLimit       :: Maybe Int
                        } deriving (Read, Show, Eq, Generic, Typeable)

data CommonHighlight =
    CommonHighlight { order             :: Maybe Text
                    , forceSource       :: Maybe Bool
                    , tag               :: Maybe HighlightTag
                    , encoder           :: Maybe HighlightEncoder
                    , noMatchSize       :: Maybe Int
                    , highlightQuery    :: Maybe Query
                    , requireFieldMatch :: Maybe Bool
                    } deriving (Read, Show, Eq, Generic, Typeable)

-- Settings that are only applicable to FastVector and Plain highlighters.
data NonPostings =
    NonPostings { fragmentSize      :: Maybe Int
                , numberOfFragments :: Maybe Int} deriving (Read, Show, Eq, Generic, Typeable)

data HighlightEncoder = DefaultEncoder
                      | HTMLEncoder
                      deriving (Read, Show, Eq, Generic, Typeable)

-- NOTE: Should the tags use some kind of HTML type, rather than Text?
data HighlightTag = TagSchema Text
                  | CustomTags ([Text], [Text]) -- Only uses more than the first value in the lists if fvh
                  deriving (Read, Show, Eq, Generic, Typeable)


data Query =
  TermQuery                     Term (Maybe Boost)
  | TermsQuery                  Text (NonEmpty Text)
  | QueryMatchQuery             MatchQuery
  | QueryMultiMatchQuery        MultiMatchQuery
  | QueryBoolQuery              BoolQuery
  | QueryBoostingQuery          BoostingQuery
  | QueryCommonTermsQuery       CommonTermsQuery
  | ConstantScoreFilter         Filter Boost
  | ConstantScoreQuery          Query Boost
  | QueryDisMaxQuery            DisMaxQuery
  | QueryFilteredQuery          FilteredQuery
  | QueryFuzzyLikeThisQuery     FuzzyLikeThisQuery
  | QueryFuzzyLikeFieldQuery    FuzzyLikeFieldQuery
  | QueryFuzzyQuery             FuzzyQuery
  | QueryHasChildQuery          HasChildQuery
  | QueryHasParentQuery         HasParentQuery
  | IdsQuery                    MappingName [DocId]
  | QueryIndicesQuery           IndicesQuery
  | MatchAllQuery               (Maybe Boost)
  | QueryMoreLikeThisQuery      MoreLikeThisQuery
  | QueryMoreLikeThisFieldQuery MoreLikeThisFieldQuery
  | QueryNestedQuery            NestedQuery
  | QueryPrefixQuery            PrefixQuery
  | QueryQueryStringQuery       QueryStringQuery
  | QuerySimpleQueryStringQuery SimpleQueryStringQuery
  | QueryRangeQuery             RangeQuery
  | QueryRegexpQuery            RegexpQuery
  | QueryTemplateQueryInline    TemplateQueryInline
  deriving (Eq, Read, Show, Generic, Typeable)

data RegexpQuery =
  RegexpQuery { regexpQueryField :: FieldName
              , regexpQuery      :: Regexp
              , regexpQueryFlags :: RegexpFlags
              , regexpQueryBoost :: Maybe Boost
              } deriving (Eq, Read, Show, Generic, Typeable)

data RangeQuery =
  RangeQuery { rangeQueryField :: FieldName
             , rangeQueryRange :: RangeValue
             , rangeQueryBoost :: Boost } deriving (Eq, Read, Show, Generic, Typeable)

mkRangeQuery :: FieldName -> RangeValue -> RangeQuery
mkRangeQuery f r = RangeQuery f r (Boost 1.0)

data SimpleQueryStringQuery =
  SimpleQueryStringQuery
    { simpleQueryStringQuery             :: QueryString
    , simpleQueryStringField             :: Maybe FieldOrFields
    , simpleQueryStringOperator          :: Maybe BooleanOperator
    , simpleQueryStringAnalyzer          :: Maybe Analyzer
    , simpleQueryStringFlags             :: Maybe (NonEmpty SimpleQueryFlag)
    , simpleQueryStringLowercaseExpanded :: Maybe LowercaseExpanded
    , simpleQueryStringLocale            :: Maybe Locale
    } deriving (Eq, Read, Show, Generic, Typeable)

data SimpleQueryFlag =
  SimpleQueryAll
  | SimpleQueryNone
  | SimpleQueryAnd
  | SimpleQueryOr
  | SimpleQueryPrefix
  | SimpleQueryPhrase
  | SimpleQueryPrecedence
  | SimpleQueryEscape
  | SimpleQueryWhitespace
  | SimpleQueryFuzzy
  | SimpleQueryNear
  | SimpleQuerySlop deriving (Eq, Read, Show, Generic, Typeable)

-- use_dis_max and tie_breaker when fields are plural?
data QueryStringQuery =
  QueryStringQuery
  { queryStringQuery                    :: QueryString
  , queryStringDefaultField             :: Maybe FieldName
  , queryStringOperator                 :: Maybe BooleanOperator
  , queryStringAnalyzer                 :: Maybe Analyzer
  , queryStringAllowLeadingWildcard     :: Maybe AllowLeadingWildcard
  , queryStringLowercaseExpanded        :: Maybe LowercaseExpanded
  , queryStringEnablePositionIncrements :: Maybe EnablePositionIncrements
  , queryStringFuzzyMaxExpansions       :: Maybe MaxExpansions
  , queryStringFuzziness                :: Maybe Fuzziness
  , queryStringFuzzyPrefixLength        :: Maybe PrefixLength
  , queryStringPhraseSlop               :: Maybe PhraseSlop
  , queryStringBoost                    :: Maybe Boost
  , queryStringAnalyzeWildcard          :: Maybe AnalyzeWildcard
  , queryStringGeneratePhraseQueries    :: Maybe GeneratePhraseQueries
  , queryStringMinimumShouldMatch       :: Maybe MinimumMatch
  , queryStringLenient                  :: Maybe Lenient
  , queryStringLocale                   :: Maybe Locale
  } deriving (Eq, Read, Show, Generic, Typeable)

mkQueryStringQuery :: QueryString -> QueryStringQuery
mkQueryStringQuery qs =
  QueryStringQuery qs Nothing Nothing
  Nothing Nothing Nothing Nothing
  Nothing Nothing Nothing Nothing
  Nothing Nothing Nothing Nothing
  Nothing Nothing

data FieldOrFields = FofField   FieldName
                   | FofFields (NonEmpty FieldName) deriving (Eq, Read, Show, Generic, Typeable)

data PrefixQuery =
  PrefixQuery
  { prefixQueryField       :: FieldName
  , prefixQueryPrefixValue :: Text
  , prefixQueryBoost       :: Maybe Boost } deriving (Eq, Read, Show, Generic, Typeable)

data NestedQuery =
  NestedQuery
  { nestedQueryPath      :: QueryPath
  , nestedQueryScoreType :: ScoreType
  , nestedQuery          :: Query } deriving (Eq, Read, Show, Generic, Typeable)

data MoreLikeThisFieldQuery =
  MoreLikeThisFieldQuery
  { moreLikeThisFieldText            :: Text
  , moreLikeThisFieldFields          :: FieldName
                                        -- default 0.3 (30%)
  , moreLikeThisFieldPercentMatch    :: Maybe PercentMatch
  , moreLikeThisFieldMinimumTermFreq :: Maybe MinimumTermFrequency
  , moreLikeThisFieldMaxQueryTerms   :: Maybe MaxQueryTerms
  , moreLikeThisFieldStopWords       :: Maybe (NonEmpty StopWord)
  , moreLikeThisFieldMinDocFrequency :: Maybe MinDocFrequency
  , moreLikeThisFieldMaxDocFrequency :: Maybe MaxDocFrequency
  , moreLikeThisFieldMinWordLength   :: Maybe MinWordLength
  , moreLikeThisFieldMaxWordLength   :: Maybe MaxWordLength
  , moreLikeThisFieldBoostTerms      :: Maybe BoostTerms
  , moreLikeThisFieldBoost           :: Maybe Boost
  , moreLikeThisFieldAnalyzer        :: Maybe Analyzer
  } deriving (Eq, Read, Show, Generic, Typeable)

data MoreLikeThisQuery =
  MoreLikeThisQuery
  { moreLikeThisText            :: Text
  , moreLikeThisFields          :: Maybe (NonEmpty FieldName)
    -- default 0.3 (30%)
  , moreLikeThisPercentMatch    :: Maybe PercentMatch
  , moreLikeThisMinimumTermFreq :: Maybe MinimumTermFrequency
  , moreLikeThisMaxQueryTerms   :: Maybe MaxQueryTerms
  , moreLikeThisStopWords       :: Maybe (NonEmpty StopWord)
  , moreLikeThisMinDocFrequency :: Maybe MinDocFrequency
  , moreLikeThisMaxDocFrequency :: Maybe MaxDocFrequency
  , moreLikeThisMinWordLength   :: Maybe MinWordLength
  , moreLikeThisMaxWordLength   :: Maybe MaxWordLength
  , moreLikeThisBoostTerms      :: Maybe BoostTerms
  , moreLikeThisBoost           :: Maybe Boost
  , moreLikeThisAnalyzer        :: Maybe Analyzer
  } deriving (Eq, Read, Show, Generic, Typeable)

data IndicesQuery =
  IndicesQuery
  { indicesQueryIndices :: [IndexName]
  , indicesQuery        :: Query
    -- default "all"
  , indicesQueryNoMatch :: Maybe Query } deriving (Eq, Read, Show, Generic, Typeable)

data HasParentQuery =
  HasParentQuery
  { hasParentQueryType      :: TypeName
  , hasParentQuery          :: Query
  , hasParentQueryScoreType :: Maybe ScoreType } deriving (Eq, Read, Show, Generic, Typeable)

data HasChildQuery =
  HasChildQuery
  { hasChildQueryType      :: TypeName
  , hasChildQuery          :: Query
  , hasChildQueryScoreType :: Maybe ScoreType } deriving (Eq, Read, Show, Generic, Typeable)

data ScoreType =
  ScoreTypeMax
  | ScoreTypeSum
  | ScoreTypeAvg
  | ScoreTypeNone deriving (Eq, Read, Show, Generic, Typeable)

data FuzzyQuery =
  FuzzyQuery { fuzzyQueryField         :: FieldName
             , fuzzyQueryValue         :: Text
             , fuzzyQueryPrefixLength  :: PrefixLength
             , fuzzyQueryMaxExpansions :: MaxExpansions
             , fuzzyQueryFuzziness     :: Fuzziness
             , fuzzyQueryBoost         :: Maybe Boost
             } deriving (Eq, Read, Show, Generic, Typeable)

data FuzzyLikeFieldQuery =
  FuzzyLikeFieldQuery
  { fuzzyLikeField                    :: FieldName
    -- anaphora is good for the soul.
  , fuzzyLikeFieldText                :: Text
  , fuzzyLikeFieldMaxQueryTerms       :: MaxQueryTerms
  , fuzzyLikeFieldIgnoreTermFrequency :: IgnoreTermFrequency
  , fuzzyLikeFieldFuzziness           :: Fuzziness
  , fuzzyLikeFieldPrefixLength        :: PrefixLength
  , fuzzyLikeFieldBoost               :: Boost
  , fuzzyLikeFieldAnalyzer            :: Maybe Analyzer
  } deriving (Eq, Read, Show, Generic, Typeable)

data FuzzyLikeThisQuery =
  FuzzyLikeThisQuery
  { fuzzyLikeFields              :: [FieldName]
  , fuzzyLikeText                :: Text
  , fuzzyLikeMaxQueryTerms       :: MaxQueryTerms
  , fuzzyLikeIgnoreTermFrequency :: IgnoreTermFrequency
  , fuzzyLikeFuzziness           :: Fuzziness
  , fuzzyLikePrefixLength        :: PrefixLength
  , fuzzyLikeBoost               :: Boost
  , fuzzyLikeAnalyzer            :: Maybe Analyzer
  } deriving (Eq, Read, Show, Generic, Typeable)

data FilteredQuery =
  FilteredQuery
  { filteredQuery  :: Query
  , filteredFilter :: Filter } deriving (Eq, Read, Show, Generic, Typeable)

data DisMaxQuery =
  DisMaxQuery { disMaxQueries    :: [Query]
                -- default 0.0
              , disMaxTiebreaker :: Tiebreaker
              , disMaxBoost      :: Maybe Boost
              } deriving (Eq, Read, Show, Generic, Typeable)

data MatchQuery =
  MatchQuery { matchQueryField           :: FieldName
             , matchQueryQueryString     :: QueryString
             , matchQueryOperator        :: BooleanOperator
             , matchQueryZeroTerms       :: ZeroTermsQuery
             , matchQueryCutoffFrequency :: Maybe CutoffFrequency
             , matchQueryMatchType       :: Maybe MatchQueryType
             , matchQueryAnalyzer        :: Maybe Analyzer
             , matchQueryMaxExpansions   :: Maybe MaxExpansions
             , matchQueryLenient         :: Maybe Lenient
             , matchQueryBoost           :: Maybe Boost } deriving (Eq, Read, Show, Generic, Typeable)

{-| 'mkMatchQuery' is a convenience function that defaults the less common parameters,
    enabling you to provide only the 'FieldName' and 'QueryString' to make a 'MatchQuery'
-}
mkMatchQuery :: FieldName -> QueryString -> MatchQuery
mkMatchQuery field query = MatchQuery field query Or ZeroTermsNone Nothing Nothing Nothing Nothing Nothing Nothing

data MatchQueryType =
  MatchPhrase
  | MatchPhrasePrefix deriving (Eq, Read, Show, Generic, Typeable)

data MultiMatchQuery =
  MultiMatchQuery { multiMatchQueryFields          :: [FieldName]
                  , multiMatchQueryString          :: QueryString
                  , multiMatchQueryOperator        :: BooleanOperator
                  , multiMatchQueryZeroTerms       :: ZeroTermsQuery
                  , multiMatchQueryTiebreaker      :: Maybe Tiebreaker
                  , multiMatchQueryType            :: Maybe MultiMatchQueryType
                  , multiMatchQueryCutoffFrequency :: Maybe CutoffFrequency
                  , multiMatchQueryAnalyzer        :: Maybe Analyzer
                  , multiMatchQueryMaxExpansions   :: Maybe MaxExpansions
                  , multiMatchQueryLenient         :: Maybe Lenient } deriving (Eq, Read, Show, Generic, Typeable)

{-| 'mkMultiMatchQuery' is a convenience function that defaults the less common parameters,
    enabling you to provide only the list of 'FieldName's and 'QueryString' to
    make a 'MultiMatchQuery'.
-}

mkMultiMatchQuery :: [FieldName] -> QueryString -> MultiMatchQuery
mkMultiMatchQuery matchFields query =
  MultiMatchQuery matchFields query
  Or ZeroTermsNone Nothing Nothing Nothing Nothing Nothing Nothing

data MultiMatchQueryType =
  MultiMatchBestFields
  | MultiMatchMostFields
  | MultiMatchCrossFields
  | MultiMatchPhrase
  | MultiMatchPhrasePrefix deriving (Eq, Read, Show, Generic, Typeable)

data BoolQuery =
  BoolQuery { boolQueryMustMatch          :: [Query]
            , boolQueryMustNotMatch       :: [Query]
            , boolQueryShouldMatch        :: [Query]
            , boolQueryMinimumShouldMatch :: Maybe MinimumMatch
            , boolQueryBoost              :: Maybe Boost
            , boolQueryDisableCoord       :: Maybe DisableCoord
            } deriving (Eq, Read, Show, Generic, Typeable)

mkBoolQuery :: [Query] -> [Query] -> [Query] -> BoolQuery
mkBoolQuery must mustNot should =
  BoolQuery must mustNot should Nothing Nothing Nothing

data BoostingQuery =
  BoostingQuery { positiveQuery :: Query
                , negativeQuery :: Query
                , negativeBoost :: Boost } deriving (Eq, Read, Show, Generic, Typeable)

data CommonTermsQuery =
  CommonTermsQuery { commonField              :: FieldName
                   , commonQuery              :: QueryString
                   , commonCutoffFrequency    :: CutoffFrequency
                   , commonLowFreqOperator    :: BooleanOperator
                   , commonHighFreqOperator   :: BooleanOperator
                   , commonMinimumShouldMatch :: Maybe CommonMinimumMatch
                   , commonBoost              :: Maybe Boost
                   , commonAnalyzer           :: Maybe Analyzer
                   , commonDisableCoord       :: Maybe DisableCoord
                   } deriving (Eq, Read, Show, Generic, Typeable)

data CommonMinimumMatch =
    CommonMinimumMatchHighLow MinimumMatchHighLow
  | CommonMinimumMatch        MinimumMatch
  deriving (Eq, Read, Show, Generic, Typeable)

data MinimumMatchHighLow =
  MinimumMatchHighLow { lowFreq  :: MinimumMatch
                      , highFreq :: MinimumMatch } deriving (Eq, Read, Show, Generic, Typeable)

data Filter = AndFilter [Filter] Cache
            | OrFilter  [Filter] Cache
            | NotFilter  Filter  Cache
            | IdentityFilter
            | BoolFilter BoolMatch
            | ExistsFilter FieldName -- always cached
            | GeoBoundingBoxFilter GeoBoundingBoxConstraint
            | GeoDistanceFilter GeoPoint Distance DistanceType OptimizeBbox Cache
            | GeoDistanceRangeFilter GeoPoint DistanceRange
            | GeoPolygonFilter FieldName [LatLon]
            | IdsFilter MappingName [DocId]
            | LimitFilter Int
            | MissingFilter FieldName Existence NullValue
            | PrefixFilter  FieldName PrefixValue Cache
            | QueryFilter   Query Cache
            | RangeFilter   FieldName RangeValue RangeExecution Cache
            | RegexpFilter  FieldName Regexp RegexpFlags CacheName Cache CacheKey
            | TermFilter    Term Cache
              deriving (Eq, Read, Show, Generic, Typeable)

data ZeroTermsQuery = ZeroTermsNone
                    | ZeroTermsAll deriving (Eq, Read, Show, Generic, Typeable)

data RangeExecution = RangeExecutionIndex
                    | RangeExecutionFielddata deriving (Eq, Read, Show, Generic, Typeable)

newtype Regexp = Regexp Text deriving (Eq, Read, Show, Generic, Typeable, FromJSON)

data RegexpFlags = AllRegexpFlags
                 | NoRegexpFlags
                 | SomeRegexpFlags (NonEmpty RegexpFlag) deriving (Eq, Read, Show, Generic, Typeable)

data RegexpFlag = AnyString
                | Automaton
                | Complement
                | Empty
                | Intersection
                | Interval deriving (Eq, Read, Show, Generic, Typeable)

newtype LessThan = LessThan Double deriving (Eq, Read, Show, Generic, Typeable)
newtype LessThanEq = LessThanEq Double deriving (Eq, Read, Show, Generic, Typeable)
newtype GreaterThan = GreaterThan Double deriving (Eq, Read, Show, Generic, Typeable)
newtype GreaterThanEq = GreaterThanEq Double deriving (Eq, Read, Show, Generic, Typeable)

newtype LessThanD = LessThanD UTCTime deriving (Eq, Read, Show, Generic, Typeable)
newtype LessThanEqD = LessThanEqD UTCTime deriving (Eq, Read, Show, Generic, Typeable)
newtype GreaterThanD = GreaterThanD UTCTime deriving (Eq, Read, Show, Generic, Typeable)
newtype GreaterThanEqD = GreaterThanEqD UTCTime deriving (Eq, Read, Show, Generic, Typeable)

data RangeValue = RangeDateLte LessThanEqD
                | RangeDateLt LessThanD
                | RangeDateGte GreaterThanEqD
                | RangeDateGt GreaterThanD
                | RangeDateGtLt GreaterThanD LessThanD
                | RangeDateGteLte GreaterThanEqD LessThanEqD
                | RangeDateGteLt GreaterThanEqD LessThanD
                | RangeDateGtLte GreaterThanD LessThanEqD
                | RangeDoubleLte LessThanEq
                | RangeDoubleLt LessThan
                | RangeDoubleGte GreaterThanEq
                | RangeDoubleGt GreaterThan
                | RangeDoubleGtLt GreaterThan LessThan
                | RangeDoubleGteLte GreaterThanEq LessThanEq
                | RangeDoubleGteLt GreaterThanEq LessThan
                | RangeDoubleGtLte GreaterThan LessThanEq
                deriving (Eq, Read, Show, Generic, Typeable)

rangeValueToPair :: RangeValue -> [Pair]
rangeValueToPair rv = case rv of
  RangeDateLte (LessThanEqD t)                       -> ["lte" .= t]
  RangeDateGte (GreaterThanEqD t)                    -> ["gte" .= t]
  RangeDateLt (LessThanD t)                          -> ["lt"  .= t]
  RangeDateGt (GreaterThanD t)                       -> ["gt"  .= t]
  RangeDateGteLte (GreaterThanEqD l) (LessThanEqD g) -> ["gte" .= l, "lte" .= g]
  RangeDateGtLte (GreaterThanD l) (LessThanEqD g)    -> ["gt"  .= l, "lte" .= g]
  RangeDateGteLt (GreaterThanEqD l) (LessThanD g)    -> ["gte" .= l, "lt"  .= g]
  RangeDateGtLt (GreaterThanD l) (LessThanD g)       -> ["gt"  .= l, "lt"  .= g]
  RangeDoubleLte (LessThanEq t)                      -> ["lte" .= t]
  RangeDoubleGte (GreaterThanEq t)                   -> ["gte" .= t]
  RangeDoubleLt (LessThan t)                         -> ["lt"  .= t]
  RangeDoubleGt (GreaterThan t)                      -> ["gt"  .= t]
  RangeDoubleGteLte (GreaterThanEq l) (LessThanEq g) -> ["gte" .= l, "lte" .= g]
  RangeDoubleGtLte (GreaterThan l) (LessThanEq g)    -> ["gt"  .= l, "lte" .= g]
  RangeDoubleGteLt (GreaterThanEq l) (LessThan g)    -> ["gte" .= l, "lt"  .= g]
  RangeDoubleGtLt (GreaterThan l) (LessThan g)       -> ["gt"  .= l, "lt"  .= g]

data Term = Term { termField :: Text
                 , termValue :: Text } deriving (Eq, Read, Show, Generic, Typeable)

data BoolMatch = MustMatch    Term  Cache
               | MustNotMatch Term  Cache
               | ShouldMatch [Term] Cache deriving (Eq, Read, Show, Generic, Typeable)

-- "memory" or "indexed"
data GeoFilterType = GeoFilterMemory
                   | GeoFilterIndexed deriving (Eq, Read, Show, Generic, Typeable)

data LatLon = LatLon { lat :: Double
                     , lon :: Double } deriving (Eq, Read, Show, Generic, Typeable)

data GeoBoundingBox =
  GeoBoundingBox { topLeft     :: LatLon
                 , bottomRight :: LatLon } deriving (Eq, Read, Show, Generic, Typeable)

data GeoBoundingBoxConstraint =
  GeoBoundingBoxConstraint { geoBBField        :: FieldName
                           , constraintBox     :: GeoBoundingBox
                           , bbConstraintcache :: Cache
                           , geoType           :: GeoFilterType
                           } deriving (Eq, Read, Show, Generic, Typeable)

data GeoPoint =
  GeoPoint { geoField :: FieldName
           , latLon   :: LatLon} deriving (Eq, Read, Show, Generic, Typeable)

data DistanceUnit = Miles
                  | Yards
                  | Feet
                  | Inches
                  | Kilometers
                  | Meters
                  | Centimeters
                  | Millimeters
                  | NauticalMiles deriving (Eq, Read, Show, Generic, Typeable)

data DistanceType = Arc
                  | SloppyArc -- doesn't exist <1.0
                  | Plane deriving (Eq, Read, Show, Generic, Typeable)

data OptimizeBbox = OptimizeGeoFilterType GeoFilterType
                  | NoOptimizeBbox deriving (Eq, Read, Show, Generic, Typeable)

data Distance =
  Distance { coefficient :: Double
           , unit        :: DistanceUnit } deriving (Eq, Read, Show, Generic, Typeable)

data DistanceRange =
  DistanceRange { distanceFrom :: Distance
                , distanceTo   :: Distance } deriving (Eq, Read, Show, Generic, Typeable)

type TemplateQueryKey = Text
type TemplateQueryValue = Text

newtype TemplateQueryKeyValuePairs = TemplateQueryKeyValuePairs (HM.HashMap TemplateQueryKey TemplateQueryValue)
  deriving (Eq, Read, Show, Generic, Typeable)

instance ToJSON TemplateQueryKeyValuePairs where
  toJSON (TemplateQueryKeyValuePairs x) = Object $ HM.map toJSON x

instance FromJSON TemplateQueryKeyValuePairs where
  parseJSON (Object o) = pure . TemplateQueryKeyValuePairs $ HM.mapMaybe getValue o
    where getValue (String x) = Just x
          getValue _          = Nothing
  parseJSON _          = fail "error parsing TemplateQueryKeyValuePairs"

data TemplateQueryInline =
  TemplateQueryInline { inline :: Query
                      , params :: TemplateQueryKeyValuePairs
                      }
  deriving (Eq, Read, Show, Generic, Typeable)

instance ToJSON TemplateQueryInline where
  toJSON TemplateQueryInline{..} = object [ "query" .= inline
                                          , "params" .= params
                                          ]

instance FromJSON TemplateQueryInline where
  parseJSON = withObject "TemplateQueryInline" parse
    where parse o = TemplateQueryInline
                    <$> o .: "query"
                    <*> o .: "params"

data SearchResult a =
  SearchResult { took         :: Int
               , timedOut     :: Bool
               , shards       :: ShardResult
               , searchHits   :: SearchHits a
               , aggregations :: Maybe AggregationResults
               , scrollId     :: Maybe ScrollId
               , suggest      :: Maybe NamedSuggestionResponse -- ^ Only one Suggestion request / response per Search is supported.
               }
  deriving (Eq, Read, Show, Generic, Typeable)

newtype ScrollId = ScrollId Text deriving (Eq, Read, Show, Generic, Ord, ToJSON, FromJSON)

type Score = Maybe Double

data SearchHits a =
  SearchHits { hitsTotal :: Int
             , maxScore  :: Score
             , hits      :: [Hit a] } deriving (Eq, Read, Show, Generic, Typeable)

instance Semigroup (SearchHits a) where
  (SearchHits ta ma ha) <> (SearchHits tb mb hb) = SearchHits (ta + tb) (max ma mb) (ha <> hb)

instance Monoid (SearchHits a) where
  mempty = SearchHits 0 Nothing mempty
  mappend = (<>)

data Hit a =
  Hit { hitIndex     :: IndexName
      , hitType      :: MappingName
      , hitDocId     :: DocId
      , hitScore     :: Score
      , hitSource    :: Maybe a
      , hitHighlight :: Maybe HitHighlight } deriving (Eq, Read, Show, Generic, Typeable)

data ShardResult =
  ShardResult { shardTotal       :: Int
              , shardsSuccessful :: Int
              , shardsFailed     :: Int } deriving (Eq, Read, Show, Generic, Typeable)

type HitHighlight = M.Map Text [Text]

showText :: Show a => a -> Text
showText = T.pack . show

readMay :: Read a => String -> Maybe a
readMay s = case reads s of
              (a, ""):_ -> Just a
              _         -> Nothing

parseReadText :: Read a => Text -> Parser a
parseReadText = maybe mzero return . readMay . T.unpack

type Aggregations = M.Map Text Aggregation

emptyAggregations :: Aggregations
emptyAggregations = M.empty

mkAggregations :: Text -> Aggregation -> Aggregations
mkAggregations name aggregation = M.insert name aggregation emptyAggregations

data TermOrder = TermOrder{ termSortField :: Text
                          , termSortOrder :: SortOrder } deriving (Eq, Read, Show, Generic, Typeable)

data TermInclusion = TermInclusion Text
                   | TermPattern Text Text deriving (Eq, Read, Show, Generic, Typeable)

data CollectionMode = BreadthFirst
                    | DepthFirst deriving (Eq, Read, Show, Generic, Typeable)

data ExecutionHint = Ordinals
                   | GlobalOrdinals
                   | GlobalOrdinalsHash
                   | GlobalOrdinalsLowCardinality
                   | Map deriving (Eq, Read, Show, Generic, Typeable)

data TimeInterval = Weeks
                  | Days
                  | Hours
                  | Minutes
                  | Seconds deriving Eq

data Interval = Year
              | Quarter
              | Month
              | Week
              | Day
              | Hour
              | Minute
              | Second
              | FractionalInterval Float TimeInterval deriving (Eq, Read, Show, Generic, Typeable)

data Aggregation = TermsAgg TermsAggregation
                 | CardinalityAgg CardinalityAggregation
                 | DateHistogramAgg DateHistogramAggregation
                 | ValueCountAgg ValueCountAggregation
                 | FilterAgg FilterAggregation
                 | DateRangeAgg DateRangeAggregation
                 | MissingAgg MissingAggregation
                 | TopHitsAgg TopHitsAggregation
  deriving (Eq, Read, Show, Generic, Typeable)

data TopHitsAggregation = TopHitsAggregation
  { taFrom :: Maybe From
  , taSize :: Maybe Size
  , taSort :: Maybe Sort
  } deriving (Eq, Read, Show)

data MissingAggregation = MissingAggregation
  { maField :: Text
  } deriving (Eq, Read, Show, Generic, Typeable)

data TermsAggregation = TermsAggregation { term              :: Either Text Text
                                         , termInclude       :: Maybe TermInclusion
                                         , termExclude       :: Maybe TermInclusion
                                         , termOrder         :: Maybe TermOrder
                                         , termMinDocCount   :: Maybe Int
                                         , termSize          :: Maybe Int
                                         , termShardSize     :: Maybe Int
                                         , termCollectMode   :: Maybe CollectionMode
                                         , termExecutionHint :: Maybe ExecutionHint
                                         , termAggs          :: Maybe Aggregations
                                    } deriving (Eq, Read, Show, Generic, Typeable)

data CardinalityAggregation = CardinalityAggregation { cardinalityField   :: FieldName,
                                                       precisionThreshold :: Maybe Int
                                                     } deriving (Eq, Read, Show, Generic, Typeable)

data DateHistogramAggregation = DateHistogramAggregation { dateField      :: FieldName
                                                         , dateInterval   :: Interval
                                                         , dateFormat     :: Maybe Text
                                                         -- pre and post deprecated in 1.5
                                                         , datePreZone    :: Maybe Text
                                                         , datePostZone   :: Maybe Text
                                                         , datePreOffset  :: Maybe Text
                                                         , datePostOffset :: Maybe Text
                                                         , dateAggs       :: Maybe Aggregations
                                                         } deriving (Eq, Read, Show, Generic, Typeable)


data DateRangeAggregation = DateRangeAggregation { draField  :: FieldName
                                                 , draFormat :: Maybe Text
                                                 , draRanges :: NonEmpty DateRangeAggRange
                                                 } deriving (Eq, Read, Show, Generic, Typeable)

data DateRangeAggRange = DateRangeFrom DateMathExpr
                       | DateRangeTo DateMathExpr
                       | DateRangeFromAndTo DateMathExpr DateMathExpr deriving (Eq, Read, Show, Generic, Typeable)

-- | See <https://www.elastic.co/guide/en/elasticsearch/reference/current/common-options.html#date-math> for more information.
data DateMathExpr = DateMathExpr DateMathAnchor [DateMathModifier] deriving (Eq, Read, Show, Generic, Typeable)


-- | Starting point for a date range. This along with the 'DateMathModifiers' gets you the date ES will start from.
data DateMathAnchor = DMNow
                    | DMDate Day deriving (Eq, Read, Show, Generic, Typeable)

data DateMathModifier = AddTime Int DateMathUnit
                      | SubtractTime Int DateMathUnit
                      | RoundDownTo DateMathUnit deriving (Eq, Read, Show, Generic, Typeable)

data DateMathUnit = DMYear
                  | DMMonth
                  | DMWeek
                  | DMDay
                  | DMHour
                  | DMMinute
                  | DMSecond deriving (Eq, Read, Show, Generic, Typeable)

-- | See <https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-metrics-valuecount-aggregation.html> for more information.
data ValueCountAggregation = FieldValueCount FieldName
                           | ScriptValueCount Script deriving (Eq, Read, Show, Generic, Typeable)

-- | Single-bucket filter aggregations. See <https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-bucket-filter-aggregation.html#search-aggregations-bucket-filter-aggregation> for more information.
data FilterAggregation = FilterAggregation { faFilter :: Filter
                                           , faAggs   :: Maybe Aggregations} deriving (Eq, Read, Show, Generic, Typeable)

mkTermsAggregation :: Text -> TermsAggregation
mkTermsAggregation t = TermsAggregation (Left t) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

mkTermsScriptAggregation :: Text -> TermsAggregation
mkTermsScriptAggregation t = TermsAggregation (Right t) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

mkDateHistogram :: FieldName -> Interval -> DateHistogramAggregation
mkDateHistogram t i = DateHistogramAggregation t i Nothing Nothing Nothing Nothing Nothing Nothing

mkCardinalityAggregation :: FieldName -> CardinalityAggregation
mkCardinalityAggregation t = CardinalityAggregation t Nothing

instance ToJSON Version where
  toJSON Version {..} = object ["number" .= number
                               ,"build_hash" .= build_hash
                               ,"build_timestamp" .= build_timestamp
                               ,"build_snapshot" .= build_snapshot
                               ,"lucene_version" .= lucene_version]

instance FromJSON Version where
  parseJSON = withObject "Version" parse
    where parse o = Version
                    <$> o .: "number"
                    <*> o .: "build_hash"
                    <*> o .: "build_timestamp"
                    <*> o .: "build_snapshot"
                    <*> o .: "lucene_version"

instance ToJSON VersionNumber where
  toJSON = toJSON . Vers.showVersion . versionNumber

instance FromJSON VersionNumber where
  parseJSON = withText "VersionNumber" (parse . T.unpack)
    where
      parse s = case filter (null . snd)(RP.readP_to_S Vers.parseVersion s) of
                  [(v, _)] -> pure (VersionNumber v)
                  [] -> fail ("Invalid version string " ++ s)
                  xs -> fail ("Ambiguous version string " ++ s ++ " (" ++ intercalate ", " (Vers.showVersion . fst <$> xs) ++ ")")

instance ToJSON TermOrder where
  toJSON (TermOrder termSortField termSortOrder) = object [termSortField .= termSortOrder]

instance ToJSON TermInclusion where
  toJSON (TermInclusion x) = toJSON x
  toJSON (TermPattern pattern flags) = omitNulls [ "pattern" .= pattern,
                                                     "flags"   .= flags]

instance ToJSON CollectionMode where
  toJSON BreadthFirst = "breadth_first"
  toJSON DepthFirst   = "depth_first"

instance ToJSON ExecutionHint where
  toJSON Ordinals                     = "ordinals"
  toJSON GlobalOrdinals               = "global_ordinals"
  toJSON GlobalOrdinalsHash           = "global_ordinals_hash"
  toJSON GlobalOrdinalsLowCardinality = "global_ordinals_low_cardinality"
  toJSON Map                          = "map"

instance ToJSON Interval where
  toJSON Year = "year"
  toJSON Quarter = "quarter"
  toJSON Month = "month"
  toJSON Week = "week"
  toJSON Day = "day"
  toJSON Hour = "hour"
  toJSON Minute = "minute"
  toJSON Second = "second"
  toJSON (FractionalInterval fraction interval) = toJSON $ show fraction ++ show interval

instance Show TimeInterval where
  show Weeks   = "w"
  show Days    = "d"
  show Hours   = "h"
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
      f  _  = fail "TimeInterval expected one of w, d, h, m, s"

instance ToJSON Aggregation where
  toJSON (TermsAgg (TermsAggregation term include exclude order minDocCount size shardSize collectMode executionHint termAggs)) =
    omitNulls ["terms" .= omitNulls [ toJSON' term,
                                      "include"        .= include,
                                      "exclude"        .= exclude,
                                      "order"          .= order,
                                      "min_doc_count"  .= minDocCount,
                                      "size"           .= size,
                                      "shard_size"     .= shardSize,
                                      "collect_mode"   .= collectMode,
                                      "execution_hint" .= executionHint
                                    ],
               "aggs"  .= termAggs ]
    where
      toJSON' x = case x of { Left y -> "field" .= y;  Right y -> "script" .= y }

  toJSON (CardinalityAgg (CardinalityAggregation field precisionThreshold)) =
    object ["cardinality" .= omitNulls [ "field"              .= field,
                                         "precisionThreshold" .= precisionThreshold
                                       ]
           ]

  toJSON (DateHistogramAgg (DateHistogramAggregation field interval format preZone postZone preOffset postOffset dateHistoAggs)) =
    omitNulls ["date_histogram" .= omitNulls [ "field"       .= field,
                                               "interval"    .= interval,
                                               "format"      .= format,
                                               "pre_zone"    .= preZone,
                                               "post_zone"   .= postZone,
                                               "pre_offset"  .= preOffset,
                                               "post_offset" .= postOffset
                                             ],
               "aggs"           .= dateHistoAggs ]
  toJSON (ValueCountAgg a) = object ["value_count" .= v]
    where v = case a of
                (FieldValueCount (FieldName n)) -> object ["field" .= n]
                (ScriptValueCount (Script s))   -> object ["script" .= s]
  toJSON (FilterAgg (FilterAggregation filt ags)) =
    omitNulls [ "filter" .= filt
              , "aggs" .= ags]
  toJSON (DateRangeAgg a) = object [ "date_range" .= a
                                   ]
  toJSON (MissingAgg (MissingAggregation{..})) =
    object ["missing" .= object ["field" .= maField]]

  toJSON (TopHitsAgg (TopHitsAggregation mfrom msize msort)) =
    omitNulls ["top_hits" .= omitNulls [ "size" .= msize
                                       , "from" .= mfrom
                                       , "sort" .= msort
                                       ]
              ]

instance ToJSON DateRangeAggregation where
  toJSON DateRangeAggregation {..} =
    omitNulls [ "field" .= draField
              , "format" .= draFormat
              , "ranges" .= toList draRanges
              ]

instance ToJSON DateRangeAggRange where
  toJSON (DateRangeFrom e)        = object [ "from" .= e ]
  toJSON (DateRangeTo e)          = object [ "to" .= e ]
  toJSON (DateRangeFromAndTo f t) = object [ "from" .= f, "to" .= t ]

instance ToJSON DateMathExpr where
  toJSON (DateMathExpr a mods) = String (fmtA a <> mconcat (fmtMod <$> mods))
    where fmtA DMNow         = "now"
          fmtA (DMDate date) = (T.pack $ showGregorian date) <> "||"
          fmtMod (AddTime n u)      = "+" <> showText n <> fmtU u
          fmtMod (SubtractTime n u) = "-" <> showText n <> fmtU u
          fmtMod (RoundDownTo u)    = "/" <> fmtU u
          fmtU DMYear   = "y"
          fmtU DMMonth  = "M"
          fmtU DMWeek   = "w"
          fmtU DMDay    = "d"
          fmtU DMHour   = "h"
          fmtU DMMinute = "m"
          fmtU DMSecond = "s"


type AggregationResults = M.Map Text Value

class BucketAggregation a where
  key :: a -> BucketValue
  docCount :: a -> Int
  aggs :: a -> Maybe AggregationResults


data Bucket a = Bucket { buckets :: [a]} deriving (Read, Show)

data BucketValue = TextValue Text
                 | ScientificValue Scientific
                 | BoolValue Bool deriving (Read, Show)

data MissingResult = MissingResult { missingDocCount :: Int } deriving (Show)

data TopHitResult a = TopHitResult { tarHits :: (SearchHits a)
                                   } deriving Show

data TermsResult = TermsResult { termKey       :: BucketValue
                               , termsDocCount :: Int
                               , termsAggs     :: Maybe AggregationResults } deriving (Read, Show)

data DateHistogramResult = DateHistogramResult { dateKey           :: Int
                                               , dateKeyStr        :: Maybe Text
                                               , dateDocCount      :: Int
                                               , dateHistogramAggs :: Maybe AggregationResults } deriving (Read, Show)

data DateRangeResult = DateRangeResult { dateRangeKey          :: Text
                                       , dateRangeFrom         :: Maybe UTCTime
                                       , dateRangeFromAsString :: Maybe Text
                                       , dateRangeTo           :: Maybe UTCTime
                                       , dateRangeToAsString   :: Maybe Text
                                       , dateRangeDocCount     :: Int
                                       , dateRangeAggs         :: Maybe AggregationResults } deriving (Read, Show, Eq, Generic, Typeable)

toTerms :: Text -> AggregationResults ->  Maybe (Bucket TermsResult)
toTerms = toAggResult

toDateHistogram :: Text -> AggregationResults -> Maybe (Bucket DateHistogramResult)
toDateHistogram = toAggResult

toMissing :: Text -> AggregationResults -> Maybe MissingResult
toMissing = toAggResult

toTopHits :: (FromJSON a) => Text -> AggregationResults -> Maybe (TopHitResult a)
toTopHits = toAggResult

toAggResult :: (FromJSON a) => Text -> AggregationResults -> Maybe a
toAggResult t a = M.lookup t a >>= deserialize
  where deserialize = parseMaybe parseJSON

instance BucketAggregation TermsResult where
  key = termKey
  docCount = termsDocCount
  aggs = termsAggs

instance BucketAggregation DateHistogramResult where
  key = TextValue . showText . dateKey
  docCount = dateDocCount
  aggs = dateHistogramAggs

instance BucketAggregation DateRangeResult where
  key = TextValue . dateRangeKey
  docCount = dateRangeDocCount
  aggs = dateRangeAggs

instance (FromJSON a) => FromJSON (Bucket a) where
  parseJSON (Object v) = Bucket <$>
                         v .: "buckets"
  parseJSON _ = mempty

instance FromJSON BucketValue where
  parseJSON (String t) = return $ TextValue t
  parseJSON (Number s) = return $ ScientificValue s
  parseJSON (Bool b)   = return $ BoolValue b
  parseJSON _          = mempty

instance FromJSON MissingResult where
  parseJSON = withObject "MissingResult" parse
    where parse v = MissingResult <$> v .: "doc_count"

instance FromJSON TermsResult where
  parseJSON (Object v) = TermsResult        <$>
                         v .:   "key"       <*>
                         v .:   "doc_count" <*>
                         (pure $ getNamedSubAgg v ["key", "doc_count"])
  parseJSON _ = mempty

instance FromJSON DateHistogramResult where
  parseJSON (Object v) = DateHistogramResult   <$>
                         v .:  "key"           <*>
                         v .:? "key_as_string" <*>
                         v .:  "doc_count"     <*>
                         (pure $ getNamedSubAgg v [ "key"
                                                  , "doc_count"
                                                  , "key_as_string"
                                                  ]
                         )
  parseJSON _ = mempty

instance FromJSON DateRangeResult where
  parseJSON = withObject "DateRangeResult" parse
    where parse v = DateRangeResult                 <$>
                    v .:  "key"                     <*>
                    (fmap posixMS <$> v .:? "from") <*>
                    v .:? "from_as_string"          <*>
                    (fmap posixMS <$> v .:? "to")   <*>
                    v .:? "to_as_string"            <*>
                    v .:  "doc_count"               <*>
                    (pure $ getNamedSubAgg v [ "key"
                                             , "from"
                                             , "from_as_string"
                                             , "to"
                                             , "to_as_string"
                                             , "doc_count"
                                             ]
                    )

instance FromJSON POSIXMS where
  parseJSON = withScientific "POSIXMS" (return . parse)
    where parse n = let n' = truncate n :: Integer
                    in POSIXMS (posixSecondsToUTCTime (fromInteger (n' `div` 1000)))

instance (FromJSON a) => FromJSON (TopHitResult a) where
  parseJSON (Object v) = TopHitResult <$>
                         v .: "hits"
  parseJSON _          = fail "Failure in FromJSON (TopHitResult a)"

instance Semigroup Filter where
  a <> b = AndFilter [a, b] defaultCache

instance Monoid Filter where
  mempty = IdentityFilter
  mappend = (<>)

instance Seminearring Filter where
  a <||> b = OrFilter [a, b] defaultCache

instance ToJSON Filter where
  toJSON (AndFilter filters cache) =
    object ["and" .=
            object [ "filters" .= fmap toJSON filters
                   , "_cache"  .= cache]]

  toJSON (OrFilter filters cache) =
    object ["or" .=
            object [ "filters" .= fmap toJSON filters
                   , "_cache"  .= cache]]

  toJSON (NotFilter notFilter cache) =
    object ["not" .=
            object ["filter"  .= notFilter
                   , "_cache" .= cache]]

  toJSON (IdentityFilter) =
    object ["match_all" .= object []]

  toJSON (TermFilter (Term termFilterField termFilterValue) cache) =
    object ["term" .= object base]
    where base = [termFilterField .= termFilterValue,
                  "_cache"        .= cache]

  toJSON (ExistsFilter (FieldName fieldName)) =
    object ["exists"  .= object
            ["field"  .= fieldName]]

  toJSON (BoolFilter boolMatch) =
    object ["bool"    .= boolMatch]

  toJSON (GeoBoundingBoxFilter bbConstraint) =
    object ["geo_bounding_box" .= bbConstraint]

  toJSON (GeoDistanceFilter (GeoPoint (FieldName distanceGeoField) geoDistLatLon)
          distance distanceType optimizeBbox cache) =
    object ["geo_distance" .=
            object ["distance" .= distance
                   , "distance_type" .= distanceType
                   , "optimize_bbox" .= optimizeBbox
                   , distanceGeoField .= geoDistLatLon
                   , "_cache" .= cache]]

  toJSON (GeoDistanceRangeFilter (GeoPoint (FieldName gddrField) drLatLon)
          (DistanceRange geoDistRangeDistFrom drDistanceTo)) =
    object ["geo_distance_range" .=
            object ["from" .= geoDistRangeDistFrom
                   , "to"  .= drDistanceTo
                   , gddrField .= drLatLon]]

  toJSON (GeoPolygonFilter (FieldName geoPolygonFilterField) latLons) =
    object ["geo_polygon" .=
            object [geoPolygonFilterField .=
                    object ["points" .= fmap toJSON latLons]]]

  toJSON (IdsFilter (MappingName mappingName) values) =
    object ["ids" .=
            object ["type" .= mappingName
                   , "values" .= fmap unpackId values]]

  toJSON (LimitFilter limit) =
    object ["limit" .= object ["value" .= limit]]

  toJSON (MissingFilter (FieldName fieldName) (Existence existence) (NullValue nullValue)) =
    object ["missing" .=
            object [ "field"      .= fieldName
                   , "existence"  .= existence
                   , "null_value" .= nullValue]]

  toJSON (PrefixFilter (FieldName fieldName) fieldValue cache) =
    object ["prefix" .=
            object [fieldName .= fieldValue
                   , "_cache" .= cache]]

  toJSON (QueryFilter query False) =
    object ["query" .= toJSON query ]
  toJSON (QueryFilter query True) =
    object ["fquery" .=
            object [ "query"  .= toJSON query
                   , "_cache" .= True ]]

  toJSON (RangeFilter (FieldName fieldName) rangeValue rangeExecution cache) =
    object ["range" .=
            object [ fieldName .= object (rangeValueToPair rangeValue)
                   , "execution" .= rangeExecution
                   , "_cache" .= cache]]

  toJSON (RegexpFilter (FieldName fieldName)
          (Regexp regexText) flags (CacheName cacheName) cache (CacheKey cacheKey)) =
    object ["regexp" .=
            object [fieldName .=
                    object ["value"  .= regexText
                           , "flags" .= flags]
                   , "_name"      .= cacheName
                   , "_cache"     .= cache
                   , "_cache_key" .= cacheKey]]

instance FromJSON Filter where
  parseJSON = withObject "Filter" parse
    where parse o = andFilter `taggedWith` "and"
                <|> orFilter `taggedWith` "or"
                <|> notFilter `taggedWith` "not"
                <|> identityFilter `taggedWith` "match_all"
                <|> boolFilter `taggedWith` "bool"
                <|> existsFilter `taggedWith` "exists"
                <|> geoBoundingBoxFilter `taggedWith` "geo_bounding_box"
                <|> geoDistanceFilter `taggedWith` "geo_distance"
                <|> geoDistanceRangeFilter `taggedWith` "geo_distance_range"
                <|> geoPolygonFilter `taggedWith` "geo_polygon"
                <|> idsFilter `taggedWith` "ids"
                <|> limitFilter `taggedWith` "limit"
                <|> missingFilter `taggedWith` "missing"
                <|> prefixFilter `taggedWith` "prefix"
                <|> queryFilter `taggedWith` "query"
                <|> fqueryFilter `taggedWith` "fquery"
                <|> rangeFilter `taggedWith` "range"
                <|> regexpFilter `taggedWith` "regexp"
                <|> termFilter `taggedWith` "term"
            where taggedWith parser k = parser =<< o .: k
          andFilter o = AndFilter <$> o .: "filters"
                                  <*> o .:? "_cache" .!= defaultCache
          orFilter o = OrFilter <$> o .: "filters"
                                <*> o .:? "_cache" .!= defaultCache
          notFilter o = NotFilter <$> o .: "filter"
                                  <*> o .: "_cache" .!= defaultCache
          identityFilter :: Object -> Parser Filter
          identityFilter m
            | HM.null m = pure IdentityFilter
            | otherwise = fail ("Identityfilter expected empty object but got " <> show m)
          boolFilter = pure . BoolFilter
          existsFilter o = ExistsFilter <$> o .: "field"
          geoBoundingBoxFilter = pure . GeoBoundingBoxFilter
          geoDistanceFilter o = do
            case HM.toList (deleteSeveral ["distance", "distance_type", "optimize_bbox", "_cache"] o) of
              [(fn, v)] -> do
                gp <- GeoPoint (FieldName fn) <$> parseJSON v
                GeoDistanceFilter gp <$> o .: "distance"
                                     <*> o .: "distance_type"
                                     <*> o .: "optimize_bbox"
                                     <*> o .:? "_cache" .!= defaultCache
              _ -> fail "Could not find GeoDistanceFilter field name"
          geoDistanceRangeFilter o = do
            case HM.toList (deleteSeveral ["from", "to"] o) of
              [(fn, v)] -> do
                gp <- GeoPoint (FieldName fn) <$> parseJSON v
                rng <- DistanceRange <$> o .: "from" <*> o .: "to"
                return (GeoDistanceRangeFilter gp rng)
              _ -> fail "Could not find GeoDistanceRangeFilter field name"
          geoPolygonFilter = fieldTagged $ \fn o -> GeoPolygonFilter fn <$> o .: "points"
          idsFilter o = IdsFilter <$> o .: "type"
                                  <*> o .: "values"
          limitFilter o = LimitFilter <$> o .: "value"
          missingFilter o = MissingFilter <$> o .: "field"
                                          <*> o .: "existence"
                                          <*> o .: "null_value"
          prefixFilter o = case HM.toList (HM.delete "_cache" o) of
                             [(fn, String v)] -> PrefixFilter (FieldName fn) v <$> o .:? "_cache" .!= defaultCache
                             _ -> fail "Could not parse PrefixFilter"

          queryFilter q = pure (QueryFilter q False)
          fqueryFilter o = QueryFilter <$> o .: "query" <*> pure True
          rangeFilter o = case HM.toList (deleteSeveral ["execution", "_cache"] o) of
                            [(fn, v)] -> RangeFilter (FieldName fn)
                                         <$> parseJSON v
                                         <*> o .: "execution"
                                         <*> o .:? "_cache" .!= defaultCache
                            _ -> fail "Could not find field name for RangeFilter"
          regexpFilter o = case HM.toList (deleteSeveral ["_name", "_cache", "_cache_key"] o) of
                              [(fn, Object o')] -> RegexpFilter (FieldName fn)
                                                   <$> o' .: "value"
                                                   <*> o' .: "flags"
                                                   <*> o .: "_name"
                                                   <*> o .:? "_cache" .!= defaultCache
                                                   <*> o .: "_cache_key"
                              _ -> fail "Could not find field name for RegexpFilter"
          termFilter o = case HM.toList (HM.delete "_cache" o) of
                         [(termField, String termVal)] -> TermFilter (Term termField termVal)
                                                          <$> o .:? "_cache" .!= defaultCache
                         _ -> fail "Could not find term field for TermFilter"

fieldTagged :: Monad m => (FieldName -> Object -> m a) -> Object -> m a
fieldTagged f o = case HM.toList o of
                    [(k, Object o')] -> f (FieldName k) o'
                    _ -> fail "Expected object with 1 field-named key"

-- Try to get an AggregationResults when we don't know the
-- field name. We filter out the known keys to try to minimize the noise.
getNamedSubAgg :: Object -> [Text] -> Maybe AggregationResults
getNamedSubAgg o knownKeys = maggRes
  where unknownKeys = HM.filterWithKey (\k _ -> k `notElem` knownKeys) o
        maggRes
          | HM.null unknownKeys = Nothing
          | otherwise           = Just . M.fromList $ HM.toList unknownKeys

instance ToJSON GeoPoint where
  toJSON (GeoPoint (FieldName geoPointField) geoPointLatLon) =
    object [ geoPointField  .= geoPointLatLon ]


instance ToJSON Query where
  toJSON (TermQuery (Term termQueryField termQueryValue) boost) =
    object [ "term" .=
             object [termQueryField .= object merged]]
    where
      base = [ "value" .= termQueryValue ]
      boosted = maybe [] (return . ("boost" .=)) boost
      merged = mappend base boosted

  toJSON (TermsQuery fieldName terms) =
    object [ "terms" .= object conjoined ]
    where conjoined = [fieldName .= terms]

  toJSON (IdsQuery idsQueryMappingName docIds) =
    object [ "ids" .= object conjoined ]
    where conjoined = [ "type"   .= idsQueryMappingName
                      , "values" .= fmap toJSON docIds ]

  toJSON (QueryQueryStringQuery qQueryStringQuery) =
    object [ "query_string" .= qQueryStringQuery ]

  toJSON (QueryMatchQuery matchQuery) =
    object [ "match" .= matchQuery ]

  toJSON (QueryMultiMatchQuery multiMatchQuery) =
      toJSON multiMatchQuery

  toJSON (QueryBoolQuery boolQuery) =
    object [ "bool" .= boolQuery ]

  toJSON (QueryBoostingQuery boostingQuery) =
    object [ "boosting" .= boostingQuery ]

  toJSON (QueryCommonTermsQuery commonTermsQuery) =
    object [ "common" .= commonTermsQuery ]

  toJSON (ConstantScoreFilter csFilter boost) =
    object ["constant_score" .= object ["filter" .= csFilter
                                       , "boost" .= boost]]

  toJSON (ConstantScoreQuery query boost) =
    object ["constant_score" .= object ["query" .= query
                                       , "boost" .= boost]]

  toJSON (QueryDisMaxQuery disMaxQuery) =
    object [ "dis_max" .= disMaxQuery ]

  toJSON (QueryFilteredQuery qFilteredQuery) =
    object [ "filtered" .= qFilteredQuery ]

  toJSON (QueryFuzzyLikeThisQuery fuzzyQuery) =
    object [ "fuzzy_like_this" .= fuzzyQuery ]

  toJSON (QueryFuzzyLikeFieldQuery fuzzyFieldQuery) =
    object [ "fuzzy_like_this_field" .= fuzzyFieldQuery ]

  toJSON (QueryFuzzyQuery fuzzyQuery) =
    object [ "fuzzy" .= fuzzyQuery ]

  toJSON (QueryHasChildQuery childQuery) =
    object [ "has_child" .= childQuery ]

  toJSON (QueryHasParentQuery parentQuery) =
    object [ "has_parent" .= parentQuery ]

  toJSON (QueryIndicesQuery qIndicesQuery) =
    object [ "indices" .= qIndicesQuery ]

  toJSON (MatchAllQuery boost) =
    object [ "match_all" .= omitNulls [ "boost" .= boost ] ]

  toJSON (QueryMoreLikeThisQuery query) =
    object [ "more_like_this" .= query ]

  toJSON (QueryMoreLikeThisFieldQuery query) =
    object [ "more_like_this_field" .= query ]

  toJSON (QueryNestedQuery query) =
    object [ "nested" .= query ]

  toJSON (QueryPrefixQuery query) =
    object [ "prefix" .= query ]

  toJSON (QueryRangeQuery query) =
    object [ "range"  .= query ]

  toJSON (QueryRegexpQuery query) =
    object [ "regexp" .= query ]

  toJSON (QuerySimpleQueryStringQuery query) =
    object [ "simple_query_string" .= query ]

  toJSON (QueryTemplateQueryInline templateQuery) =
    object [ "template" .= templateQuery ]

instance FromJSON Query where
  parseJSON v = withObject "Query" parse v
    where parse o = termQuery `taggedWith` "term"
                <|> termsQuery `taggedWith` "terms"
                <|> idsQuery `taggedWith` "ids"
                <|> queryQueryStringQuery `taggedWith` "query_string"
                <|> queryMatchQuery `taggedWith` "match"
                <|> queryMultiMatchQuery
                <|> queryBoolQuery `taggedWith` "bool"
                <|> queryBoostingQuery `taggedWith` "boosting"
                <|> queryCommonTermsQuery `taggedWith` "common"
                <|> constantScoreFilter `taggedWith` "constant_score"
                <|> constantScoreQuery `taggedWith` "constant_score"
                <|> queryDisMaxQuery `taggedWith` "dis_max"
                <|> queryFilteredQuery `taggedWith` "filtered"
                <|> queryFuzzyLikeThisQuery `taggedWith` "fuzzy_like_this"
                <|> queryFuzzyLikeFieldQuery `taggedWith` "fuzzy_like_this_field"
                <|> queryFuzzyQuery `taggedWith` "fuzzy"
                <|> queryHasChildQuery `taggedWith` "has_child"
                <|> queryHasParentQuery `taggedWith` "has_parent"
                <|> queryIndicesQuery `taggedWith` "indices"
                <|> matchAllQuery `taggedWith` "match_all"
                <|> queryMoreLikeThisQuery `taggedWith` "more_like_this"
                <|> queryMoreLikeThisFieldQuery `taggedWith` "more_like_this_field"
                <|> queryNestedQuery `taggedWith` "nested"
                <|> queryPrefixQuery `taggedWith` "prefix"
                <|> queryRangeQuery `taggedWith` "range"
                <|> queryRegexpQuery `taggedWith` "regexp"
                <|> querySimpleQueryStringQuery `taggedWith` "simple_query_string"
                <|> queryTemplateQueryInline `taggedWith` "template"
            where taggedWith parser k = parser =<< o .: k
          termQuery = fieldTagged $ \(FieldName fn) o ->
                        TermQuery <$> (Term fn <$> o .: "value") <*> o .:? "boost"
          termsQuery o = case HM.toList o of
                           [(fn, vs)] -> do vals <- parseJSON vs
                                            case vals of
                                              x:xs -> return (TermsQuery fn (x :| xs))
                                              _ -> fail "Expected non empty list of values"
                           _ -> fail "Expected object with 1 field-named key"
          idsQuery o = IdsQuery <$> o .: "type"
                                <*> o .: "values"
          queryQueryStringQuery = pure . QueryQueryStringQuery
          queryMatchQuery = pure . QueryMatchQuery
          queryMultiMatchQuery = QueryMultiMatchQuery <$> parseJSON v
          queryBoolQuery = pure . QueryBoolQuery
          queryBoostingQuery = pure . QueryBoostingQuery
          queryCommonTermsQuery = pure . QueryCommonTermsQuery
          constantScoreFilter o = case HM.lookup "filter" o of
            Just x -> ConstantScoreFilter <$> parseJSON x
                                          <*> o .: "boost"
            _ -> fail "Does not appear to be a ConstantScoreFilter"
          constantScoreQuery o = case HM.lookup "query" o of
            Just x -> ConstantScoreQuery <$> parseJSON x
                                         <*> o .: "boost"
            _ -> fail "Does not appear to be a ConstantScoreQuery"
          queryDisMaxQuery = pure . QueryDisMaxQuery
          queryFilteredQuery = pure . QueryFilteredQuery
          queryFuzzyLikeThisQuery = pure . QueryFuzzyLikeThisQuery
          queryFuzzyLikeFieldQuery = pure . QueryFuzzyLikeFieldQuery
          queryFuzzyQuery = pure . QueryFuzzyQuery
          queryHasChildQuery = pure . QueryHasChildQuery
          queryHasParentQuery = pure . QueryHasParentQuery
          queryIndicesQuery = pure . QueryIndicesQuery
          matchAllQuery o = MatchAllQuery <$> o .:? "boost"
          queryMoreLikeThisQuery = pure . QueryMoreLikeThisQuery
          queryMoreLikeThisFieldQuery = pure . QueryMoreLikeThisFieldQuery
          queryNestedQuery = pure . QueryNestedQuery
          queryPrefixQuery = pure . QueryPrefixQuery
          queryRangeQuery = pure . QueryRangeQuery
          queryRegexpQuery = pure . QueryRegexpQuery
          querySimpleQueryStringQuery = pure . QuerySimpleQueryStringQuery
          queryTemplateQueryInline = pure . QueryTemplateQueryInline


omitNulls :: [(Text, Value)] -> Value
omitNulls = object . filter notNull where
  notNull (_, Null)    = False
  notNull (_, Array a) = (not . V.null) a
  notNull _            = True


instance ToJSON SimpleQueryStringQuery where
  toJSON SimpleQueryStringQuery {..} =
    omitNulls (base ++ maybeAdd)
    where base = [ "query" .= simpleQueryStringQuery ]
          maybeAdd = [ "fields" .= simpleQueryStringField
                     , "default_operator" .= simpleQueryStringOperator
                     , "analyzer" .= simpleQueryStringAnalyzer
                     , "flags" .= simpleQueryStringFlags
                     , "lowercase_expanded_terms" .= simpleQueryStringLowercaseExpanded
                     , "locale" .= simpleQueryStringLocale ]

instance FromJSON SimpleQueryStringQuery where
  parseJSON = withObject "SimpleQueryStringQuery" parse
    where parse o = SimpleQueryStringQuery <$> o .: "query"
                                           <*> o .:? "fields"
                                           <*> o .:? "default_operator"
                                           <*> o .:? "analyzer"
                                           <*> (parseFlags <$> o .:? "flags")
                                           <*> o .:? "lowercase_expanded_terms"
                                           <*> o .:? "locale"
          parseFlags (Just (x:xs)) = Just (x :| xs)
          parseFlags _             = Nothing

instance ToJSON FieldOrFields where
  toJSON (FofField fieldName) =
    toJSON fieldName
  toJSON (FofFields fieldNames) =
    toJSON fieldNames

instance FromJSON FieldOrFields where
  parseJSON v = FofField  <$> parseJSON v
            <|> FofFields <$> (parseNEJSON =<< parseJSON v)

instance ToJSON SimpleQueryFlag where
  toJSON SimpleQueryAll        = "ALL"
  toJSON SimpleQueryNone       = "NONE"
  toJSON SimpleQueryAnd        = "AND"
  toJSON SimpleQueryOr         = "OR"
  toJSON SimpleQueryPrefix     = "PREFIX"
  toJSON SimpleQueryPhrase     = "PHRASE"
  toJSON SimpleQueryPrecedence = "PRECEDENCE"
  toJSON SimpleQueryEscape     = "ESCAPE"
  toJSON SimpleQueryWhitespace = "WHITESPACE"
  toJSON SimpleQueryFuzzy      = "FUZZY"
  toJSON SimpleQueryNear       = "NEAR"
  toJSON SimpleQuerySlop       = "SLOP"

instance FromJSON SimpleQueryFlag where
  parseJSON = withText "SimpleQueryFlag" parse
    where parse "ALL"        = pure SimpleQueryAll
          parse "NONE"       = pure SimpleQueryNone
          parse "AND"        = pure SimpleQueryAnd
          parse "OR"         = pure SimpleQueryOr
          parse "PREFIX"     = pure SimpleQueryPrefix
          parse "PHRASE"     = pure SimpleQueryPhrase
          parse "PRECEDENCE" = pure SimpleQueryPrecedence
          parse "ESCAPE"     = pure SimpleQueryEscape
          parse "WHITESPACE" = pure SimpleQueryWhitespace
          parse "FUZZY"      = pure SimpleQueryFuzzy
          parse "NEAR"       = pure SimpleQueryNear
          parse "SLOP"       = pure SimpleQuerySlop
          parse f            = fail ("Unexpected SimpleQueryFlag: " <> show f)

instance ToJSON RegexpQuery where
  toJSON (RegexpQuery (FieldName rqQueryField)
          (Regexp regexpQueryQuery) rqQueryFlags
          rqQueryBoost) =
   object [ rqQueryField .= omitNulls base ]
   where base = [ "value" .= regexpQueryQuery
                , "flags" .= rqQueryFlags
                , "boost" .= rqQueryBoost ]

instance FromJSON RegexpQuery where
  parseJSON = withObject "RegexpQuery" parse
    where parse = fieldTagged $ \fn o ->
                    RegexpQuery fn
                    <$> o .: "value"
                    <*> o .: "flags"
                    <*> o .:? "boost"

instance ToJSON QueryStringQuery where
  toJSON (QueryStringQuery qsQueryString
          qsDefaultField qsOperator
          qsAnalyzer qsAllowWildcard
          qsLowercaseExpanded  qsEnablePositionIncrements
          qsFuzzyMaxExpansions qsFuzziness
          qsFuzzyPrefixLength qsPhraseSlop
          qsBoost qsAnalyzeWildcard
          qsGeneratePhraseQueries qsMinimumShouldMatch
          qsLenient qsLocale) =
    omitNulls base
    where
      base = [ "query" .= qsQueryString
             , "default_field" .= qsDefaultField
             , "default_operator" .= qsOperator
             , "analyzer" .= qsAnalyzer
             , "allow_leading_wildcard" .= qsAllowWildcard
             , "lowercase_expanded_terms" .= qsLowercaseExpanded
             , "enable_position_increments" .= qsEnablePositionIncrements
             , "fuzzy_max_expansions" .= qsFuzzyMaxExpansions
             , "fuzziness" .= qsFuzziness
             , "fuzzy_prefix_length" .= qsFuzzyPrefixLength
             , "phrase_slop" .= qsPhraseSlop
             , "boost" .= qsBoost
             , "analyze_wildcard" .= qsAnalyzeWildcard
             , "auto_generate_phrase_queries" .= qsGeneratePhraseQueries
             , "minimum_should_match" .= qsMinimumShouldMatch
             , "lenient" .= qsLenient
             , "locale" .= qsLocale ]

instance FromJSON QueryStringQuery where
  parseJSON = withObject "QueryStringQuery" parse
    where parse o = QueryStringQuery
                    <$> o .: "query"
                    <*> o .:? "default_field"
                    <*> o .:? "default_operator"
                    <*> o .:? "analyzer"
                    <*> o .:? "allow_leading_wildcard"
                    <*> o .:? "lowercase_expanded_terms"
                    <*> o .:? "enable_position_increments"
                    <*> o .:? "fuzzy_max_expansions"
                    <*> o .:? "fuzziness"
                    <*> o .:? "fuzzy_prefix_length"
                    <*> o .:? "phrase_slop"
                    <*> o .:? "boost"
                    <*> o .:? "analyze_wildcard"
                    <*> o .:? "auto_generate_phrase_queries"
                    <*> o .:? "minimum_should_match"
                    <*> o .:? "lenient"
                    <*> o .:? "locale"

instance ToJSON RangeQuery where
  toJSON (RangeQuery (FieldName fieldName) range boost) =
    object [ fieldName .= object conjoined ]
    where conjoined = [ "boost" .= boost ] ++ (rangeValueToPair range)

instance FromJSON RangeQuery where
  parseJSON = withObject "RangeQuery" parse
    where parse = fieldTagged $ \fn o ->
                    RangeQuery fn
                    <$> parseJSON (Object o)
                    <*> o .: "boost"

instance FromJSON RangeValue where
  parseJSON = withObject "RangeValue" parse
    where parse o = parseDate o
                <|> parseDouble o
          parseDate o = do lt <- o .:? "lt"
                           lte <- o .:? "lte"
                           gt <- o .:? "gt"
                           gte <- o .:? "gte"
                           case (lt, lte, gt, gte) of
                             (Just a, _, Just b, _) -> return (RangeDateGtLt (GreaterThanD b) (LessThanD a))
                             (Just a, _, _, Just b)-> return (RangeDateGteLt (GreaterThanEqD b) (LessThanD a))
                             (_, Just a, Just b, _)-> return (RangeDateGtLte (GreaterThanD b) (LessThanEqD a))
                             (_, Just a, _, Just b)-> return (RangeDateGteLte (GreaterThanEqD b) (LessThanEqD a))
                             (_, _, Just a, _)-> return (RangeDateGt (GreaterThanD a))
                             (Just a, _, _, _)-> return (RangeDateLt (LessThanD a))
                             (_, _, _, Just a)-> return (RangeDateGte (GreaterThanEqD a))
                             (_, Just a, _, _)-> return (RangeDateLte (LessThanEqD a))
                             (Nothing, Nothing, Nothing, Nothing) -> mzero
          parseDouble o = do lt <- o .:? "lt"
                             lte <- o .:? "lte"
                             gt <- o .:? "gt"
                             gte <- o .:? "gte"
                             case (lt, lte, gt, gte) of
                               (Just a, _, Just b, _) -> return (RangeDoubleGtLt (GreaterThan b) (LessThan a))
                               (Just a, _, _, Just b)-> return (RangeDoubleGteLt (GreaterThanEq b) (LessThan a))
                               (_, Just a, Just b, _)-> return (RangeDoubleGtLte (GreaterThan b) (LessThanEq a))
                               (_, Just a, _, Just b)-> return (RangeDoubleGteLte (GreaterThanEq b) (LessThanEq a))
                               (_, _, Just a, _)-> return (RangeDoubleGt (GreaterThan a))
                               (Just a, _, _, _)-> return (RangeDoubleLt (LessThan a))
                               (_, _, _, Just a)-> return (RangeDoubleGte (GreaterThanEq a))
                               (_, Just a, _, _)-> return (RangeDoubleLte (LessThanEq a))
                               (Nothing, Nothing, Nothing, Nothing) -> mzero

instance ToJSON PrefixQuery where
  toJSON (PrefixQuery (FieldName fieldName) queryValue boost) =
    object [ fieldName .= omitNulls base ]
    where base = [ "value" .= queryValue
                 , "boost" .= boost ]

instance FromJSON PrefixQuery where
  parseJSON = withObject "PrefixQuery" parse
    where parse = fieldTagged $ \fn o ->
                    PrefixQuery fn
                    <$> o .: "value"
                    <*> o .:? "boost"

instance ToJSON NestedQuery where
  toJSON (NestedQuery nqPath nqScoreType nqQuery) =
    object [ "path"       .= nqPath
           , "score_mode" .= nqScoreType
           , "query"      .= nqQuery ]

instance FromJSON NestedQuery where
  parseJSON = withObject "NestedQuery" parse
    where parse o = NestedQuery
                    <$> o .: "path"
                    <*> o .: "score_mode"
                    <*> o .: "query"

instance ToJSON MoreLikeThisFieldQuery where
  toJSON (MoreLikeThisFieldQuery text (FieldName fieldName)
          percent mtf mqt stopwords mindf maxdf
          minwl maxwl boostTerms boost analyzer) =
    object [ fieldName .= omitNulls base ]
    where base = [ "like_text" .= text
                 , "percent_terms_to_match" .= percent
                 , "min_term_freq" .= mtf
                 , "max_query_terms" .= mqt
                 , "stop_words" .= stopwords
                 , "min_doc_freq" .= mindf
                 , "max_doc_freq" .= maxdf
                 , "min_word_length" .= minwl
                 , "max_word_length" .= maxwl
                 , "boost_terms" .= boostTerms
                 , "boost" .= boost
                 , "analyzer" .= analyzer ]

instance FromJSON MoreLikeThisFieldQuery where
  parseJSON = withObject "MoreLikeThisFieldQuery" parse
    where parse = fieldTagged $ \fn o ->
                    MoreLikeThisFieldQuery
                    <$> o .: "like_text"
                    <*> pure fn
                    <*> o .:? "percent_terms_to_match"
                    <*> o .:? "min_term_freq"
                    <*> o .:? "max_query_terms"
                    -- <*> (optionalNE =<< o .:? "stop_words")
                    <*> o .:? "stop_words"
                    <*> o .:? "min_doc_freq"
                    <*> o .:? "max_doc_freq"
                    <*> o .:? "min_word_length"
                    <*> o .:? "max_word_length"
                    <*> o .:? "boost_terms"
                    <*> o .:? "boost"
                    <*> o .:? "analyzer"
          -- optionalNE = maybe (pure Nothing) (fmap Just . parseNEJSON)

instance ToJSON MoreLikeThisQuery where
  toJSON (MoreLikeThisQuery text fields percent
          mtf mqt stopwords mindf maxdf
          minwl maxwl boostTerms boost analyzer) =
    omitNulls base
    where base = [ "like_text" .= text
                 , "fields" .= fields
                 , "percent_terms_to_match" .= percent
                 , "min_term_freq" .= mtf
                 , "max_query_terms" .= mqt
                 , "stop_words" .= stopwords
                 , "min_doc_freq" .= mindf
                 , "max_doc_freq" .= maxdf
                 , "min_word_length" .= minwl
                 , "max_word_length" .= maxwl
                 , "boost_terms" .= boostTerms
                 , "boost" .= boost
                 , "analyzer" .= analyzer ]

instance FromJSON MoreLikeThisQuery where
  parseJSON = withObject "MoreLikeThisQuery" parse
    where parse o = MoreLikeThisQuery
                    <$> o .: "like_text"
                    -- <*> (optionalNE =<< o .:? "fields")
                    <*> o .:? "fields"
                    <*> o .:? "percent_terms_to_match"
                    <*> o .:? "min_term_freq"
                    <*> o .:? "max_query_terms"
                    -- <*> (optionalNE =<< o .:? "stop_words")
                    <*> o .:? "stop_words"
                    <*> o .:? "min_doc_freq"
                    <*> o .:? "max_doc_freq"
                    <*> o .:? "min_word_length"
                    <*> o .:? "max_word_length"
                    <*> o .:? "boost_terms"
                    <*> o .:? "boost"
                    <*> o .:? "analyzer"
          -- optionalNE = maybe (pure Nothing) (fmap Just . parseNEJSON)

instance ToJSON IndicesQuery where
  toJSON (IndicesQuery indices query noMatch) =
    omitNulls [ "indices" .= indices
              , "no_match_query" .= noMatch
              , "query" .= query ]

instance FromJSON IndicesQuery where
  parseJSON = withObject "IndicesQuery" parse
    where parse o = IndicesQuery
                    <$> o .:? "indices" .!= []
                    <*> o .: "query"
                    <*> o .:? "no_match_query"

instance ToJSON HasParentQuery where
  toJSON (HasParentQuery queryType query scoreType) =
    omitNulls [ "parent_type" .= queryType
              , "score_type" .= scoreType
              , "query" .= query ]

instance FromJSON HasParentQuery where
  parseJSON = withObject "HasParentQuery" parse
    where parse o = HasParentQuery
                    <$> o .: "parent_type"
                    <*> o .: "query"
                    <*> o .:? "score_type"

instance ToJSON HasChildQuery where
  toJSON (HasChildQuery queryType query scoreType) =
    omitNulls [ "query" .= query
              , "score_type" .= scoreType
              , "type"  .= queryType ]

instance FromJSON HasChildQuery where
  parseJSON = withObject "HasChildQuery" parse
    where parse o = HasChildQuery
                    <$> o .: "type"
                    <*> o .: "query"
                    <*> o .:? "score_type"

instance ToJSON FuzzyQuery where
  toJSON (FuzzyQuery (FieldName fieldName) queryText
          prefixLength maxEx fuzziness boost) =
    object [ fieldName .= omitNulls base ]
    where base = [ "value"          .= queryText
                 , "fuzziness"      .= fuzziness
                 , "prefix_length"  .= prefixLength
                 , "boost" .= boost
                 , "max_expansions" .= maxEx ]

instance FromJSON FuzzyQuery where
  parseJSON = withObject "FuzzyQuery" parse
    where parse = fieldTagged $ \fn o ->
                    FuzzyQuery fn
                    <$> o .: "value"
                    <*> o .: "prefix_length"
                    <*> o .: "max_expansions"
                    <*> o .: "fuzziness"
                    <*> o .:? "boost"

instance ToJSON FuzzyLikeFieldQuery where
  toJSON (FuzzyLikeFieldQuery (FieldName fieldName)
          fieldText maxTerms ignoreFreq fuzziness prefixLength
          boost analyzer) =
    object [ fieldName .=
             omitNulls [ "like_text"       .= fieldText
                       , "max_query_terms" .= maxTerms
                       , "ignore_tf"       .= ignoreFreq
                       , "fuzziness"       .= fuzziness
                       , "prefix_length"   .= prefixLength
                       , "analyzer" .= analyzer
                       , "boost"           .= boost ]]

instance FromJSON FuzzyLikeFieldQuery where
  parseJSON = withObject "FuzzyLikeFieldQuery" parse
    where parse = fieldTagged $ \fn o ->
                    FuzzyLikeFieldQuery fn
                    <$> o .: "like_text"
                    <*> o .: "max_query_terms"
                    <*> o .: "ignore_tf"
                    <*> o .: "fuzziness"
                    <*> o .: "prefix_length"
                    <*> o .: "boost"
                    <*> o .:? "analyzer"

instance ToJSON FuzzyLikeThisQuery where
  toJSON (FuzzyLikeThisQuery fields text maxTerms
          ignoreFreq fuzziness prefixLength boost analyzer) =
    omitNulls base
    where base = [ "fields"          .= fields
                 , "like_text"       .= text
                 , "max_query_terms" .= maxTerms
                 , "ignore_tf"       .= ignoreFreq
                 , "fuzziness"       .= fuzziness
                 , "prefix_length"   .= prefixLength
                 , "analyzer"        .= analyzer
                 , "boost"           .= boost ]

instance FromJSON FuzzyLikeThisQuery where
  parseJSON = withObject "FuzzyLikeThisQuery" parse
    where parse o = FuzzyLikeThisQuery
                    <$> o .:? "fields" .!= []
                    <*> o .: "like_text"
                    <*> o .: "max_query_terms"
                    <*> o .: "ignore_tf"
                    <*> o .: "fuzziness"
                    <*> o .: "prefix_length"
                    <*> o .: "boost"
                    <*> o .:? "analyzer"

instance ToJSON FilteredQuery where
  toJSON (FilteredQuery query fFilter) =
    object [ "query"  .= query
           , "filter" .= fFilter ]

instance FromJSON FilteredQuery where
  parseJSON = withObject "FilteredQuery" parse
    where parse o = FilteredQuery
                    <$> o .: "query"
                    <*> o .: "filter"

instance ToJSON DisMaxQuery where
  toJSON (DisMaxQuery queries tiebreaker boost) =
    omitNulls base
    where base = [ "queries"     .= queries
                 , "boost"       .= boost
                 , "tie_breaker" .= tiebreaker ]

instance FromJSON DisMaxQuery where
  parseJSON = withObject "DisMaxQuery" parse
    where parse o = DisMaxQuery
                    <$> o .:? "queries" .!= []
                    <*> o .: "tie_breaker"
                    <*> o .:? "boost"

instance ToJSON CommonTermsQuery where
  toJSON (CommonTermsQuery (FieldName fieldName)
          (QueryString query) cf lfo hfo msm
          boost analyzer disableCoord) =
    object [fieldName .= omitNulls base ]
    where base = [ "query"              .= query
                 , "cutoff_frequency"   .= cf
                 , "low_freq_operator"  .= lfo
                 , "minimum_should_match" .= msm
                 , "boost" .= boost
                 , "analyzer" .= analyzer
                 , "disable_coord" .= disableCoord
                 , "high_freq_operator" .= hfo ]

instance FromJSON CommonTermsQuery where
  parseJSON = withObject "CommonTermsQuery" parse
    where parse = fieldTagged $ \fn o ->
                    CommonTermsQuery fn
                    <$> o .: "query"
                    <*> o .: "cutoff_frequency"
                    <*> o .: "low_freq_operator"
                    <*> o .: "high_freq_operator"
                    <*> o .:? "minimum_should_match"
                    <*> o .:? "boost"
                    <*> o .:? "analyzer"
                    <*> o .:? "disable_coord"

instance ToJSON CommonMinimumMatch where
  toJSON (CommonMinimumMatch mm) = toJSON mm
  toJSON (CommonMinimumMatchHighLow (MinimumMatchHighLow lowF highF)) =
    object [ "low_freq"  .= lowF
           , "high_freq" .= highF ]

instance FromJSON CommonMinimumMatch where
  parseJSON v = parseMinimum v
            <|> parseMinimumHighLow v
    where parseMinimum = fmap CommonMinimumMatch . parseJSON
          parseMinimumHighLow = fmap CommonMinimumMatchHighLow . withObject "CommonMinimumMatchHighLow" (\o ->
                                  MinimumMatchHighLow
                                  <$> o .: "low_freq"
                                  <*> o .: "high_freq")


instance ToJSON BoostingQuery where
  toJSON (BoostingQuery bqPositiveQuery bqNegativeQuery bqNegativeBoost) =
    object [ "positive"       .= bqPositiveQuery
           , "negative"       .= bqNegativeQuery
           , "negative_boost" .= bqNegativeBoost ]

instance FromJSON BoostingQuery where
  parseJSON = withObject "BoostingQuery" parse
    where parse o = BoostingQuery
                    <$> o .: "positive"
                    <*> o .: "negative"
                    <*> o .: "negative_boost"

instance ToJSON BoolQuery where
  toJSON (BoolQuery mustM notM shouldM bqMin boost disableCoord) =
    omitNulls base
    where base = [ "must" .= mustM
                 , "must_not" .= notM
                 , "should" .= shouldM
                 , "minimum_should_match" .= bqMin
                 , "boost" .= boost
                 , "disable_coord" .= disableCoord ]

instance FromJSON BoolQuery where
  parseJSON = withObject "BoolQuery" parse
    where parse o = BoolQuery
                    <$> o .:? "must" .!= []
                    <*> o .:? "must_not" .!= []
                    <*> o .:? "should" .!= []
                    <*> o .:? "minimum_should_match"
                    <*> o .:? "boost"
                    <*> o .:? "disable_coord"

instance ToJSON MatchQuery where
  toJSON (MatchQuery (FieldName fieldName)
          (QueryString mqQueryString) booleanOperator
          zeroTermsQuery cutoffFrequency matchQueryType
          analyzer maxExpansions lenient boost) =
    object [ fieldName .= omitNulls base ]
    where base = [ "query" .= mqQueryString
                 , "operator" .= booleanOperator
                 , "zero_terms_query" .= zeroTermsQuery
                 , "cutoff_frequency" .= cutoffFrequency
                 , "type" .= matchQueryType
                 , "analyzer" .= analyzer
                 , "max_expansions" .= maxExpansions
                 , "lenient" .= lenient
                 , "boost" .= boost ]

instance FromJSON MatchQuery where
  parseJSON = withObject "MatchQuery" parse
    where parse = fieldTagged $ \fn o ->
                    MatchQuery fn
                    <$> o .:  "query"
                    <*> o .:  "operator"
                    <*> o .:  "zero_terms_query"
                    <*> o .:? "cutoff_frequency"
                    <*> o .:? "type"
                    <*> o .:? "analyzer"
                    <*> o .:? "max_expansions"
                    <*> o .:? "lenient"
                    <*> o .:? "boost"

instance ToJSON MultiMatchQuery where
  toJSON (MultiMatchQuery fields (QueryString query) boolOp
          ztQ tb mmqt cf analyzer maxEx lenient) =
    object ["multi_match" .= omitNulls base]
    where base = [ "fields" .= fmap toJSON fields
                 , "query" .= query
                 , "operator" .= boolOp
                 , "zero_terms_query" .= ztQ
                 , "tie_breaker" .= tb
                 , "type" .= mmqt
                 , "cutoff_frequency" .= cf
                 , "analyzer" .= analyzer
                 , "max_expansions" .= maxEx
                 , "lenient" .= lenient ]

instance FromJSON MultiMatchQuery where
  parseJSON = withObject "MultiMatchQuery" parse
    where parse raw = do o <- raw .: "multi_match"
                         MultiMatchQuery
                           <$> o .:? "fields" .!= []
                           <*> o .: "query"
                           <*> o .: "operator"
                           <*> o .: "zero_terms_query"
                           <*> o .:? "tie_breaker"
                           <*> o .:? "type"
                           <*> o .:? "cutoff_frequency"
                           <*> o .:? "analyzer"
                           <*> o .:? "max_expansions"
                           <*> o .:? "lenient"

instance ToJSON MultiMatchQueryType where
  toJSON MultiMatchBestFields   = "best_fields"
  toJSON MultiMatchMostFields   = "most_fields"
  toJSON MultiMatchCrossFields  = "cross_fields"
  toJSON MultiMatchPhrase       = "phrase"
  toJSON MultiMatchPhrasePrefix = "phrase_prefix"

instance FromJSON MultiMatchQueryType where
  parseJSON = withText "MultiMatchPhrasePrefix" parse
    where parse "best_fields"   = pure MultiMatchBestFields
          parse "most_fields"   = pure MultiMatchMostFields
          parse "cross_fields"  = pure MultiMatchCrossFields
          parse "phrase"        = pure MultiMatchPhrase
          parse "phrase_prefix" = pure MultiMatchPhrasePrefix
          parse t = fail ("Unexpected MultiMatchPhrasePrefix: " <> show t)

instance ToJSON BooleanOperator where
  toJSON And = String "and"
  toJSON Or  = String "or"

instance FromJSON BooleanOperator where
  parseJSON = withText "BooleanOperator" parse
    where parse "and" = pure And
          parse "or"  = pure Or
          parse o     = fail ("Unexpected BooleanOperator: " <> show o)

instance ToJSON ZeroTermsQuery where
  toJSON ZeroTermsNone = String "none"
  toJSON ZeroTermsAll  = String "all"

instance FromJSON ZeroTermsQuery where
  parseJSON = withText "ZeroTermsQuery" parse
    where parse "none" = pure ZeroTermsNone
          parse "all"  = pure ZeroTermsAll
          parse q      = fail ("Unexpected ZeroTermsQuery: " <> show q)

instance ToJSON MatchQueryType where
  toJSON MatchPhrase       = "phrase"
  toJSON MatchPhrasePrefix = "phrase_prefix"

instance FromJSON MatchQueryType where
  parseJSON = withText "MatchQueryType" parse
    where parse "phrase"        = pure MatchPhrase
          parse "phrase_prefix" = pure MatchPhrasePrefix
          parse t               = fail ("Unexpected MatchQueryType: " <> show t)

instance FromJSON Status where
  parseJSON (Object v) = Status <$>
                         v .:? "ok" <*>
                         (v .:? "status" .!= 200) <*>
                         v .: "name" <*>
                         v .: "version" <*>
                         v .: "tagline"
  parseJSON _          = empty


instance ToJSON IndexSettings where
  toJSON (IndexSettings s r) = object ["settings" .=
                                 object ["index" .=
                                   object ["number_of_shards" .= s, "number_of_replicas" .= r]
                                 ]
                               ]

instance FromJSON IndexSettings where
  parseJSON = withObject "IndexSettings" parse
    where parse o = do s <- o .: "settings"
                       i <- s .: "index"
                       IndexSettings <$> i .: "number_of_shards"
                                     <*> i .: "number_of_replicas"

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
  toJSON (IndexCompoundFormat x) = oPath ("index" :| ["compound_format"]) x
  toJSON (IndexCompoundOnFlush x) = oPath ("index" :| ["compound_on_flush"]) x
  toJSON (WarmerEnabled x) = oPath ("index" :| ["warmer", "enabled"]) x
  toJSON (BlocksReadOnly x) = oPath ("blocks" :| ["read_only"]) x
  toJSON (BlocksRead x) = oPath ("blocks" :| ["read"]) x
  toJSON (BlocksWrite x) = oPath ("blocks" :| ["write"]) x
  toJSON (BlocksMetaData x) = oPath ("blocks" :| ["metadata"]) x

instance FromJSON UpdatableIndexSetting where
  parseJSON = withObject "UpdatableIndexSetting" parse
    where parse o = numberOfReplicas `taggedAt` ["index", "number_of_replicas"]
                <|> autoExpandReplicas `taggedAt` ["index", "auto_expand_replicas"]
                <|> refreshInterval `taggedAt` ["index", "refresh_interval"]
                <|> indexConcurrency `taggedAt` ["index", "concurrency"]
                <|> failOnMergeFailure `taggedAt` ["index", "fail_on_merge_failure"]
                <|> translogFlushThresholdOps `taggedAt` ["index", "translog", "flush_threshold_ops"]
                <|> translogFlushThresholdSize `taggedAt` ["index", "translog", "flush_threshold_size"]
                <|> translogFlushThresholdPeriod `taggedAt` ["index", "translog", "flush_threshold_period"]
                <|> translogDisableFlush `taggedAt` ["index", "translog", "disable_flush"]
                <|> cacheFilterMaxSize `taggedAt` ["index", "cache", "filter", "max_size"]
                <|> cacheFilterExpire `taggedAt` ["index", "cache", "filter", "expire"]
                <|> gatewaySnapshotInterval `taggedAt` ["index", "gateway", "snapshot_interval"]
                <|> routingAllocationInclude `taggedAt` ["index", "routing", "allocation", "include"]
                <|> routingAllocationExclude `taggedAt` ["index", "routing", "allocation", "exclude"]
                <|> routingAllocationRequire `taggedAt` ["index", "routing", "allocation", "require"]
                <|> routingAllocationEnable `taggedAt` ["index", "routing", "allocation", "enable"]
                <|> routingAllocationShardsPerNode `taggedAt` ["index", "routing", "allocation", "total_shards_per_node"]
                <|> recoveryInitialShards `taggedAt` ["index", "recovery", "initial_shards"]
                <|> gcDeletes `taggedAt` ["index", "gc_deletes"]
                <|> ttlDisablePurge `taggedAt` ["index", "ttl", "disable_purge"]
                <|> translogFSType `taggedAt` ["index", "translog", "fs", "type"]
                <|> compoundFormat `taggedAt` ["index", "compound_format"]
                <|> compoundOnFlush `taggedAt` ["index", "compound_on_flush"]
                <|> warmerEnabled `taggedAt` ["index", "warmer", "enabled"]
                <|> blocksReadOnly `taggedAt` ["blocks", "read_only"]
                <|> blocksRead `taggedAt` ["blocks", "read"]
                <|> blocksWrite `taggedAt` ["blocks", "write"]
                <|> blocksMetaData `taggedAt` ["blocks", "metadata"]
            where taggedAt f ks = taggedAt' f (Object o) ks
          taggedAt' f v [] = f =<< (parseJSON v <|> (parseJSON (unStringlyTypeJSON v)))
          taggedAt' f v (k:ks) = withObject "Object" (\o -> do v' <- o .: k
                                                               taggedAt' f v' ks) v
          numberOfReplicas               = pure . NumberOfReplicas
          autoExpandReplicas             = pure . AutoExpandReplicas
          refreshInterval                = pure . RefreshInterval . ndtJSON
          indexConcurrency               = pure . IndexConcurrency
          failOnMergeFailure             = pure . FailOnMergeFailure
          translogFlushThresholdOps      = pure . TranslogFlushThresholdOps
          translogFlushThresholdSize     = pure . TranslogFlushThresholdSize
          translogFlushThresholdPeriod   = pure . TranslogFlushThresholdPeriod . ndtJSON
          translogDisableFlush           = pure . TranslogDisableFlush
          cacheFilterMaxSize             = pure . CacheFilterMaxSize
          cacheFilterExpire              = pure . CacheFilterExpire . fmap ndtJSON
          gatewaySnapshotInterval        = pure . GatewaySnapshotInterval . ndtJSON
          routingAllocationInclude       = fmap RoutingAllocationInclude . parseAttrFilter
          routingAllocationExclude       = fmap RoutingAllocationExclude . parseAttrFilter
          routingAllocationRequire       = fmap RoutingAllocationRequire . parseAttrFilter
          routingAllocationEnable        = pure . RoutingAllocationEnable
          routingAllocationShardsPerNode = pure . RoutingAllocationShardsPerNode
          recoveryInitialShards          = pure . RecoveryInitialShards
          gcDeletes                      = pure . GCDeletes . ndtJSON
          ttlDisablePurge                = pure . TTLDisablePurge
          translogFSType                 = pure . TranslogFSType
          compoundFormat                 = pure . IndexCompoundFormat
          compoundOnFlush                = pure . IndexCompoundOnFlush
          warmerEnabled                  = pure . WarmerEnabled
          blocksReadOnly                 = pure . BlocksReadOnly
          blocksRead                     = pure . BlocksRead
          blocksWrite                    = pure . BlocksWrite
          blocksMetaData                 = pure . BlocksMetaData

instance FromJSON IndexSettingsSummary where
  parseJSON = withObject "IndexSettingsSummary" parse
    where parse o = case HM.toList o of
                      [(ixn, v@(Object o'))] -> IndexSettingsSummary (IndexName ixn)
                                                <$> parseJSON v
                                                <*> (fmap (filter (not . redundant)) . parseSettings =<< o' .: "settings")
                      _ -> fail "Expected single-key object with index name"
          redundant (NumberOfReplicas _) = True
          redundant _                    = False

-- | For some reason in several settings APIs, all leaf values get returned
-- as strings. This function attepmts to recover from this for all
-- non-recursive JSON types. If nothing can be done, the value is left alone.
unStringlyTypeJSON :: Value -> Value
unStringlyTypeJSON (String "true") = Bool True
unStringlyTypeJSON (String "false") = Bool False
unStringlyTypeJSON (String "null") = Null
unStringlyTypeJSON v@(String t) = case readMay (T.unpack t) of
                                  Just n  -> Number n
                                  Nothing -> v
unStringlyTypeJSON v = v


parseSettings :: Object -> Parser [UpdatableIndexSetting]
parseSettings o = do
  o' <- o .: "index"
  -- slice the index object into singleton hashmaps and try to parse each
  parses <- forM (HM.toList o') $ \(k, v) -> do
    -- blocks are now nested into the "index" key, which is not how they're serialized
    let atRoot = Object (HM.singleton k v)
    let atIndex = Object (HM.singleton "index" atRoot)
    optional (parseJSON atRoot <|> parseJSON atIndex)
  return (catMaybes parses)

oPath :: ToJSON a => NonEmpty Text -> a -> Value
oPath (k :| []) v   = object [k .= v]
oPath (k:| (h:t)) v = object [k .= oPath (h :| t) v]

attrFilterJSON :: NonEmpty NodeAttrFilter -> Value
attrFilterJSON fs = object [ n .= T.intercalate "," (toList vs)
                           | NodeAttrFilter (NodeAttrName n) vs <- toList fs]

parseAttrFilter :: Value -> Parser (NonEmpty NodeAttrFilter)
parseAttrFilter = withObject "NonEmpty NodeAttrFilter" parse
  where parse o = case HM.toList o of
                    []   -> fail "Expected non-empty list of NodeAttrFilters"
                    x:xs -> DT.mapM (uncurry parse') (x :| xs)
        parse' n = withText "Text" $ \t ->
          case T.splitOn "," t of
            fv:fvs -> return (NodeAttrFilter (NodeAttrName n) (fv :| fvs))
            []     -> fail "Expected non-empty list of filter values"

instance ToJSON ReplicaBounds where
  toJSON (ReplicasBounded a b)    = String (showText a <> "-" <> showText b)
  toJSON (ReplicasLowerBounded a) = String (showText a <> "-all")
  toJSON ReplicasUnbounded        = Bool False

instance FromJSON ReplicaBounds where
  parseJSON v = withText "ReplicaBounds" parseText v
            <|> withBool "ReplicaBounds" parseBool v
    where parseText t = case T.splitOn "-" t of
                          [a, "all"] -> ReplicasLowerBounded <$> parseReadText a
                          [a, b] -> ReplicasBounded <$> parseReadText a
                                                    <*> parseReadText b
                          _ -> fail ("Could not parse ReplicaBounds: " <> show t)
          parseBool False = pure ReplicasUnbounded
          parseBool _ = fail "ReplicasUnbounded cannot be represented with True"

instance ToJSON AllocationPolicy where
  toJSON AllocAll          = String "all"
  toJSON AllocPrimaries    = String "primaries"
  toJSON AllocNewPrimaries = String "new_primaries"
  toJSON AllocNone         = String "none"

instance FromJSON AllocationPolicy where
  parseJSON = withText "AllocationPolicy" parse
    where parse "all" = pure AllocAll
          parse "primaries" = pure AllocPrimaries
          parse "new_primaries" = pure AllocNewPrimaries
          parse "none" = pure AllocNone
          parse t = fail ("Invlaid AllocationPolicy: " <> show t)

instance ToJSON InitialShardCount where
  toJSON QuorumShards       = String "quorum"
  toJSON QuorumMinus1Shards = String "quorum-1"
  toJSON FullShards         = String "full"
  toJSON FullMinus1Shards   = String "full-1"
  toJSON (ExplicitShards x) = toJSON x

instance FromJSON InitialShardCount where
  parseJSON v = withText "InitialShardCount" parseText v
            <|> ExplicitShards <$> parseJSON v
    where parseText "quorum"   = pure QuorumShards
          parseText "quorum-1" = pure QuorumMinus1Shards
          parseText "full"     = pure FullShards
          parseText "full-1"   = pure FullMinus1Shards
          parseText _          = mzero

instance ToJSON FSType where
  toJSON FSSimple   = "simple"
  toJSON FSBuffered = "buffered"

instance FromJSON FSType where
  parseJSON = withText "FSType" parse
    where parse "simple"   = pure FSSimple
          parse "buffered" = pure FSBuffered
          parse t          = fail ("Invalid FSType: " <> show t)

instance ToJSON CompoundFormat where
  toJSON (CompoundFileFormat x)       = Bool x
  toJSON (MergeSegmentVsTotalIndex x) = toJSON x

instance FromJSON CompoundFormat where
  parseJSON v = CompoundFileFormat <$> parseJSON v
            <|> MergeSegmentVsTotalIndex <$> parseJSON v

instance ToJSON NominalDiffTimeJSON where
  toJSON (NominalDiffTimeJSON t) = String (showText (round t :: Integer) <> "s")

instance FromJSON NominalDiffTimeJSON where
  parseJSON = withText "NominalDiffTime" parse
    where parse t = case T.takeEnd 1 t of
                      "s" -> NominalDiffTimeJSON . fromInteger <$> parseReadText (T.dropEnd 1 t)
                      _ -> fail "Invalid or missing NominalDiffTime unit (expected s)"

instance ToJSON IndexTemplate where
  toJSON (IndexTemplate p s m) = merge
    (object [ "template" .= p
            , "mappings" .= foldl' merge (object []) m
            ])
    (toJSON s)
   where
     merge (Object o1) (Object o2) = toJSON $ HM.union o1 o2
     merge o           Null        = o
     merge _           _           = undefined

instance (FromJSON a) => FromJSON (EsResult a) where
  parseJSON jsonVal@(Object v) = do
    found <- v .:? "found" .!= False
    fr <- if found
             then parseJSON jsonVal
             else return Nothing
    EsResult <$> v .:  "_index"   <*>
                 v .:  "_type"    <*>
                 v .:  "_id"      <*>
                 pure fr
  parseJSON _          = empty

instance (FromJSON a) => FromJSON (EsResultFound a) where
  parseJSON (Object v) = EsResultFound <$>
                         v .: "_version" <*>
                         v .: "_source"
  parseJSON _          = empty

instance FromJSON EsError where
  parseJSON (Object v) = EsError <$>
                         v .: "status" <*>
                         (v .: "error" <|> (v .: "error" >>= (.: "reason")))
  parseJSON _ = empty

instance FromJSON IndexAliasesSummary where
  parseJSON = withObject "IndexAliasesSummary" parse
    where parse o = IndexAliasesSummary . mconcat <$> mapM (uncurry go) (HM.toList o)
          go ixn = withObject "index aliases" $ \ia -> do
                     aliases <- ia .:? "aliases" .!= mempty
                     forM (HM.toList aliases) $ \(aName, v) -> do
                       let indexAlias = IndexAlias (IndexName ixn) (IndexAliasName (IndexName aName))
                       IndexAliasSummary indexAlias <$> parseJSON v


instance ToJSON IndexAliasAction where
  toJSON (AddAlias ia opts) = object ["add" .= (iaObj <> optsObj)]
    where Object iaObj = toJSON ia
          Object optsObj = toJSON opts
  toJSON (RemoveAlias ia) = object ["remove" .= iaObj]
    where Object iaObj = toJSON ia

instance ToJSON IndexAlias where
  toJSON IndexAlias {..} = object ["index" .= srcIndex
                                  , "alias" .= indexAlias
                                  ]

instance ToJSON IndexAliasCreate where
  toJSON IndexAliasCreate {..} = Object (filterObj <> routingObj)
    where filterObj = maybe mempty (HM.singleton "filter" . toJSON) aliasCreateFilter
          Object routingObj = maybe (Object mempty) toJSON aliasCreateRouting

instance ToJSON AliasRouting where
  toJSON (AllAliasRouting v) = object ["routing" .= v]
  toJSON (GranularAliasRouting srch idx) = object (catMaybes prs)
    where prs = [("search_routing" .=) <$> srch
                ,("index_routing" .=) <$> idx]

instance FromJSON AliasRouting where
  parseJSON = withObject "AliasRouting" parse
    where parse o = parseAll o <|> parseGranular o
          parseAll o = AllAliasRouting <$> o .: "routing"
          parseGranular o = do
            sr <- o .:? "search_routing"
            ir <- o .:? "index_routing"
            if isNothing sr && isNothing ir
               then fail "Both search_routing and index_routing can't be blank"
               else return (GranularAliasRouting sr ir)

instance FromJSON IndexAliasCreate where
  parseJSON v = withObject "IndexAliasCreate" parse v
    where parse o = IndexAliasCreate <$> optional (parseJSON v)
                                     <*> o .:? "filter"

instance ToJSON SearchAliasRouting where
  toJSON (SearchAliasRouting rvs) = toJSON (T.intercalate "," (routingValue <$> toList rvs))

instance FromJSON SearchAliasRouting where
  parseJSON = withText "SearchAliasRouting" parse
    where parse t = SearchAliasRouting <$> parseNEJSON (String <$> T.splitOn "," t)

instance ToJSON Search where
  toJSON (Search query sFilter sort searchAggs highlight sTrackSortScores sFrom sSize _ sFields sSource sSuggest) =
    omitNulls [ "query"        .= query
              , "filter"       .= sFilter
              , "sort"         .= sort
              , "aggregations" .= searchAggs
              , "highlight"    .= highlight
              , "from"         .= sFrom
              , "size"         .= sSize
              , "track_scores" .= sTrackSortScores
              , "fields"       .= sFields
              , "_source"      .= sSource
              , "suggest"      .= sSuggest]


instance ToJSON Source where
    toJSON NoSource                         = toJSON False
    toJSON (SourcePatterns patterns)        = toJSON patterns
    toJSON (SourceIncludeExclude incl excl) = object [ "include" .= incl, "exclude" .= excl ]

instance ToJSON PatternOrPatterns where
  toJSON (PopPattern pattern)   = toJSON pattern
  toJSON (PopPatterns patterns) = toJSON patterns

instance ToJSON Include where
  toJSON (Include patterns) = toJSON patterns

instance ToJSON Exclude where
  toJSON (Exclude patterns) = toJSON patterns

instance ToJSON Pattern where
  toJSON (Pattern pattern) = toJSON pattern


instance ToJSON FieldHighlight where
    toJSON (FieldHighlight (FieldName fName) (Just fSettings)) =
        object [ fName .= fSettings ]
    toJSON (FieldHighlight (FieldName fName) Nothing) =
        object [ fName .= emptyObject ]

instance ToJSON Highlights where
    toJSON (Highlights global fields) =
        omitNulls (("fields" .= fields)
                  : highlightSettingsPairs global)

instance ToJSON HighlightSettings where
    toJSON hs = omitNulls (highlightSettingsPairs (Just hs))

highlightSettingsPairs :: Maybe HighlightSettings -> [Pair]
highlightSettingsPairs Nothing = []
highlightSettingsPairs (Just (Plain plh)) = plainHighPairs (Just plh)
highlightSettingsPairs (Just (Postings ph)) = postHighPairs (Just ph)
highlightSettingsPairs (Just (FastVector fvh)) = fastVectorHighPairs (Just fvh)


plainHighPairs :: Maybe PlainHighlight -> [Pair]
plainHighPairs Nothing = []
plainHighPairs (Just (PlainHighlight plCom plNonPost)) =
    [ "type" .= String "plain"]
    ++ commonHighlightPairs plCom
    ++ nonPostingsToPairs plNonPost

postHighPairs :: Maybe PostingsHighlight -> [Pair]
postHighPairs Nothing = []
postHighPairs (Just (PostingsHighlight pCom)) =
    ("type" .= String "postings")
    : commonHighlightPairs pCom

fastVectorHighPairs :: Maybe FastVectorHighlight -> [Pair]
fastVectorHighPairs Nothing = []
fastVectorHighPairs (Just
                     (FastVectorHighlight fvCom fvNonPostSettings fvBoundChars
                                          fvBoundMaxScan fvFragOff fvMatchedFields
                                          fvPhraseLim)) =
                        [ "type" .= String "fvh"
                        , "boundary_chars" .= fvBoundChars
                        , "boundary_max_scan" .= fvBoundMaxScan
                        , "fragment_offset" .= fvFragOff
                        , "matched_fields" .= fvMatchedFields
                        , "phraseLimit" .= fvPhraseLim]
                        ++ commonHighlightPairs fvCom
                        ++ nonPostingsToPairs fvNonPostSettings

deleteSeveral :: (Eq k, Hashable k) => [k] -> HM.HashMap k v -> HM.HashMap k v
deleteSeveral ks hm = foldr HM.delete hm ks

commonHighlightPairs :: Maybe CommonHighlight -> [Pair]
commonHighlightPairs Nothing = []
commonHighlightPairs (Just (CommonHighlight chScore chForceSource chTag chEncoder
                                      chNoMatchSize chHighlightQuery
                                      chRequireFieldMatch)) =
    [ "order" .= chScore
    , "force_source" .= chForceSource
    , "encoder" .= chEncoder
    , "no_match_size" .= chNoMatchSize
    , "highlight_query" .= chHighlightQuery
    , "require_fieldMatch" .= chRequireFieldMatch]
    ++ highlightTagToPairs chTag


nonPostingsToPairs :: Maybe NonPostings -> [Pair]
nonPostingsToPairs Nothing = []
nonPostingsToPairs (Just (NonPostings npFragSize npNumOfFrags)) =
    [ "fragment_size" .= npFragSize
    , "number_of_fragments" .= npNumOfFrags]

parseNEJSON :: (FromJSON a) => [Value] -> Parser (NonEmpty a)
parseNEJSON []     = fail "Expected non-empty list"
parseNEJSON (x:xs) = DT.mapM parseJSON (x :| xs)


instance ToJSON HighlightEncoder where
    toJSON DefaultEncoder = String "default"
    toJSON HTMLEncoder    = String "html"

highlightTagToPairs :: Maybe HighlightTag -> [Pair]
highlightTagToPairs (Just (TagSchema _))            = [ "scheme"    .=  String "default"]
highlightTagToPairs (Just (CustomTags (pre, post))) = [ "pre_tags"  .= pre
                                                      , "post_tags" .= post]
highlightTagToPairs Nothing = []

instance ToJSON SortSpec where
  toJSON (DefaultSortSpec
          (DefaultSort (FieldName dsSortFieldName) dsSortOrder dsIgnoreUnmapped
           dsSortMode dsMissingSort dsNestedFilter)) =
    object [dsSortFieldName .= omitNulls base] where
      base = [ "order" .= dsSortOrder
             , "ignore_unmapped" .= dsIgnoreUnmapped
             , "mode" .= dsSortMode
             , "missing" .= dsMissingSort
             , "nested_filter" .= dsNestedFilter ]

  toJSON (GeoDistanceSortSpec gdsSortOrder (GeoPoint (FieldName field) gdsLatLon) units) =
    object [ "unit" .= units
           , field .= gdsLatLon
           , "order" .= gdsSortOrder ]


instance ToJSON SortOrder where
  toJSON Ascending  = String "asc"
  toJSON Descending = String "desc"


instance ToJSON SortMode where
  toJSON SortMin = String "min"
  toJSON SortMax = String "max"
  toJSON SortSum = String "sum"
  toJSON SortAvg = String "avg"


instance ToJSON Missing where
  toJSON LastMissing         = String "_last"
  toJSON FirstMissing        = String "_first"
  toJSON (CustomMissing txt) = String txt


instance ToJSON ScoreType where
  toJSON ScoreTypeMax  = "max"
  toJSON ScoreTypeAvg  = "avg"
  toJSON ScoreTypeSum  = "sum"
  toJSON ScoreTypeNone = "none"

instance FromJSON ScoreType where
  parseJSON = withText "ScoreType" parse
    where parse "max"  = pure ScoreTypeMax
          parse "avg"  = pure ScoreTypeAvg
          parse "sum"  = pure ScoreTypeSum
          parse "none" = pure ScoreTypeNone
          parse t      = fail ("Unexpected ScoreType: " <> show t)

instance ToJSON Distance where
  toJSON (Distance dCoefficient dUnit) =
    String boltedTogether where
      coefText = showText dCoefficient
      (String unitText) = toJSON dUnit
      boltedTogether = mappend coefText unitText

instance FromJSON Distance where
  parseJSON = withText "Distance" parse
    where parse t = Distance <$> parseCoeff nT
                             <*> parseJSON (String unitT)
            where (nT, unitT) = T.span validForNumber t
                  -- may be a better way to do this
                  validForNumber '-' = True
                  validForNumber '.' = True
                  validForNumber 'e' = True
                  validForNumber c   = isNumber c
                  parseCoeff "" = fail "Empty string cannot be parsed as number"
                  parseCoeff s = return (read (T.unpack s))

instance ToJSON DistanceUnit where
  toJSON Miles         = String "mi"
  toJSON Yards         = String "yd"
  toJSON Feet          = String "ft"
  toJSON Inches        = String "in"
  toJSON Kilometers    = String "km"
  toJSON Meters        = String "m"
  toJSON Centimeters   = String "cm"
  toJSON Millimeters   = String "mm"
  toJSON NauticalMiles = String "nmi"


instance FromJSON DistanceUnit where
  parseJSON = withText "DistanceUnit" parse
    where parse "mi"  = pure Miles
          parse "yd"  = pure Yards
          parse "ft"  = pure Feet
          parse "in"  = pure Inches
          parse "km"  = pure Kilometers
          parse "m"   = pure Meters
          parse "cm"  = pure Centimeters
          parse "mm"  = pure Millimeters
          parse "nmi" = pure NauticalMiles
          parse u     = fail ("Unrecognized DistanceUnit: " <> show u)

instance ToJSON DistanceType where
  toJSON Arc       = String "arc"
  toJSON SloppyArc = String "sloppy_arc"
  toJSON Plane     = String "plane"

instance FromJSON DistanceType where
  parseJSON = withText "DistanceType" parse
    where parse "arc"        = pure Arc
          parse "sloppy_arc" = pure SloppyArc
          parse "plane"      = pure Plane
          parse t            = fail ("Unrecognized DistanceType: " <> show t)


instance ToJSON OptimizeBbox where
  toJSON NoOptimizeBbox              = String "none"
  toJSON (OptimizeGeoFilterType gft) = toJSON gft

instance FromJSON OptimizeBbox where
  parseJSON v = withText "NoOptimizeBbox" parseNoOptimize v
            <|> parseOptimize v
    where parseNoOptimize "none" = pure NoOptimizeBbox
          parseNoOptimize _      = mzero
          parseOptimize = fmap OptimizeGeoFilterType . parseJSON

instance ToJSON GeoBoundingBoxConstraint where
  toJSON (GeoBoundingBoxConstraint
          (FieldName gbbcGeoBBField) gbbcConstraintBox cache type') =
    object [gbbcGeoBBField .= gbbcConstraintBox
           , "_cache"  .= cache
           , "type" .= type']

instance FromJSON GeoBoundingBoxConstraint where
  parseJSON = withObject "GeoBoundingBoxConstraint" parse
    where parse o = case HM.toList (deleteSeveral ["type", "_cache"] o) of
                      [(fn, v)] -> GeoBoundingBoxConstraint (FieldName fn)
                                   <$> parseJSON v
                                   <*> o .:? "_cache" .!= defaultCache
                                   <*> o .: "type"
                      _ -> fail "Could not find field name for GeoBoundingBoxConstraint"

instance ToJSON GeoFilterType where
  toJSON GeoFilterMemory  = String "memory"
  toJSON GeoFilterIndexed = String "indexed"

instance FromJSON GeoFilterType where
  parseJSON = withText "GeoFilterType" parse
    where parse "memory"  = pure GeoFilterMemory
          parse "indexed" = pure GeoFilterIndexed
          parse t         = fail ("Unrecognized GeoFilterType: " <> show t)

instance ToJSON GeoBoundingBox where
  toJSON (GeoBoundingBox gbbTopLeft gbbBottomRight) =
    object ["top_left"      .= gbbTopLeft
           , "bottom_right" .= gbbBottomRight]

instance FromJSON GeoBoundingBox where
  parseJSON = withObject "GeoBoundingBox" parse
    where parse o = GeoBoundingBox
                    <$> o .: "top_left"
                    <*> o .: "bottom_right"

instance ToJSON LatLon where
  toJSON (LatLon lLat lLon) =
    object ["lat"  .= lLat
           , "lon" .= lLon]

instance FromJSON LatLon where
  parseJSON = withObject "LatLon" parse
    where parse o = LatLon <$> o .: "lat"
                           <*> o .: "lon"

-- index for smaller ranges, fielddata for longer ranges
instance ToJSON RangeExecution where
  toJSON RangeExecutionIndex     = "index"
  toJSON RangeExecutionFielddata = "fielddata"


instance FromJSON RangeExecution where
  parseJSON = withText "RangeExecution" parse
    where parse "index"     = pure RangeExecutionIndex
          parse "fielddata" = pure RangeExecutionFielddata
          parse t           = error ("Unrecognized RangeExecution " <> show t)

instance ToJSON RegexpFlags where
  toJSON AllRegexpFlags              = String "ALL"
  toJSON NoRegexpFlags               = String "NONE"
  toJSON (SomeRegexpFlags (h :| fs)) = String $ T.intercalate "|" flagStrs
    where flagStrs             = map flagStr . nub $ h:fs
          flagStr AnyString    = "ANYSTRING"
          flagStr Automaton    = "AUTOMATON"
          flagStr Complement   = "COMPLEMENT"
          flagStr Empty        = "EMPTY"
          flagStr Intersection = "INTERSECTION"
          flagStr Interval     = "INTERVAL"

instance FromJSON RegexpFlags where
  parseJSON = withText "RegexpFlags" parse
    where parse "ALL" = pure AllRegexpFlags
          parse "NONE" = pure NoRegexpFlags
          parse t = SomeRegexpFlags <$> parseNEJSON (String <$> T.splitOn "|" t)

instance FromJSON RegexpFlag where
  parseJSON = withText "RegexpFlag" parse
    where parse "ANYSTRING"    = pure AnyString
          parse "AUTOMATON"    = pure Automaton
          parse "COMPLEMENT"   = pure Complement
          parse "EMPTY"        = pure Empty
          parse "INTERSECTION" = pure Intersection
          parse "INTERVAL"     = pure Interval
          parse f              = fail ("Unknown RegexpFlag: " <> show f)

instance ToJSON Term where
  toJSON (Term field value) = object ["term" .= object
                                      [field .= value]]

instance FromJSON Term where
  parseJSON = withObject "Term" parse
    where parse o = do termObj <- o .: "term"
                       case HM.toList termObj of
                         [(fn, v)] -> Term fn <$> parseJSON v
                         _ -> fail "Expected object with 1 field-named key"

instance ToJSON BoolMatch where
  toJSON (MustMatch    term  cache) = object ["must"     .= term,
                                              "_cache" .= cache]
  toJSON (MustNotMatch term  cache) = object ["must_not" .= term,
                                              "_cache" .= cache]
  toJSON (ShouldMatch  terms cache) = object ["should"   .= fmap toJSON terms,
                                              "_cache" .= cache]

instance FromJSON BoolMatch where
  parseJSON = withObject "BoolMatch" parse
    where parse o = mustMatch `taggedWith` "must"
                <|> mustNotMatch `taggedWith` "must_not"
                <|> shouldMatch `taggedWith` "should"
            where taggedWith parser k = parser =<< o .: k
                  mustMatch t = MustMatch t <$> o .:? "_cache" .!= defaultCache
                  mustNotMatch t = MustNotMatch t <$> o .:? "_cache" .!= defaultCache
                  shouldMatch t = ShouldMatch t <$> o .:? "_cache" .!= defaultCache

instance (FromJSON a) => FromJSON (SearchResult a) where
  parseJSON (Object v) = SearchResult <$>
                         v .:  "took"         <*>
                         v .:  "timed_out"    <*>
                         v .:  "_shards"      <*>
                         v .:  "hits"         <*>
                         v .:? "aggregations" <*>
                         v .:? "_scroll_id"   <*>
                         v .:? "suggest"
  parseJSON _          = empty

instance (FromJSON a) => FromJSON (SearchHits a) where
  parseJSON (Object v) = SearchHits <$>
                         v .: "total"     <*>
                         v .: "max_score" <*>
                         v .: "hits"
  parseJSON _          = empty

instance (FromJSON a) => FromJSON (Hit a) where
  parseJSON (Object v) = Hit <$>
                         v .:  "_index"   <*>
                         v .:  "_type"    <*>
                         v .:  "_id"      <*>
                         v .:  "_score"   <*>
                         v .:?  "_source" <*>
                         v .:? "highlight"
  parseJSON _          = empty

instance FromJSON ShardResult where
  parseJSON (Object v) = ShardResult       <$>
                         v .: "total"      <*>
                         v .: "successful" <*>
                         v .: "failed"
  parseJSON _          = empty


instance FromJSON DocVersion where
  parseJSON v = do
    i <- parseJSON v
    maybe (fail "DocVersion out of range") return $ mkDocVersion i

-- This insanity is because ES *sometimes* returns Replica/Shard counts as strings
instance FromJSON ReplicaCount where
  parseJSON v = parseAsInt v
            <|> parseAsString v
    where parseAsInt = fmap ReplicaCount . parseJSON
          parseAsString = withText "ReplicaCount" (fmap ReplicaCount . parseReadText)

instance FromJSON ShardCount where
  parseJSON v = parseAsInt v
            <|> parseAsString v
    where parseAsInt = fmap ShardCount . parseJSON
          parseAsString = withText "ShardCount" (fmap ShardCount . parseReadText)

instance Bounded DocVersion where
  minBound = DocVersion 1
  maxBound = DocVersion 9200000000000000000 -- 9.2e+18

instance Enum DocVersion where
  succ x
    | x /= maxBound = DocVersion (succ $ docVersionNumber x)
    | otherwise     = succError "DocVersion"
  pred x
    | x /= minBound = DocVersion (pred $ docVersionNumber x)
    | otherwise     = predError "DocVersion"
  toEnum i =
    fromMaybe (error $ show i ++ " out of DocVersion range") $ mkDocVersion i
  fromEnum = docVersionNumber
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

-- | Username type used for HTTP Basic authentication. See 'basicAuthHook'.
newtype EsUsername = EsUsername { esUsername :: Text } deriving (Read, Show, Eq)

-- | Password type used for HTTP Basic authentication. See 'basicAuthHook'.
newtype EsPassword = EsPassword { esPassword :: Text } deriving (Read, Show, Eq)


data SnapshotRepoSelection = SnapshotRepoList (NonEmpty SnapshotRepoPattern)
                           | AllSnapshotRepos deriving (Eq, Generic, Show, Typeable)


-- | Either specifies an exact repo name or one with globs in it,
-- e.g. @RepoPattern "foo*"@ __NOTE__: Patterns are not supported on ES < 1.7
data SnapshotRepoPattern = ExactRepo SnapshotRepoName
                         | RepoPattern Text
                         deriving (Eq, Generic, Show, Typeable)

-- | The unique name of a snapshot repository.
newtype SnapshotRepoName = SnapshotRepoName  { snapshotRepoName :: Text }
                         deriving (Eq, Ord, Generic, Show, Typeable, ToJSON, FromJSON)


-- | A generic representation of a snapshot repo. This is what gets
-- sent to and parsed from the server. For repo types enabled by
-- plugins that aren't exported by this library, consider making a
-- custom type which implements 'SnapshotRepo'. If it is a common repo
-- type, consider submitting a pull request to have it included in the
-- library proper
data GenericSnapshotRepo = GenericSnapshotRepo {
      gSnapshotRepoName     :: SnapshotRepoName
    , gSnapshotRepoType     :: SnapshotRepoType
    , gSnapshotRepoSettings :: GenericSnapshotRepoSettings
    } deriving (Eq, Generic, Show, Typeable)


instance SnapshotRepo GenericSnapshotRepo where
  toGSnapshotRepo = id
  fromGSnapshotRepo = Right


newtype SnapshotRepoType = SnapshotRepoType { snapshotRepoType :: Text }
                         deriving (Eq, Ord, Generic, Show, Typeable, ToJSON, FromJSON)


-- | Opaque representation of snapshot repo settings. Instances of
-- 'SnapshotRepo' will produce this.
newtype GenericSnapshotRepoSettings = GenericSnapshotRepoSettings { gSnapshotRepoSettingsObject :: Object }
                                    deriving (Eq, Generic, Show, Typeable, ToJSON)


 -- Regardless of whether you send strongly typed json, my version of
 -- ES sends back stringly typed json in the settings, e.g. booleans
 -- as strings, so we'll try to convert them.
instance FromJSON GenericSnapshotRepoSettings where
  parseJSON = fmap (GenericSnapshotRepoSettings . fmap unStringlyTypeJSON). parseJSON

-- | The result of running 'verifySnapshotRepo'.
newtype SnapshotVerification = SnapshotVerification { snapshotNodeVerifications :: [SnapshotNodeVerification] }
                             deriving (Eq, Generic, Show, Typeable)


instance FromJSON SnapshotVerification where
  parseJSON = withObject "SnapshotVerification" parse
    where
      parse o = do
        o2 <- o .: "nodes"
        SnapshotVerification <$> mapM (uncurry parse') (HM.toList o2)
      parse' rawFullId = withObject "SnapshotNodeVerification" $ \o ->
        SnapshotNodeVerification (FullNodeId rawFullId) <$> o .: "name"


-- | A node that has verified a snapshot
data SnapshotNodeVerification = SnapshotNodeVerification {
      snvFullId   :: FullNodeId
    , snvNodeName :: NodeName
    } deriving (Eq, Generic, Show, Typeable)


-- | Unique, automatically-generated name assigned to nodes that are
-- usually returned in node-oriented APIs.
newtype FullNodeId = FullNodeId { fullNodeId :: Text }
                   deriving (Eq, Ord, Generic, Show, Typeable, FromJSON)


-- | A human-readable node name that is supplied by the user in the
-- node config or automatically generated by ElasticSearch.
newtype NodeName = NodeName { nodeName :: Text }
                 deriving (Eq, Ord, Generic, Show, Typeable, FromJSON)

newtype ClusterName = ClusterName { clusterName :: Text }
                 deriving (Eq, Ord, Generic, Show, Typeable, FromJSON)

data NodesInfo = NodesInfo {
      nodesInfo        :: [NodeInfo]
    , nodesClusterName :: ClusterName
    } deriving (Eq, Show, Generic, Typeable)

data NodesStats = NodesStats {
      nodesStats            :: [NodeStats]
    , nodesStatsClusterName :: ClusterName
    } deriving (Eq, Show, Generic, Typeable)

data NodeStats = NodeStats {
      nodeStatsName          :: NodeName
    , nodeStatsFullId        :: FullNodeId
    , nodeStatsBreakersStats :: Maybe NodeBreakersStats
    , nodeStatsHTTP          :: NodeHTTPStats
    , nodeStatsTransport     :: NodeTransportStats
    , nodeStatsFS            :: NodeFSStats
    , nodeStatsNetwork       :: NodeNetworkStats
    , nodeStatsThreadPool    :: NodeThreadPoolsStats
    , nodeStatsJVM           :: NodeJVMStats
    , nodeStatsProcess       :: NodeProcessStats
    , nodeStatsOS            :: NodeOSStats
    , nodeStatsIndices       :: NodeIndicesStats
    } deriving (Eq, Show, Generic, Typeable)

data NodeBreakersStats = NodeBreakersStats {
      nodeStatsParentBreaker    :: NodeBreakerStats
    , nodeStatsRequestBreaker   :: NodeBreakerStats
    , nodeStatsFieldDataBreaker :: NodeBreakerStats
    } deriving (Eq, Show, Generic, Typeable)

data NodeBreakerStats = NodeBreakerStats {
      nodeBreakersTripped   :: Int
    , nodeBreakersOverhead  :: Double
    , nodeBreakersEstSize   :: Bytes
    , nodeBreakersLimitSize :: Bytes
    } deriving (Eq, Show, Generic, Typeable)

data NodeHTTPStats = NodeHTTPStats {
      nodeHTTPTotalOpened :: Int
    , nodeHTTPCurrentOpen :: Int
    } deriving (Eq, Show, Generic, Typeable)

data NodeTransportStats = NodeTransportStats {
      nodeTransportTXSize     :: Bytes
    , nodeTransportCount      :: Int
    , nodeTransportRXSize     :: Bytes
    , nodeTransportRXCount    :: Int
    , nodeTransportServerOpen :: Int
    } deriving (Eq, Show, Generic, Typeable)

data NodeFSStats = NodeFSStats {
      nodeFSDataPaths :: [NodeDataPathStats]
    , nodeFSTotal     :: NodeFSTotalStats
    , nodeFSTimestamp :: UTCTime
    } deriving (Eq, Show, Generic, Typeable)

data NodeDataPathStats = NodeDataPathStats {
      nodeDataPathDiskServiceTime :: Maybe Double
    , nodeDataPathDiskQueue       :: Maybe Double
    , nodeDataPathIOSize          :: Maybe Bytes
    , nodeDataPathWriteSize       :: Maybe Bytes
    , nodeDataPathReadSize        :: Maybe Bytes
    , nodeDataPathIOOps           :: Maybe Int
    , nodeDataPathWrites          :: Maybe Int
    , nodeDataPathReads           :: Maybe Int
    , nodeDataPathAvailable       :: Bytes
    , nodeDataPathFree            :: Bytes
    , nodeDataPathTotal           :: Bytes
    , nodeDataPathType            :: Maybe Text
    , nodeDataPathDevice          :: Text
    , nodeDataPathMount           :: Text
    , nodeDataPathPath            :: Text
    } deriving (Eq, Show, Generic, Typeable)

data NodeFSTotalStats = NodeFSTotalStats {
      nodeFSTotalDiskServiceTime :: Maybe Double
    , nodeFSTotalDiskQueue       :: Maybe Double
    , nodeFSTotalIOSize          :: Maybe Bytes
    , nodeFSTotalWriteSize       :: Maybe Bytes
    , nodeFSTotalReadSize        :: Maybe Bytes
    , nodeFSTotalIOOps           :: Maybe Int
    , nodeFSTotalWrites          :: Maybe Int
    , nodeFSTotalReads           :: Maybe Int
    , nodeFSTotalAvailable       :: Bytes
    , nodeFSTotalFree            :: Bytes
    , nodeFSTotalTotal           :: Bytes
    } deriving (Eq, Show, Generic, Typeable)

data NodeNetworkStats = NodeNetworkStats {
      nodeNetTCPOutRSTs      :: Int
    , nodeNetTCPInErrs       :: Int
    , nodeNetTCPAttemptFails :: Int
    , nodeNetTCPEstabResets  :: Int
    , nodeNetTCPRetransSegs  :: Int
    , nodeNetTCPOutSegs      :: Int
    , nodeNetTCPInSegs       :: Int
    , nodeNetTCPCurrEstab    :: Int
    , nodeNetTCPPassiveOpens :: Int
    , nodeNetTCPActiveOpens  :: Int
    } deriving (Eq, Show, Generic, Typeable)

data NodeThreadPoolsStats = NodeThreadPoolsStats {
      nodeThreadPoolsStatsSnapshot          :: NodeThreadPoolStats
    , nodeThreadPoolsStatsBulk              :: NodeThreadPoolStats
    , nodeThreadPoolsStatsMerge             :: NodeThreadPoolStats
    , nodeThreadPoolsStatsGet               :: NodeThreadPoolStats
    , nodeThreadPoolsStatsManagement        :: NodeThreadPoolStats
    , nodeThreadPoolsStatsFetchShardStore   :: Maybe NodeThreadPoolStats
    , nodeThreadPoolsStatsOptimize          :: NodeThreadPoolStats
    , nodeThreadPoolsStatsFlush             :: NodeThreadPoolStats
    , nodeThreadPoolsStatsSearch            :: NodeThreadPoolStats
    , nodeThreadPoolsStatsWarmer            :: NodeThreadPoolStats
    , nodeThreadPoolsStatsGeneric           :: NodeThreadPoolStats
    , nodeThreadPoolsStatsSuggest           :: NodeThreadPoolStats
    , nodeThreadPoolsStatsRefresh           :: NodeThreadPoolStats
    , nodeThreadPoolsStatsIndex             :: NodeThreadPoolStats
    , nodeThreadPoolsStatsListener          :: Maybe NodeThreadPoolStats
    , nodeThreadPoolsStatsFetchShardStarted :: Maybe NodeThreadPoolStats
    , nodeThreadPoolsStatsPercolate         :: NodeThreadPoolStats
    } deriving (Eq, Show, Generic, Typeable)

data NodeThreadPoolStats = NodeThreadPoolStats {
      nodeThreadPoolCompleted :: Int
    , nodeThreadPoolLargest   :: Int
    , nodeThreadPoolRejected  :: Int
    , nodeThreadPoolActive    :: Int
    , nodeThreadPoolQueue     :: Int
    , nodeThreadPoolThreads   :: Int
    } deriving (Eq, Show, Generic, Typeable)

data NodeJVMStats = NodeJVMStats {
      nodeJVMStatsMappedBufferPool :: JVMBufferPoolStats
    , nodeJVMStatsDirectBufferPool :: JVMBufferPoolStats
    , nodeJVMStatsGCOldCollector   :: JVMGCStats
    , nodeJVMStatsGCYoungCollector :: JVMGCStats
    , nodeJVMStatsPeakThreadsCount :: Int
    , nodeJVMStatsThreadsCount     :: Int
    , nodeJVMStatsOldPool          :: JVMPoolStats
    , nodeJVMStatsSurvivorPool     :: JVMPoolStats
    , nodeJVMStatsYoungPool        :: JVMPoolStats
    , nodeJVMStatsNonHeapCommitted :: Bytes
    , nodeJVMStatsNonHeapUsed      :: Bytes
    , nodeJVMStatsHeapMax          :: Bytes
    , nodeJVMStatsHeapCommitted    :: Bytes
    , nodeJVMStatsHeapUsedPercent  :: Int
    , nodeJVMStatsHeapUsed         :: Bytes
    , nodeJVMStatsUptime           :: NominalDiffTime
    , nodeJVMStatsTimestamp        :: UTCTime
    } deriving (Eq, Show, Generic, Typeable)

data JVMBufferPoolStats = JVMBufferPoolStats {
      jvmBufferPoolStatsTotalCapacity :: Bytes
    , jvmBufferPoolStatsUsed          :: Bytes
    , jvmBufferPoolStatsCount         :: Int
    } deriving (Eq, Show, Generic, Typeable)

data JVMGCStats = JVMGCStats {
      jvmGCStatsCollectionTime  :: NominalDiffTime
    , jvmGCStatsCollectionCount :: Int
    } deriving (Eq, Show, Generic, Typeable)

data JVMPoolStats = JVMPoolStats {
      jvmPoolStatsPeakMax  :: Bytes
    , jvmPoolStatsPeakUsed :: Bytes
    , jvmPoolStatsMax      :: Bytes
    , jvmPoolStatsUsed     :: Bytes
    } deriving (Eq, Show, Generic, Typeable)

data NodeProcessStats = NodeProcessStats {
      nodeProcessMemTotalVirtual :: Bytes
    , nodeProcessMemShare        :: Bytes
    , nodeProcessMemResident     :: Bytes
    , nodeProcessCPUTotal        :: NominalDiffTime
    , nodeProcessCPUUser         :: NominalDiffTime
    , nodeProcessCPUSys          :: NominalDiffTime
    , nodeProcessCPUPercent      :: Int
    , nodeProcessOpenFDs         :: Int
    , nodeProcessTimestamp       :: UTCTime
    } deriving (Eq, Show, Generic, Typeable)

data NodeOSStats = NodeOSStats {
      nodeOSSwapFree       :: Bytes
    , nodeOSSwapUsed       :: Bytes
    , nodeOSMemActualUsed  :: Bytes
    , nodeOSMemActualFree  :: Bytes
    , nodeOSMemUsedPercent :: Int
    , nodeOSMemFreePercent :: Int
    , nodeOSMemUsed        :: Bytes
    , nodeOSMemFree        :: Bytes
    , nodeOSCPUStolen      :: Int
    , nodeOSCPUUsage       :: Int
    , nodeOSCPUIdle        :: Int
    , nodeOSCPUUser        :: Int
    , nodeOSCPUSys         :: Int
    , nodeOSLoad           :: Maybe LoadAvgs
    , nodeOSUptime         :: NominalDiffTime
    , nodeOSTimestamp      :: UTCTime
    } deriving (Eq, Show, Generic, Typeable)

data LoadAvgs = LoadAvgs {
     loadAvg1Min  :: Double
   , loadAvg5Min  :: Double
   , loadAvg15Min :: Double
   } deriving (Eq, Show, Generic, Typeable)

data NodeIndicesStats = NodeIndicesStats {
      nodeIndicesStatsRecoveryThrottleTime    :: Maybe NominalDiffTime
    , nodeIndicesStatsRecoveryCurrentAsTarget :: Maybe Int
    , nodeIndicesStatsRecoveryCurrentAsSource :: Maybe Int
    , nodeIndicesStatsQueryCacheMisses        :: Maybe Int
    , nodeIndicesStatsQueryCacheHits          :: Maybe Int
    , nodeIndicesStatsQueryCacheEvictions     :: Maybe Int
    , nodeIndicesStatsQueryCacheSize          :: Maybe Bytes
    , nodeIndicesStatsSuggestCurrent          :: Int
    , nodeIndicesStatsSuggestTime             :: NominalDiffTime
    , nodeIndicesStatsSuggestTotal            :: Int
    , nodeIndicesStatsTranslogSize            :: Bytes
    , nodeIndicesStatsTranslogOps             :: Int
    , nodeIndicesStatsSegFixedBitSetMemory    :: Maybe Bytes
    , nodeIndicesStatsSegVersionMapMemory     :: Bytes
    , nodeIndicesStatsSegIndexWriterMaxMemory :: Maybe Bytes
    , nodeIndicesStatsSegIndexWriterMemory    :: Bytes
    , nodeIndicesStatsSegMemory               :: Bytes
    , nodeIndicesStatsSegCount                :: Int
    , nodeIndicesStatsCompletionSize          :: Bytes
    , nodeIndicesStatsPercolateQueries        :: Int
    , nodeIndicesStatsPercolateMemory         :: Bytes
    , nodeIndicesStatsPercolateCurrent        :: Int
    , nodeIndicesStatsPercolateTime           :: NominalDiffTime
    , nodeIndicesStatsPercolateTotal          :: Int
    , nodeIndicesStatsFieldDataEvictions      :: Int
    , nodeIndicesStatsFieldDataMemory         :: Bytes
    , nodeIndicesStatsIDCacheMemory           :: Bytes
    , nodeIndicesStatsFilterCacheEvictions    :: Int
    , nodeIndicesStatsFilterCacheMemory       :: Bytes
    , nodeIndicesStatsWarmerTotalTime         :: NominalDiffTime
    , nodeIndicesStatsWarmerTotal             :: Int
    , nodeIndicesStatsWarmerCurrent           :: Int
    , nodeIndicesStatsFlushTotalTime          :: NominalDiffTime
    , nodeIndicesStatsFlushTotal              :: Int
    , nodeIndicesStatsRefreshTotalTime        :: NominalDiffTime
    , nodeIndicesStatsRefreshTotal            :: Int
    , nodeIndicesStatsMergesTotalSize         :: Bytes
    , nodeIndicesStatsMergesTotalDocs         :: Int
    , nodeIndicesStatsMergesTotalTime         :: NominalDiffTime
    , nodeIndicesStatsMergesTotal             :: Int
    , nodeIndicesStatsMergesCurrentSize       :: Bytes
    , nodeIndicesStatsMergesCurrentDocs       :: Int
    , nodeIndicesStatsMergesCurrent           :: Int
    , nodeIndicesStatsSearchFetchCurrent      :: Int
    , nodeIndicesStatsSearchFetchTime         :: NominalDiffTime
    , nodeIndicesStatsSearchFetchTotal        :: Int
    , nodeIndicesStatsSearchQueryCurrent      :: Int
    , nodeIndicesStatsSearchQueryTime         :: NominalDiffTime
    , nodeIndicesStatsSearchQueryTotal        :: Int
    , nodeIndicesStatsSearchOpenContexts      :: Int
    , nodeIndicesStatsGetCurrent              :: Int
    , nodeIndicesStatsGetMissingTime          :: NominalDiffTime
    , nodeIndicesStatsGetMissingTotal         :: Int
    , nodeIndicesStatsGetExistsTime           :: NominalDiffTime
    , nodeIndicesStatsGetExistsTotal          :: Int
    , nodeIndicesStatsGetTime                 :: NominalDiffTime
    , nodeIndicesStatsGetTotal                :: Int
    , nodeIndicesStatsIndexingThrottleTime    :: Maybe NominalDiffTime
    , nodeIndicesStatsIndexingIsThrottled     :: Maybe Bool
    , nodeIndicesStatsIndexingNoopUpdateTotal :: Maybe Int
    , nodeIndicesStatsIndexingDeleteCurrent   :: Int
    , nodeIndicesStatsIndexingDeleteTime      :: NominalDiffTime
    , nodeIndicesStatsIndexingDeleteTotal     :: Int
    , nodeIndicesStatsIndexingIndexCurrent    :: Int
    , nodeIndicesStatsIndexingIndexTime       :: NominalDiffTime
    , nodeIndicesStatsIndexingTotal           :: Int
    , nodeIndicesStatsStoreThrottleTime       :: NominalDiffTime
    , nodeIndicesStatsStoreSize               :: Bytes
    , nodeIndicesStatsDocsDeleted             :: Int
    , nodeIndicesStatsDocsCount               :: Int
    } deriving (Eq, Show, Generic, Typeable)

-- | A quirky address format used throughout ElasticSearch. An example
-- would be inet[/1.1.1.1:9200]. inet may be a placeholder for a
-- <https://en.wikipedia.org/wiki/Fully_qualified_domain_name FQDN>.
newtype EsAddress = EsAddress { esAddress :: Text }
                 deriving (Eq, Ord, Generic, Show, Typeable, FromJSON)

-- | Typically a 7 character hex string.
newtype BuildHash = BuildHash { buildHash :: Text }
                 deriving (Eq, Ord, Generic, Read, Show, Typeable, FromJSON, ToJSON)

newtype PluginName = PluginName { pluginName :: Text }
                 deriving (Eq, Ord, Generic, Show, Typeable, FromJSON)

data NodeInfo = NodeInfo {
      nodeInfoHTTPAddress      :: EsAddress
    , nodeInfoBuild            :: BuildHash
    , nodeInfoESVersion        :: VersionNumber
    , nodeInfoIP               :: Server
    , nodeInfoHost             :: Server
    , nodeInfoTransportAddress :: EsAddress
    , nodeInfoName             :: NodeName
    , nodeInfoFullId           :: FullNodeId
    , nodeInfoPlugins          :: [NodePluginInfo]
    , nodeInfoHTTP             :: NodeHTTPInfo
    , nodeInfoTransport        :: NodeTransportInfo
    , nodeInfoNetwork          :: NodeNetworkInfo
    , nodeInfoThreadPool       :: NodeThreadPoolsInfo
    , nodeInfoJVM              :: NodeJVMInfo
    , nodeInfoProcess          :: NodeProcessInfo
    , nodeInfoOS               :: NodeOSInfo
    , nodeInfoSettings         :: Object
    -- ^ The members of the settings objects are not consistent,
    -- dependent on plugins, etc.
    } deriving (Eq, Show, Generic, Typeable)

data NodePluginInfo = NodePluginInfo {
      nodePluginSite        :: Bool
    -- ^ Is this a site plugin?
    , nodePluginJVM         :: Bool
    -- ^ Is this plugin running on the JVM
    , nodePluginDescription :: Text
    , nodePluginVersion     :: MaybeNA VersionNumber
    , nodePluginName        :: PluginName
    } deriving (Eq, Show, Generic, Typeable)

data NodeHTTPInfo = NodeHTTPInfo {
      nodeHTTPMaxContentLength :: Bytes
    , nodeHTTPTransportAddress :: BoundTransportAddress
    } deriving (Eq, Show, Generic, Typeable)

data NodeTransportInfo = NodeTransportInfo {
      nodeTransportProfiles :: [BoundTransportAddress]
    , nodeTransportAddress  :: BoundTransportAddress
    } deriving (Eq, Show, Generic, Typeable)

data BoundTransportAddress = BoundTransportAddress {
      publishAddress :: EsAddress
    , boundAddress   :: EsAddress
    } deriving (Eq, Show, Generic, Typeable)

data NodeNetworkInfo = NodeNetworkInfo {
      nodeNetworkPrimaryInterface :: NodeNetworkInterface
    , nodeNetworkRefreshInterval  :: NominalDiffTime
    } deriving (Eq, Show, Generic, Typeable)

newtype MacAddress = MacAddress { macAddress :: Text }
                 deriving (Eq, Ord, Generic, Show, Typeable, FromJSON)

newtype NetworkInterfaceName = NetworkInterfaceName { networkInterfaceName :: Text }
                 deriving (Eq, Ord, Generic, Show, Typeable, FromJSON)

data NodeNetworkInterface = NodeNetworkInterface {
      nodeNetIfaceMacAddress :: MacAddress
    , nodeNetIfaceName       :: NetworkInterfaceName
    , nodeNetIfaceAddress    :: Server
    } deriving (Eq, Show, Generic, Typeable)

data NodeThreadPoolsInfo = NodeThreadPoolsInfo {
      nodeThreadPoolsRefresh           :: NodeThreadPoolInfo
    , nodeThreadPoolsManagement        :: NodeThreadPoolInfo
    , nodeThreadPoolsPercolate         :: NodeThreadPoolInfo
    , nodeThreadPoolsListener          :: Maybe NodeThreadPoolInfo
    , nodeThreadPoolsFetchShardStarted :: Maybe NodeThreadPoolInfo
    , nodeThreadPoolsSearch            :: NodeThreadPoolInfo
    , nodeThreadPoolsFlush             :: NodeThreadPoolInfo
    , nodeThreadPoolsWarmer            :: NodeThreadPoolInfo
    , nodeThreadPoolsOptimize          :: NodeThreadPoolInfo
    , nodeThreadPoolsBulk              :: NodeThreadPoolInfo
    , nodeThreadPoolsSuggest           :: NodeThreadPoolInfo
    , nodeThreadPoolsMerge             :: NodeThreadPoolInfo
    , nodeThreadPoolsSnapshot          :: NodeThreadPoolInfo
    , nodeThreadPoolsGet               :: NodeThreadPoolInfo
    , nodeThreadPoolsFetchShardStore   :: Maybe NodeThreadPoolInfo
    , nodeThreadPoolsIndex             :: NodeThreadPoolInfo
    , nodeThreadPoolsGeneric           :: NodeThreadPoolInfo
    } deriving (Eq, Show, Generic, Typeable)

data NodeThreadPoolInfo = NodeThreadPoolInfo {
      nodeThreadPoolQueueSize :: ThreadPoolSize
    , nodeThreadPoolKeepalive :: Maybe NominalDiffTime
    , nodeThreadPoolMin       :: Maybe Int
    , nodeThreadPoolMax       :: Maybe Int
    , nodeThreadPoolType      :: ThreadPoolType
    } deriving (Eq, Show, Generic, Typeable)

data ThreadPoolSize = ThreadPoolBounded Int
                    | ThreadPoolUnbounded
                    deriving (Eq, Show, Generic, Typeable)

data ThreadPoolType = ThreadPoolScaling
                    | ThreadPoolFixed
                    | ThreadPoolCached
                    deriving (Eq, Show, Generic, Typeable)

data NodeJVMInfo = NodeJVMInfo {
      nodeJVMInfoMemoryPools             :: [JVMMemoryPool]
    , nodeJVMInfoMemoryPoolsGCCollectors :: [JVMGCCollector]
    , nodeJVMInfoMemoryInfo              :: JVMMemoryInfo
    , nodeJVMInfoStartTime               :: UTCTime
    , nodeJVMInfoVMVendor                :: Text
    , nodeJVMVMVersion                   :: VersionNumber
    -- ^ JVM doesn't seme to follow normal version conventions
    , nodeJVMVMName                      :: Text
    , nodeJVMVersion                     :: VersionNumber
    , nodeJVMPID                         :: PID
    } deriving (Eq, Show, Generic, Typeable)

-- | Handles quirks in the way JVM versions are rendered (1.7.0_101 -> 1.7.0.101)
newtype JVMVersion = JVMVersion { unJVMVersion :: VersionNumber }

data JVMMemoryInfo = JVMMemoryInfo {
      jvmMemoryInfoDirectMax   :: Bytes
    , jvmMemoryInfoNonHeapMax  :: Bytes
    , jvmMemoryInfoNonHeapInit :: Bytes
    , jvmMemoryInfoHeapMax     :: Bytes
    , jvmMemoryInfoHeapInit    :: Bytes
    } deriving (Eq, Show, Generic, Typeable)

newtype JVMMemoryPool = JVMMemoryPool {
      jvmMemoryPool :: Text
    } deriving (Eq, Show, Generic, Typeable, FromJSON)

newtype JVMGCCollector = JVMGCCollector {
      jvmGCCollector :: Text
    } deriving (Eq, Show, Generic, Typeable, FromJSON)

newtype PID = PID {
      pid :: Int
    } deriving (Eq, Show, Generic, Typeable, FromJSON)

data NodeOSInfo = NodeOSInfo {
      nodeOSSwap                :: Bytes
    , nodeOSMem                 :: Bytes
    , nodeOSCPUInfo             :: CPUInfo
    , nodeOSAvailableProcessors :: Int
    , nodeOSRefreshInterval     :: NominalDiffTime
    } deriving (Eq, Show, Generic, Typeable)

data CPUInfo = CPUInfo {
      cpuCacheSize      :: Bytes
    , cpuCoresPerSocket :: Int
    , cpuTotalSockets   :: Int
    , cpuTotalCores     :: Int
    , cpuMHZ            :: Int
    , cpuModel          :: Text
    , cpuVendor         :: Text
    } deriving (Eq, Show, Generic, Typeable)

data NodeProcessInfo = NodeProcessInfo {
      nodeProcessMLockAll           :: Bool
    -- ^ See <https://www.elastic.co/guide/en/elasticsearch/reference/current/setup-configuration.html>
    , nodeProcessMaxFileDescriptors :: Int
    , nodeProcessId                 :: PID
    , nodeProcessRefreshInterval    :: NominalDiffTime
    } deriving (Eq, Show, Generic, Typeable)

data SnapshotRepoUpdateSettings = SnapshotRepoUpdateSettings {
     repoUpdateVerify :: Bool
     -- ^ After creation/update, synchronously check that nodes can
     -- write to this repo. Defaults to True. You may use False if you
     -- need a faster response and plan on verifying manually later
     -- with 'verifySnapshotRepo'.
    } deriving (Eq, Show, Generic, Typeable)


-- | Reasonable defaults for repo creation/update
--
-- * repoUpdateVerify True
defaultSnapshotRepoUpdateSettings :: SnapshotRepoUpdateSettings
defaultSnapshotRepoUpdateSettings = SnapshotRepoUpdateSettings True


-- | A filesystem-based snapshot repo that ships with
-- ElasticSearch. This is an instance of 'SnapshotRepo' so it can be
-- used with 'updateSnapshotRepo'
data FsSnapshotRepo = FsSnapshotRepo {
      fsrName                   :: SnapshotRepoName
    , fsrLocation               :: FilePath
    , fsrCompressMetadata       :: Bool
    , fsrChunkSize              :: Maybe Bytes
    -- ^ Size by which to split large files during snapshotting.
    , fsrMaxRestoreBytesPerSec  :: Maybe Bytes
    -- ^ Throttle node restore rate. If not supplied, defaults to 40mb/sec
    , fsrMaxSnapshotBytesPerSec :: Maybe Bytes
    -- ^ Throttle node snapshot rate. If not supplied, defaults to 40mb/sec
    } deriving (Eq, Generic, Show, Typeable)


instance SnapshotRepo FsSnapshotRepo where
  toGSnapshotRepo FsSnapshotRepo {..} =
    GenericSnapshotRepo fsrName fsRepoType (GenericSnapshotRepoSettings settings)
    where
      Object settings = object $ [ "location" .= fsrLocation
                                 , "compress" .= fsrCompressMetadata
                                 ] ++ optionalPairs
      optionalPairs = catMaybes [ ("chunk_size" .=) <$> fsrChunkSize
                                , ("max_restore_bytes_per_sec" .=) <$> fsrMaxRestoreBytesPerSec
                                , ("max_snapshot_bytes_per_sec" .=) <$> fsrMaxSnapshotBytesPerSec
                                ]
  fromGSnapshotRepo GenericSnapshotRepo {..}
    | gSnapshotRepoType == fsRepoType = do
      let o = gSnapshotRepoSettingsObject gSnapshotRepoSettings
      parseRepo $ do
        FsSnapshotRepo gSnapshotRepoName <$> o .: "location"
                                         <*> o .:? "compress" .!= False
                                         <*> o .:? "chunk_size"
                                         <*> o .:? "max_restore_bytes_per_sec"
                                         <*> o .:? "max_snapshot_bytes_per_sec"
    | otherwise = Left (RepoTypeMismatch fsRepoType gSnapshotRepoType)


parseRepo :: Parser a -> Either SnapshotRepoConversionError a
parseRepo parser = case parseEither (const parser) () of
  Left e  -> Left (OtherRepoConversionError (T.pack e))
  Right a -> Right a


fsRepoType :: SnapshotRepoType
fsRepoType = SnapshotRepoType "fs"

-- | Law: fromGSnapshotRepo (toGSnapshotRepo r) == Right r
class SnapshotRepo r where
  toGSnapshotRepo :: r -> GenericSnapshotRepo
  fromGSnapshotRepo :: GenericSnapshotRepo -> Either SnapshotRepoConversionError r


data SnapshotRepoConversionError = RepoTypeMismatch SnapshotRepoType SnapshotRepoType
                                 -- ^ Expected type and actual type
                                 | OtherRepoConversionError Text
                                 deriving (Show, Eq, Generic, Typeable)


instance Exception SnapshotRepoConversionError


data SnapshotCreateSettings = SnapshotCreateSettings {
      snapWaitForCompletion  :: Bool
      -- ^ Should the API call return immediately after initializing
      -- the snapshot or wait until completed? Note that if this is
      -- enabled it could wait a long time, so you should adjust your
      -- 'ManagerSettings' accordingly to set long timeouts or
      -- explicitly handle timeouts.
    , snapIndices            :: Maybe IndexSelection
    -- ^ Nothing will snapshot all indices. Just [] is permissable and
    -- will essentially be a no-op snapshot.
    , snapIgnoreUnavailable  :: Bool
    -- ^ If set to True, any matched indices that don't exist will be
    -- ignored. Otherwise it will be an error and fail.
    , snapIncludeGlobalState :: Bool
    , snapPartial            :: Bool
    -- ^ If some indices failed to snapshot (e.g. if not all primary
    -- shards are available), should the process proceed?
    } deriving (Eq, Generic, Show, Typeable)


-- | Reasonable defaults for snapshot creation
--
-- * snapWaitForCompletion False
-- * snapIndices Nothing
-- * snapIgnoreUnavailable False
-- * snapIncludeGlobalState True
-- * snapPartial False
defaultSnapshotCreateSettings :: SnapshotCreateSettings
defaultSnapshotCreateSettings = SnapshotCreateSettings {
      snapWaitForCompletion = False
    , snapIndices = Nothing
    , snapIgnoreUnavailable = False
    , snapIncludeGlobalState = True
    , snapPartial = False
    }


data SnapshotSelection = SnapshotList (NonEmpty SnapshotPattern)
                       | AllSnapshots deriving (Eq, Generic, Show, Typeable)


-- | Either specifies an exact snapshot name or one with globs in it,
-- e.g. @SnapPattern "foo*"@ __NOTE__: Patterns are not supported on
-- ES < 1.7
data SnapshotPattern = ExactSnap SnapshotName
                     | SnapPattern Text
                     deriving (Eq, Generic, Show, Typeable)


-- | General information about the state of a snapshot. Has some
-- redundancies with 'SnapshotStatus'
data SnapshotInfo = SnapshotInfo {
      snapInfoShards    :: ShardResult
    , snapInfoFailures  :: [SnapshotShardFailure]
    , snapInfoDuration  :: NominalDiffTime
    , snapInfoEndTime   :: UTCTime
    , snapInfoStartTime :: UTCTime
    , snapInfoState     :: SnapshotState
    , snapInfoIndices   :: [IndexName]
    , snapInfoName      :: SnapshotName
    } deriving (Eq, Generic, Show, Typeable)


instance FromJSON SnapshotInfo where
  parseJSON = withObject "SnapshotInfo" parse
    where
      parse o = SnapshotInfo <$> o .: "shards"
                             <*> o .: "failures"
                             <*> (unMS <$> o .: "duration_in_millis")
                             <*> (posixMS <$> o .: "end_time_in_millis")
                             <*> (posixMS <$> o .: "start_time_in_millis")
                             <*> o .: "state"
                             <*> o .: "indices"
                             <*> o .: "snapshot"

data SnapshotShardFailure = SnapshotShardFailure {
      snapShardFailureIndex   :: IndexName
    , snapShardFailureNodeId  :: Maybe NodeName -- I'm not 100% sure this isn't actually 'FullNodeId'
    , snapShardFailureReason  :: Text
    , snapShardFailureShardId :: ShardId
    } deriving (Eq, Show, Generic, Typeable)


instance FromJSON SnapshotShardFailure where
  parseJSON = withObject "SnapshotShardFailure" parse
    where
      parse o = SnapshotShardFailure <$> o .: "index"
                                     <*> o .:? "node_id"
                                     <*> o .: "reason"
                                     <*> o .: "shard_id"


newtype ShardId = ShardId { shardId :: Int }
                deriving (Eq, Show, Generic, Typeable, FromJSON)

-- | Milliseconds
newtype MS = MS NominalDiffTime


-- keeps the unexported constructor warnings at bay
unMS :: MS -> NominalDiffTime
unMS (MS t) = t


instance FromJSON MS where
  parseJSON = withScientific "MS" (return . MS . parse)
    where
      parse n = fromInteger ((truncate n) * 1000)


data SnapshotState = SnapshotInit
                   | SnapshotStarted
                   | SnapshotSuccess
                   | SnapshotFailed
                   | SnapshotAborted
                   | SnapshotMissing
                   | SnapshotWaiting
                   deriving (Show, Eq, Generic, Typeable)

instance FromJSON SnapshotState where
  parseJSON = withText "SnapshotState" parse
    where
      parse "INIT"    = return SnapshotInit
      parse "STARTED" = return SnapshotStarted
      parse "SUCCESS" = return SnapshotSuccess
      parse "FAILED"  = return SnapshotFailed
      parse "ABORTED" = return SnapshotAborted
      parse "MISSING" = return SnapshotMissing
      parse "WAITING" = return SnapshotWaiting
      parse t         = fail ("Invalid snapshot state " <> T.unpack t)


newtype SnapshotName = SnapshotName { snapshotName :: Text }
                     deriving (Show, Eq, Ord, Generic, Typeable, ToJSON, FromJSON)


data SnapshotRestoreSettings = SnapshotRestoreSettings {
      snapRestoreWaitForCompletion      :: Bool
      -- ^ Should the API call return immediately after initializing
      -- the restore or wait until completed? Note that if this is
      -- enabled, it could wait a long time, so you should adjust your
      -- 'ManagerSettings' accordingly to set long timeouts or
      -- explicitly handle timeouts.
    , snapRestoreIndices                :: Maybe IndexSelection
    -- ^ Nothing will restore all indices in the snapshot. Just [] is
    -- permissable and will essentially be a no-op restore.
    , snapRestoreIgnoreUnavailable      :: Bool
    -- ^ If set to True, any indices that do not exist will be ignored
    -- during snapshot rather than failing the restore.
    , snapRestoreIncludeGlobalState     :: Bool
    -- ^ If set to false, will ignore any global state in the snapshot
    -- and will not restore it.
    , snapRestoreRenamePattern          :: Maybe RestoreRenamePattern
    -- ^ A regex pattern for matching indices. Used with
    -- 'snapRestoreRenameReplacement', the restore can reference the
    -- matched index and create a new index name upon restore.
    , snapRestoreRenameReplacement      :: Maybe (NonEmpty RestoreRenameToken)
    -- ^ Expression of how index renames should be constructed.
    , snapRestorePartial                :: Bool
    -- ^ If some indices fail to restore, should the process proceed?
    , snapRestoreIncludeAliases         :: Bool
    -- ^ Should the restore also restore the aliases captured in the
    -- snapshot.
    , snapRestoreIndexSettingsOverrides :: Maybe RestoreIndexSettings
    -- ^ Settings to apply during the restore process. __NOTE:__ This
    -- option is not supported in ES < 1.5 and should be set to
    -- Nothing in that case.
    , snapRestoreIgnoreIndexSettings    :: Maybe (NonEmpty Text)
    -- ^ This type could be more rich but it isn't clear which
    -- settings are allowed to be ignored during restore, so we're
    -- going with including this feature in a basic form rather than
    -- omitting it. One example here would be
    -- "index.refresh_interval". Any setting specified here will
    -- revert back to the server default during the restore process.
    } deriving (Eq, Generic, Show, Typeable)

-- | Regex-stype pattern, e.g. "index_(.+)" to match index names
newtype RestoreRenamePattern = RestoreRenamePattern { rrPattern :: Text }
                             deriving (Show, Eq, Generic, Typeable, Ord, ToJSON)


-- | A single token in a index renaming scheme for a restore. These
-- are concatenated into a string before being sent to
-- ElasticSearch. Check out these Java
-- <https://docs.oracle.com/javase/7/docs/api/java/util/regex/Matcher.html docs> to find out more if you're into that sort of thing.
data RestoreRenameToken = RRTLit Text
                        -- ^ Just a literal string of characters
                        | RRSubWholeMatch
                        -- ^ Equivalent to $0. The entire matched pattern, not any subgroup
                        | RRSubGroup RRGroupRefNum
                        -- ^ A specific reference to a group number
                        deriving (Show, Eq, Generic, Typeable)


-- | A group number for regex matching. Only values from 1-9 are
-- supported. Construct with 'mkRRGroupRefNum'
newtype RRGroupRefNum = RRGroupRefNum { rrGroupRefNum :: Int }
                      deriving (Show, Eq, Generic, Typeable, Ord)

instance Bounded RRGroupRefNum where
  minBound = RRGroupRefNum 1
  maxBound = RRGroupRefNum 9


-- | Only allows valid group number references (1-9).
mkRRGroupRefNum :: Int -> Maybe RRGroupRefNum
mkRRGroupRefNum i
  | i >= (rrGroupRefNum minBound) && i <= (rrGroupRefNum maxBound) =
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
defaultSnapshotRestoreSettings = SnapshotRestoreSettings {
      snapRestoreWaitForCompletion = False
    , snapRestoreIndices = Nothing
    , snapRestoreIgnoreUnavailable  = False
    , snapRestoreIncludeGlobalState = True
    , snapRestoreRenamePattern = Nothing
    , snapRestoreRenameReplacement = Nothing
    , snapRestorePartial = False
    , snapRestoreIncludeAliases = True
    , snapRestoreIndexSettingsOverrides = Nothing
    , snapRestoreIgnoreIndexSettings = Nothing
    }


-- | Index settings that can be overridden. The docs only mention you
-- can update number of replicas, but there may be more. You
-- definitely cannot override shard count.
data RestoreIndexSettings = RestoreIndexSettings {
      restoreOverrideReplicas :: Maybe ReplicaCount
    } deriving (Show, Eq, Generic, Typeable)


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
      parse o = NodeBreakerStats <$> o .: "tripped"
                                 <*> o .: "overhead"
                                 <*> o .: "estimated_size_in_bytes"
                                 <*> o .: "limit_size_in_bytes"

instance FromJSON NodeHTTPStats where
  parseJSON = withObject "NodeHTTPStats" parse
    where
      parse o = NodeHTTPStats <$> o .: "total_opened"
                              <*> o .: "current_open"

instance FromJSON NodeTransportStats where
  parseJSON = withObject "NodeTransportStats" parse
    where
      parse o = NodeTransportStats <$> o .: "tx_size_in_bytes"
                                   <*> o .: "tx_count"
                                   <*> o .: "rx_size_in_bytes"
                                   <*> o .: "rx_count"
                                   <*> o .: "server_open"

instance FromJSON NodeFSStats where
  parseJSON = withObject "NodeFSStats" parse
    where
      parse o = NodeFSStats <$> o .: "data"
                            <*> o .: "total"
                            <*> (posixMS <$> o .: "timestamp")

instance FromJSON NodeDataPathStats where
  parseJSON = withObject "NodeDataPathStats" parse
    where
      parse o =
        NodeDataPathStats <$> (fmap unStringlyTypedDouble <$> o .:? "disk_service_time")
                          <*> (fmap unStringlyTypedDouble <$> o .:? "disk_queue")
                          <*> o .:? "disk_io_size_in_bytes"
                          <*> o .:? "disk_write_size_in_bytes"
                          <*> o .:? "disk_read_size_in_bytes"
                          <*> o .:? "disk_io_op"
                          <*> o .:? "disk_writes"
                          <*> o .:? "disk_reads"
                          <*> o .: "available_in_bytes"
                          <*> o .: "free_in_bytes"
                          <*> o .: "total_in_bytes"
                          <*> o .:? "type"
                          <*> o .: "dev"
                          <*> o .: "mount"
                          <*> o .: "path"

newtype StringlyTypedDouble = StringlyTypedDouble { unStringlyTypedDouble :: Double }


instance FromJSON StringlyTypedDouble where
  parseJSON = fmap StringlyTypedDouble . parseJSON . unStringlyTypeJSON


instance FromJSON NodeFSTotalStats where
  parseJSON = withObject "NodeFSTotalStats" parse
    where
      parse o = NodeFSTotalStats <$> (fmap unStringlyTypedDouble <$> o .:? "disk_service_time")
                                 <*> (fmap unStringlyTypedDouble <$> o .:? "disk_queue")
                                 <*> o .:? "disk_io_size_in_bytes"
                                 <*> o .:? "disk_write_size_in_bytes"
                                 <*> o .:? "disk_read_size_in_bytes"
                                 <*> o .:? "disk_io_op"
                                 <*> o .:? "disk_writes"
                                 <*> o .:? "disk_reads"
                                 <*> o .: "available_in_bytes"
                                 <*> o .: "free_in_bytes"
                                 <*> o .: "total_in_bytes"

instance FromJSON NodeNetworkStats where
  parseJSON = withObject "NodeNetworkStats" parse
    where
      parse o = do
        tcp <- o .: "tcp"
        NodeNetworkStats <$> tcp .: "out_rsts"
                         <*> tcp .: "in_errs"
                         <*> tcp .: "attempt_fails"
                         <*> tcp .: "estab_resets"
                         <*> tcp .: "retrans_segs"
                         <*> tcp .: "out_segs"
                         <*> tcp .: "in_segs"
                         <*> tcp .: "curr_estab"
                         <*> tcp .: "passive_opens"
                         <*> tcp .: "active_opens"

instance FromJSON NodeThreadPoolsStats where
  parseJSON = withObject "NodeThreadPoolsStats" parse
    where
      parse o = NodeThreadPoolsStats <$> o .: "snapshot"
                                     <*> o .: "bulk"
                                     <*> o .: "merge"
                                     <*> o .: "get"
                                     <*> o .: "management"
                                     <*> o .:? "fetch_shard_store"
                                     <*> o .: "optimize"
                                     <*> o .: "flush"
                                     <*> o .: "search"
                                     <*> o .: "warmer"
                                     <*> o .: "generic"
                                     <*> o .: "suggest"
                                     <*> o .: "refresh"
                                     <*> o .: "index"
                                     <*> o .:? "listener"
                                     <*> o .:? "fetch_shard_started"
                                     <*> o .: "percolate"
instance FromJSON NodeThreadPoolStats where
    parseJSON = withObject "NodeThreadPoolStats" parse
      where
        parse o = NodeThreadPoolStats <$> o .: "completed"
                                      <*> o .: "largest"
                                      <*> o .: "rejected"
                                      <*> o .: "active"
                                      <*> o .: "queue"
                                      <*> o .: "threads"

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
        NodeJVMStats <$> pure mapped
                     <*> pure direct
                     <*> pure oldC
                     <*> pure youngC
                     <*> threads .: "peak_count"
                     <*> threads .: "count"
                     <*> pure oldM
                     <*> pure survivorM
                     <*> pure youngM
                     <*> mem .: "non_heap_committed_in_bytes"
                     <*> mem .: "non_heap_used_in_bytes"
                     <*> mem .: "heap_max_in_bytes"
                     <*> mem .: "heap_committed_in_bytes"
                     <*> mem .: "heap_used_percent"
                     <*> mem .: "heap_used_in_bytes"
                     <*> (unMS <$> o .: "uptime_in_millis")
                     <*> (posixMS <$> o .: "timestamp")

instance FromJSON JVMBufferPoolStats where
  parseJSON = withObject "JVMBufferPoolStats" parse
    where
      parse o = JVMBufferPoolStats <$> o .: "total_capacity_in_bytes"
                                   <*> o .: "used_in_bytes"
                                   <*> o .: "count"

instance FromJSON JVMGCStats where
  parseJSON = withObject "JVMGCStats" parse
    where
      parse o = JVMGCStats <$> (unMS <$> o .: "collection_time_in_millis")
                           <*> o .: "collection_count"

instance FromJSON JVMPoolStats where
  parseJSON = withObject "JVMPoolStats" parse
    where
      parse o = JVMPoolStats <$> o .: "peak_max_in_bytes"
                             <*> o .: "peak_used_in_bytes"
                             <*> o .: "max_in_bytes"
                             <*> o .: "used_in_bytes"

instance FromJSON NodeProcessStats where
  parseJSON = withObject "NodeProcessStats" parse
    where
      parse o = do
        mem <- o .: "mem"
        cpu <- o .: "cpu"
        NodeProcessStats <$> mem .: "total_virtual_in_bytes"
                         <*> mem .: "share_in_bytes"
                         <*> mem .: "resident_in_bytes"
                         <*> (unMS <$> cpu .: "total_in_millis")
                         <*> (unMS <$> cpu .: "user_in_millis")
                         <*> (unMS <$> cpu .: "sys_in_millis")
                         <*> cpu .: "percent"
                         <*> o .: "open_file_descriptors"
                         <*> (posixMS <$> o .: "timestamp")

instance FromJSON NodeOSStats where
  parseJSON = withObject "NodeOSStats" parse
    where
      parse o = do
        swap <- o .: "swap"
        mem <- o .: "mem"
        cpu <- o .: "cpu"
        load <- o .:? "load_average"
        NodeOSStats <$> swap .: "free_in_bytes"
                    <*> swap .: "used_in_bytes"
                    <*> mem .: "actual_used_in_bytes"
                    <*> mem .: "actual_free_in_bytes"
                    <*> mem .: "used_percent"
                    <*> mem .: "free_percent"
                    <*> mem .: "used_in_bytes"
                    <*> mem .: "free_in_bytes"
                    <*> cpu .: "stolen"
                    <*> cpu .: "usage"
                    <*> cpu .: "idle"
                    <*> cpu .: "user"
                    <*> cpu .: "sys"
                    <*> pure load
                    <*> (unMS <$> o .: "uptime_in_millis")
                    <*> (posixMS <$> o .: "timestamp")

instance FromJSON LoadAvgs where
  parseJSON = withArray "LoadAvgs" parse
    where
      parse v = case V.toList v of
        [one, five, fifteen] -> LoadAvgs <$> parseJSON one
                                         <*> parseJSON five
                                         <*> parseJSON fifteen
        _                    -> fail "Expecting a triple of Doubles"

instance FromJSON NodeIndicesStats where
  parseJSON = withObject "NodeIndicesStats" parse
    where
      parse o = do
        let (.::) mv k = case mv of
              Just v  -> Just <$> v .: k
              Nothing -> pure Nothing
        mRecovery <- o .:? "recovery"
        mQueryCache <- o .:? "query_cache"
        suggest <- o .: "suggest"
        translog <- o .: "translog"
        segments <- o .: "segments"
        completion <- o .: "completion"
        percolate <- o .: "percolate"
        fielddata <- o .: "fielddata"
        idCache <- o .: "id_cache"
        filterCache <- o .: "filter_cache"
        warmer <- o .: "warmer"
        flush <- o .: "flush"
        refresh <- o .: "refresh"
        merges <- o .: "merges"
        search <- o .: "search"
        getStats <- o .: "get"
        indexing <- o .: "indexing"
        store <- o .: "store"
        docs <- o .: "docs"
        NodeIndicesStats <$> (fmap unMS <$> mRecovery .:: "throttle_time_in_millis")
                         <*> mRecovery .:: "current_as_target"
                         <*> mRecovery .:: "current_as_source"
                         <*> mQueryCache .:: "miss_count"
                         <*> mQueryCache .:: "hit_count"
                         <*> mQueryCache .:: "evictions"
                         <*> mQueryCache .:: "memory_size_in_bytes"
                         <*> suggest .: "current"
                         <*> (unMS <$> suggest .: "time_in_millis")
                         <*> suggest .: "total"
                         <*> translog .: "size_in_bytes"
                         <*> translog .: "operations"
                         <*> segments .:? "fixed_bit_set_memory_in_bytes"
                         <*> segments .: "version_map_memory_in_bytes"
                         <*> segments .:? "index_writer_max_memory_in_bytes"
                         <*> segments .: "index_writer_memory_in_bytes"
                         <*> segments .: "memory_in_bytes"
                         <*> segments .: "count"
                         <*> completion .: "size_in_bytes"
                         <*> percolate .: "queries"
                         <*> percolate .: "memory_size_in_bytes"
                         <*> percolate .: "current"
                         <*> (unMS <$> percolate .: "time_in_millis")
                         <*> percolate .: "total"
                         <*> fielddata .: "evictions"
                         <*> fielddata .: "memory_size_in_bytes"
                         <*> idCache .: "memory_size_in_bytes"
                         <*> filterCache .: "evictions"
                         <*> filterCache .: "memory_size_in_bytes"
                         <*> (unMS <$> warmer .: "total_time_in_millis")
                         <*> warmer .: "total"
                         <*> warmer .: "current"
                         <*> (unMS <$> flush .: "total_time_in_millis")
                         <*> flush .: "total"
                         <*> (unMS <$> refresh .: "total_time_in_millis")
                         <*> refresh .: "total"
                         <*> merges .: "total_size_in_bytes"
                         <*> merges .: "total_docs"
                         <*> (unMS <$> merges .: "total_time_in_millis")
                         <*> merges .: "total"
                         <*> merges .: "current_size_in_bytes"
                         <*> merges .: "current_docs"
                         <*> merges .: "current"
                         <*> search .: "fetch_current"
                         <*> (unMS <$> search .: "fetch_time_in_millis")
                         <*> search .: "fetch_total"
                         <*> search .: "query_current"
                         <*> (unMS <$> search .: "query_time_in_millis")
                         <*> search .: "query_total"
                         <*> search .: "open_contexts"
                         <*> getStats .: "current"
                         <*> (unMS <$> getStats .: "missing_time_in_millis")
                         <*> getStats .: "missing_total"
                         <*> (unMS <$> getStats .: "exists_time_in_millis")
                         <*> getStats .: "exists_total"
                         <*> (unMS <$> getStats .: "time_in_millis")
                         <*> getStats .: "total"
                         <*> (fmap unMS <$> indexing .:? "throttle_time_in_millis")
                         <*> indexing .:? "is_throttled"
                         <*> indexing .:? "noop_update_total"
                         <*> indexing .: "delete_current"
                         <*> (unMS <$> indexing .: "delete_time_in_millis")
                         <*> indexing .: "delete_total"
                         <*> indexing .: "index_current"
                         <*> (unMS <$> indexing .: "index_time_in_millis")
                         <*> indexing .: "index_total"
                         <*> (unMS <$> store .: "throttle_time_in_millis")
                         <*> store .: "size_in_bytes"
                         <*> docs .: "deleted"
                         <*> docs .: "count"

instance FromJSON NodeBreakersStats where
  parseJSON = withObject "NodeBreakersStats" parse
    where
      parse o = NodeBreakersStats <$> o .: "parent"
                                  <*> o .: "request"
                                  <*> o .: "fielddata"

parseNodeStats :: FullNodeId -> Object -> Parser NodeStats
parseNodeStats fnid o = do
  NodeStats <$> o .: "name"
            <*> pure fnid
            <*> o .:? "breakers"
            <*> o .: "http"
            <*> o .: "transport"
            <*> o .: "fs"
            <*> o .: "network"
            <*> o .: "thread_pool"
            <*> o .: "jvm"
            <*> o .: "process"
            <*> o .: "os"
            <*> o .: "indices"

parseNodeInfo :: FullNodeId -> Object -> Parser NodeInfo
parseNodeInfo nid o =
  NodeInfo <$> o .: "http_address"
           <*> o .: "build"
           <*> o .: "version"
           <*> o .: "ip"
           <*> o .: "host"
           <*> o .: "transport_address"
           <*> o .: "name"
           <*> pure nid
           <*> o .: "plugins"
           <*> o .: "http"
           <*> o .: "transport"
           <*> o .: "network"
           <*> o .: "thread_pool"
           <*> o .: "jvm"
           <*> o .: "process"
           <*> o .: "os"
           <*> o .: "settings"

instance FromJSON NodePluginInfo where
  parseJSON = withObject "NodePluginInfo" parse
    where
      parse o = NodePluginInfo <$> o .:  "site"
                               <*> o .:  "jvm"
                               <*> o .:  "description"
                               <*> o .: "version"
                               <*> o .:  "name"

instance FromJSON NodeHTTPInfo where
  parseJSON = withObject "NodeHTTPInfo" parse
    where
      parse o = NodeHTTPInfo <$> o .: "max_content_length_in_bytes"
                             <*> parseJSON (Object o)

instance FromJSON BoundTransportAddress where
  parseJSON = withObject "BoundTransportAddress" parse
    where
      parse o = BoundTransportAddress <$> o .: "publish_address"
                                      <*> o .: "bound_address"

instance FromJSON NodeOSInfo where
  parseJSON = withObject "NodeOSInfo" parse
    where
      parse o = do
        swap <- o .: "swap"
        mem <- o .: "mem"
        NodeOSInfo <$> swap .: "total_in_bytes"
                   <*> mem .: "total_in_bytes"
                   <*> o .: "cpu"
                   <*> o .: "available_processors"
                   <*> (unMS <$> o .: "refresh_interval_in_millis")


instance FromJSON CPUInfo where
  parseJSON = withObject "CPUInfo" parse
    where
      parse o = CPUInfo <$> o .: "cache_size_in_bytes"
                        <*> o .: "cores_per_socket"
                        <*> o .: "total_sockets"
                        <*> o .: "total_cores"
                        <*> o .: "mhz"
                        <*> o .: "model"
                        <*> o .: "vendor"

instance FromJSON NodeProcessInfo where
  parseJSON = withObject "NodeProcessInfo" parse
    where
      parse o = NodeProcessInfo <$> o .: "mlockall"
                                <*> o .: "max_file_descriptors"
                                <*> o .: "id"
                                <*> (unMS <$> o .: "refresh_interval_in_millis")

instance FromJSON NodeJVMInfo where
  parseJSON = withObject "NodeJVMInfo" parse
    where
      parse o = NodeJVMInfo <$> o .: "memory_pools"
                            <*> o .: "gc_collectors"
                            <*> o .: "mem"
                            <*> (posixMS <$> o .: "start_time_in_millis")
                            <*> o .: "vm_vendor"
                            <*> o .: "vm_version"
                            <*> o .: "vm_name"
                            <*> (unJVMVersion <$> o .: "version")
                            <*> o .: "pid"

instance FromJSON JVMVersion where
  parseJSON (String t) =
    JVMVersion <$> parseJSON (String (T.replace "_" "." t))
  parseJSON v = JVMVersion <$> parseJSON v

instance FromJSON JVMMemoryInfo where
  parseJSON = withObject "JVMMemoryInfo" parse
    where
      parse o = JVMMemoryInfo <$> o .: "direct_max_in_bytes"
                              <*> o .: "non_heap_max_in_bytes"
                              <*> o .: "non_heap_init_in_bytes"
                              <*> o .: "heap_max_in_bytes"
                              <*> o .: "heap_init_in_bytes"

instance FromJSON NodeThreadPoolsInfo where
  parseJSON = withObject "NodeThreadPoolsInfo" parse
    where
      parse o = NodeThreadPoolsInfo <$> o .: "refresh"
                                    <*> o .: "management"
                                    <*> o .: "percolate"
                                    <*> o .:? "listener"
                                    <*> o .:? "fetch_shard_started"
                                    <*> o .: "search"
                                    <*> o .: "flush"
                                    <*> o .: "warmer"
                                    <*> o .: "optimize"
                                    <*> o .: "bulk"
                                    <*> o .: "suggest"
                                    <*> o .: "merge"
                                    <*> o .: "snapshot"
                                    <*> o .: "get"
                                    <*> o .:? "fetch_shard_store"
                                    <*> o .: "index"
                                    <*> o .: "generic"

instance FromJSON NodeThreadPoolInfo where
  parseJSON = withObject "NodeThreadPoolInfo" parse
    where
      parse o = do
        ka <- maybe (return Nothing) (fmap Just . parseStringInterval) =<< o .:? "keep_alive"
        NodeThreadPoolInfo <$> (parseJSON . unStringlyTypeJSON =<< o .: "queue_size")
                           <*> pure ka
                           <*> o .:? "min"
                           <*> o .:? "max"
                           <*> o .: "type"

parseStringInterval :: (Monad m) => String -> m NominalDiffTime
parseStringInterval s = case span isNumber s of
  ("", _) -> fail "Invalid interval"
  (nS, unitS) -> case (readMay nS, readMay unitS) of
    (Just n, Just unit) -> return (fromInteger (n * unitNDT unit))
    (Nothing, _)        -> fail "Invalid interval number"
    (_, Nothing)        -> fail "Invalid interval unit"
  where
    unitNDT Seconds = 1
    unitNDT Minutes = 60
    unitNDT Hours   = 60 * 60
    unitNDT Days    = 24 * 60 * 60
    unitNDT Weeks   = 7 * 24 * 60 * 60

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
          (Just n, "")  -> return (ThreadPoolBounded n)
          _             -> fail ("Invalid thread pool size " <> T.unpack t)

instance FromJSON ThreadPoolType where
  parseJSON = withText "ThreadPoolType" parse
    where
      parse "scaling" = return ThreadPoolScaling
      parse "fixed"   = return ThreadPoolFixed
      parse "cached"  = return ThreadPoolCached
      parse e         = fail ("Unexpected thread pool type" <> T.unpack e)

instance FromJSON NodeTransportInfo where
  parseJSON = withObject "NodeTransportInfo" parse
    where
      parse o = NodeTransportInfo <$> (maybe (return mempty) parseProfiles =<< o .:? "profiles")
                                  <*> parseJSON (Object o)
      parseProfiles (Object o)  | HM.null o = return []
      parseProfiles v@(Array _) = parseJSON v
      parseProfiles Null        = return []
      parseProfiles _           = fail "Could not parse profiles"

instance FromJSON NodeNetworkInfo where
  parseJSON = withObject "NodeNetworkInfo" parse
    where
      parse o = NodeNetworkInfo <$> o .: "primary_interface"
                                <*> (unMS <$> o .: "refresh_interval_in_millis")


instance FromJSON NodeNetworkInterface where
  parseJSON = withObject "NodeNetworkInterface" parse
    where
      parse o = NodeNetworkInterface <$> o .: "mac_address"
                                     <*> o .: "name"
                                     <*> o .: "address"

newtype MaybeNA a = MaybeNA { unMaybeNA :: Maybe a }
  deriving (Show, Eq)

instance FromJSON a => FromJSON (MaybeNA a) where
  parseJSON (String "NA") = pure $ MaybeNA Nothing
  parseJSON o             = MaybeNA . Just <$> parseJSON o


data Suggest = Suggest { suggestText :: Text
                       , suggestName :: Text
                       , suggestType :: SuggestType
                       }
 deriving (Show, Generic, Eq, Read, Typeable)

instance ToJSON Suggest where
  toJSON Suggest{..} = object [ "text" .= suggestText
                              , suggestName .= suggestType
                              ]

instance FromJSON Suggest where
  parseJSON (Object o) = do
    suggestText' <- o .: "text"
    let dropTextList = HM.toList $ HM.filterWithKey (\x _ -> x /= "text") o
    suggestName' <- case dropTextList of
                        [(x, _)] -> return x
                        _ -> fail "error parsing Suggest field name"
    suggestType' <- o .: suggestName'
    return $ Suggest suggestText' suggestName' suggestType'
  parseJSON x = typeMismatch "Suggest" x

data SuggestType = SuggestTypePhraseSuggester PhraseSuggester
  deriving (Show, Generic, Eq, Read, Typeable)

instance ToJSON SuggestType where
  toJSON (SuggestTypePhraseSuggester x) = object ["phrase" .= x]

instance FromJSON SuggestType where
  parseJSON = withObject "SuggestType" parse
    where parse o = phraseSuggester `taggedWith` "phrase"
           where taggedWith parser k = parser =<< o .: k
                 phraseSuggester = pure . SuggestTypePhraseSuggester

data PhraseSuggester =
  PhraseSuggester { phraseSuggesterField :: FieldName
                  , phraseSuggesterGramSize :: Maybe Int
                  , phraseSuggesterRealWordErrorLikelihood :: Maybe Int
                  , phraseSuggesterConfidence :: Maybe Int
                  , phraseSuggesterMaxErrors :: Maybe Int
                  , phraseSuggesterSeparator :: Maybe Text
                  , phraseSuggesterSize :: Maybe Size
                  , phraseSuggesterAnalyzer :: Maybe Analyzer
                  , phraseSuggesterShardSize :: Maybe Int
                  , phraseSuggesterHighlight :: Maybe PhraseSuggesterHighlighter
                  , phraseSuggesterCollate :: Maybe PhraseSuggesterCollate
                  , phraseSuggesterCandidateGenerators :: [DirectGenerators]
                  }
  deriving (Show, Generic, Eq, Read, Typeable)

instance ToJSON PhraseSuggester where
  toJSON PhraseSuggester{..} = omitNulls [ "field" .= phraseSuggesterField
                                         , "gram_size" .= phraseSuggesterGramSize
                                         , "real_word_error_likelihood" .= phraseSuggesterRealWordErrorLikelihood
                                         , "confidence" .= phraseSuggesterConfidence
                                         , "max_errors" .= phraseSuggesterMaxErrors
                                         , "separator" .= phraseSuggesterSeparator
                                         , "size" .= phraseSuggesterSize
                                         , "analyzer" .= phraseSuggesterAnalyzer
                                         , "shard_size" .= phraseSuggesterShardSize
                                         , "highlight" .= phraseSuggesterHighlight
                                         , "collate" .= phraseSuggesterCollate
                                         , "direct_generator" .= phraseSuggesterCandidateGenerators
                                        ]

instance FromJSON PhraseSuggester where
  parseJSON = withObject "PhraseSuggester" parse
    where parse o = PhraseSuggester
                      <$> o .: "field"
                      <*> o .:? "gram_size"
                      <*> o .:? "real_word_error_likelihood"
                      <*> o .:? "confidence"
                      <*> o .:? "max_errors"
                      <*> o .:? "separator"
                      <*> o .:? "size"
                      <*> o .:? "analyzer"
                      <*> o .:? "shard_size"
                      <*> o .:? "highlight"
                      <*> o .:? "collate"
                      <*> o .:? "direct_generator" .!= []

mkPhraseSuggester :: FieldName -> PhraseSuggester
mkPhraseSuggester fName =
  PhraseSuggester fName Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing []

data PhraseSuggesterHighlighter =
  PhraseSuggesterHighlighter { phraseSuggesterHighlighterPreTag :: Text
                             , phraseSuggesterHighlighterPostTag :: Text
                             }
  deriving (Show, Generic, Eq, Read, Typeable)

instance ToJSON PhraseSuggesterHighlighter where
  toJSON PhraseSuggesterHighlighter{..} =
            object [ "pre_tag" .= phraseSuggesterHighlighterPreTag
                   , "post_tag" .= phraseSuggesterHighlighterPostTag
                   ]

instance FromJSON PhraseSuggesterHighlighter where
  parseJSON = withObject "PhraseSuggesterHighlighter" parse
    where parse o = PhraseSuggesterHighlighter
                      <$> o .: "pre_tag"
                      <*> o .: "post_tag"

data PhraseSuggesterCollate =
  PhraseSuggesterCollate { phraseSuggesterCollateTemplateQuery :: TemplateQueryInline
                         , phraseSuggesterCollatePrune :: Bool
                         }
  deriving (Show, Generic, Eq, Read, Typeable)

instance ToJSON PhraseSuggesterCollate where
  toJSON PhraseSuggesterCollate{..} = object [ "query" .= object
                                              [ "inline" .= (inline phraseSuggesterCollateTemplateQuery)
                                              ]
                                             , "params" .= (params phraseSuggesterCollateTemplateQuery)
                                             , "prune" .= phraseSuggesterCollatePrune
                                             ]

instance FromJSON PhraseSuggesterCollate where
  parseJSON (Object o) = do
    query' <- o .: "query"
    inline' <- query' .: "inline"
    params' <- o .: "params"
    prune' <- o .:? "prune" .!= False
    return $ PhraseSuggesterCollate (TemplateQueryInline inline' params') prune'
  parseJSON x = typeMismatch "PhraseSuggesterCollate" x

data SuggestOptions =
  SuggestOptions { suggestOptionsText :: Text
                 , suggestOptionsScore :: Double
                 , suggestOptionsFreq :: Maybe Int
                 , suggestOptionsHighlighted :: Maybe Text
                 }
  deriving (Eq, Read, Show)

instance FromJSON SuggestOptions where
  parseJSON = withObject "SuggestOptions" parse
    where parse o = SuggestOptions
                    <$> o .: "text"
                    <*> o .: "score"
                    <*> o .:? "freq"
                    <*> o .:? "highlighted"

data SuggestResponse =
  SuggestResponse { suggestResponseText :: Text
                  , suggestResponseOffset :: Int
                  , suggestResponseLength :: Int
                  , suggestResponseOptions :: [SuggestOptions]
                  }
  deriving (Eq, Read, Show)

instance FromJSON SuggestResponse where
  parseJSON = withObject "SuggestResponse" parse
    where parse o = SuggestResponse
                    <$> o .: "text"
                    <*> o .: "offset"
                    <*> o .: "length"
                    <*> o .: "options"

data NamedSuggestionResponse =
  NamedSuggestionResponse { nsrName :: Text
                          , nsrResponses :: [SuggestResponse]
                          }
  deriving (Eq, Read, Show)

instance FromJSON NamedSuggestionResponse where
  parseJSON (Object o) = do
    suggestionName' <- case HM.toList o of
                        [(x, _)] -> return x
                        _ -> fail "error parsing NamedSuggestionResponse name"
    suggestionResponses' <- o .: suggestionName'
    return $ NamedSuggestionResponse suggestionName' suggestionResponses'

  parseJSON x = typeMismatch "NamedSuggestionResponse" x

data DirectGeneratorSuggestModeTypes = DirectGeneratorSuggestModeMissing
                                | DirectGeneratorSuggestModePopular
                                | DirectGeneratorSuggestModeAlways
  deriving (Show, Eq, Read, Generic, Typeable)

instance ToJSON DirectGeneratorSuggestModeTypes where
  toJSON DirectGeneratorSuggestModeMissing = "missing"
  toJSON DirectGeneratorSuggestModePopular = "popular"
  toJSON DirectGeneratorSuggestModeAlways = "always"

instance FromJSON DirectGeneratorSuggestModeTypes where
  parseJSON = withText "DirectGeneratorSuggestModeTypes" parse
    where parse "missing"        = pure DirectGeneratorSuggestModeMissing
          parse "popular"       = pure DirectGeneratorSuggestModePopular
          parse "always"        = pure DirectGeneratorSuggestModeAlways
          parse f            = fail ("Unexpected DirectGeneratorSuggestModeTypes: " <> show f)

data DirectGenerators = DirectGenerators
  { directGeneratorsField :: FieldName
  , directGeneratorsSize :: Maybe Int
  , directGeneratorSuggestMode :: DirectGeneratorSuggestModeTypes
  , directGeneratorMaxEdits :: Maybe Double
  , directGeneratorPrefixLength :: Maybe Int
  , directGeneratorMinWordLength :: Maybe Int
  , directGeneratorMaxInspections :: Maybe Int
  , directGeneratorMinDocFreq :: Maybe Double
  , directGeneratorMaxTermFreq :: Maybe Double
  , directGeneratorPreFilter :: Maybe Text
  , directGeneratorPostFilter :: Maybe Text
  }
  deriving (Show, Eq, Read, Generic, Typeable)


instance ToJSON DirectGenerators where
  toJSON DirectGenerators{..} = omitNulls [ "field" .= directGeneratorsField
                                         , "size" .= directGeneratorsSize
                                         , "suggest_mode" .= directGeneratorSuggestMode
                                         , "max_edits" .= directGeneratorMaxEdits
                                         , "prefix_length" .= directGeneratorPrefixLength
                                         , "min_word_length" .= directGeneratorMinWordLength
                                         , "max_inspections" .= directGeneratorMaxInspections
                                         , "min_doc_freq" .= directGeneratorMinDocFreq
                                         , "max_term_freq" .= directGeneratorMaxTermFreq
                                         , "pre_filter" .= directGeneratorPreFilter
                                         , "post_filter" .= directGeneratorPostFilter
                                        ]

instance FromJSON DirectGenerators where
  parseJSON = withObject "DirectGenerators" parse
    where parse o = DirectGenerators
                      <$> o .: "field"
                      <*> o .:? "size"
                      <*> o .:  "suggest_mode"
                      <*> o .:? "max_edits"
                      <*> o .:? "prefix_length"
                      <*> o .:? "min_word_length"
                      <*> o .:? "max_inspections"
                      <*> o .:? "min_doc_freq"
                      <*> o .:? "max_term_freq"
                      <*> o .:? "pre_filter"
                      <*> o .:? "post_filter"

mkDirectGenerators :: FieldName -> DirectGenerators
mkDirectGenerators fn = DirectGenerators fn Nothing DirectGeneratorSuggestModeMissing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 
