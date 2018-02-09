{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
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
-- Portability : RecordWildCards
--
-- Data types for describing actions and data structures performed to interact
-- with Elasticsearch. The two main buckets your queries against Elasticsearch
-- will fall into are 'Query's and 'Filter's. 'Filter's are more like
-- traditional database constraints and often have preferable performance
-- properties. 'Query's support human-written textual queries, such as fuzzy
-- queries.
-------------------------------------------------------------------------------



module Database.V5.Bloodhound.Types
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
       , mkStatsAggregation
       , mkExtendedStatsAggregation
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
       , Compression(..)
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
       , HitFields(..)
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
       , ScriptFields(..)
       , ScriptFieldName
       , ScriptFieldValue
       , Script(..)
       , ScriptLanguage(..)
       , ScriptInline(..)
       , ScriptId(..)
       , ScriptParams(..)
       , ScriptParamName
       , ScriptParamValue
       , IndexName(..)
       , IndexSelection(..)
       , NodeSelection(..)
       , NodeSelector(..)
       , ForceMergeIndexSettings(..)
       , defaultForceMergeIndexSettings
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
       , FunctionScoreQuery(..)
       , BoostMode(..)
       , ScoreMode(..)
       , FunctionScoreFunctions(..)
       , ComponentFunctionScoreFunction(..)
       , FunctionScoreFunction(..)
       , Weight(..)
       , Seed(..)
       , FieldValueFactor(..)
       , Factor(..)
       , FactorModifier(..)
       , FactorMissingFieldValue(..)
       , DisMaxQuery(..)
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
       , Tokenizer(..)
       , TokenFilter(..)
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
       , StatisticsAggregation(..)

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

       , Analysis(..)
       , AnalyzerDefinition(..)
       , TokenizerDefinition(..)
       , TokenFilterDefinition(..)
       , Ngram(..)
       , TokenChar(..)
       , Shingle(..)
       , Language(..)
         ) where

import           Bloodhound.Import

import qualified Data.Traversable                      as DT
import qualified Data.HashMap.Strict                   as HM
import qualified Data.Map.Strict                       as M
import qualified Data.Text                             as T

import           Database.V5.Bloodhound.Types.Class
import           Database.V5.Bloodhound.Internal.Analysis
import           Database.V5.Bloodhound.Internal.Client
import           Database.V5.Bloodhound.Internal.Newtypes
import           Database.V5.Bloodhound.Internal.Query
import           Database.V5.Bloodhound.Internal.StringlyTyped

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
              | GeoDistanceSortSpec SortOrder GeoPoint DistanceUnit deriving (Eq, Show)

{-| 'DefaultSort' is usually the kind of 'SortSpec' you'll want. There's a
    'mkSort' convenience function for when you want to specify only the most
    common parameters.

    The `ignoreUnmapped`, when `Just` field is used to set the elastic 'unmapped_type'

<http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/search-request-sort.html#search-request-sort>
-}
data DefaultSort =
  DefaultSort { sortFieldName  :: FieldName
              , sortOrder      :: SortOrder
                                  -- default False
              , ignoreUnmapped :: Maybe Text
              , sortMode       :: Maybe SortMode
              , missingSort    :: Maybe Missing
              , nestedFilter   :: Maybe Filter } deriving (Eq, Show)

{-| 'SortOrder' is 'Ascending' or 'Descending', as you might expect. These get
    encoded into "asc" or "desc" when turned into JSON.

<http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/search-request-sort.html#search-request-sort>
-}
data SortOrder = Ascending
               | Descending deriving (Eq, Show)

{-| 'Missing' prescribes how to handle missing fields. A missing field can be
    sorted last, first, or using a custom value as a substitute.

<http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/search-request-sort.html#_missing_values>
-}
data Missing = LastMissing
             | FirstMissing
             | CustomMissing Text deriving (Eq, Show)

{-| 'SortMode' prescribes how to handle sorting array/multi-valued fields.

http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/search-request-sort.html#_sort_mode_option
-}
data SortMode = SortMin
              | SortMax
              | SortSum
              | SortAvg deriving (Eq, Show)

{-| 'mkSort' defaults everything but the 'FieldName' and the 'SortOrder' so
    that you can concisely describe the usual kind of 'SortSpec's you want.
-}
mkSort :: FieldName -> SortOrder -> DefaultSort
mkSort fieldName sOrder = DefaultSort fieldName sOrder Nothing Nothing Nothing Nothing

{-| 'unpackId' is a silly convenience function that gets used once.
-}
unpackId :: DocId -> Text
unpackId (DocId docId) = docId

type TrackSortScores = Bool
newtype From = From Int deriving (Eq, Show, ToJSON)
newtype Size = Size Int deriving (Eq, Show, ToJSON, FromJSON)

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
                     , scriptFields    :: Maybe ScriptFields
                     , source          :: Maybe Source
                     , suggestBody     :: Maybe Suggest -- ^ Only one Suggestion request / response per Search is supported.
                     } deriving (Eq, Show)

data SearchType = SearchTypeQueryThenFetch
                | SearchTypeDfsQueryThenFetch
  deriving (Eq, Show)

data Source =
    NoSource
  | SourcePatterns PatternOrPatterns
  | SourceIncludeExclude Include Exclude
    deriving (Eq, Show)

data PatternOrPatterns = PopPattern   Pattern
                       | PopPatterns [Pattern] deriving (Eq, Read, Show)

data Include = Include [Pattern] deriving (Eq, Read, Show)
data Exclude = Exclude [Pattern] deriving (Eq, Read, Show)

newtype Pattern = Pattern Text deriving (Eq, Read, Show)

data Highlights = Highlights
  { globalsettings  :: Maybe HighlightSettings
  , highlightFields :: [FieldHighlight]
  } deriving (Eq, Show)

data FieldHighlight =
  FieldHighlight FieldName (Maybe HighlightSettings)
  deriving (Eq, Show)


data HighlightSettings =
    Plain PlainHighlight
  | Postings PostingsHighlight
  | FastVector FastVectorHighlight
  deriving (Eq, Show)

data PlainHighlight =
  PlainHighlight { plainCommon  :: Maybe CommonHighlight
                 , plainNonPost :: Maybe NonPostings }
  deriving (Eq, Show)

 -- This requires that index_options are set to 'offset' in the mapping.
data PostingsHighlight =
  PostingsHighlight (Maybe CommonHighlight)
  deriving (Eq, Show)

-- This requires that term_vector is set to 'with_positions_offsets' in the mapping.
data FastVectorHighlight =
    FastVectorHighlight { fvCommon          :: Maybe CommonHighlight
                        , fvNonPostSettings :: Maybe NonPostings
                        , boundaryChars     :: Maybe Text
                        , boundaryMaxScan   :: Maybe Int
                        , fragmentOffset    :: Maybe Int
                        , matchedFields     :: [Text]
                        , phraseLimit       :: Maybe Int
                        } deriving (Eq, Show)

data CommonHighlight =
    CommonHighlight { order             :: Maybe Text
                    , forceSource       :: Maybe Bool
                    , tag               :: Maybe HighlightTag
                    , encoder           :: Maybe HighlightEncoder
                    , noMatchSize       :: Maybe Int
                    , highlightQuery    :: Maybe Query
                    , requireFieldMatch :: Maybe Bool
                    } deriving (Eq, Show)

-- Settings that are only applicable to FastVector and Plain highlighters.
data NonPostings =
    NonPostings { fragmentSize      :: Maybe Int
                , numberOfFragments :: Maybe Int
                } deriving (Eq, Show)

data HighlightEncoder = DefaultEncoder
                      | HTMLEncoder
                      deriving (Eq, Show)

-- NOTE: Should the tags use some kind of HTML type, rather than Text?
data HighlightTag =
    TagSchema Text
    -- Only uses more than the first value in the lists if fvh
  | CustomTags ([Text], [Text]) 
  deriving (Eq, Show)

data SearchResult a =
  SearchResult { took         :: Int
               , timedOut     :: Bool
               , shards       :: ShardResult
               , searchHits   :: SearchHits a
               , aggregations :: Maybe AggregationResults
               , scrollId     :: Maybe ScrollId
               -- ^ Only one Suggestion request / response per Search is supported.
               , suggest      :: Maybe NamedSuggestionResponse
               }
  deriving (Eq, Show)

newtype ScrollId =
  ScrollId Text
  deriving (Eq, Show, Ord, ToJSON, FromJSON)

data SearchHits a =
  SearchHits { hitsTotal :: Int
             , maxScore  :: Score
             , hits      :: [Hit a] } deriving (Eq, Show)

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
      , hitFields    :: Maybe HitFields
      , hitHighlight :: Maybe HitHighlight } deriving (Eq, Show)

newtype HitFields =
  HitFields (M.Map Text [Value])
  deriving (Eq, Show)

type HitHighlight = M.Map Text [Text]


type Aggregations = M.Map Text Aggregation

emptyAggregations :: Aggregations
emptyAggregations = M.empty

mkAggregations :: Text -> Aggregation -> Aggregations
mkAggregations name aggregation = M.insert name aggregation emptyAggregations

data TermOrder = TermOrder{ termSortField :: Text
                          , termSortOrder :: SortOrder } deriving (Eq, Show)

data TermInclusion = TermInclusion Text
                   | TermPattern Text Text deriving (Eq, Show)

data CollectionMode = BreadthFirst
                    | DepthFirst deriving (Eq, Show)

data ExecutionHint = Ordinals
                   | GlobalOrdinals
                   | GlobalOrdinalsHash
                   | GlobalOrdinalsLowCardinality
                   | Map deriving (Eq, Show)

data Aggregation = TermsAgg TermsAggregation
                 | CardinalityAgg CardinalityAggregation
                 | DateHistogramAgg DateHistogramAggregation
                 | ValueCountAgg ValueCountAggregation
                 | FilterAgg FilterAggregation
                 | DateRangeAgg DateRangeAggregation
                 | MissingAgg MissingAggregation
                 | TopHitsAgg TopHitsAggregation
                 | StatsAgg StatisticsAggregation
  deriving (Eq, Show)

data TopHitsAggregation = TopHitsAggregation
  { taFrom :: Maybe From
  , taSize :: Maybe Size
  , taSort :: Maybe Sort
  } deriving (Eq, Show)

data MissingAggregation = MissingAggregation
  { maField :: Text
  } deriving (Eq, Show)

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
                                    } deriving (Eq, Show)

data CardinalityAggregation = CardinalityAggregation
  { cardinalityField   :: FieldName,
    precisionThreshold :: Maybe Int
  } deriving (Eq, Show)

data DateHistogramAggregation = DateHistogramAggregation
  { dateField      :: FieldName
  , dateInterval   :: Interval
  , dateFormat     :: Maybe Text
    -- pre and post deprecated in 1.5
  , datePreZone    :: Maybe Text
  , datePostZone   :: Maybe Text
  , datePreOffset  :: Maybe Text
  , datePostOffset :: Maybe Text
  , dateAggs       :: Maybe Aggregations
  } deriving (Eq, Show)

data DateRangeAggregation = DateRangeAggregation
  { draField  :: FieldName
  , draFormat :: Maybe Text
  , draRanges :: NonEmpty DateRangeAggRange
  } deriving (Eq, Show)

data DateRangeAggRange =
    DateRangeFrom DateMathExpr
  | DateRangeTo DateMathExpr
  | DateRangeFromAndTo DateMathExpr DateMathExpr
  deriving (Eq, Show)

-- | See <https://www.elastic.co/guide/en/elasticsearch/reference/current/common-options.html#date-math> for more information.
data DateMathExpr =
  DateMathExpr DateMathAnchor [DateMathModifier]
  deriving (Eq, Show)


-- | Starting point for a date range. This along with the 'DateMathModifiers' gets you the date ES will start from.
data DateMathAnchor =
    DMNow
  | DMDate Day
  deriving (Eq, Show)

data DateMathModifier =
    AddTime Int DateMathUnit
  | SubtractTime Int DateMathUnit
  | RoundDownTo DateMathUnit
  deriving (Eq, Show)

data DateMathUnit =
    DMYear
  | DMMonth
  | DMWeek
  | DMDay
  | DMHour
  | DMMinute
  | DMSecond
  deriving (Eq, Show)

-- | See <https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-metrics-valuecount-aggregation.html> for more information.
data ValueCountAggregation =
    FieldValueCount FieldName
  | ScriptValueCount Script
  deriving (Eq, Show)

-- | Single-bucket filter aggregations. See <https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-bucket-filter-aggregation.html#search-aggregations-bucket-filter-aggregation> for more information.
data FilterAggregation = FilterAggregation
  { faFilter :: Filter
  , faAggs   :: Maybe Aggregations }
  deriving (Eq, Show)

data StatisticsAggregation = StatisticsAggregation
  { statsType :: StatsType
  , statsField :: FieldName }
  deriving (Eq, Show)

data StatsType
  = Basic
  | Extended
  deriving (Eq, Show)

mkTermsAggregation :: Text -> TermsAggregation
mkTermsAggregation t =
  TermsAggregation (Left t)
  Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

mkTermsScriptAggregation :: Text -> TermsAggregation
mkTermsScriptAggregation t = TermsAggregation (Right t) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

mkDateHistogram :: FieldName -> Interval -> DateHistogramAggregation
mkDateHistogram t i = DateHistogramAggregation t i Nothing Nothing Nothing Nothing Nothing Nothing

mkCardinalityAggregation :: FieldName -> CardinalityAggregation
mkCardinalityAggregation t = CardinalityAggregation t Nothing

mkStatsAggregation :: FieldName -> StatisticsAggregation
mkStatsAggregation = StatisticsAggregation Basic

mkExtendedStatsAggregation :: FieldName -> StatisticsAggregation
mkExtendedStatsAggregation = StatisticsAggregation Extended

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
  toJSON Year    = "year"
  toJSON Quarter = "quarter"
  toJSON Month   = "month"
  toJSON Week    = "week"
  toJSON Day     = "day"
  toJSON Hour    = "hour"
  toJSON Minute  = "minute"
  toJSON Second  = "second"

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

  toJSON (DateHistogramAgg
          (DateHistogramAggregation field interval format
           preZone postZone preOffset postOffset dateHistoAggs)) =
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
                (FieldValueCount (FieldName n)) ->
                  object ["field" .= n]
                (ScriptValueCount s) ->
                  object ["script" .= s]
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

  toJSON (StatsAgg (StatisticsAggregation typ field)) =
    object [stType .= omitNulls [ "field" .= field ]]
    where
      stType | typ == Basic = "stats"
             | otherwise = "extended_stats"

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

data DateRangeResult =
  DateRangeResult { dateRangeKey          :: Text
                  , dateRangeFrom         :: Maybe UTCTime
                  , dateRangeFromAsString :: Maybe Text
                  , dateRangeTo           :: Maybe UTCTime
                  , dateRangeToAsString   :: Maybe Text
                  , dateRangeDocCount     :: Int
                  , dateRangeAggs         :: Maybe AggregationResults }
  deriving (Eq, Show)

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

instance (FromJSON a) => FromJSON (TopHitResult a) where
  parseJSON (Object v) = TopHitResult <$>
                         v .: "hits"
  parseJSON _          = fail "Failure in FromJSON (TopHitResult a)"

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

instance FromJSON Status where
  parseJSON (Object v) = Status <$>
                         v .: "name" <*>
                         v .: "cluster_name" <*>
                         v .: "cluster_uuid" <*>
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
  toJSON (CompressionSetting x) = oPath ("index" :| ["codec"]) x
  toJSON (IndexCompoundFormat x) = oPath ("index" :| ["compound_format"]) x
  toJSON (IndexCompoundOnFlush x) = oPath ("index" :| ["compound_on_flush"]) x
  toJSON (WarmerEnabled x) = oPath ("index" :| ["warmer", "enabled"]) x
  toJSON (BlocksReadOnly x) = oPath ("blocks" :| ["read_only"]) x
  toJSON (BlocksRead x) = oPath ("blocks" :| ["read"]) x
  toJSON (BlocksWrite x) = oPath ("blocks" :| ["write"]) x
  toJSON (BlocksMetaData x) = oPath ("blocks" :| ["metadata"]) x
  toJSON (MappingTotalFieldsLimit x) = oPath ("index" :| ["mapping","total_fields","limit"]) x
  toJSON (AnalysisSetting x) = oPath ("index" :| ["analysis"]) x

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
                <|> compressionSetting `taggedAt` ["index", "codec"]
                <|> compoundFormat `taggedAt` ["index", "compound_format"]
                <|> compoundOnFlush `taggedAt` ["index", "compound_on_flush"]
                <|> warmerEnabled `taggedAt` ["index", "warmer", "enabled"]
                <|> blocksReadOnly `taggedAt` ["blocks", "read_only"]
                <|> blocksRead `taggedAt` ["blocks", "read"]
                <|> blocksWrite `taggedAt` ["blocks", "write"]
                <|> blocksMetaData `taggedAt` ["blocks", "metadata"]
                <|> mappingTotalFieldsLimit `taggedAt` ["index", "mapping", "total_fields", "limit"]
                <|> analysisSetting `taggedAt` ["index", "analysis"]
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
          compressionSetting             = pure . CompressionSetting
          compoundFormat                 = pure . IndexCompoundFormat
          compoundOnFlush                = pure . IndexCompoundOnFlush
          warmerEnabled                  = pure . WarmerEnabled
          blocksReadOnly                 = pure . BlocksReadOnly
          blocksRead                     = pure . BlocksRead
          blocksWrite                    = pure . BlocksWrite
          blocksMetaData                 = pure . BlocksMetaData
          mappingTotalFieldsLimit        = pure . MappingTotalFieldsLimit
          analysisSetting                = pure . AnalysisSetting

instance FromJSON IndexSettingsSummary where
  parseJSON = withObject "IndexSettingsSummary" parse
    where parse o = case HM.toList o of
                      [(ixn, v@(Object o'))] -> IndexSettingsSummary (IndexName ixn)
                                                <$> parseJSON v
                                                <*> (fmap (filter (not . redundant)) . parseSettings =<< o' .: "settings")
                      _ -> fail "Expected single-key object with index name"
          redundant (NumberOfReplicas _) = True
          redundant _                    = False


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
  toJSON (Search mquery sFilter sort searchAggs
          highlight sTrackSortScores sFrom sSize _ sFields
          sScriptFields sSource sSuggest) =
    omitNulls [ "query"         .= query'
              , "sort"          .= sort
              , "aggregations"  .= searchAggs
              , "highlight"     .= highlight
              , "from"          .= sFrom
              , "size"          .= sSize
              , "track_scores"  .= sTrackSortScores
              , "fields"        .= sFields
              , "script_fields" .= sScriptFields
              , "_source"       .= sSource
              , "suggest"       .= sSuggest]

    where query' = case sFilter of
                    Nothing -> mquery
                    Just x ->
                        Just
                      . QueryBoolQuery
                      $ mkBoolQuery (maybeToList mquery)
                        [x] [] []

instance ToJSON Source where
    toJSON NoSource                         = toJSON False
    toJSON (SourcePatterns patterns)        = toJSON patterns
    toJSON (SourceIncludeExclude incl excl) = object [ "includes" .= incl, "excludes" .= excl ]

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
             , "unmapped_type" .= dsIgnoreUnmapped
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
                         v .:? "_source"  <*>
                         v .:? "fields"   <*>
                         v .:? "highlight"
  parseJSON _          = empty

instance FromJSON HitFields where
  parseJSON x
    = HitFields <$> parseJSON x

instance FromJSON DocVersion where
  parseJSON v = do
    i <- parseJSON v
    maybe (fail "DocVersion out of range") return $ mkDocVersion i

data Suggest = Suggest { suggestText :: Text
                       , suggestName :: Text
                       , suggestType :: SuggestType
                       }
 deriving (Eq, Show)

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
  deriving (Eq, Show)

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
  deriving (Eq, Show)

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
  deriving (Eq, Show)

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
  deriving (Eq, Show)

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
  deriving (Eq, Show)

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
  deriving (Eq, Show)


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
