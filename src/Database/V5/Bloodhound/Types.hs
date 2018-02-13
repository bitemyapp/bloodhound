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

import qualified Data.HashMap.Strict                   as HM
import qualified Data.Map.Strict                       as M
import qualified Data.Text                             as T

import           Database.V5.Bloodhound.Types.Class
import           Database.V5.Bloodhound.Internal.Analysis
import           Database.V5.Bloodhound.Internal.Client
import           Database.V5.Bloodhound.Internal.Newtypes
import           Database.V5.Bloodhound.Internal.Query

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
data SortSpec =
    DefaultSortSpec DefaultSort
  | GeoDistanceSortSpec SortOrder GeoPoint DistanceUnit
  deriving (Eq, Show)

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

instance ToJSON SortOrder where
  toJSON Ascending  = String "asc"
  toJSON Descending = String "desc"

{-| 'Missing' prescribes how to handle missing fields. A missing field can be
    sorted last, first, or using a custom value as a substitute.

<http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/search-request-sort.html#_missing_values>
-}
data Missing = LastMissing
             | FirstMissing
             | CustomMissing Text deriving (Eq, Show)

instance ToJSON Missing where
  toJSON LastMissing         = String "_last"
  toJSON FirstMissing        = String "_first"
  toJSON (CustomMissing txt) = String txt

{-| 'SortMode' prescribes how to handle sorting array/multi-valued fields.

http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/search-request-sort.html#_sort_mode_option
-}
data SortMode = SortMin
              | SortMax
              | SortSum
              | SortAvg deriving (Eq, Show)

instance ToJSON SortMode where
  toJSON SortMin = String "min"
  toJSON SortMax = String "max"
  toJSON SortSum = String "sum"
  toJSON SortAvg = String "avg"

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

instance ToJSON HighlightEncoder where
    toJSON DefaultEncoder = String "default"
    toJSON HTMLEncoder    = String "html"

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

newtype ScrollId =
  ScrollId Text
  deriving (Eq, Show, Ord, ToJSON, FromJSON)

data SearchHits a =
  SearchHits { hitsTotal :: Int
             , maxScore  :: Score
             , hits      :: [Hit a] } deriving (Eq, Show)


instance (FromJSON a) => FromJSON (SearchHits a) where
  parseJSON (Object v) = SearchHits <$>
                         v .: "total"     <*>
                         v .: "max_score" <*>
                         v .: "hits"
  parseJSON _          = empty

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

newtype HitFields =
  HitFields (M.Map Text [Value])
  deriving (Eq, Show)

instance FromJSON HitFields where
  parseJSON x
    = HitFields <$> parseJSON x

type HitHighlight = M.Map Text [Text]

type Aggregations = M.Map Text Aggregation

emptyAggregations :: Aggregations
emptyAggregations = M.empty

mkAggregations :: Text -> Aggregation -> Aggregations
mkAggregations name aggregation = M.insert name aggregation emptyAggregations

data TermOrder = TermOrder
  { termSortField :: Text
  , termSortOrder :: SortOrder } deriving (Eq, Show)

instance ToJSON TermOrder where
  toJSON (TermOrder termSortField termSortOrder) =
    object [termSortField .= termSortOrder]

data TermInclusion = TermInclusion Text
                   | TermPattern Text Text deriving (Eq, Show)

instance ToJSON TermInclusion where
  toJSON (TermInclusion x) = toJSON x
  toJSON (TermPattern pattern flags) =
    omitNulls [ "pattern" .= pattern
              , "flags"   .= flags]

data CollectionMode = BreadthFirst
                    | DepthFirst deriving (Eq, Show)

instance ToJSON CollectionMode where
  toJSON BreadthFirst = "breadth_first"
  toJSON DepthFirst   = "depth_first"

data ExecutionHint = Ordinals
                   | GlobalOrdinals
                   | GlobalOrdinalsHash
                   | GlobalOrdinalsLowCardinality
                   | Map deriving (Eq, Show)

instance ToJSON ExecutionHint where
  toJSON Ordinals                     = "ordinals"
  toJSON GlobalOrdinals               = "global_ordinals"
  toJSON GlobalOrdinalsHash           = "global_ordinals_hash"
  toJSON GlobalOrdinalsLowCardinality = "global_ordinals_low_cardinality"
  toJSON Map                          = "map"

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

data TopHitsAggregation = TopHitsAggregation
  { taFrom :: Maybe From
  , taSize :: Maybe Size
  , taSort :: Maybe Sort
  } deriving (Eq, Show)

data MissingAggregation = MissingAggregation
  { maField :: Text
  } deriving (Eq, Show)

data TermsAggregation = TermsAggregation
  { term              :: Either Text Text
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

instance ToJSON DateRangeAggregation where
  toJSON DateRangeAggregation {..} =
    omitNulls [ "field" .= draField
              , "format" .= draFormat
              , "ranges" .= toList draRanges
              ]

data DateRangeAggRange =
    DateRangeFrom DateMathExpr
  | DateRangeTo DateMathExpr
  | DateRangeFromAndTo DateMathExpr DateMathExpr
  deriving (Eq, Show)

instance ToJSON DateRangeAggRange where
  toJSON (DateRangeFrom e)        = object [ "from" .= e ]
  toJSON (DateRangeTo e)          = object [ "to" .= e ]
  toJSON (DateRangeFromAndTo f t) = object [ "from" .= f, "to" .= t ]

-- | See <https://www.elastic.co/guide/en/elasticsearch/reference/current/common-options.html#date-math> for more information.
data DateMathExpr =
  DateMathExpr DateMathAnchor [DateMathModifier]
  deriving (Eq, Show)

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

type AggregationResults = M.Map Text Value

class BucketAggregation a where
  key :: a -> BucketValue
  docCount :: a -> Int
  aggs :: a -> Maybe AggregationResults

data Bucket a = Bucket
  { buckets :: [a]
  } deriving (Read, Show)

instance (FromJSON a) => FromJSON (Bucket a) where
  parseJSON (Object v) = Bucket <$>
                         v .: "buckets"
  parseJSON _ = mempty

data BucketValue = TextValue Text
                 | ScientificValue Scientific
                 | BoolValue Bool deriving (Read, Show)

instance FromJSON BucketValue where
  parseJSON (String t) = return $ TextValue t
  parseJSON (Number s) = return $ ScientificValue s
  parseJSON (Bool b)   = return $ BoolValue b
  parseJSON _          = mempty

data MissingResult = MissingResult
  { missingDocCount :: Int
  } deriving (Show)

instance FromJSON MissingResult where
  parseJSON = withObject "MissingResult" parse
    where parse v = MissingResult <$> v .: "doc_count"

data TopHitResult a = TopHitResult
  { tarHits :: (SearchHits a)
  } deriving Show

instance (FromJSON a) => FromJSON (TopHitResult a) where
  parseJSON (Object v) = TopHitResult <$>
                         v .: "hits"
  parseJSON _          = fail "Failure in FromJSON (TopHitResult a)"

data TermsResult = TermsResult
  { termKey       :: BucketValue
  , termsDocCount :: Int
  , termsAggs     :: Maybe AggregationResults
  } deriving (Read, Show)

instance FromJSON TermsResult where
  parseJSON (Object v) = TermsResult        <$>
                         v .:   "key"       <*>
                         v .:   "doc_count" <*>
                         (pure $ getNamedSubAgg v ["key", "doc_count"])
  parseJSON _ = mempty

instance BucketAggregation TermsResult where
  key = termKey
  docCount = termsDocCount
  aggs = termsAggs

data DateHistogramResult = DateHistogramResult
  { dateKey           :: Int
  , dateKeyStr        :: Maybe Text
  , dateDocCount      :: Int
  , dateHistogramAggs :: Maybe AggregationResults
  } deriving (Show)

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

instance BucketAggregation DateHistogramResult where
  key = TextValue . showText . dateKey
  docCount = dateDocCount
  aggs = dateHistogramAggs

data DateRangeResult = DateRangeResult
  { dateRangeKey          :: Text
  , dateRangeFrom         :: Maybe UTCTime
  , dateRangeFromAsString :: Maybe Text
  , dateRangeTo           :: Maybe UTCTime
  , dateRangeToAsString   :: Maybe Text
  , dateRangeDocCount     :: Int
  , dateRangeAggs         :: Maybe AggregationResults
  } deriving (Eq, Show)

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

instance BucketAggregation DateRangeResult where
  key = TextValue . dateRangeKey
  docCount = dateRangeDocCount
  aggs = dateRangeAggs

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

-- Try to get an AggregationResults when we don't know the
-- field name. We filter out the known keys to try to minimize the noise.
getNamedSubAgg :: Object -> [Text] -> Maybe AggregationResults
getNamedSubAgg o knownKeys = maggRes
  where unknownKeys = HM.filterWithKey (\k _ -> k `notElem` knownKeys) o
        maggRes
          | HM.null unknownKeys = Nothing
          | otherwise           = Just . M.fromList $ HM.toList unknownKeys

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

highlightTagToPairs :: Maybe HighlightTag -> [Pair]
highlightTagToPairs (Just (TagSchema _))            = [ "scheme"    .=  String "default"]
highlightTagToPairs (Just (CustomTags (pre, post))) = [ "pre_tags"  .= pre
                                                      , "post_tags" .= post]
highlightTagToPairs Nothing = []

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

data PhraseSuggester = PhraseSuggester
  { phraseSuggesterField :: FieldName
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
  } deriving (Eq, Show)

instance ToJSON PhraseSuggester where
  toJSON PhraseSuggester{..} =
    omitNulls [ "field" .= phraseSuggesterField
              , "gram_size" .= phraseSuggesterGramSize
              , "real_word_error_likelihood" .=
                phraseSuggesterRealWordErrorLikelihood
              , "confidence" .= phraseSuggesterConfidence
              , "max_errors" .= phraseSuggesterMaxErrors
              , "separator" .= phraseSuggesterSeparator
              , "size" .= phraseSuggesterSize
              , "analyzer" .= phraseSuggesterAnalyzer
              , "shard_size" .= phraseSuggesterShardSize
              , "highlight" .= phraseSuggesterHighlight
              , "collate" .= phraseSuggesterCollate
              , "direct_generator" .=
                phraseSuggesterCandidateGenerators
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

data PhraseSuggesterCollate = PhraseSuggesterCollate
  { phraseSuggesterCollateTemplateQuery :: TemplateQueryInline
  , phraseSuggesterCollatePrune :: Bool
  } deriving (Eq, Show)

instance ToJSON PhraseSuggesterCollate where
  toJSON PhraseSuggesterCollate{..} =
    object [ "query" .= object
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
    return $ PhraseSuggesterCollate
             (TemplateQueryInline inline' params') prune'
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

data NamedSuggestionResponse = NamedSuggestionResponse
  { nsrName :: Text
  , nsrResponses :: [SuggestResponse]
  } deriving (Eq, Read, Show)

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
