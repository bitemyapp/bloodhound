{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
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

import           Bloodhound.Import

import           Database.V1.Bloodhound.Types.Class
import           Database.V1.Bloodhound.Internal.Aggregation
import           Database.V1.Bloodhound.Internal.Client
import           Database.V1.Bloodhound.Internal.Highlight
import           Database.V1.Bloodhound.Internal.Newtypes
import           Database.V1.Bloodhound.Internal.Query
import           Database.V1.Bloodhound.Internal.Sort
import           Database.V1.Bloodhound.Internal.Suggest

data SearchResult a =
  SearchResult { took         :: Int
               , timedOut     :: Bool
               , shards       :: ShardResult
               , searchHits   :: SearchHits a
               , aggregations :: Maybe AggregationResults
               , scrollId     :: Maybe ScrollId
               , suggest      :: Maybe NamedSuggestionResponse -- ^ Only one Suggestion request / response per Search is supported.
               }
  deriving (Eq, Show)

type TrackSortScores = Bool

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
                     } deriving (Eq, Show)

data SearchType = SearchTypeQueryThenFetch
                | SearchTypeDfsQueryThenFetch
                | SearchTypeCount
                | SearchTypeScan
                | SearchTypeQueryAndFetch
                | SearchTypeDfsQueryAndFetch
  deriving (Eq, Show)

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
