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
-- Copyright : (C) 2014, 2018 Chris Allen
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



module Database.Bloodhound.Types
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
       , JoinRelation(..)
       , IndexDocumentSettings(..)
       , Query(..)
       , Search(..)
       , SearchType(..)
       , SearchResult(..)
       , ScrollId(..)
       , HitsTotalRelation(..)
       , HitsTotal(..)
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
       , ScriptSource(..)
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
       , IndexPattern(..)
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
       , SearchTemplateId(..)
       , SearchTemplateSource(..)
       , SearchTemplate(..)
       , GetTemplateScript(..)
       , TemplateQueryKeyValuePairs(..)
       , WildcardQuery(..)
       , BooleanOperator(..)
       , ZeroTermsQuery(..)
       , CutoffFrequency(..)
       , Analyzer(..)
       , Tokenizer(..)
       , TokenFilter(..)
       , CharFilter(..)
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
       , AggregateParentScore(..)
       , IgnoreUnmapped(..)
       , MinChildren(..)
       , MaxChildren(..)
       , ScoreType(..)
       , Score
       , Cache
       , RelationName(..)
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
       , UpsertActionMetadata(..)
       , buildUpsertActionMetadata
       , UpsertPayload(..)
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
       , SearchAfterKey

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
       , CharFilterDefinition(..)
       , Ngram(..)
       , TokenChar(..)
       , Shingle(..)
       , Language(..)
       ) where

import           Bloodhound.Import

import           Database.Bloodhound.Internal.Aggregation
import           Database.Bloodhound.Internal.Analysis
import           Database.Bloodhound.Internal.Client
import           Database.Bloodhound.Internal.Highlight
import           Database.Bloodhound.Internal.Newtypes
import           Database.Bloodhound.Internal.Query
import           Database.Bloodhound.Internal.Sort
import           Database.Bloodhound.Internal.Suggest
import qualified Data.HashMap.Strict as HM

{-| 'unpackId' is a silly convenience function that gets used once.
-}
unpackId :: DocId -> Text
unpackId (DocId docId) = docId

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
                     , searchAfterKey  :: Maybe SearchAfterKey
                     , fields          :: Maybe [FieldName]
                     , scriptFields    :: Maybe ScriptFields
                     , source          :: Maybe Source
                     , suggestBody     :: Maybe Suggest -- ^ Only one Suggestion request / response per Search is supported.
                     } deriving (Eq, Show)


instance ToJSON Search where
  toJSON (Search mquery sFilter sort searchAggs
          highlight sTrackSortScores sFrom sSize _ sAfter sFields
          sScriptFields sSource sSuggest) =
    omitNulls [ "query"         .= query'
              , "sort"          .= sort
              , "aggregations"  .= searchAggs
              , "highlight"     .= highlight
              , "from"          .= sFrom
              , "size"          .= sSize
              , "track_scores"  .= sTrackSortScores
              , "search_after"  .= sAfter
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

data SearchType = SearchTypeQueryThenFetch
                | SearchTypeDfsQueryThenFetch
  deriving (Eq, Show)

instance ToJSON SearchType where
  toJSON SearchTypeQueryThenFetch = String "query_then_fetch"
  toJSON SearchTypeDfsQueryThenFetch = String "dfs_query_then_fetch"

instance FromJSON SearchType where
  parseJSON (String "query_then_fetch") = pure $ SearchTypeQueryThenFetch
  parseJSON (String "dfs_query_then_fetch") = pure $ SearchTypeDfsQueryThenFetch
  parseJSON _          = empty

data Source =
    NoSource
  | SourcePatterns PatternOrPatterns
  | SourceIncludeExclude Include Exclude
    deriving (Eq, Show)

instance ToJSON Source where
    toJSON NoSource                         = toJSON False
    toJSON (SourcePatterns patterns)        = toJSON patterns
    toJSON (SourceIncludeExclude incl excl) = object [ "includes" .= incl, "excludes" .= excl ]

data PatternOrPatterns = PopPattern   Pattern
                       | PopPatterns [Pattern] deriving (Eq, Read, Show)

instance ToJSON PatternOrPatterns where
  toJSON (PopPattern pattern)   = toJSON pattern
  toJSON (PopPatterns patterns) = toJSON patterns

data Include = Include [Pattern] deriving (Eq, Read, Show)
data Exclude = Exclude [Pattern] deriving (Eq, Read, Show)

instance ToJSON Include where
  toJSON (Include patterns) = toJSON patterns

instance ToJSON Exclude where
  toJSON (Exclude patterns) = toJSON patterns

newtype Pattern = Pattern Text deriving (Eq, Read, Show)

instance ToJSON Pattern where
  toJSON (Pattern pattern) = toJSON pattern

data SearchResult a =
  SearchResult { took         :: Int
               , timedOut     :: Bool
               , shards       :: ShardResult
               , searchHits   :: SearchHits a
               , aggregations :: Maybe AggregationResults
               , scrollId     :: Maybe ScrollId
               -- ^ Only one Suggestion request / response per
               --   Search is supported.
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

newtype SearchTemplateId = SearchTemplateId Text deriving (Eq, Show)

instance ToJSON SearchTemplateId where
  toJSON (SearchTemplateId x) = toJSON x

newtype SearchTemplateSource = SearchTemplateSource Text deriving (Eq, Show)

instance ToJSON SearchTemplateSource where
  toJSON (SearchTemplateSource x) = toJSON x

instance FromJSON SearchTemplateSource where
  parseJSON (String s) = pure $ SearchTemplateSource s
  parseJSON _          = empty

data ExpandWildcards = ExpandWildcardsAll
  | ExpandWildcardsOpen
  | ExpandWildcardsClosed
  | ExpandWildcardsNone
  deriving (Eq, Show)

instance ToJSON ExpandWildcards where
  toJSON ExpandWildcardsAll = String "all"
  toJSON ExpandWildcardsOpen = String "open"
  toJSON ExpandWildcardsClosed = String "closed"
  toJSON ExpandWildcardsNone = String "none"

instance FromJSON ExpandWildcards where
  parseJSON (String "all") = pure $ ExpandWildcardsAll
  parseJSON (String "open") = pure $ ExpandWildcardsOpen
  parseJSON (String "closed") = pure $ ExpandWildcardsClosed
  parseJSON (String "none") = pure $ ExpandWildcardsNone
  parseJSON _          = empty

data TimeUnits = TimeUnitDays 
  | TimeUnitHours
  | TimeUnitMinutes
  | TimeUnitSeconds
  | TimeUnitMilliseconds
  | TimeUnitMicroseconds
  | TimeUnitNanoseconds
  deriving (Eq, Show)

instance ToJSON TimeUnits where
  toJSON TimeUnitDays = String "d"
  toJSON TimeUnitHours = String "h"
  toJSON TimeUnitMinutes = String "m"
  toJSON TimeUnitSeconds = String "s"
  toJSON TimeUnitMilliseconds = String "ms"
  toJSON TimeUnitMicroseconds = String "micros"
  toJSON TimeUnitNanoseconds = String "nanos"

instance FromJSON TimeUnits where
  parseJSON (String "d") = pure $ TimeUnitDays
  parseJSON (String "h") = pure $ TimeUnitHours
  parseJSON ( String "m") = pure $ TimeUnitMinutes
  parseJSON (String "s") = pure $ TimeUnitSeconds
  parseJSON (String "ms") = pure $ TimeUnitMilliseconds
  parseJSON (String "micros") = pure $ TimeUnitMicroseconds
  parseJSON (String "nanos") = pure $ TimeUnitNanoseconds
  parseJSON _          = empty

data SearchTemplate = SearchTemplate {
    searchTemplate                      :: Either SearchTemplateId SearchTemplateSource
    , params                              :: TemplateQueryKeyValuePairs
    , explainSearchTemplate               :: Maybe Bool
    , profileSearchTemplate               :: Maybe Bool
  } deriving (Eq, Show)

instance ToJSON SearchTemplate where
  toJSON SearchTemplate{..} = omitNulls [ 
    either ("id" .=) ("source" .=) searchTemplate
    , "params" .= params
    , "explain" .= explainSearchTemplate
    , "profile" .= profileSearchTemplate
    ]

data GetTemplateScript = GetTemplateScript {
    getTemplateScriptLang      :: Maybe Text
    , getTemplateScriptSource  :: Maybe SearchTemplateSource
    , getTemplateScriptOptions :: Maybe (HM.HashMap Text Text)
    , getTemplateScriptId      :: Text
    , getTemplateScriptFound   :: Bool
  } deriving (Eq, Show)

instance FromJSON GetTemplateScript where
  parseJSON (Object v) = do
    script <- v .:? "script"
    maybe
      (GetTemplateScript Nothing Nothing Nothing <$> v .: "_id" <*> v .: "found")
      (\s -> GetTemplateScript <$>
        s .:? "lang"    <*>
        s .:? "source"  <*>
        s .:? "options" <*>
        v .: "_id"      <*>
        v .: "found"
      )
      script
  parseJSON _          = empty

