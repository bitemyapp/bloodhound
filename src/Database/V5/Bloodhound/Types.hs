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

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M

import           Database.V5.Bloodhound.Types.Class
import           Database.V5.Bloodhound.Internal.Analysis
import           Database.V5.Bloodhound.Internal.Aggregation
import           Database.V5.Bloodhound.Internal.Client
import           Database.V5.Bloodhound.Internal.Highlight
import           Database.V5.Bloodhound.Internal.Newtypes
import           Database.V5.Bloodhound.Internal.Query
import           Database.V5.Bloodhound.Internal.Sort

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
                     , fields          :: Maybe [FieldName]
                     , scriptFields    :: Maybe ScriptFields
                     , source          :: Maybe Source
                     , suggestBody     :: Maybe Suggest -- ^ Only one Suggestion request / response per Search is supported.
                     } deriving (Eq, Show)


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
  (SearchHits ta ma ha) <> (SearchHits tb mb hb) =
    SearchHits (ta + tb) (max ma mb) (ha <> hb)

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

toTerms :: Text -> AggregationResults -> Maybe (Bucket TermsResult)
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
commonHighlightPairs (Just (CommonHighlight chScore chForceSource
                            chTag chEncoder chNoMatchSize
                            chHighlightQuery chRequireFieldMatch)) =
    [ "order" .= chScore
    , "force_source" .= chForceSource
    , "encoder" .= chEncoder
    , "no_match_size" .= chNoMatchSize
    , "highlight_query" .= chHighlightQuery
    , "require_fieldMatch" .= chRequireFieldMatch
    ]
    ++ highlightTagToPairs chTag


nonPostingsToPairs :: Maybe NonPostings -> [Pair]
nonPostingsToPairs Nothing = []
nonPostingsToPairs (Just (NonPostings npFragSize npNumOfFrags)) =
    [ "fragment_size" .= npFragSize
    , "number_of_fragments" .= npNumOfFrags
    ]

highlightTagToPairs :: Maybe HighlightTag -> [Pair]
highlightTagToPairs (Just (TagSchema _)) =
  [ "scheme" .= String "default"
  ]
highlightTagToPairs (Just (CustomTags (pre, post))) =
  [ "pre_tags"  .= pre
  , "post_tags" .= post
  ]
highlightTagToPairs Nothing = []

data Suggest = Suggest
  { suggestText :: Text
  , suggestName :: Text
  , suggestType :: SuggestType
  } deriving (Eq, Show)

instance ToJSON Suggest where
  toJSON Suggest{..} =
    object [ "text" .= suggestText
           , suggestName .= suggestType
           ]

instance FromJSON Suggest where
  parseJSON (Object o) = do
    suggestText' <- o .: "text"
    let dropTextList =
            HM.toList
          $ HM.filterWithKey (\x _ -> x /= "text") o
    suggestName' <-
      case dropTextList of
        [(x, _)] -> return x
        _ -> fail "error parsing Suggest field name"
    suggestType' <- o .: suggestName'
    return $ Suggest suggestText' suggestName' suggestType'
  parseJSON x = typeMismatch "Suggest" x

data SuggestType =
  SuggestTypePhraseSuggester PhraseSuggester
  deriving (Eq, Show)

instance ToJSON SuggestType where
  toJSON (SuggestTypePhraseSuggester x) =
    object [ "phrase" .= x ]

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
    where
      parse o = SuggestOptions
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
    where
      parse "missing" =
        pure DirectGeneratorSuggestModeMissing
      parse "popular" =
        pure DirectGeneratorSuggestModePopular
      parse "always" =
        pure DirectGeneratorSuggestModeAlways
      parse f =
        fail ("Unexpected DirectGeneratorSuggestModeTypes: " <> show f)

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
  } deriving (Eq, Show)

instance ToJSON DirectGenerators where
  toJSON DirectGenerators{..} =
    omitNulls [ "field" .= directGeneratorsField
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
