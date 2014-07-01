{-# LANGUAGE DeriveGeneric #-}

{-| Data types for describing actions and data structures performed to interact
    with Elasticsearch. The two main buckets your queries against Elasticsearch
    will fall into are 'Query's and 'Filter's. 'Filter's are more like
    traditional database constraints and often have preferable performance
    properties. 'Query's support human-written textual queries, such as fuzzy
    queries.
-}

module Database.Bloodhound.Types
       ( defaultCache
       , defaultIndexSettings
       , halfRangeToKV
       , maybeJson
       , maybeJsonF
       , mkSort
       , rangeToKV
       , showText
       , unpackId
       , mkMatchQuery
       , mkMultiMatchQuery
       , mkBoolQuery
       , mkRangeQuery
       , mkQueryStringQuery
       , Version(..)
       , Status(..)
       , Existence(..)
       , NullValue(..)
       , IndexSettings(..)
       , Server(..)
       , Reply
       , EsResult(..)
       , Query(..)
       , Search(..)
       , SearchResult(..)
       , SearchHits(..)
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
       , Range(..)
       , HalfRange(..)
       , RangeExecution(..)
       , LessThan(..)
       , LessThanEq(..)
       , GreaterThan(..)
       , GreaterThanEq(..)
       , Regexp(..)
       , RegexpFlags(..)
       , RegexpFlag(..)
       , FieldName(..)
       , IndexName(..)
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
         ) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Database.Bloodhound.Types.Class
import GHC.Generics (Generic)
import Network.HTTP.Client
import qualified Network.HTTP.Types.Method as NHTM


{-| 'Version' is embedded in 'Status' -}
data Version = Version { number          :: Text
                       , build_hash      :: Text
                       , build_timestamp :: UTCTime
                       , build_snapshot  :: Bool
                       , lucene_version  :: Text } deriving (Eq, Show, Generic)

{-| 'Status' is a data type for describing the JSON body returned by
    Elasticsearch when you query its status. This was deprecated in 1.2.0.

   <http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/indices-status.html#indices-status>
-}

data Status = Status { ok      :: Bool
                     , status  :: Int
                     , name    :: Text
                     , version :: Version
                     , tagline :: Text } deriving (Eq, Show)

{-| 'IndexSettings' is used to configure the shards and replicas when you create
   an Elasticsearch Index.

   <http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/indices-create-index.html>
-}

data IndexSettings =
  IndexSettings { indexShards   :: ShardCount
                , indexReplicas :: ReplicaCount } deriving (Eq, Show)

{-| 'defaultIndexSettings' is an 'IndexSettings' with 3 shards and 2 replicas. -}
defaultIndexSettings :: IndexSettings
defaultIndexSettings =  IndexSettings (ShardCount 3) (ReplicaCount 2)

{-| 'Reply' and 'Method' are type synonyms from 'Network.HTTP.Types.Method.Method' -}
type Reply = Network.HTTP.Client.Response L.ByteString
type Method = NHTM.Method

{-| 'OpenCloseIndex' is a sum type for opening and closing indices.

   <http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/indices-open-close.html>
-}
data OpenCloseIndex = OpenIndex | CloseIndex deriving (Eq, Show)

data FieldType = GeoPointType
               | GeoShapeType
               | FloatType
               | IntegerType
               | LongType
               | ShortType
               | ByteType deriving (Eq, Show)

data FieldDefinition =
  FieldDefinition { fieldType :: FieldType } deriving (Eq, Show)

data MappingField =
  MappingField   { mappingFieldName       :: FieldName
                 , fieldDefinition        :: FieldDefinition } deriving (Eq, Show)

{-| Support for type reification of 'Mapping's is currently incomplete, for
    now the mapping API verbiage expects a 'ToJSON'able blob.

    Indexes have mappings, mappings are schemas for the documents contained in the
    index. I'd recommend having only one mapping per index, always having a mapping,
    and keeping different kinds of documents separated if possible.
-}
data Mapping = Mapping { typeName      :: TypeName
                       , mappingFields :: [MappingField] } deriving (Eq, Show)

{-| 'BulkOperation' is a sum type for expressing the four kinds of bulk
    operation index, create, delete, and update. 'BulkIndex' behaves like an
    "upsert", 'BulkCreate' will fail if a document already exists at the DocId.

   <http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/docs-bulk.html#docs-bulk>
-}
data BulkOperation =
    BulkIndex  IndexName MappingName DocId Value
  | BulkCreate IndexName MappingName DocId Value
  | BulkDelete IndexName MappingName DocId
  | BulkUpdate IndexName MappingName DocId Value deriving (Eq, Show)

{-| 'EsResult' describes the standard wrapper JSON document that you see in
    successful Elasticsearch responses.
-}
data EsResult a = EsResult { _index   :: Text
                           , _type    :: Text
                           , _id      :: Text
                           , _version :: Int
                           , found    :: Maybe Bool
                           , _source  :: a } deriving (Eq, Show)

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

<http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/search-request-sort.html#search-request-sort>
-}
data DefaultSort =
  DefaultSort { sortFieldName  :: FieldName
              , sortOrder      :: SortOrder
                                  -- default False
              , ignoreUnmapped :: Bool
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
data BooleanOperator = And | Or deriving (Eq, Show)

{-| 'ShardCount' is part of 'IndexSettings'
-}
newtype ShardCount = ShardCount Int deriving (Eq, Show, Generic)

{-| 'ReplicaCount' is part of 'IndexSettings'
-}
newtype ReplicaCount = ReplicaCount Int deriving (Eq, Show, Generic)

{-| 'Server' is used with the client functions to point at the ES instance
-}
newtype Server = Server String deriving (Eq, Show)

{-| 'IndexName' is used to describe which index to query/create/delete
-}
newtype IndexName = IndexName String deriving (Eq, Generic, Show)

{-| 'MappingName' is part of mappings which are how ES describes and schematizes
    the data in the indices.
-}
newtype MappingName = MappingName String deriving (Eq, Generic, Show)

{-| 'DocId' is a generic wrapper value for expressing unique Document IDs.
    Can be set by the user or created by ES itself. Often used in client
    functions for poking at specific documents.
-}
newtype DocId = DocId String deriving (Eq, Generic, Show)

{-| 'QueryString' is used to wrap query text bodies, be they human written or not.
-}
newtype QueryString = QueryString Text deriving (Eq, Generic, Show)

{-| 'FieldName' is used all over the place wherever a specific field within
     a document needs to be specified, usually in 'Query's or 'Filter's.
-}
newtype FieldName = FieldName Text deriving (Eq, Show)

{-| 'CacheName' is used in 'RegexpFilter' for describing the
    'CacheKey' keyed caching behavior.
-}
newtype CacheName = CacheName Text deriving (Eq, Show)

{-| 'CacheKey' is used in 'RegexpFilter' to key regex caching.
-}
newtype CacheKey =
  CacheKey Text deriving (Eq, Show)
newtype Existence =
  Existence Bool deriving (Eq, Show)
newtype NullValue =
  NullValue Bool deriving (Eq, Show)
newtype CutoffFrequency =
  CutoffFrequency Double deriving (Eq, Show, Generic)
newtype Analyzer =
  Analyzer Text deriving (Eq, Show, Generic)
newtype MaxExpansions =
  MaxExpansions Int deriving (Eq, Show, Generic)

{-| 'Lenient', if set to true, will cause format based failures to be
    ignored. I don't know what the bloody default is, Elasticsearch
    documentation didn't say what it was. Let me know if you figure it out.
-}
newtype Lenient =
  Lenient Bool deriving (Eq, Show, Generic)
newtype Tiebreaker =
  Tiebreaker Double deriving (Eq, Show, Generic)
newtype Boost =
  Boost Double deriving (Eq, Show, Generic)
newtype BoostTerms =
  BoostTerms Double deriving (Eq, Show, Generic)

{-| 'MinimumMatch' controls how many should clauses in the bool query should
     match. Can be an absolute value (2) or a percentage (30%) or a
     combination of both.
-}
newtype MinimumMatch =
  MinimumMatch Int deriving (Eq, Show, Generic)
newtype MinimumMatchText =
  MinimumMatchText Text deriving (Eq, Show)
newtype DisableCoord =
  DisableCoord Bool deriving (Eq, Show, Generic)
newtype IgnoreTermFrequency =
  IgnoreTermFrequency Bool deriving (Eq, Show, Generic)
newtype MinimumTermFrequency =
  MinimumTermFrequency Int deriving (Eq, Show, Generic)
newtype MaxQueryTerms =
  MaxQueryTerms Int deriving (Eq, Show, Generic)
newtype Fuzziness =
  Fuzziness Double deriving (Eq, Show, Generic)

{-| 'PrefixLength' is the prefix length used in queries, defaults to 0. -}
newtype PrefixLength =
  PrefixLength Int deriving (Eq, Show, Generic)
newtype TypeName =
  TypeName Text deriving (Eq, Show, Generic)
newtype PercentMatch =
  PercentMatch Double deriving (Eq, Show, Generic)
newtype StopWord =
  StopWord Text deriving (Eq, Show, Generic)
newtype QueryPath =
  QueryPath Text deriving (Eq, Show, Generic)

{-| Allowing a wildcard at the beginning of a word (eg "*ing") is particularly
    heavy, because all terms in the index need to be examined, just in case
    they match. Leading wildcards can be disabled by setting
    'AllowLeadingWildcard' to false. -}
newtype AllowLeadingWildcard =
  AllowLeadingWildcard     Bool deriving (Eq, Show, Generic)
newtype LowercaseExpanded =
  LowercaseExpanded        Bool deriving (Eq, Show, Generic)
newtype EnablePositionIncrements =
  EnablePositionIncrements Bool deriving (Eq, Show, Generic)

{-| By default, wildcard terms in a query are not analyzed.
    Setting 'AnalyzeWildcard' to true enables best-effort analysis.
-}
newtype AnalyzeWildcard = AnalyzeWildcard Bool deriving (Eq, Show, Generic)

{-| 'GeneratePhraseQueries' defaults to false.
-}
newtype GeneratePhraseQueries =
  GeneratePhraseQueries Bool deriving (Eq, Show, Generic)

{-| 'Locale' is used for string conversions - defaults to ROOT.
-}
newtype Locale        = Locale        Text deriving (Eq, Show, Generic)
newtype MaxWordLength = MaxWordLength Int  deriving (Eq, Show, Generic)
newtype MinWordLength = MinWordLength Int  deriving (Eq, Show, Generic)

{-| 'PhraseSlop' sets the default slop for phrases, 0 means exact
     phrase matches. Default is 0.
-}
newtype PhraseSlop      = PhraseSlop      Int deriving (Eq, Show, Generic)
newtype MinDocFrequency = MinDocFrequency Int deriving (Eq, Show, Generic)
newtype MaxDocFrequency = MaxDocFrequency Int deriving (Eq, Show, Generic)

{-| 'unpackId' is a silly convenience function that gets used once.
-}
unpackId :: DocId -> String
unpackId (DocId docId) = docId

type TrackSortScores = Bool
type From = Int
type Size = Int

data Search = Search { queryBody  :: Maybe Query
                     , filterBody :: Maybe Filter
                     , sortBody   :: Maybe Sort
                       -- default False
                     , trackSortScores :: TrackSortScores
                     , from :: From
                     , size :: Size } deriving (Eq, Show)

data Query =
  TermQuery                     Term (Maybe Boost)
  | TermsQuery                  [Term] MinimumMatch
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
  deriving (Eq, Show)

data RegexpQuery =
  RegexpQuery { regexpQueryField     :: FieldName
              , regexpQuery          :: Regexp
              , regexpQueryFlags     :: RegexpFlags
              , regexpQueryBoost     :: Maybe Boost
              } deriving (Eq, Show)

data RangeQuery =
  RangeQuery { rangeQueryField :: FieldName
             , rangeQueryRange :: Either HalfRange Range
             , rangeQueryBoost :: Boost } deriving (Eq, Show)

mkRangeQuery :: FieldName -> Either HalfRange Range -> RangeQuery
mkRangeQuery f r = RangeQuery f r (Boost 1.0)

data SimpleQueryStringQuery =
  SimpleQueryStringQuery
    { simpleQueryStringQuery             :: QueryString
    , simpleQueryStringField             :: Maybe FieldOrFields
    , simpleQueryStringOperator          :: Maybe BooleanOperator
    , simpleQueryStringAnalyzer          :: Maybe Analyzer
    , simpleQueryStringFlags             :: Maybe [SimpleQueryFlag]
    , simpleQueryStringLowercaseExpanded :: Maybe LowercaseExpanded
    , simpleQueryStringLocale            :: Maybe Locale
    } deriving (Eq, Show)

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
  | SimpleQuerySlop deriving (Eq, Show)

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
  } deriving (Eq, Show)

mkQueryStringQuery :: QueryString -> QueryStringQuery
mkQueryStringQuery qs =
  QueryStringQuery qs Nothing Nothing
  Nothing Nothing Nothing Nothing
  Nothing Nothing Nothing Nothing
  Nothing Nothing Nothing Nothing
  Nothing Nothing

data FieldOrFields = FofField   FieldName
                   | FofFields [FieldName] deriving (Eq, Show)

data PrefixQuery =
  PrefixQuery
  { prefixQueryField       :: FieldName
  , prefixQueryPrefixValue :: Text
  , prefixQueryBoost       :: Maybe Boost } deriving (Eq, Show)

data NestedQuery =
  NestedQuery
  { nestedQueryPath      :: QueryPath
  , nestedQueryScoreType :: ScoreType
  , nestedQuery          :: Query } deriving (Eq, Show)

data MoreLikeThisFieldQuery =
  MoreLikeThisFieldQuery
  { moreLikeThisFieldText            :: Text
  , moreLikeThisFieldFields          :: FieldName
                                        -- default 0.3 (30%)
  , moreLikeThisFieldPercentMatch    :: Maybe PercentMatch
  , moreLikeThisFieldMinimumTermFreq :: Maybe MinimumTermFrequency
  , moreLikeThisFieldMaxQueryTerms   :: Maybe MaxQueryTerms
  , moreLikeThisFieldStopWords       :: Maybe [StopWord]
  , moreLikeThisFieldMinDocFrequency :: Maybe MinDocFrequency
  , moreLikeThisFieldMaxDocFrequency :: Maybe MaxDocFrequency
  , moreLikeThisFieldMinWordLength   :: Maybe MinWordLength
  , moreLikeThisFieldMaxWordLength   :: Maybe MaxWordLength
  , moreLikeThisFieldBoostTerms      :: Maybe BoostTerms
  , moreLikeThisFieldBoost           :: Maybe Boost
  , moreLikeThisFieldAnalyzer        :: Maybe Analyzer
  } deriving (Eq, Show)

data MoreLikeThisQuery =
  MoreLikeThisQuery
  { moreLikeThisText            :: Text
  , moreLikeThisFields          :: Maybe [FieldName]
    -- default 0.3 (30%)
  , moreLikeThisPercentMatch    :: Maybe PercentMatch
  , moreLikeThisMinimumTermFreq :: Maybe MinimumTermFrequency
  , moreLikeThisMaxQueryTerms   :: Maybe MaxQueryTerms
  , moreLikeThisStopWords       :: Maybe [StopWord]
  , moreLikeThisMinDocFrequency :: Maybe MinDocFrequency
  , moreLikeThisMaxDocFrequency :: Maybe MaxDocFrequency
  , moreLikeThisMinWordLength   :: Maybe MinWordLength
  , moreLikeThisMaxWordLength   :: Maybe MaxWordLength
  , moreLikeThisBoostTerms      :: Maybe BoostTerms
  , moreLikeThisBoost           :: Maybe Boost
  , moreLikeThisAnalyzer        :: Maybe Analyzer
  } deriving (Eq, Show)

data IndicesQuery =
  IndicesQuery
  { indicesQueryIndices :: [IndexName]
  , indicesQuery          :: Query
    -- default "all"
  , indicesQueryNoMatch   :: Maybe Query } deriving (Eq, Show)

data HasParentQuery =
  HasParentQuery
  { hasParentQueryType      :: TypeName
  , hasParentQuery          :: Query
  , hasParentQueryScoreType :: Maybe ScoreType } deriving (Eq, Show)

data HasChildQuery =
  HasChildQuery
  { hasChildQueryType      :: TypeName
  , hasChildQuery          :: Query
  , hasChildQueryScoreType :: Maybe ScoreType } deriving (Eq, Show)

data ScoreType =
  ScoreTypeMax
  | ScoreTypeSum
  | ScoreTypeAvg
  | ScoreTypeNone deriving (Eq, Show)

data FuzzyQuery =
  FuzzyQuery { fuzzyQueryField         :: FieldName
             , fuzzyQueryValue         :: Text
             , fuzzyQueryPrefixLength  :: PrefixLength
             , fuzzyQueryMaxExpansions :: MaxExpansions
             , fuzzyQueryFuzziness     :: Fuzziness
             , fuzzyQueryBoost         :: Maybe Boost
             } deriving (Eq, Show)

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
  } deriving (Eq, Show)

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
  } deriving (Eq, Show)

data FilteredQuery =
  FilteredQuery
  { filteredQuery  :: Query
  , filteredFilter :: Filter } deriving (Eq, Show)

data DisMaxQuery =
  DisMaxQuery { disMaxQueries    :: [Query]
                -- default 0.0
              , disMaxTiebreaker :: Tiebreaker
              , disMaxBoost      :: Maybe Boost
              } deriving (Eq, Show)

data MatchQuery =
  MatchQuery { matchQueryField           :: FieldName
             , matchQueryQueryString     :: QueryString
             , matchQueryOperator        :: BooleanOperator
             , matchQueryZeroTerms       :: ZeroTermsQuery
             , matchQueryCutoffFrequency :: Maybe CutoffFrequency
             , matchQueryMatchType       :: Maybe MatchQueryType
             , matchQueryAnalyzer        :: Maybe Analyzer
             , matchQueryMaxExpansions   :: Maybe MaxExpansions
             , matchQueryLenient         :: Maybe Lenient } deriving (Eq, Show)

{-| 'mkMatchQuery' is a convenience function that defaults the less common parameters,
    enabling you to provide only the 'FieldName' and 'QueryString' to make a 'MatchQuery'
-}
mkMatchQuery :: FieldName -> QueryString -> MatchQuery
mkMatchQuery field query = MatchQuery field query Or ZeroTermsNone Nothing Nothing Nothing Nothing Nothing

data MatchQueryType =
  MatchPhrase
  | MatchPhrasePrefix deriving (Eq, Show)

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
                  , multiMatchQueryLenient         :: Maybe Lenient } deriving (Eq, Show)

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
  | MultiMatchPhrasePrefix deriving (Eq, Show)

data BoolQuery =
  BoolQuery { boolQueryMustMatch          :: Maybe Query
            , boolQueryMustNotMatch       :: Maybe Query
            , boolQueryShouldMatch        :: Maybe [Query]
            , boolQueryMinimumShouldMatch :: Maybe MinimumMatch
            , boolQueryBoost              :: Maybe Boost
            , boolQueryDisableCoord       :: Maybe DisableCoord
            } deriving (Eq, Show)

mkBoolQuery :: Maybe Query -> Maybe Query -> Maybe [Query] -> BoolQuery
mkBoolQuery must mustNot should =
  BoolQuery must mustNot should Nothing Nothing Nothing

data BoostingQuery =
  BoostingQuery { positiveQuery :: Query
                , negativeQuery :: Query
                , negativeBoost :: Boost } deriving (Eq, Show)

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
                   } deriving (Eq, Show)

data CommonMinimumMatch =
    CommonMinimumMatchHighLow MinimumMatchHighLow
  | CommonMinimumMatch        MinimumMatch
  deriving (Eq, Show)

data MinimumMatchHighLow =
  MinimumMatchHighLow { lowFreq :: MinimumMatch
                      , highFreq :: MinimumMatch } deriving (Eq, Show)

data Filter = AndFilter [Filter] Cache
            | OrFilter  [Filter] Cache
            | NotFilter  Filter  Cache
            | IdentityFilter
            | BoolFilter BoolMatch
            | ExistsFilter FieldName -- always cached
            | GeoBoundingBoxFilter GeoBoundingBoxConstraint GeoFilterType
            | GeoDistanceFilter GeoPoint Distance DistanceType OptimizeBbox Cache
            | GeoDistanceRangeFilter GeoPoint DistanceRange
            | GeoPolygonFilter FieldName [LatLon]
            | IdsFilter MappingName [DocId]
            | LimitFilter Int
            | MissingFilter FieldName Existence NullValue
            | PrefixFilter  FieldName PrefixValue Cache
            | RangeFilter   FieldName (Either HalfRange Range) RangeExecution Cache
            | RegexpFilter  FieldName Regexp RegexpFlags CacheName Cache CacheKey
            | TermFilter    Term Cache
              deriving (Eq, Show)

data ZeroTermsQuery = ZeroTermsNone
                    | ZeroTermsAll deriving (Eq, Show)

-- lt, lte | gt, gte
newtype LessThan      = LessThan      Double deriving (Eq, Show)
newtype LessThanEq    = LessThanEq    Double deriving (Eq, Show)
newtype GreaterThan   = GreaterThan   Double deriving (Eq, Show)
newtype GreaterThanEq = GreaterThanEq Double deriving (Eq, Show)

data HalfRange = HalfRangeLt  LessThan
               | HalfRangeLte LessThanEq
               | HalfRangeGt  GreaterThan
               | HalfRangeGte GreaterThanEq deriving (Eq, Show)

data Range = RangeLtGt   LessThan GreaterThan
           | RangeLtGte  LessThan GreaterThanEq
           | RangeLteGt  LessThanEq GreaterThan
           | RangeLteGte LessThanEq GreaterThanEq deriving (Eq, Show)

data RangeExecution = RangeExecutionIndex
                    | RangeExecutionFielddata deriving (Eq, Show)

newtype Regexp = Regexp Text deriving (Eq, Show)

data RegexpFlags = AllRegexpFlags
                 | NoRegexpFlags
                 | SomeRegexpFlags (NonEmpty RegexpFlag) deriving (Eq, Show)

data RegexpFlag = AnyString
                | Automaton
                | Complement
                | Empty
                | Intersection
                | Interval deriving (Eq, Show)

halfRangeToKV :: HalfRange -> (Text, Double)
halfRangeToKV (HalfRangeLt  (LessThan n))      = ("lt",  n)
halfRangeToKV (HalfRangeLte (LessThanEq n))    = ("lte", n)
halfRangeToKV (HalfRangeGt  (GreaterThan n))   = ("gt",  n)
halfRangeToKV (HalfRangeGte (GreaterThanEq n)) = ("gte", n)

rangeToKV :: Range -> (Text, Double, Text, Double)
rangeToKV (RangeLtGt   (LessThan m)   (GreaterThan   n)) = ("lt",  m, "gt",  n)
rangeToKV (RangeLtGte  (LessThan m)   (GreaterThanEq n)) = ("lt",  m, "gte", n)
rangeToKV (RangeLteGt  (LessThanEq m) (GreaterThan   n)) = ("lte", m, "gt",  n)
rangeToKV (RangeLteGte (LessThanEq m) (GreaterThanEq n)) = ("lte", m, "gte", n)

-- phew. Coulda used Agda style case breaking there but, you know, whatever. :)

data Term = Term { termField :: Text
                 , termValue :: Text } deriving (Eq, Show)

data BoolMatch = MustMatch    Term  Cache
               | MustNotMatch Term  Cache
               | ShouldMatch [Term] Cache deriving (Eq, Show)

-- "memory" or "indexed"
data GeoFilterType = GeoFilterMemory
                   | GeoFilterIndexed deriving (Eq, Show)

data LatLon = LatLon { lat :: Double
                     , lon :: Double } deriving (Eq, Show)

data GeoBoundingBox =
  GeoBoundingBox { topLeft     :: LatLon
                 , bottomRight :: LatLon } deriving (Eq, Show)

data GeoBoundingBoxConstraint =
  GeoBoundingBoxConstraint { geoBBField        :: FieldName
                           , constraintBox     :: GeoBoundingBox
                           , bbConstraintcache :: Cache
                           } deriving (Eq, Show)

data GeoPoint =
  GeoPoint { geoField :: FieldName
           , latLon   :: LatLon} deriving (Eq, Show)

data DistanceUnit = Miles
                  | Yards
                  | Feet
                  | Inches
                  | Kilometers
                  | Meters
                  | Centimeters
                  | Millimeters
                  | NauticalMiles deriving (Eq, Show)

data DistanceType = Arc
                  | SloppyArc -- doesn't exist <1.0
                  | Plane deriving (Eq, Show)

data OptimizeBbox = OptimizeGeoFilterType GeoFilterType
                  | NoOptimizeBbox deriving (Eq, Show)

data Distance =
  Distance { coefficient :: Double
           , unit        :: DistanceUnit } deriving (Eq, Show)

data DistanceRange =
  DistanceRange { distanceFrom :: Distance
                , distanceTo   :: Distance } deriving (Eq, Show)

data FromJSON a => SearchResult a =
  SearchResult { took       :: Int
               , timedOut   :: Bool
               , shards     :: ShardResult
               , searchHits :: SearchHits a } deriving (Eq, Show)

type Score = Double

data FromJSON a => SearchHits a =
  SearchHits { hitsTotal :: Int
             , maxScore  :: Score
             , hits      :: [Hit a] } deriving (Eq, Show)

data FromJSON a => Hit a =
  Hit { hitIndex      :: IndexName
      , hitType       :: MappingName
      , hitDocId      :: DocId
      , hitScore      :: Score
      , hitSource     :: a } deriving (Eq, Show)

data ShardResult =
  ShardResult { shardTotal       :: Int
              , shardsSuccessful :: Int
              , shardsFailed     :: Int } deriving (Eq, Show, Generic)

maybeJson :: ToJSON a => Text -> Maybe a -> [(Text, Value)]
maybeJson field (Just value) = [field .= toJSON value]
maybeJson _ _ = []

maybeJsonF :: (ToJSON (f Value), ToJSON a, Functor f) =>
             Text -> Maybe (f a) -> [(Text, Value)]
maybeJsonF field (Just value) = [field .= fmap toJSON value]
maybeJsonF _ _ = []

showText :: Show a => a -> Text
showText = T.pack . show
