{-# LANGUAGE DeriveGeneric #-}

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
       , Version(..)
       , Status(..)
       , Existence(..)
       , NullValue(..)
       , IndexSettings(..)
       , Server(..)
       , Reply(..)
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
       , FieldName(..)
       , IndexName(..)
       , MappingName(..)
       , DocId(..)
       , CacheName(..)
       , CacheKey(..)
       , BulkOperation(..)
       , ReplicaCount(..)
       , ShardCount(..)
       , Sort(..)
       , SortMode(..)
       , SortOrder(..)
       , SortSpec(..)
       , DefaultSort(..)
       , Missing(..)
       , OpenCloseIndex(..)
       , Method(..)
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
       , AnIndicesQuery(..)
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
         ) where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import Database.Bloodhound.Types.Class
import GHC.Generics (Generic)
import Network.HTTP.Conduit
import qualified Network.HTTP.Types.Method as NHTM

data Version = Version { number          :: Text
                       , build_hash      :: Text
                       , build_timestamp :: UTCTime
                       , build_snapshot  :: Bool
                       , lucene_version  :: Text } deriving (Show, Generic)

data Status a = Status { ok      :: Bool
                       , status  :: Int
                       , name    :: Text
                       , version :: a
                       , tagline :: Text } deriving (Eq, Show)

data IndexSettings =
  IndexSettings { indexShards   :: ShardCount
                , indexReplicas :: ReplicaCount } deriving (Eq, Show)

defaultIndexSettings = IndexSettings (ShardCount 3) (ReplicaCount 2)

data Strategy = RoundRobinStrat | RandomStrat | HeadStrat deriving (Eq, Show)


type Reply = Network.HTTP.Conduit.Response L.ByteString
type Method = NHTM.Method

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

data Mapping = Mapping { typeName :: TypeName
                       , fields   :: [MappingField] } deriving (Eq, Show)

data BulkOperation =
    BulkIndex  IndexName MappingName DocId Value
  | BulkCreate IndexName MappingName DocId Value
  | BulkDelete IndexName MappingName DocId
  | BulkUpdate IndexName MappingName DocId Value deriving (Eq, Show)

data EsResult a = EsResult { _index   :: Text
                           , _type    :: Text
                           , _id      :: Text
                           , _version :: Int
                           , found    :: Maybe Bool
                           , _source  :: a } deriving (Eq, Show)

type Sort = [SortSpec]

data SortSpec = DefaultSortSpec DefaultSort
              | GeoDistanceSortSpec SortOrder GeoPoint DistanceUnit deriving (Eq, Show)

data DefaultSort =
  DefaultSort { sortFieldName  :: FieldName
              , sortOrder      :: SortOrder
                                  -- default False
              , ignoreUnmapped :: Bool
              , sortMode       :: Maybe SortMode
              , missingSort    :: Maybe Missing
              , nestedFilter   :: Maybe Filter } deriving (Eq, Show)

data SortOrder = Ascending
               | Descending deriving (Eq, Show)

data Missing = LastMissing
             | FirstMissing
             | CustomMissing Text deriving (Eq, Show)

data SortMode = SortMin
              | SortMax
              | SortSum
              | SortAvg deriving (Eq, Show)

mkSort :: FieldName -> SortOrder -> DefaultSort
mkSort fieldName sortOrder = DefaultSort fieldName sortOrder False Nothing Nothing Nothing

type Cache     = Bool -- caching on/off
defaultCache   = False

type PrefixValue = Text

data BooleanOperator = And | Or deriving (Eq, Show)

newtype ShardCount               = ShardCount   Int deriving (Eq, Show, Generic)
newtype ReplicaCount             = ReplicaCount Int deriving (Eq, Show, Generic)
newtype Server                   = Server String deriving (Eq, Show)
newtype IndexName                = IndexName String deriving (Eq, Generic, Show)
newtype MappingName              = MappingName String deriving (Eq, Generic, Show)
newtype DocId                    = DocId String deriving (Eq, Generic, Show)
newtype QueryString              = QueryString Text deriving (Eq, Show)
newtype FieldName                = FieldName Text deriving (Eq, Show)
newtype CacheName                = CacheName Text deriving (Eq, Show)
newtype CacheKey                 = CacheKey  Text deriving (Eq, Show)
newtype Existence                = Existence Bool deriving (Eq, Show)
newtype NullValue                = NullValue Bool deriving (Eq, Show)
newtype CutoffFrequency          = CutoffFrequency Double deriving (Eq, Show, Generic)
newtype Analyzer                 = Analyzer Text deriving (Eq, Show, Generic)
newtype MaxExpansions            = MaxExpansions Int deriving (Eq, Show, Generic)
newtype Lenient                  = Lenient Bool deriving (Eq, Show, Generic)
newtype Tiebreaker               = Tiebreaker Double deriving (Eq, Show, Generic)
newtype Boost                    = Boost Double deriving (Eq, Show, Generic)
newtype BoostTerms               = BoostTerms Double deriving (Eq, Show)
newtype MinimumMatch             = MinimumMatch Int deriving (Eq, Show, Generic)
newtype MinimumMatchText         = MinimumMatchText Text deriving (Eq, Show)
newtype DisableCoord             = DisableCoord Bool deriving (Eq, Show, Generic)
newtype IgnoreTermFrequency      = IgnoreTermFrequency Bool deriving (Eq, Show)
newtype MinimumTermFrequency     = MinimumTermFrequency Int deriving (Eq, Show)
newtype MaxQueryTerms            = MaxQueryTerms Int deriving (Eq, Show)
newtype Fuzziness                = Fuzziness Double deriving (Eq, Show)
newtype PrefixLength             = PrefixLength Int deriving (Eq, Show)
newtype TypeName                 = TypeName Text deriving (Eq, Show)
newtype PercentMatch             = PercentMatch Double deriving (Eq, Show)
newtype StopWord                 = StopWord Text deriving (Eq, Show)
newtype QueryPath                = QueryPath Text deriving (Eq, Show)
newtype AllowLeadingWildcard     = AllowLeadingWildcard Bool deriving (Eq, Show)
newtype LowercaseExpanded        = LowercaseExpanded Bool deriving (Eq, Show)
newtype EnablePositionIncrements = EnablePositionIncrements Bool deriving (Eq, Show)
newtype AnalyzeWildcard          = AnalyzeWildcard Bool deriving (Eq, Show)
newtype GeneratePhraseQueries    = GeneratePhraseQueries Bool deriving (Eq, Show)
newtype Locale                   = Locale Text deriving (Eq, Show)
newtype MaxWordLength            = MaxWordLength Int deriving (Eq, Show)
newtype MinWordLength            = MinWordLength Int deriving (Eq, Show)
newtype PhraseSlop               = PhraseSlop Int deriving (Eq, Show)
newtype MinDocFrequency          = MinDocFrequency Int deriving (Eq, Show)
newtype MaxDocFrequency          = MaxDocFrequency Int deriving (Eq, Show)

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

data Query = TermQuery                   Term (Maybe Boost)
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
              , regexpQueryCacheName :: CacheName
              , regexpQueryCache     :: Cache
              , regexpQueryCacheKey  :: CacheKey } deriving (Eq, Show)

data RangeQuery =
  RangeQuery { rangeQueryField :: FieldName
             , rangeQueryRange :: Either HalfRange Range
             , rangeQueryBoost :: Boost } deriving (Eq, Show)

data SimpleQueryStringQuery =
  SimpleQueryStringQuery { simpleQueryStringQuery             :: QueryString
                         , simpleQueryStringField             :: Maybe FieldOrFields
                         , simpleQueryStringOperator          :: Maybe BooleanOperator
                         , simpleQueryStringAnalyzer          :: Maybe Analyzer
                         , simpleQueryStringFlags             :: Maybe [SimpleQueryFlag]
                         , simpleQueryStringLowercaseExpanded :: Maybe LowercaseExpanded
                         , simpleQueryStringLocale            :: Maybe Locale
                         } deriving (Eq, Show)

data SimpleQueryFlag = SimpleQueryAll
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
  QueryStringQuery { queryStringQuery                    :: QueryString
                   , queryStringDefaultField             :: Maybe FieldOrFields
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

data FieldOrFields = FofField FieldName
                   | FofFields [FieldName] deriving (Eq, Show)

data PrefixQuery =
  PrefixQuery { prefixQueryField       :: FieldName
              , prefixQueryPrefixValue :: Text
              , prefixQueryBoost       :: Maybe Boost } deriving (Eq, Show)

data NestedQuery =
  NestedQuery { nestedQueryPath      :: QueryPath
              , nestedQueryScoreType :: ScoreType
              , nestedQuery          :: Query } deriving (Eq, Show)

data MoreLikeThisFieldQuery =
  MoreLikeThisFieldQuery { moreLikeThisFieldText            :: Text
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
  MoreLikeThisQuery { moreLikeThisText            :: Text
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
  IndicesQuery { indicesQueryIndices :: [IndexName]
               , indicesQuery          :: Query
                 -- default "all"
               , indicesQueryNoMatch   :: Maybe Query } deriving (Eq, Show)

data HasParentQuery =
  HasParentQuery { hasParentQueryType      :: TypeName
                 , hasParentQuery          :: Query
                 , hasParentQueryScoreType :: Maybe ScoreType } deriving (Eq, Show)

data HasChildQuery =
  HasChildQuery { hasChildQueryType      :: TypeName
                , hasChildQuery          :: Query
                , hasChildQueryScoreType :: Maybe ScoreType } deriving (Eq, Show)

data ScoreType = ScoreTypeMax
               | ScoreTypeSum
               | ScoreTypeAvg
               | ScoreTypeNone deriving (Eq, Show)

data FuzzyQuery = FuzzyQuery { fuzzyQueryField         :: FieldName
                             , fuzzyQueryValue         :: Text
                             , fuzzyQueryPrefixLength  :: PrefixLength
                             , fuzzyQueryMaxExpansions :: MaxExpansions
                             , fuzzyQueryFuzziness     :: Fuzziness
                             , fuzzyQueryBoost         :: Maybe Boost
                             } deriving (Eq, Show)

data FuzzyLikeFieldQuery =
  FuzzyLikeFieldQuery { fuzzyLikeField                    :: FieldName
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
  FuzzyLikeThisQuery { fuzzyLikeFields              :: [FieldName]
                     , fuzzyLikeText                :: Text
                     , fuzzyLikeMaxQueryTerms       :: MaxQueryTerms
                     , fuzzyLikeIgnoreTermFrequency :: IgnoreTermFrequency
                     , fuzzyLikeFuzziness           :: Fuzziness
                     , fuzzyLikePrefixLength        :: PrefixLength
                     , fuzzyLikeBoost               :: Boost
                     , fuzzyLikeAnalyzer            :: Maybe Analyzer
                     } deriving (Eq, Show)

data FilteredQuery =
  FilteredQuery { filteredQuery  :: Query
                , filteredFilter :: Filter } deriving (Eq, Show)

data DisMaxQuery = DisMaxQuery { disMaxQueries    :: [Query]
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

mkMatchQuery :: FieldName -> QueryString -> MatchQuery
mkMatchQuery field query = MatchQuery field query Or ZeroTermsNone Nothing Nothing Nothing Nothing Nothing

data MatchQueryType = MatchPhrase
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

mkMultiMatchQuery :: [FieldName] -> QueryString -> MultiMatchQuery
mkMultiMatchQuery fields query =
  MultiMatchQuery fields query Or ZeroTermsNone Nothing Nothing Nothing Nothing Nothing Nothing

data MultiMatchQueryType = MultiMatchBestFields
                         | MultiMatchMostFields
                         | MultiMatchCrossFields
                         | MultiMatchPhrase
                         | MultiMatchPhrasePrefix deriving (Eq, Show)

data BoolQuery = BoolQuery { boolQueryMustMatch          :: Maybe Query
                           , boolQueryMustNotMatch       :: Maybe Query
                           , boolQueryShouldMatch        :: Maybe [Query]
                           , boolQueryMinimumShouldMatch :: Maybe MinimumMatch
                           , boolQueryBoost              :: Maybe Boost
                           , boolQueryDisableCoord       :: Maybe DisableCoord
                           } deriving (Eq, Show)

mkBoolQuery :: Maybe Query -> Maybe Query -> Maybe [Query] -> BoolQuery
mkBoolQuery must mustNot should = BoolQuery must mustNot should Nothing Nothing Nothing

data BoostingQuery = BoostingQuery { positiveQuery :: Query
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

data CommonMinimumMatch = CommonMinimumMatchHighLow MinimumMatchHighLow
                        | CommonMinimumMatch MinimumMatch deriving (Eq, Show)

data MinimumMatchHighLow =
  MinimumMatchHighLow { lowFreq :: MinimumMatch
                      , highFreq :: MinimumMatch } deriving (Eq, Show)

data Filter = AndFilter [Filter] Cache
            | OrFilter  [Filter] Cache
            | NotFilter Filter   Cache
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
              deriving (Eq, Show)

data ZeroTermsQuery = ZeroTermsNone | ZeroTermsAll deriving (Eq, Show)

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
newtype RegexpFlags = RegexpFlags Text deriving (Eq, Show)

halfRangeToKV :: HalfRange -> (Text, Double)
halfRangeToKV (HalfRangeLt  (LessThan n))      = ("lt",  n)
halfRangeToKV (HalfRangeLte (LessThanEq n))    = ("lte", n)
halfRangeToKV (HalfRangeGt  (GreaterThan n))   = ("gt",  n)
halfRangeToKV (HalfRangeGte (GreaterThanEq n)) = ("gte", n)

rangeToKV :: Range -> (Text, Double, Text, Double)
rangeToKV (RangeLtGt   (LessThan m)   (GreaterThan n))   = ("lt",  m, "gt",  n)
rangeToKV (RangeLtGte  (LessThan m)   (GreaterThanEq n)) = ("lt",  m, "gte", n)
rangeToKV (RangeLteGt  (LessThanEq m) (GreaterThan n))   = ("lte", m, "gt",  n)
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

-- maybeJsonF :: (ToJSON (f Value), ToJSON a, Functor f) =>
--              Text -> Maybe (f a) -> [(Text, Value)]
maybeJsonF field (Just value) = [field .= fmap toJSON value]
maybeJsonF _ _ = []

showText :: Show a => a -> Text
showText = T.pack . show
