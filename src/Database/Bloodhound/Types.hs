{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE UndecidableInstances       #-}

-------------------------------------------------------------------------------
-- |
-- Module : Database.Bloodhound.Types
-- Copyright : (C) 2014 Chris Allen
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
       , mkDocVersion
       , docVersionNumber
       , toTerms
       , toDateHistogram
       , omitNulls
       , BH(..)
       , runBH
       , BHEnv(..)
       , MonadBH(..)
       , Version(..)
       , Status(..)
       , Existence(..)
       , NullValue(..)
       , IndexSettings(..)
       , IndexTemplate(..)
       , Server(..)
       , Reply
       , EsResult(..)
       , EsResultFound(..)
       , EsError(..)
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
       , ScrollId
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

       , Aggregation(..)
       , Aggregations
       , AggregationResults
       , Bucket(..)
       , BucketAggregation(..)
       , TermsAggregation(..)
       , ValueCountAggregation(..)
       , FilterAggregation(..)
       , DateHistogramAggregation(..)

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

       , TermsResult(..)
       , DateHistogramResult(..)
         ) where

import           Control.Applicative
import           Control.Monad.Catch
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Data.Aeson
import           Data.Aeson.Types                (Pair, Parser, emptyObject,
                                                  parseMaybe)
import qualified Data.ByteString.Lazy.Char8      as L
import           Data.Char
import           Data.Hashable                   (Hashable)
import qualified Data.HashMap.Strict             as HM
import           Data.List                       (foldl', nub)
import           Data.List.NonEmpty              (NonEmpty (..), toList)
import qualified Data.Map.Strict                 as M
import           Data.Maybe
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Data.Time.Clock                 (UTCTime)
import qualified Data.Traversable                as DT
import           Data.Typeable                   (Typeable)
import qualified Data.Vector                     as V
import           GHC.Enum
import           GHC.Generics                    (Generic)
import           Network.HTTP.Client
import qualified Network.HTTP.Types.Method       as NHTM

import           Database.Bloodhound.Types.Class

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Database.Bloodhound
-- >>> let testServer = (Server "http://localhost:9200")
-- >>> let testIndex = IndexName "twitter"
-- >>> let testMapping = MappingName "tweet"
-- >>> let defaultIndexSettings = IndexSettings (ShardCount 3) (ReplicaCount 2)

-- defaultIndexSettings is exported by Database.Bloodhound as well
-- no trailing slashes in servers, library handles building the path.

{-| Common environment for Elasticsearch calls. Connections will be
    pipelined according to the provided HTTP connection manager.
-}
data BHEnv = BHEnv { bhServer  :: Server
                   , bhManager :: Manager
                   }

{-| All API calls to Elasticsearch operate within
    MonadBH. The idea is that it can be easily embedded in your
    own monad transformer stack. A default instance for a ReaderT and
    alias 'BH' is provided for the simple case.
-}
class (Functor m, Applicative m, MonadIO m) => MonadBH m where
  getBHEnv :: m BHEnv

newtype BH m a = BH {
      unBH :: ReaderT BHEnv m a
    } deriving ( Functor
               , Applicative
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


instance (Functor m, Applicative m, MonadIO m) => MonadBH (ReaderT BHEnv m) where
  getBHEnv = ask

runBH :: BHEnv -> BH m a -> m a
runBH e f = runReaderT (unBH f) e

{-| 'Version' is embedded in 'Status' -}
data Version = Version { number          :: Text
                       , build_hash      :: Text
                       , build_timestamp :: UTCTime
                       , build_snapshot  :: Bool
                       , lucene_version  :: Text } deriving (Eq, Show, Generic, Typeable)

{-| 'Status' is a data type for describing the JSON body returned by
    Elasticsearch when you query its status. This was deprecated in 1.2.0.

   <http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/indices-status.html#indices-status>
-}

data Status = Status { ok      :: Maybe Bool
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
                 , fieldDefinition  :: FieldDefinition } deriving (Eq, Show)

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
    successful Elasticsearch lookups or lookups that couldn't find the document.
-}
data EsResult a = EsResult { _index      :: Text
                           , _type       :: Text
                           , _id         :: Text
                           , foundResult :: Maybe (EsResultFound a)} deriving (Eq, Show)

{-| 'EsResultFound' contains the document and its metadata inside of an
    'EsResult' when the document was successfully found.
-}
data EsResultFound a = EsResultFound {  _version :: DocVersion
                                     , _source   :: a } deriving (Eq, Show)

{-| 'EsError' is the generic type that will be returned when there was a
    problem. If you can't parse the expected response, its a good idea to
    try parsing this.
-}
data EsError = EsError { errorStatus  :: Int
                       , errorMessage :: Text } deriving (Eq, Show)

data IndexAlias = IndexAlias { srcIndex   :: IndexName
                             , indexAlias :: IndexAliasName } deriving (Eq, Show)

newtype IndexAliasName = IndexAliasName { indexAliasName :: IndexName } deriving (Eq, Show, ToJSON)

data IndexAliasAction = AddAlias IndexAlias IndexAliasCreate
                      | RemoveAlias IndexAlias deriving (Show, Eq, Typeable)

data IndexAliasCreate = IndexAliasCreate { aliasCreateRouting :: Maybe AliasRouting
                                         , aliasCreateFilter  :: Maybe Filter}
                                         deriving (Show, Eq, Typeable)

data AliasRouting = AllAliasRouting RoutingValue
                  | GranularAliasRouting (Maybe SearchAliasRouting) (Maybe IndexAliasRouting)
                  deriving (Show, Eq, Typeable)

newtype SearchAliasRouting = SearchAliasRouting (NonEmpty RoutingValue) deriving (Show, Eq, Typeable)

newtype IndexAliasRouting = IndexAliasRouting RoutingValue deriving (Show, Eq, ToJSON, FromJSON, Typeable)

newtype RoutingValue = RoutingValue { routingValue :: Text } deriving (Show, Eq, ToJSON, FromJSON, Typeable)

newtype IndexAliasesSummary = IndexAliasesSummary { indexAliasesSummary :: [IndexAliasSummary] } deriving (Show, Eq, Typeable)

{-| 'IndexAliasSummary' is a summary of an index alias configured for a server. -}
data IndexAliasSummary = IndexAliasSummary { indexAliasSummaryAlias  :: IndexAlias
                                           , indexAliasSummaryCreate :: IndexAliasCreate} deriving (Show, Eq)

{-| 'DocVersion' is an integer version number for a document between 1
and 9.2e+18 used for <<https://www.elastic.co/guide/en/elasticsearch/guide/current/optimistic-concurrency-control.html optimistic concurrency control>>.
-}
newtype DocVersion = DocVersion {
      docVersionNumber :: Int
    } deriving (Eq, Show, Ord, ToJSON)

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
    deriving (Eq, Show, Ord, Bounded, Enum, ToJSON)

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
                    deriving (Show, Eq, Ord)

{-| 'DocumentParent' is used to specify a parent document.
-}
newtype DocumentParent = DocumentParent DocId
  deriving (Eq, Show)

{-| 'IndexDocumentSettings' are special settings supplied when indexing
a document. For the best backwards compatiblity when new fields are
added, you should probably prefer to start with 'defaultIndexDocumentSettings'
-}
data IndexDocumentSettings =
  IndexDocumentSettings { idsVersionControl :: VersionControl
                        , idsParent         :: Maybe DocumentParent
                        } deriving (Eq, Show)

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
data BooleanOperator = And | Or deriving (Eq, Show, Typeable)

{-| 'ShardCount' is part of 'IndexSettings'
-}
newtype ShardCount = ShardCount Int deriving (Eq, Show, Generic, ToJSON, FromJSON, Typeable)

{-| 'ReplicaCount' is part of 'IndexSettings'
-}
newtype ReplicaCount = ReplicaCount Int deriving (Eq, Show, Generic, ToJSON, FromJSON, Typeable)

{-| 'Server' is used with the client functions to point at the ES instance
-}
newtype Server = Server Text deriving (Eq, Show)

{-| 'IndexName' is used to describe which index to query/create/delete
-}
newtype IndexName = IndexName Text deriving (Eq, Generic, Show, ToJSON, FromJSON, Typeable)

{-| 'TemplateName' is used to describe which template to query/create/delete
-}
newtype TemplateName = TemplateName Text deriving (Eq, Show, Generic, ToJSON, FromJSON, Typeable)

{-| 'TemplatePattern' represents a pattern which is matched against index names
-}
newtype TemplatePattern = TemplatePattern Text deriving (Eq, Show, Generic, ToJSON, FromJSON, Typeable)

{-| 'MappingName' is part of mappings which are how ES describes and schematizes
    the data in the indices.
-}
newtype MappingName = MappingName Text deriving (Eq, Generic, Show, ToJSON, FromJSON, Typeable)

{-| 'DocId' is a generic wrapper value for expressing unique Document IDs.
    Can be set by the user or created by ES itself. Often used in client
    functions for poking at specific documents.
-}
newtype DocId = DocId Text deriving (Eq, Generic, Show, ToJSON, FromJSON, Typeable)

{-| 'QueryString' is used to wrap query text bodies, be they human written or not.
-}
newtype QueryString = QueryString Text deriving (Eq, Generic, Show, ToJSON, FromJSON, Typeable)

{-| 'FieldName' is used all over the place wherever a specific field within
     a document needs to be specified, usually in 'Query's or 'Filter's.
-}
newtype FieldName = FieldName Text deriving (Eq, Show, ToJSON, FromJSON, Typeable)


{-| 'Script' is often used in place of 'FieldName' to specify more
complex ways of extracting a value from a document.
-}
newtype Script = Script { scriptText :: Text } deriving (Eq, Show)

{-| 'CacheName' is used in 'RegexpFilter' for describing the
    'CacheKey' keyed caching behavior.
-}
newtype CacheName = CacheName Text deriving (Eq, Show, ToJSON, FromJSON, Typeable)

{-| 'CacheKey' is used in 'RegexpFilter' to key regex caching.
-}
newtype CacheKey =
  CacheKey Text deriving (Eq, Show, ToJSON, FromJSON, Typeable)
newtype Existence =
  Existence Bool deriving (Eq, Show, ToJSON, FromJSON, Typeable)
newtype NullValue =
  NullValue Bool deriving (Eq, Show, ToJSON, FromJSON, Typeable)
newtype CutoffFrequency =
  CutoffFrequency Double deriving (Eq, Show, Generic, ToJSON, FromJSON, Typeable)
newtype Analyzer =
  Analyzer Text deriving (Eq, Show, Generic, ToJSON, FromJSON, Typeable)
newtype MaxExpansions =
  MaxExpansions Int deriving (Eq, Show, Generic, ToJSON, FromJSON, Typeable)

{-| 'Lenient', if set to true, will cause format based failures to be
    ignored. I don't know what the bloody default is, Elasticsearch
    documentation didn't say what it was. Let me know if you figure it out.
-}
newtype Lenient =
  Lenient Bool deriving (Eq, Show, Generic, ToJSON, FromJSON, Typeable)
newtype Tiebreaker =
  Tiebreaker Double deriving (Eq, Show, Generic, ToJSON, FromJSON, Typeable)
newtype Boost =
  Boost Double deriving (Eq, Show, Generic, ToJSON, FromJSON, Typeable)
newtype BoostTerms =
  BoostTerms Double deriving (Eq, Show, Generic, ToJSON, FromJSON, Typeable)

{-| 'MinimumMatch' controls how many should clauses in the bool query should
     match. Can be an absolute value (2) or a percentage (30%) or a
     combination of both.
-}
newtype MinimumMatch =
  MinimumMatch Int deriving (Eq, Show, Generic, ToJSON, FromJSON, Typeable)
newtype DisableCoord =
  DisableCoord Bool deriving (Eq, Show, Generic, ToJSON, FromJSON, Typeable)
newtype IgnoreTermFrequency =
  IgnoreTermFrequency Bool deriving (Eq, Show, Generic, ToJSON, FromJSON, Typeable)
newtype MinimumTermFrequency =
  MinimumTermFrequency Int deriving (Eq, Show, Generic, ToJSON, FromJSON, Typeable)
newtype MaxQueryTerms =
  MaxQueryTerms Int deriving (Eq, Show, Generic, ToJSON, FromJSON, Typeable)
newtype Fuzziness =
  Fuzziness Double deriving (Eq, Show, Generic, ToJSON, FromJSON, Typeable)

{-| 'PrefixLength' is the prefix length used in queries, defaults to 0. -}
newtype PrefixLength =
  PrefixLength Int deriving (Eq, Show, Generic, ToJSON, FromJSON, Typeable)
newtype TypeName =
  TypeName Text deriving (Eq, Show, Generic, ToJSON, FromJSON, Typeable)
newtype PercentMatch =
  PercentMatch Double deriving (Eq, Show, Generic, ToJSON, FromJSON, Typeable)
newtype StopWord =
  StopWord Text deriving (Eq, Show, Generic, ToJSON, FromJSON, Typeable)
newtype QueryPath =
  QueryPath Text deriving (Eq, Show, Generic, ToJSON, FromJSON, Typeable)

{-| Allowing a wildcard at the beginning of a word (eg "*ing") is particularly
    heavy, because all terms in the index need to be examined, just in case
    they match. Leading wildcards can be disabled by setting
    'AllowLeadingWildcard' to false. -}
newtype AllowLeadingWildcard =
  AllowLeadingWildcard     Bool deriving (Eq, Show, Generic, ToJSON, FromJSON, Typeable)
newtype LowercaseExpanded =
  LowercaseExpanded        Bool deriving (Eq, Show, Generic, ToJSON, FromJSON, Typeable)
newtype EnablePositionIncrements =
  EnablePositionIncrements Bool deriving (Eq, Show, Generic, ToJSON, FromJSON, Typeable)

{-| By default, wildcard terms in a query are not analyzed.
    Setting 'AnalyzeWildcard' to true enables best-effort analysis.
-}
newtype AnalyzeWildcard = AnalyzeWildcard Bool deriving (Eq, Show, Generic, ToJSON, FromJSON, Typeable)

{-| 'GeneratePhraseQueries' defaults to false.
-}
newtype GeneratePhraseQueries =
  GeneratePhraseQueries Bool deriving (Eq, Show, Generic, ToJSON, FromJSON, Typeable)

{-| 'Locale' is used for string conversions - defaults to ROOT.
-}
newtype Locale        = Locale        Text deriving (Eq, Show, Generic, ToJSON, FromJSON, Typeable)
newtype MaxWordLength = MaxWordLength Int  deriving (Eq, Show, Generic, ToJSON, FromJSON, Typeable)
newtype MinWordLength = MinWordLength Int  deriving (Eq, Show, Generic, ToJSON, FromJSON, Typeable)

{-| 'PhraseSlop' sets the default slop for phrases, 0 means exact
     phrase matches. Default is 0.
-}
newtype PhraseSlop      = PhraseSlop      Int deriving (Eq, Show, Generic, ToJSON, FromJSON, Typeable)
newtype MinDocFrequency = MinDocFrequency Int deriving (Eq, Show, Generic, ToJSON, FromJSON, Typeable)
newtype MaxDocFrequency = MaxDocFrequency Int deriving (Eq, Show, Generic, ToJSON, FromJSON, Typeable)

{-| 'unpackId' is a silly convenience function that gets used once.
-}
unpackId :: DocId -> Text
unpackId (DocId docId) = docId

type TrackSortScores = Bool
newtype From = From Int deriving (Eq, Show, ToJSON)
newtype Size = Size Int deriving (Eq, Show, ToJSON)

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
                     , source          :: Maybe Source } deriving (Eq, Show)

data SearchType = SearchTypeQueryThenFetch
                | SearchTypeDfsQueryThenFetch
                | SearchTypeCount
                | SearchTypeScan
                | SearchTypeQueryAndFetch
                | SearchTypeDfsQueryAndFetch
  deriving (Eq, Show)

data Source =
    NoSource
  | SourcePatterns PatternOrPatterns
  | SourceIncludeExclude Include Exclude
    deriving (Show, Eq)

data PatternOrPatterns = PopPattern   Pattern
                       | PopPatterns [Pattern] deriving (Eq, Show)

data Include = Include [Pattern] deriving (Eq, Show)
data Exclude = Exclude [Pattern] deriving (Eq, Show)

newtype Pattern = Pattern Text deriving (Eq, Show)

data Highlights = Highlights { globalsettings  :: Maybe HighlightSettings
                             , highlightFields :: [FieldHighlight]
                             } deriving (Show, Eq)

data FieldHighlight = FieldHighlight FieldName (Maybe HighlightSettings)
                      deriving (Show, Eq)


data HighlightSettings = Plain PlainHighlight
                       | Postings PostingsHighlight
                       | FastVector FastVectorHighlight
                         deriving (Show, Eq)
data PlainHighlight =
    PlainHighlight { plainCommon  :: Maybe CommonHighlight
                   , plainNonPost :: Maybe NonPostings } deriving (Show, Eq)

 -- This requires that index_options are set to 'offset' in the mapping.
data PostingsHighlight = PostingsHighlight (Maybe CommonHighlight) deriving (Show, Eq)

-- This requires that term_vector is set to 'with_positions_offsets' in the mapping.
data FastVectorHighlight =
    FastVectorHighlight { fvCommon          :: Maybe CommonHighlight
                        , fvNonPostSettings :: Maybe NonPostings
                        , boundaryChars     :: Maybe Text
                        , boundaryMaxScan   :: Maybe Int
                        , fragmentOffset    :: Maybe Int
                        , matchedFields     :: [Text]
                        , phraseLimit       :: Maybe Int
                        } deriving (Show, Eq)

data CommonHighlight =
    CommonHighlight { order             :: Maybe Text
                    , forceSource       :: Maybe Bool
                    , tag               :: Maybe HighlightTag
                    , encoder           :: Maybe HighlightEncoder
                    , noMatchSize       :: Maybe Int
                    , highlightQuery    :: Maybe Query
                    , requireFieldMatch :: Maybe Bool
                    } deriving (Show, Eq)

-- Settings that are only applicable to FastVector and Plain highlighters.
data NonPostings =
    NonPostings { fragmentSize      :: Maybe Int
                , numberOfFragments :: Maybe Int} deriving (Show, Eq)

data HighlightEncoder = DefaultEncoder
                      | HTMLEncoder
                      deriving (Show, Eq)

-- NOTE: Should the tags use some kind of HTML type, rather than Text?
data HighlightTag = TagSchema Text
                  | CustomTags ([Text], [Text]) -- Only uses more than the first value in the lists if fvh
                  deriving (Show, Eq)


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
  deriving (Eq, Show, Typeable)

data RegexpQuery =
  RegexpQuery { regexpQueryField :: FieldName
              , regexpQuery      :: Regexp
              , regexpQueryFlags :: RegexpFlags
              , regexpQueryBoost :: Maybe Boost
              } deriving (Eq, Show, Typeable)

data RangeQuery =
  RangeQuery { rangeQueryField :: FieldName
             , rangeQueryRange :: RangeValue
             , rangeQueryBoost :: Boost } deriving (Eq, Show, Typeable)

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
    } deriving (Eq, Show, Typeable)

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
  | SimpleQuerySlop deriving (Eq, Show, Typeable)

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
  } deriving (Eq, Show, Typeable)

mkQueryStringQuery :: QueryString -> QueryStringQuery
mkQueryStringQuery qs =
  QueryStringQuery qs Nothing Nothing
  Nothing Nothing Nothing Nothing
  Nothing Nothing Nothing Nothing
  Nothing Nothing Nothing Nothing
  Nothing Nothing

data FieldOrFields = FofField   FieldName
                   | FofFields (NonEmpty FieldName) deriving (Eq, Show, Typeable)

data PrefixQuery =
  PrefixQuery
  { prefixQueryField       :: FieldName
  , prefixQueryPrefixValue :: Text
  , prefixQueryBoost       :: Maybe Boost } deriving (Eq, Show, Typeable)

data NestedQuery =
  NestedQuery
  { nestedQueryPath      :: QueryPath
  , nestedQueryScoreType :: ScoreType
  , nestedQuery          :: Query } deriving (Eq, Show, Typeable)

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
  } deriving (Eq, Show, Typeable)

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
  } deriving (Eq, Show, Typeable)

data IndicesQuery =
  IndicesQuery
  { indicesQueryIndices :: [IndexName]
  , indicesQuery        :: Query
    -- default "all"
  , indicesQueryNoMatch :: Maybe Query } deriving (Eq, Show, Typeable)

data HasParentQuery =
  HasParentQuery
  { hasParentQueryType      :: TypeName
  , hasParentQuery          :: Query
  , hasParentQueryScoreType :: Maybe ScoreType } deriving (Eq, Show, Typeable)

data HasChildQuery =
  HasChildQuery
  { hasChildQueryType      :: TypeName
  , hasChildQuery          :: Query
  , hasChildQueryScoreType :: Maybe ScoreType } deriving (Eq, Show, Typeable)

data ScoreType =
  ScoreTypeMax
  | ScoreTypeSum
  | ScoreTypeAvg
  | ScoreTypeNone deriving (Eq, Show, Typeable)

data FuzzyQuery =
  FuzzyQuery { fuzzyQueryField         :: FieldName
             , fuzzyQueryValue         :: Text
             , fuzzyQueryPrefixLength  :: PrefixLength
             , fuzzyQueryMaxExpansions :: MaxExpansions
             , fuzzyQueryFuzziness     :: Fuzziness
             , fuzzyQueryBoost         :: Maybe Boost
             } deriving (Eq, Show, Typeable)

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
  } deriving (Eq, Show, Typeable)

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
  } deriving (Eq, Show, Typeable)

data FilteredQuery =
  FilteredQuery
  { filteredQuery  :: Query
  , filteredFilter :: Filter } deriving (Eq, Show, Typeable)

data DisMaxQuery =
  DisMaxQuery { disMaxQueries    :: [Query]
                -- default 0.0
              , disMaxTiebreaker :: Tiebreaker
              , disMaxBoost      :: Maybe Boost
              } deriving (Eq, Show, Typeable)

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
             , matchQueryBoost           :: Maybe Boost } deriving (Eq, Show, Typeable)

{-| 'mkMatchQuery' is a convenience function that defaults the less common parameters,
    enabling you to provide only the 'FieldName' and 'QueryString' to make a 'MatchQuery'
-}
mkMatchQuery :: FieldName -> QueryString -> MatchQuery
mkMatchQuery field query = MatchQuery field query Or ZeroTermsNone Nothing Nothing Nothing Nothing Nothing Nothing

data MatchQueryType =
  MatchPhrase
  | MatchPhrasePrefix deriving (Eq, Show, Typeable)

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
                  , multiMatchQueryLenient         :: Maybe Lenient } deriving (Eq, Show, Typeable)

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
  | MultiMatchPhrasePrefix deriving (Eq, Show, Typeable)

data BoolQuery =
  BoolQuery { boolQueryMustMatch          :: [Query]
            , boolQueryMustNotMatch       :: [Query]
            , boolQueryShouldMatch        :: [Query]
            , boolQueryMinimumShouldMatch :: Maybe MinimumMatch
            , boolQueryBoost              :: Maybe Boost
            , boolQueryDisableCoord       :: Maybe DisableCoord
            } deriving (Eq, Show, Typeable)

mkBoolQuery :: [Query] -> [Query] -> [Query] -> BoolQuery
mkBoolQuery must mustNot should =
  BoolQuery must mustNot should Nothing Nothing Nothing

data BoostingQuery =
  BoostingQuery { positiveQuery :: Query
                , negativeQuery :: Query
                , negativeBoost :: Boost } deriving (Eq, Show, Typeable)

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
                   } deriving (Eq, Show, Typeable)

data CommonMinimumMatch =
    CommonMinimumMatchHighLow MinimumMatchHighLow
  | CommonMinimumMatch        MinimumMatch
  deriving (Eq, Show, Typeable)

data MinimumMatchHighLow =
  MinimumMatchHighLow { lowFreq  :: MinimumMatch
                      , highFreq :: MinimumMatch } deriving (Eq, Show, Typeable)

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
              deriving (Eq, Show, Typeable)

data ZeroTermsQuery = ZeroTermsNone
                    | ZeroTermsAll deriving (Eq, Show, Typeable)

data RangeExecution = RangeExecutionIndex
                    | RangeExecutionFielddata deriving (Eq, Show, Typeable)

newtype Regexp = Regexp Text deriving (Eq, Show, FromJSON)

data RegexpFlags = AllRegexpFlags
                 | NoRegexpFlags
                 | SomeRegexpFlags (NonEmpty RegexpFlag) deriving (Eq, Show)

data RegexpFlag = AnyString
                | Automaton
                | Complement
                | Empty
                | Intersection
                | Interval deriving (Eq, Show)

newtype LessThan = LessThan Double deriving (Eq, Show)
newtype LessThanEq = LessThanEq Double deriving (Eq, Show)
newtype GreaterThan = GreaterThan Double deriving (Eq, Show)
newtype GreaterThanEq = GreaterThanEq Double deriving (Eq, Show)

newtype LessThanD = LessThanD UTCTime deriving (Eq, Show)
newtype LessThanEqD = LessThanEqD UTCTime deriving (Eq, Show)
newtype GreaterThanD = GreaterThanD UTCTime deriving (Eq, Show)
newtype GreaterThanEqD = GreaterThanEqD UTCTime deriving (Eq, Show)

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
                deriving (Eq, Show)

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
                 , termValue :: Text } deriving (Eq, Show, Typeable)

data BoolMatch = MustMatch    Term  Cache
               | MustNotMatch Term  Cache
               | ShouldMatch [Term] Cache deriving (Eq, Show, Typeable)

-- "memory" or "indexed"
data GeoFilterType = GeoFilterMemory
                   | GeoFilterIndexed deriving (Eq, Show, Typeable)

data LatLon = LatLon { lat :: Double
                     , lon :: Double } deriving (Eq, Show, Typeable)

data GeoBoundingBox =
  GeoBoundingBox { topLeft     :: LatLon
                 , bottomRight :: LatLon } deriving (Eq, Show, Typeable)

data GeoBoundingBoxConstraint =
  GeoBoundingBoxConstraint { geoBBField        :: FieldName
                           , constraintBox     :: GeoBoundingBox
                           , bbConstraintcache :: Cache
                           , geoType           :: GeoFilterType
                           } deriving (Eq, Show, Typeable)

data GeoPoint =
  GeoPoint { geoField :: FieldName
           , latLon   :: LatLon} deriving (Eq, Show, Typeable)

data DistanceUnit = Miles
                  | Yards
                  | Feet
                  | Inches
                  | Kilometers
                  | Meters
                  | Centimeters
                  | Millimeters
                  | NauticalMiles deriving (Eq, Show, Typeable)

data DistanceType = Arc
                  | SloppyArc -- doesn't exist <1.0
                  | Plane deriving (Eq, Show, Typeable)

data OptimizeBbox = OptimizeGeoFilterType GeoFilterType
                  | NoOptimizeBbox deriving (Eq, Show, Typeable)

data Distance =
  Distance { coefficient :: Double
           , unit        :: DistanceUnit } deriving (Eq, Show, Typeable)

data DistanceRange =
  DistanceRange { distanceFrom :: Distance
                , distanceTo   :: Distance } deriving (Eq, Show)

data SearchResult a =
  SearchResult { took         :: Int
               , timedOut     :: Bool
               , shards       :: ShardResult
               , searchHits   :: SearchHits a
               , aggregations :: Maybe AggregationResults
               , scrollId     :: Maybe ScrollId } deriving (Eq, Show)

type ScrollId = Text  -- Fixme: Newtype

type Score = Maybe Double

data SearchHits a =
  SearchHits { hitsTotal :: Int
             , maxScore  :: Score
             , hits      :: [Hit a] } deriving (Eq, Show)


instance Monoid (SearchHits a) where
  mempty = SearchHits 0 Nothing mempty
  mappend (SearchHits ta ma ha) (SearchHits tb mb hb) =
    SearchHits (ta + tb) (max ma mb) (ha <> hb)


data Hit a =
  Hit { hitIndex     :: IndexName
      , hitType      :: MappingName
      , hitDocId     :: DocId
      , hitScore     :: Score
      , hitSource    :: Maybe a
      , hitHighlight :: Maybe HitHighlight } deriving (Eq, Show)

data ShardResult =
  ShardResult { shardTotal       :: Int
              , shardsSuccessful :: Int
              , shardsFailed     :: Int } deriving (Eq, Show, Generic)

type HitHighlight = M.Map Text [Text]

showText :: Show a => a -> Text
showText = T.pack . show

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

data TimeInterval = Weeks
                  | Days
                  | Hours
                  | Minutes
                  | Seconds deriving (Eq)

data Interval = Year
              | Quarter
              | Month
              | Week
              | Day
              | Hour
              | Minute
              | Second
              | FractionalInterval Float TimeInterval deriving (Eq, Show)

data Aggregation = TermsAgg TermsAggregation
                 | DateHistogramAgg DateHistogramAggregation
                 | ValueCountAgg ValueCountAggregation
                 | FilterAgg FilterAggregation deriving (Eq, Show)


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

data DateHistogramAggregation = DateHistogramAggregation { dateField      :: FieldName
                                                         , dateInterval   :: Interval
                                                         , dateFormat     :: Maybe Text
                                                         -- pre and post deprecated in 1.5
                                                         , datePreZone    :: Maybe Text
                                                         , datePostZone   :: Maybe Text
                                                         , datePreOffset  :: Maybe Text
                                                         , datePostOffset :: Maybe Text
                                                         , dateAggs       :: Maybe Aggregations
                                                         } deriving (Eq, Show)

-- | See <https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-metrics-valuecount-aggregation.html> for more information.
data ValueCountAggregation = FieldValueCount FieldName
                           | ScriptValueCount Script deriving (Eq, Show)

-- | Single-bucket filter aggregations. See <https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-bucket-filter-aggregation.html#search-aggregations-bucket-filter-aggregation> for more information.
data FilterAggregation = FilterAggregation { faFilter :: Filter
                                           , faAggs   :: Maybe Aggregations} deriving (Eq, Show)

mkTermsAggregation :: Text -> TermsAggregation
mkTermsAggregation t = TermsAggregation (Left t) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

mkTermsScriptAggregation :: Text -> TermsAggregation
mkTermsScriptAggregation t = TermsAggregation (Right t) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

mkDateHistogram :: FieldName -> Interval -> DateHistogramAggregation
mkDateHistogram t i = DateHistogramAggregation t i Nothing Nothing Nothing Nothing Nothing Nothing

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
  show Weeks    = "w"
  show Days     = "d"
  show Hours    = "h"
  show Minutes  = "m"
  show Seconds  = "s"

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

type AggregationResults = M.Map Text Value

class BucketAggregation a where
  key :: a -> Text
  docCount :: a -> Int
  aggs :: a -> Maybe AggregationResults


data Bucket a = Bucket { buckets :: [a]} deriving (Show)

data TermsResult = TermsResult { termKey       :: Text
                               , termsDocCount :: Int
                               , termsAggs     :: Maybe AggregationResults } deriving (Show)

data DateHistogramResult = DateHistogramResult { dateKey           :: Int
                                               , dateKeyStr        :: Maybe Text
                                               , dateDocCount      :: Int
                                               , dateHistogramAggs :: Maybe AggregationResults } deriving (Show)

toTerms :: Text -> AggregationResults ->  Maybe (Bucket TermsResult)
toTerms t a = M.lookup t a >>= deserialize
  where deserialize = parseMaybe parseJSON

toDateHistogram :: Text -> AggregationResults -> Maybe (Bucket DateHistogramResult)
toDateHistogram t a = M.lookup t a >>= deserialize
  where deserialize = parseMaybe parseJSON

instance BucketAggregation TermsResult where
  key = termKey
  docCount = termsDocCount
  aggs = termsAggs

instance BucketAggregation DateHistogramResult where
  key = showText . dateKey
  docCount = dateDocCount
  aggs = dateHistogramAggs

instance (FromJSON a, BucketAggregation a) => FromJSON (Bucket a) where
  parseJSON (Object v) = Bucket <$>
                         v .: "buckets"
  parseJSON _ = mempty

instance FromJSON TermsResult where
  parseJSON (Object v) = TermsResult <$>
                         v .:   "key"       <*>
                         v .:   "doc_count" <*>
                         v .:?  "aggregations"
  parseJSON _ = mempty

instance FromJSON DateHistogramResult where
  parseJSON (Object v) = DateHistogramResult   <$>
                         v .:  "key"           <*>
                         v .:? "key_as_string" <*>
                         v .:  "doc_count"     <*>
                         v .:? "aggregations"
  parseJSON _ = mempty

instance Monoid Filter where
  mempty = IdentityFilter
  mappend a b = AndFilter [a, b] defaultCache

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
            object ["field"       .= fieldName
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

instance FromJSON Query where
  parseJSON v = withObject "Query" parse v
    where parse o = termQuery `taggedWith` "term"
                <|> termsQuery `taggedWith` "terms"
                <|> idsQuery `taggedWith` "ids"
                <|> queryQueryStringQuery `taggedWith` "query_string"
                <|> queryMatchQuery `taggedWith` "match"
                <|> queryMultiMatchQuery --TODO: is this a precedence issue?
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


omitNulls :: [(Text, Value)] -> Value
omitNulls = object . filter notNull where
  notNull (_, Null)      = False
  notNull (_, Array a) = (not . V.null) a
  notNull _              = True


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
                    <*> (optionalNE =<< o .:? "stop_words")
                    <*> o .:? "min_doc_freq"
                    <*> o .:? "max_doc_freq"
                    <*> o .:? "min_word_length"
                    <*> o .:? "max_word_length"
                    <*> o .:? "boost_terms"
                    <*> o .:? "boost"
                    <*> o .:? "analyzer"
          optionalNE = maybe (pure Nothing) (fmap Just . parseNEJSON)

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
                    <*> (optionalNE =<< o .:? "fields")
                    <*> o .:? "percent_terms_to_match"
                    <*> o .:? "min_term_freq"
                    <*> o .:? "max_query_terms"
                    <*> (optionalNE =<< o .:? "stop_words")
                    <*> o .:? "min_doc_freq"
                    <*> o .:? "max_doc_freq"
                    <*> o .:? "min_word_length"
                    <*> o .:? "max_word_length"
                    <*> o .:? "boost_terms"
                    <*> o .:? "boost"
                    <*> o .:? "analyzer"
          optionalNE = maybe (pure Nothing) (fmap Just . parseNEJSON)

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
                 , "tiebreaker" .= tb
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
                           <*> o .:? "tiebreaker"
                           <*> o .:? "type"
                           <*> o .:? "cutoff_frequency"
                           <*> o .:? "analyzer"
                           <*> o .:? "max_expansions"
                           <*> o .:? "lenient"

instance ToJSON MultiMatchQueryType where
  toJSON MultiMatchBestFields = "best_fields"
  toJSON MultiMatchMostFields = "most_fields"
  toJSON MultiMatchCrossFields = "cross_fields"
  toJSON MultiMatchPhrase = "phrase"
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
  toJSON Or = String "or"

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
  toJSON MatchPhrase = "phrase"
  toJSON MatchPhrasePrefix = "phrase_prefix"

instance FromJSON MatchQueryType where
  parseJSON = withText "MatchQueryType" parse
    where parse "phrase"        = pure MatchPhrase
          parse "phrase_prefix" = pure MatchPhrasePrefix
          parse t               = fail ("Unexpected MatchQueryType: " <> show t)

instance FromJSON Status where
  parseJSON (Object v) = Status <$>
                         v .:? "ok" <*>
                         v .: "status" <*>
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
                         v .: "error"
  parseJSON _ = empty

instance FromJSON IndexAliasesSummary where
  parseJSON = withObject "IndexAliasesSummary" parse
    where parse o = IndexAliasesSummary . mconcat <$> mapM (uncurry go) (HM.toList o)
          go ixn = withObject "index aliases" $ \ia -> do
                     aliases <- ia .: "aliases"
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
  toJSON (Search query sFilter sort searchAggs highlight sTrackSortScores sFrom sSize _ sFields sSource) =
    omitNulls [ "query"        .= query
              , "filter"       .= sFilter
              , "sort"         .= sort
              , "aggregations" .= searchAggs
              , "highlight"    .= highlight
              , "from"         .= sFrom
              , "size"         .= sSize
              , "track_scores" .= sTrackSortScores
              , "fields"       .= sFields
              , "_source"      .= sSource]


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
  toJSON LastMissing = String "_last"
  toJSON FirstMissing = String "_first"
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
          parse t = fail ("Unexpected ScoreType: " <> show t)

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
                  validForNumber c = isNumber c
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
          parse u = fail ("Unrecognized DistanceUnit: " <> show u)

instance ToJSON DistanceType where
  toJSON Arc       = String "arc"
  toJSON SloppyArc = String "sloppy_arc"
  toJSON Plane     = String "plane"

instance FromJSON DistanceType where
  parseJSON = withText "DistanceType" parse
    where parse "arc" = pure Arc
          parse "sloppy_arc" = pure SloppyArc
          parse "plane" = pure Plane
          parse t = fail ("Unrecognized DistanceType: " <> show t)


instance ToJSON OptimizeBbox where
  toJSON NoOptimizeBbox = String "none"
  toJSON (OptimizeGeoFilterType gft) = toJSON gft

instance FromJSON OptimizeBbox where
  parseJSON v = withText "NoOptimizeBbox" parseNoOptimize v
            <|> parseOptimize v
    where parseNoOptimize "none" = pure NoOptimizeBbox
          parseNoOptimize _ = mzero
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
                         v .:? "_scroll_id"
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
