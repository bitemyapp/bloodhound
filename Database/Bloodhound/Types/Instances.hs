{-# LANGUAGE DeriveGeneric #-}

module Database.Bloodhound.Types.Instances
       ( Monoid(..)
       , Seminearring(..)
       , ToJSON(..)
       ) where

import Control.Applicative
import Data.Aeson
import Data.Maybe (catMaybes)
import Data.Monoid
import qualified Data.Text as T
import Database.Bloodhound.Types
import Database.Bloodhound.Types.Class ()
import Prelude hiding (filter)

instance Monoid Filter where
  mempty = IdentityFilter
  mappend a b = AndFilter [a, b] defaultCache

instance Seminearring Filter where
  a <||> b = OrFilter [a, b] defaultCache

instance ToJSON Filter where
  toJSON (AndFilter filters cache) =
    object ["and"     .= fmap toJSON filters
           , "_cache" .= cache]

  toJSON (OrFilter filters cache) =
    object ["or"      .= fmap toJSON filters
           , "_cache" .= cache]

  toJSON (NotFilter filter cache) =
    object ["not" .=
            object ["filter"  .= toJSON filter
                   , "_cache" .= cache]]

  toJSON (IdentityFilter) =
    object ["match_all" .= object []]

  toJSON (ExistsFilter (FieldName fieldName)) =
    object ["exists"  .= object
            ["field"  .= fieldName]]

  toJSON (BoolFilter boolMatch) =
    object ["bool"    .= toJSON boolMatch]

  toJSON (GeoBoundingBoxFilter bbConstraint filterType) =
    object ["geo_bounding_box" .= toJSON bbConstraint
           , "type" .= toJSON filterType]

  toJSON (GeoDistanceFilter (GeoPoint (FieldName distanceGeoField) geoDistLatLon)
          distance distanceType optimizeBbox cache) =
    object ["geo_distance" .=
            object ["distance" .= toJSON distance
                   , "distance_type" .= toJSON distanceType
                   , "optimize_bbox" .= optimizeBbox
                   , distanceGeoField .= toJSON geoDistLatLon
                   , "_cache" .= cache]]                   

  toJSON (GeoDistanceRangeFilter (GeoPoint (FieldName gddrField) drLatLon)
          (DistanceRange geoDistRangeDistFrom drDistanceTo)) =
    object ["geo_distance_range" .=
            object ["from" .= toJSON geoDistRangeDistFrom
                   , "to"  .= toJSON drDistanceTo
                   , gddrField .= toJSON drLatLon]]

  toJSON (GeoPolygonFilter (FieldName geoPolygonFilterField) latLons) =
    object ["geo_polygon" .=
            object [geoPolygonFilterField .=
                    object ["points" .= fmap toJSON latLons]]]

  toJSON (IdsFilter (MappingName mappingName) values) =
    object ["ids" .=
            object ["type" .= mappingName
                   , "values" .= fmap (T.pack . unpackId) values]]

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

  toJSON (RangeFilter (FieldName fieldName) (Left halfRange) rangeExecution cache) =
    object ["range" .=
            object [fieldName .=
                    object [key .= val]
                   , "execution" .= toJSON rangeExecution
                   , "_cache" .= cache]]
    where
      (key, val) = halfRangeToKV halfRange

  toJSON (RangeFilter (FieldName fieldName) (Right range) rangeExecution cache) =
    object ["range" .=
            object [fieldName .=
                    object [lessKey .= lessVal
                           , greaterKey .= greaterVal]
                   , "execution" .= toJSON rangeExecution
                   , "_cache" .= cache]]
    where
      (lessKey, lessVal, greaterKey, greaterVal) = rangeToKV range

  toJSON (RegexpFilter (FieldName fieldName)
          (Regexp regexText) flags (CacheName cacheName) cache (CacheKey cacheKey)) =
    object ["regexp" .=
            object [fieldName .=
                    object ["value"  .= regexText
                           , "flags" .= toJSON flags]
                   , "_name"      .= cacheName
                   , "_cache"     .= cache
                   , "_cache_key" .= cacheKey]]

instance ToJSON GeoPoint where
  toJSON (GeoPoint (FieldName geoPointField) geoPointLatLon) =
    object [ geoPointField  .= toJSON geoPointLatLon ]


instance ToJSON Query where
  toJSON (TermQuery (Term termQueryField termQueryValue) boost) =
    object [ "term" .=
             object [termQueryField .= object merged]]
    where
      base = [ "value" .= termQueryValue ]
      boosted = maybe [] (return . ("boost" .=)) boost
      merged = mappend base boosted

  toJSON (TermsQuery terms termsQueryMinimumMatch) =
    object [ "terms" .= object conjoined ]
    where conjoined = [ "tags" .= fmap toJSON terms
                      , "minimum_should_match" .= toJSON termsQueryMinimumMatch ]

  toJSON (QueryMatchQuery matchQuery) =
    object [ "match" .= toJSON matchQuery ]

  toJSON (QueryMultiMatchQuery multiMatchQuery) =
    object [ "multi_match" .= toJSON multiMatchQuery ]

  toJSON (QueryBoolQuery boolQuery) =
    object [ "bool" .= toJSON boolQuery ]

  toJSON (QueryBoostingQuery boostingQuery) =
    object [ "boosting" .= toJSON boostingQuery ]

  toJSON (QueryCommonTermsQuery commonTermsQuery) =
    object [ "common" .= toJSON commonTermsQuery ]

  toJSON (ConstantScoreFilter filter boost) =
    object [ "constant_score" .= toJSON filter
           , "boost" .= toJSON boost]

  toJSON (ConstantScoreQuery query boost) =
    object [ "constant_score" .= toJSON query
           , "boost"          .= toJSON boost]

  toJSON (QueryDisMaxQuery disMaxQuery) =
    object [ "dis_max" .= toJSON disMaxQuery ]

  toJSON (QueryFilteredQuery qFilteredQuery) =
    object [ "filtered" .= toJSON qFilteredQuery ]

  toJSON (QueryFuzzyLikeThisQuery fuzzyQuery) =
    object [ "fuzzy_like_this" .= toJSON fuzzyQuery ]

  toJSON (QueryFuzzyLikeFieldQuery fuzzyFieldQuery) =
    object [ "fuzzy_like_this_field" .= toJSON fuzzyFieldQuery ]

  toJSON (QueryFuzzyQuery fuzzyQuery) =
    object [ "fuzzy" .= toJSON fuzzyQuery ]

  toJSON (QueryHasChildQuery childQuery) =
    object [ "has_child" .= toJSON childQuery ]

  toJSON (QueryHasParentQuery parentQuery) =
    object [ "has_parent" .= toJSON parentQuery ]

  toJSON (QueryIndicesQuery qIndicesQuery) =
    object [ "indices" .= toJSON qIndicesQuery ]

  toJSON (MatchAllQuery boost) =
    object [ "match_all" .= object maybeAdd ]
    where maybeAdd = catMaybes [ mField "boost" boost ]

  toJSON (QueryMoreLikeThisQuery query) =
    object [ "more_like_this" .= toJSON query ]

  toJSON (QueryMoreLikeThisFieldQuery query) =
    object [ "more_like_this_field" .= toJSON query ]

  toJSON (QueryNestedQuery query) =
    object [ "nested" .= toJSON query ]

  toJSON (QueryPrefixQuery query) =
    object [ "prefix" .= toJSON query ]

  toJSON (QueryRangeQuery query) =
    object [ "range" .= toJSON query ]

mField :: (ToJSON a, Functor f) => T.Text -> f a -> f (T.Text, Value)
mField field = fmap ((field .=) . toJSON)

instance ToJSON RangeQuery where
  toJSON (RangeQuery (FieldName fieldName) (Right range) boost) =
    object [ fieldName .= conjoined ]
    where conjoined = [ "boost" .= toJSON boost
                      , lessKey .= lessVal
                      , greaterKey .= greaterVal ]
          (lessKey, lessVal, greaterKey, greaterVal) = rangeToKV range

  toJSON (RangeQuery (FieldName fieldName) (Left halfRange) boost) =
    object [ fieldName .= conjoined ]
    where conjoined = [ "boost" .= toJSON boost
                      , key     .= val ]
          (key, val) = halfRangeToKV halfRange

instance ToJSON PrefixQuery where
  toJSON (PrefixQuery (FieldName fieldName) queryValue boost) =
    object [ fieldName .= object conjoined ]
    where base = [ "value" .= toJSON queryValue ]
          maybeAdd = catMaybes [ mField "boost" boost ]
          conjoined = base ++ maybeAdd


instance ToJSON NestedQuery where
  toJSON (NestedQuery path scoreType query) =
    object [ "path"       .= toJSON path
           , "score_mode" .= toJSON scoreType
           , "query"      .= toJSON query ]


instance ToJSON MoreLikeThisFieldQuery where
  toJSON (MoreLikeThisFieldQuery text (FieldName fieldName)
          percent mtf mqt stopwords mindf maxdf
          minwl maxwl boostTerms boost analyzer) =
    object [ fieldName .= object conjoined ]
    where base = [ "like_text" .= toJSON text ]
          maybeAdd = catMaybes [ mField "percent_terms_to_match" percent
                               , mField "min_term_freq" mtf
                               , mField "max_query_terms" mqt
                               , mField "stop_words" stopwords
                               , mField "min_doc_freq" mindf
                               , mField "max_doc_freq" maxdf
                               , mField "min_word_length" minwl
                               , mField "max_word_length" maxwl
                               , mField "boost_terms" boostTerms
                               , mField "boost" boost
                               , mField "analyzer" analyzer ]
          conjoined = base ++ maybeAdd


instance ToJSON MoreLikeThisQuery where
  toJSON (MoreLikeThisQuery text fields percent
          mtf mqt stopwords mindf maxdf
          minwl maxwl boostTerms boost analyzer) =
    object conjoined
    where base = [ "like_text" .= toJSON text ]
          maybeAdd = catMaybes [ mField "fields" fields
                               , mField "percent_terms_to_match" percent
                               , mField "min_term_freq" mtf
                               , mField "max_query_terms" mqt
                               , mField "stop_words" stopwords
                               , mField "min_doc_freq" mindf
                               , mField "max_doc_freq" maxdf
                               , mField "min_word_length" minwl
                               , mField "max_word_length" maxwl
                               , mField "boost_terms" boostTerms
                               , mField "boost" boost
                               , mField "analyzer" analyzer ]
          conjoined = base ++ maybeAdd


instance ToJSON IndicesQuery where
  toJSON (IndicesQuery indices query noMatch) =
    object $ [ "indices" .= toJSON indices
             , "query"   .= toJSON query ] ++ maybeAdd
    where maybeAdd = catMaybes [ mField "no_match_query" noMatch ]


instance ToJSON HasParentQuery where
  toJSON (HasParentQuery queryType query scoreType) =
    object $ [ "parent_type" .= toJSON queryType
             , "query" .= toJSON query ] ++ maybeAdd
    where maybeAdd = catMaybes [ mField "score_type" scoreType ]


instance ToJSON HasChildQuery where
  toJSON (HasChildQuery queryType query scoreType) =
    object $ [ "query" .= toJSON query
             , "type"  .= toJSON queryType ] ++ maybeAdd
    where maybeAdd = catMaybes [ mField "score_type" scoreType ]


instance ToJSON FuzzyQuery where
  toJSON (FuzzyQuery (FieldName fieldName) queryText
          prefixLength maxEx fuzziness boost) =
    object [ fieldName .= object conjoined ]
    where base = [ "value"          .= toJSON queryText
                 , "fuzziness"      .= toJSON fuzziness
                 , "prefix_length"  .= toJSON prefixLength
                 , "max_expansions" .= toJSON maxEx ]
          maybeAdd = catMaybes [ mField "boost" boost ]
          conjoined = base ++ maybeAdd


instance ToJSON FuzzyLikeFieldQuery where
  toJSON (FuzzyLikeFieldQuery (FieldName fieldName)
          fieldText maxTerms ignoreFreq fuzziness prefixLength
          boost analyzer) =
    object $ [ fieldName .=
               object [ "like_text"       .= toJSON fieldText
                      , "max_query_terms" .= toJSON maxTerms
                      , "ignore_tf"       .= toJSON ignoreFreq
                      , "fuzziness"       .= toJSON fuzziness
                      , "prefix_length"   .= toJSON prefixLength
                      , "boost"           .= toJSON boost ]] ++ maybeAdd
    where maybeAdd = catMaybes [ mField "analyzer" analyzer ]


instance ToJSON FuzzyLikeThisQuery where
  toJSON (FuzzyLikeThisQuery fields text maxTerms
          ignoreFreq fuzziness prefixLength boost analyzer) =
    object conjoined
    where base = [ "fields"          .= toJSON fields
                 , "like_text"       .= toJSON text
                 , "max_query_terms" .= toJSON maxTerms
                 , "ignore_tf"       .= toJSON ignoreFreq
                 , "fuzziness"       .= toJSON fuzziness
                 , "prefix_length"   .= toJSON prefixLength
                 , "boost"           .= toJSON boost ]
          maybeAdd = catMaybes [ mField "analyzer" analyzer ]
          conjoined = base ++ maybeAdd


instance ToJSON FilteredQuery where
  toJSON (FilteredQuery query filter) =
    object [ "query"  .= toJSON query
           , "filter" .= toJSON filter ]


instance ToJSON DisMaxQuery where
  toJSON (DisMaxQuery queries tiebreaker boost) =
    object conjoined
    where maybeAdd = catMaybes [mField "boost" boost]
          base = [ "queries"     .= toJSON queries
                 , "tie_breaker" .= toJSON tiebreaker ]
          conjoined = base ++ maybeAdd


instance ToJSON CommonTermsQuery where
  toJSON (CommonTermsQuery (FieldName fieldName)
          (QueryString query) cf lfo hfo msm
          boost analyzer disableCoord) =
    object [fieldName .= object conjoined]
    where base = [ "query"              .= query
                 , "cutoff_frequency"   .= toJSON cf
                 , "low_freq_operator"  .= toJSON lfo
                 , "high_freq_operator" .= toJSON hfo ]
          extension = catMaybes
                      [ mField "minimum_should_match" msm
                      , mField "boost" boost
                      , mField "analyzer" analyzer
                      , mField "disable_coord" disableCoord ]
          conjoined = base ++ extension


instance ToJSON CommonMinimumMatch where
  toJSON (CommonMinimumMatch mm) = toJSON mm
  toJSON (CommonMinimumMatchHighLow (MinimumMatchHighLow lowF highF)) =
    object [ "low_freq"  .= toJSON lowF
           , "high_freq" .= toJSON highF ]

instance ToJSON BoostingQuery where
  toJSON (BoostingQuery bqPositiveQuery bqNegativeQuery bqNegativeBoost) =
    object [ "positive"       .= toJSON bqPositiveQuery
           , "negative"       .= toJSON bqNegativeQuery
           , "negative_boost" .= toJSON bqNegativeBoost ]


instance ToJSON BoolQuery where
  toJSON (BoolQuery mustM notM shouldM bqMin boost disableCoord) =
    object filtered
    where filtered = catMaybes
                      [ mField "must" mustM
                      , mField "must_not" notM
                      , mField "should" shouldM
                      , mField "minimum_should_match" bqMin
                      , mField "boost" boost
                      , mField "disable_coord" disableCoord ]


instance ToJSON MatchQuery where
  toJSON (MatchQuery (FieldName fieldName)
          (QueryString queryString) booleanOperator
          zeroTermsQuery cutoffFrequency matchQueryType
          analyzer maxExpansions lenient) =
    object [ fieldName .= object conjoined ]
    where conjoined = [ "query" .= queryString
                      , "operator" .= toJSON booleanOperator
                      , "zero_terms_query" .= toJSON zeroTermsQuery]
                      ++ maybeAdd
          maybeAdd   = catMaybes [ mField "cutoff_frequency" cutoffFrequency
                                 , mField "type" matchQueryType
                                 , mField "analyzer" analyzer
                                 , mField "max_expansions" maxExpansions
                                 , mField "lenient" lenient ]


instance ToJSON MultiMatchQuery where
  toJSON (MultiMatchQuery fields (QueryString query) boolOp
          ztQ tb mmqt cf analyzer maxEx lenient) =
    object ["multi_match" .= object conjoined]
    where baseQuery = [ "fields" .= fmap toJSON fields
                      , "query" .= query
                      , "operator" .= toJSON boolOp
                      , "zero_terms_query" .= toJSON ztQ ]
          maybeAdd = catMaybes [ mField "tiebreaker" tb
                               , mField "type" mmqt
                               , mField "cutoff_frequency" cf
                               , mField "analyzer" analyzer
                               , mField "max_expansions" maxEx
                               , mField "lenient" lenient ]
          conjoined = baseQuery ++ maybeAdd


instance ToJSON MultiMatchQueryType where
  toJSON MultiMatchBestFields = "best_fields"
  toJSON MultiMatchMostFields = "most_fields"
  toJSON MultiMatchCrossFields = "cross_fields"
  toJSON MultiMatchPhrase = "phrase"
  toJSON MultiMatchPhrasePrefix = "phrase_prefix"

instance ToJSON BooleanOperator where
  toJSON And = String "and"
  toJSON Or = String "or"

instance ToJSON ZeroTermsQuery where
  toJSON ZeroTermsNone = String "none"
  toJSON ZeroTermsAll  = String "all"

instance ToJSON MatchQueryType where
  toJSON MatchPhrase = "phrase"
  toJSON MatchPhrasePrefix = "phrase_prefix"

instance ToJSON FieldName where
  toJSON (FieldName fieldName) = String fieldName

instance ToJSON ReplicaCount
instance ToJSON ShardCount
instance ToJSON CutoffFrequency
instance ToJSON Analyzer
instance ToJSON MaxExpansions
instance ToJSON Lenient
instance ToJSON Boost
instance ToJSON Version
instance ToJSON Tiebreaker
instance ToJSON MinimumMatch
instance ToJSON DisableCoord
instance ToJSON PrefixLength
instance ToJSON Fuzziness
instance ToJSON IgnoreTermFrequency
instance ToJSON MaxQueryTerms
instance ToJSON TypeName
instance ToJSON IndexName
instance ToJSON BoostTerms
instance ToJSON MaxWordLength
instance ToJSON MinWordLength
instance ToJSON MaxDocFrequency
instance ToJSON MinDocFrequency
instance ToJSON PhraseSlop
instance ToJSON StopWord
instance ToJSON QueryPath
instance ToJSON MinimumTermFrequency
instance ToJSON PercentMatch
instance FromJSON Version
instance FromJSON IndexName
instance FromJSON MappingName
instance FromJSON DocId


instance FromJSON Status where
  parseJSON (Object v) = Status <$>
                         v .: "ok" <*>
                         v .: "status" <*>
                         v .: "name" <*>
                         v .: "version" <*>
                         v .: "tagline"
  parseJSON _          = empty


instance ToJSON IndexSettings where
  toJSON (IndexSettings s r) = object ["settings" .= object ["shards" .= s, "replicas" .= r]]


instance (FromJSON a) => FromJSON (EsResult a) where
  parseJSON (Object v) = EsResult <$>
                         v .:  "_index"   <*>
                         v .:  "_type"    <*>
                         v .:  "_id"      <*>
                         v .:  "_version" <*>
                         v .:? "found"    <*>
                         v .:  "_source"
  parseJSON _          = empty


instance ToJSON Search where
  toJSON (Search query filter sort sTrackSortScores sFrom sSize) =
    object merged where
      lQuery  = maybeJson  "query" query
      lFilter = maybeJson  "filter" filter
      lSort   = maybeJsonF "sort" sort
      merged  = mconcat [[ "from" .= sFrom
                         , "size" .= sSize
                         , "track_scores" .= sTrackSortScores]
                        , lQuery
                        , lFilter
                        , lSort]


instance ToJSON SortSpec where
  toJSON (DefaultSortSpec
          (DefaultSort (FieldName dsSortFieldName) dsSortOrder dsIgnoreUnmapped
           dsSortMode dsMissingSort dsNestedFilter)) =
    object [dsSortFieldName .= object merged] where
      base = ["order" .= toJSON dsSortOrder
             , "ignore_unmapped" .= dsIgnoreUnmapped]
      lSortMode = maybeJson "mode" dsSortMode
      lMissingSort = maybeJson "missing" dsMissingSort
      lNestedFilter = maybeJson "nested_filter" dsNestedFilter
      merged = mconcat [base, lSortMode, lMissingSort, lNestedFilter]

  toJSON (GeoDistanceSortSpec gdsSortOrder (GeoPoint (FieldName field) gdsLatLon) units) =
    object [ "unit" .= toJSON units
           , field .= toJSON gdsLatLon
           , "order" .= toJSON gdsSortOrder ]


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


instance ToJSON Distance where
  toJSON (Distance dCoefficient dUnit) =
    String boltedTogether where
      coefText = showText dCoefficient
      (String unitText) = toJSON dUnit
      boltedTogether = mappend coefText unitText


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


instance ToJSON DistanceType where
  toJSON Arc       = String "arc"
  toJSON SloppyArc = String "sloppy_arc"
  toJSON Plane     = String "plane"


instance ToJSON OptimizeBbox where
  toJSON NoOptimizeBbox = String "none"
  toJSON (OptimizeGeoFilterType gft) = toJSON gft


instance ToJSON GeoBoundingBoxConstraint where
  toJSON (GeoBoundingBoxConstraint
          (FieldName gbbcGeoBBField) gbbcConstraintBox cache) =
    object [gbbcGeoBBField .= toJSON gbbcConstraintBox
           , "_cache"  .= cache]


instance ToJSON GeoFilterType where
  toJSON GeoFilterMemory  = String "memory"
  toJSON GeoFilterIndexed = String "indexed"


instance ToJSON GeoBoundingBox where
  toJSON (GeoBoundingBox gbbTopLeft gbbBottomRight) =
    object ["top_left"      .= toJSON gbbTopLeft
           , "bottom_right" .= toJSON gbbBottomRight]


instance ToJSON LatLon where
  toJSON (LatLon lLat lLon) =
    object ["lat"  .= lLat
           , "lon" .= lLon]


-- index for smaller ranges, fielddata for longer ranges
instance ToJSON RangeExecution where
  toJSON RangeExecutionIndex     = "index"
  toJSON RangeExecutionFielddata = "fielddata"


instance ToJSON RegexpFlags where
  toJSON (RegexpFlags txt) = String txt


instance ToJSON Term where
  toJSON (Term field value) = object ["term" .= object
                                      [field .= value]]


instance ToJSON BoolMatch where
  toJSON (MustMatch    term  cache) = object ["must"     .= toJSON term,
                                              "_cache" .= cache]
  toJSON (MustNotMatch term  cache) = object ["must_not" .= toJSON term,
                                              "_cache" .= cache]
  toJSON (ShouldMatch  terms cache) = object ["should"   .= fmap toJSON terms,
                                              "_cache" .= cache]


instance (FromJSON a) => FromJSON (SearchResult a) where
  parseJSON (Object v) = SearchResult <$>
                         v .: "took"      <*>
                         v .: "timed_out" <*>
                         v .: "_shards"   <*>
                         v .: "hits"
  parseJSON _          = empty

instance (FromJSON a) => FromJSON (SearchHits a) where
  parseJSON (Object v) = SearchHits <$>
                         v .: "total"     <*>
                         v .: "max_score" <*>
                         v .: "hits"
  parseJSON _          = empty

instance (FromJSON a) => FromJSON (Hit a) where
  parseJSON (Object v) = Hit <$>
                         v .: "_index" <*>
                         v .: "_type"  <*>
                         v .: "_id"    <*>
                         v .: "_score" <*>
                         v .: "_source"
  parseJSON _          = empty

instance FromJSON ShardResult where
  parseJSON (Object v) = ShardResult <$>
                         v .: "total"      <*>
                         v .: "successful" <*>
                         v .: "failed"
  parseJSON _          = empty
