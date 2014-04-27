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
import Database.Bloodhound.Types.Class
import Data.Scientific

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

  toJSON (GeoDistanceFilter (GeoPoint (FieldName geoField) latLon)
          distance distanceType optimizeBbox cache) =
    object ["geo_distance" .=
            object ["distance" .= toJSON distance
                   , "distance_type" .= toJSON distanceType
                   , "optimize_bbox" .= optimizeBbox
                   , geoField .= toJSON latLon
                   , "_cache" .= cache]]                   

  toJSON (GeoDistanceRangeFilter (GeoPoint (FieldName geoField) latLon)
          (DistanceRange distanceFrom distanceTo)) =
    object ["geo_distance_range" .=
            object ["from" .= toJSON distanceFrom
                   , "to"  .= toJSON distanceTo
                   , geoField .= toJSON latLon]]

  toJSON (GeoPolygonFilter (FieldName geoField) latLons) =
    object ["geo_polygon" .=
            object [geoField .=
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
  toJSON (GeoPoint (FieldName geoField) latLon) =
    object [ geoField  .= toJSON latLon ]


instance ToJSON Query where
  toJSON (TermQuery (Term termField termValue) boost) =
    object [ "term" .=
             object [termField .= object merged]]
    where
      base = [ "value" .= termValue ]
      boosted = maybe [] (return . ("boost" .=)) boost
      merged = mappend base boosted

  toJSON (QueryMatchQuery matchQuery) =
    object [ "match" .= toJSON matchQuery ]

  toJSON (QueryMultiMatchQuery multiMatchQuery) =
    object [ "multi_match" .= toJSON multiMatchQuery ]

  toJSON (QueryBoolQuery boolQuery) =
    object [ "bool" .= toJSON boolQuery ]

  toJSON (QueryBoostingQuery boostingQuery) =
    object ["boosting" .= toJSON boostingQuery]

  toJSON (QueryCommonTermsQuery commonTermsQuery) =
    object ["common" .= toJSON commonTermsQuery]

mField :: (ToJSON a, Functor f) => T.Text -> f a -> f (T.Text, Value)
mField field = fmap ((field .=) . toJSON)


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
  toJSON (BoostingQuery positiveQuery negativeQuery negativeBoost) =
    object [ "positive"       .= toJSON positiveQuery
           , "negative"       .= toJSON negativeQuery
           , "negative_boost" .= toJSON negativeBoost ]


instance ToJSON BoolQuery where
  toJSON (BoolQuery mustM notM shouldM min boost disableCoord) =
    object filtered
    where filtered = catMaybes
                      [ mField "must" mustM
                      , mField "must_not" notM
                      , mField "should" shouldM
                      , mField "minimum_should_match" min
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
instance FromJSON Version
instance FromJSON IndexName
instance FromJSON MappingName
instance FromJSON DocId


instance (FromJSON a) => FromJSON (Status a) where
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
  toJSON (Search query filter sort trackSortScores from size) =
    object merged where
      lQuery  = maybeJson  "query" query
      lFilter = maybeJson  "filter" filter
      lSort   = maybeJsonF "sort" sort
      merged  = mconcat [[ "from" .= from
                         , "size" .= size
                         , "track_scores" .= trackSortScores]
                        , lQuery
                        , lFilter
                        , lSort]


instance ToJSON SortSpec where
  toJSON (DefaultSortSpec
          (DefaultSort (FieldName sortFieldName) sortOrder ignoreUnmapped
           sortMode missingSort nestedFilter)) =
    object [sortFieldName .= object merged] where
      base = ["order" .= toJSON sortOrder
             , "ignore_unmapped" .= ignoreUnmapped]
      lSortMode = maybeJson "mode" sortMode
      lMissingSort = maybeJson "missing" missingSort
      lNestedFilter = maybeJson "nested_filter" nestedFilter
      merged = mconcat [base, lSortMode, lMissingSort, lNestedFilter]

  toJSON (GeoDistanceSortSpec sortOrder (GeoPoint (FieldName field) latLon) units) =
    object [ "unit" .= toJSON units
           , field .= toJSON latLon
           , "order" .= toJSON sortOrder ]


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


instance ToJSON Distance where
  toJSON (Distance coefficient unit) =
    String boltedTogether where
      coefText = showText coefficient
      (String unitText) = (toJSON unit)
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
  toJSON (GeoBoundingBoxConstraint (FieldName geoBBField) constraintBox cache) =
    object [geoBBField .= toJSON constraintBox
           , "_cache"  .= cache]


instance ToJSON GeoFilterType where
  toJSON GeoFilterMemory  = String "memory"
  toJSON GeoFilterIndexed = String "indexed"


instance ToJSON GeoBoundingBox where
  toJSON (GeoBoundingBox topLeft bottomRight) =
    object ["top_left"      .= toJSON topLeft
           , "bottom_right" .= toJSON bottomRight]


instance ToJSON LatLon where
  toJSON (LatLon lat lon) =
    object ["lat"  .= lat
           , "lon" .= lon]


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
