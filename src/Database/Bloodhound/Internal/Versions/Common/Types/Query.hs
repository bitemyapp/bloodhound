{-# LANGUAGE OverloadedStrings #-}

module Database.Bloodhound.Internal.Versions.Common.Types.Query
  ( module X,
    BoolMatch (..),
    BoolQuery (..),
    BoostingQuery (..),
    Cache,
    ComponentFunctionScoreFunction (..),
    DisMaxQuery (..),
    Distance (..),
    DistanceRange (..),
    DistanceType (..),
    DistanceUnit (..),
    Filter (..),
    FunctionScoreFunctions (..),
    FunctionScoreQuery (..),
    GeoBoundingBox (..),
    GeoBoundingBoxConstraint (..),
    GeoFilterType (..),
    GeoPoint (..),
    HasChildQuery (..),
    HasParentQuery (..),
    IndicesQuery (..),
    InnerHits (..),
    LatLon (..),
    NestedQuery (..),
    OptimizeBbox (..),
    Query (..),
    RangeExecution (..),
    ScoreType (..),
    TemplateQueryKeyValuePairs (..),
    TemplateQueryValue,
    Term (..),
    defaultCache,
    functionScoreFunctionsPair,
    mkBoolQuery,
    showDistanceUnit,
  )
where

import qualified Data.Aeson.KeyMap as X
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Database.Bloodhound.Internal.Utils.Imports
import Database.Bloodhound.Internal.Versions.Common.Types.Newtypes
import Database.Bloodhound.Internal.Versions.Common.Types.Query.CommonTerms as X
import Database.Bloodhound.Internal.Versions.Common.Types.Query.Commons as X
import Database.Bloodhound.Internal.Versions.Common.Types.Query.Fuzzy as X
import Database.Bloodhound.Internal.Versions.Common.Types.Query.Match as X
import Database.Bloodhound.Internal.Versions.Common.Types.Query.MoreLikeThis as X
import Database.Bloodhound.Internal.Versions.Common.Types.Query.MoreLikeThisField as X
import Database.Bloodhound.Internal.Versions.Common.Types.Query.Prefix as X
import Database.Bloodhound.Internal.Versions.Common.Types.Query.QueryString as X
import Database.Bloodhound.Internal.Versions.Common.Types.Query.Range as X
import Database.Bloodhound.Internal.Versions.Common.Types.Query.Regexp as X
import Database.Bloodhound.Internal.Versions.Common.Types.Query.SimpleQueryString as X
import Database.Bloodhound.Internal.Versions.Common.Types.Query.Wildcard as X
import Database.Bloodhound.Internal.Versions.Common.Types.Script as X
import GHC.Generics

data Query
  = TermQuery Term (Maybe Boost)
  | TermsQuery Key (NonEmpty Text)
  | QueryMatchQuery MatchQuery
  | QueryMultiMatchQuery MultiMatchQuery
  | QueryBoolQuery BoolQuery
  | QueryBoostingQuery BoostingQuery
  | QueryCommonTermsQuery CommonTermsQuery
  | ConstantScoreQuery Query Boost
  | QueryFunctionScoreQuery FunctionScoreQuery
  | QueryDisMaxQuery DisMaxQuery
  | QueryFuzzyLikeThisQuery FuzzyLikeThisQuery
  | QueryFuzzyLikeFieldQuery FuzzyLikeFieldQuery
  | QueryFuzzyQuery FuzzyQuery
  | QueryHasChildQuery HasChildQuery
  | QueryHasParentQuery HasParentQuery
  | IdsQuery [DocId]
  | QueryIndicesQuery IndicesQuery
  | MatchAllQuery (Maybe Boost)
  | QueryMoreLikeThisQuery MoreLikeThisQuery
  | QueryMoreLikeThisFieldQuery MoreLikeThisFieldQuery
  | QueryNestedQuery NestedQuery
  | QueryPrefixQuery PrefixQuery
  | QueryQueryStringQuery QueryStringQuery
  | QuerySimpleQueryStringQuery SimpleQueryStringQuery
  | QueryRangeQuery RangeQuery
  | QueryRegexpQuery RegexpQuery
  | QueryExistsQuery FieldName
  | QueryMatchNoneQuery
  | QueryWildcardQuery WildcardQuery
  deriving stock (Eq, Show, Generic)

instance ToJSON Query where
  toJSON (TermQuery (Term termQueryField termQueryValue) boost) =
    object
      [ "term"
          .= object [termQueryField .= object merged]
      ]
    where
      base = ["value" .= termQueryValue]
      boosted = maybe [] (return . ("boost" .=)) boost
      merged = mappend base boosted
  toJSON (TermsQuery fieldName terms) =
    object ["terms" .= object conjoined]
    where
      conjoined = [fieldName .= terms]
  toJSON (IdsQuery docIds) =
    object ["ids" .= object conjoined]
    where
      conjoined = ["values" .= fmap toJSON docIds]
  toJSON (QueryQueryStringQuery qQueryStringQuery) =
    object ["query_string" .= qQueryStringQuery]
  toJSON (QueryMatchQuery matchQuery) =
    object ["match" .= matchQuery]
  toJSON (QueryMultiMatchQuery multiMatchQuery) =
    toJSON multiMatchQuery
  toJSON (QueryBoolQuery boolQuery) =
    object ["bool" .= boolQuery]
  toJSON (QueryBoostingQuery boostingQuery) =
    object ["boosting" .= boostingQuery]
  toJSON (QueryCommonTermsQuery commonTermsQuery) =
    object ["common" .= commonTermsQuery]
  toJSON (ConstantScoreQuery query boost) =
    object
      [ "constant_score"
          .= object
            [ "filter" .= query,
              "boost" .= boost
            ]
      ]
  toJSON (QueryFunctionScoreQuery functionScoreQuery') =
    object ["function_score" .= functionScoreQuery']
  toJSON (QueryDisMaxQuery disMaxQuery) =
    object ["dis_max" .= disMaxQuery]
  toJSON (QueryFuzzyLikeThisQuery fuzzyQuery) =
    object ["fuzzy_like_this" .= fuzzyQuery]
  toJSON (QueryFuzzyLikeFieldQuery fuzzyFieldQuery) =
    object ["fuzzy_like_this_field" .= fuzzyFieldQuery]
  toJSON (QueryFuzzyQuery fuzzyQuery) =
    object ["fuzzy" .= fuzzyQuery]
  toJSON (QueryHasChildQuery childQuery) =
    object ["has_child" .= childQuery]
  toJSON (QueryHasParentQuery parentQuery) =
    object ["has_parent" .= parentQuery]
  toJSON (QueryIndicesQuery qIndicesQuery) =
    object ["indices" .= qIndicesQuery]
  toJSON (MatchAllQuery boost) =
    object ["match_all" .= omitNulls ["boost" .= boost]]
  toJSON (QueryMoreLikeThisQuery query) =
    object ["more_like_this" .= query]
  toJSON (QueryMoreLikeThisFieldQuery query) =
    object ["more_like_this_field" .= query]
  toJSON (QueryNestedQuery query) =
    object ["nested" .= query]
  toJSON (QueryPrefixQuery query) =
    object ["prefix" .= query]
  toJSON (QueryRangeQuery query) =
    object ["range" .= query]
  toJSON (QueryRegexpQuery query) =
    object ["regexp" .= query]
  toJSON (QuerySimpleQueryStringQuery query) =
    object ["simple_query_string" .= query]
  toJSON (QueryExistsQuery (FieldName fieldName)) =
    object
      [ "exists"
          .= object
            ["field" .= fieldName]
      ]
  toJSON QueryMatchNoneQuery =
    object ["match_none" .= object []]
  toJSON (QueryWildcardQuery query) =
    object ["wildcard" .= query]

instance FromJSON Query where
  parseJSON v = withObject "Query" parse v
    where
      parse o =
        termQuery
          `taggedWith` "term"
          <|> termsQuery
            `taggedWith` "terms"
          <|> idsQuery
            `taggedWith` "ids"
          <|> queryQueryStringQuery
            `taggedWith` "query_string"
          <|> queryMatchQuery
            `taggedWith` "match"
          <|> queryMultiMatchQuery
          <|> queryBoolQuery
            `taggedWith` "bool"
          <|> queryBoostingQuery
            `taggedWith` "boosting"
          <|> queryCommonTermsQuery
            `taggedWith` "common"
          <|> constantScoreQuery
            `taggedWith` "constant_score"
          <|> queryFunctionScoreQuery
            `taggedWith` "function_score"
          <|> queryDisMaxQuery
            `taggedWith` "dis_max"
          <|> queryFuzzyLikeThisQuery
            `taggedWith` "fuzzy_like_this"
          <|> queryFuzzyLikeFieldQuery
            `taggedWith` "fuzzy_like_this_field"
          <|> queryFuzzyQuery
            `taggedWith` "fuzzy"
          <|> queryHasChildQuery
            `taggedWith` "has_child"
          <|> queryHasParentQuery
            `taggedWith` "has_parent"
          <|> queryIndicesQuery
            `taggedWith` "indices"
          <|> matchAllQuery
            `taggedWith` "match_all"
          <|> queryMoreLikeThisQuery
            `taggedWith` "more_like_this"
          <|> queryMoreLikeThisFieldQuery
            `taggedWith` "more_like_this_field"
          <|> queryNestedQuery
            `taggedWith` "nested"
          <|> queryPrefixQuery
            `taggedWith` "prefix"
          <|> queryRangeQuery
            `taggedWith` "range"
          <|> queryRegexpQuery
            `taggedWith` "regexp"
          <|> querySimpleQueryStringQuery
            `taggedWith` "simple_query_string"
          <|> queryWildcardQuery
            `taggedWith` "wildcard"
        where
          taggedWith parser k = parser =<< o .: k
      termQuery = fieldTagged $ \(FieldName fn) o ->
        TermQuery <$> (Term (fromText fn) <$> o .: "value") <*> o .:? "boost"
      termsQuery o = case HM.toList o of
        [(fn, vs)] -> do
          vals <- parseJSON vs
          case vals of
            x : xs -> return (TermsQuery fn (x :| xs))
            _ -> fail "Expected non empty list of values"
        _ -> fail "Expected object with 1 field-named key"
      idsQuery o = IdsQuery <$> o .: "values"
      queryQueryStringQuery = pure . QueryQueryStringQuery
      queryMatchQuery = pure . QueryMatchQuery
      queryMultiMatchQuery = QueryMultiMatchQuery <$> parseJSON v
      queryBoolQuery = pure . QueryBoolQuery
      queryBoostingQuery = pure . QueryBoostingQuery
      queryCommonTermsQuery = pure . QueryCommonTermsQuery
      constantScoreQuery o = case X.lookup "filter" o of
        Just x ->
          ConstantScoreQuery
            <$> parseJSON x
            <*> o .: "boost"
        _ -> fail "Does not appear to be a ConstantScoreQuery"
      queryFunctionScoreQuery = pure . QueryFunctionScoreQuery
      queryDisMaxQuery = pure . QueryDisMaxQuery
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
      -- queryExistsQuery o = QueryExistsQuery <$> o .: "field"
      queryWildcardQuery = pure . QueryWildcardQuery

-- | As of Elastic 2.0, 'Filters' are just 'Queries' housed in a
--  Bool Query, and flagged in a different context.
newtype Filter = Filter {unFilter :: Query}
  deriving stock (Eq, Show)

instance ToJSON Filter where
  toJSON = toJSON . unFilter

instance FromJSON Filter where
  parseJSON v = Filter <$> parseJSON v

data NestedQuery = NestedQuery
  { nestedQueryPath :: QueryPath,
    nestedQueryScoreType :: ScoreType,
    nestedQuery :: Query,
    nestedQueryInnerHits :: Maybe InnerHits
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON NestedQuery where
  toJSON (NestedQuery nqPath nqScoreType nqQuery nqInnerHits) =
    omitNulls
      [ "path" .= nqPath,
        "score_mode" .= nqScoreType,
        "query" .= nqQuery,
        "inner_hits" .= nqInnerHits
      ]

instance FromJSON NestedQuery where
  parseJSON = withObject "NestedQuery" parse
    where
      parse o =
        NestedQuery
          <$> o .: "path"
          <*> o .: "score_mode"
          <*> o .: "query"
          <*> o .:? "inner_hits"

data IndicesQuery = IndicesQuery
  { indicesQueryIndices :: [IndexName],
    indicesQuery :: Query,
    -- default "all"
    indicesQueryNoMatch :: Maybe Query
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON IndicesQuery where
  toJSON (IndicesQuery indices query noMatch) =
    omitNulls
      [ "indices" .= indices,
        "no_match_query" .= noMatch,
        "query" .= query
      ]

instance FromJSON IndicesQuery where
  parseJSON = withObject "IndicesQuery" parse
    where
      parse o =
        IndicesQuery
          <$> o .:? "indices" .!= []
          <*> o .: "query"
          <*> o .:? "no_match_query"

data HasParentQuery = HasParentQuery
  { hasParentQueryType :: RelationName,
    hasParentQuery :: Query,
    hasParentQueryScore :: Maybe AggregateParentScore,
    hasParentIgnoreUnmapped :: Maybe IgnoreUnmapped
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON HasParentQuery where
  toJSON (HasParentQuery queryType query scoreType ignoreUnmapped) =
    omitNulls
      [ "parent_type" .= queryType,
        "score" .= scoreType,
        "query" .= query,
        "ignore_unmapped" .= ignoreUnmapped
      ]

instance FromJSON HasParentQuery where
  parseJSON = withObject "HasParentQuery" parse
    where
      parse o =
        HasParentQuery
          <$> o .: "parent_type"
          <*> o .: "query"
          <*> o .:? "score"
          <*> o .:? "ignore_unmapped"

data HasChildQuery = HasChildQuery
  { hasChildQueryType :: RelationName,
    hasChildQuery :: Query,
    hasChildQueryScoreType :: Maybe ScoreType,
    hasChildIgnoreUnmappped :: Maybe IgnoreUnmapped,
    hasChildMinChildren :: Maybe MinChildren,
    hasChildMaxChildren :: Maybe MaxChildren
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON HasChildQuery where
  toJSON (HasChildQuery queryType query scoreType ignoreUnmapped minChildren maxChildren) =
    omitNulls
      [ "query" .= query,
        "score_mode" .= scoreType,
        "type" .= queryType,
        "min_children" .= minChildren,
        "max_children" .= maxChildren,
        "ignore_unmapped" .= ignoreUnmapped
      ]

instance FromJSON HasChildQuery where
  parseJSON = withObject "HasChildQuery" parse
    where
      parse o =
        HasChildQuery
          <$> o .: "type"
          <*> o .: "query"
          <*> o .:? "score_mode"
          <*> o .:? "ignore_unmapped"
          <*> o .:? "min_children"
          <*> o .:? "max_children"

data ScoreType
  = ScoreTypeMax
  | ScoreTypeSum
  | ScoreTypeAvg
  | ScoreTypeNone
  deriving stock (Eq, Show, Generic)

instance ToJSON ScoreType where
  toJSON ScoreTypeMax = "max"
  toJSON ScoreTypeAvg = "avg"
  toJSON ScoreTypeSum = "sum"
  toJSON ScoreTypeNone = "none"

instance FromJSON ScoreType where
  parseJSON = withText "ScoreType" parse
    where
      parse "max" = pure ScoreTypeMax
      parse "avg" = pure ScoreTypeAvg
      parse "sum" = pure ScoreTypeSum
      parse "none" = pure ScoreTypeNone
      parse t = fail ("Unexpected ScoreType: " <> show t)

data DisMaxQuery = DisMaxQuery
  { disMaxQueries :: [Query],
    -- default 0.0
    disMaxTiebreaker :: Tiebreaker,
    disMaxBoost :: Maybe Boost
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON DisMaxQuery where
  toJSON (DisMaxQuery queries tiebreaker boost) =
    omitNulls base
    where
      base =
        [ "queries" .= queries,
          "boost" .= boost,
          "tie_breaker" .= tiebreaker
        ]

instance FromJSON DisMaxQuery where
  parseJSON = withObject "DisMaxQuery" parse
    where
      parse o =
        DisMaxQuery
          <$> o .:? "queries" .!= []
          <*> o .: "tie_breaker"
          <*> o .:? "boost"

data BoolQuery = BoolQuery
  { boolQueryMustMatch :: [Query],
    boolQueryFilter :: [Filter],
    boolQueryMustNotMatch :: [Query],
    boolQueryShouldMatch :: [Query],
    boolQueryMinimumShouldMatch :: Maybe MinimumMatch,
    boolQueryBoost :: Maybe Boost,
    boolQueryDisableCoord :: Maybe DisableCoord
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON BoolQuery where
  toJSON (BoolQuery mustM filterM' notM shouldM bqMin boost disableCoord) =
    omitNulls base
    where
      base =
        [ "must" .= mustM,
          "filter" .= filterM',
          "must_not" .= notM,
          "should" .= shouldM,
          "minimum_should_match" .= bqMin,
          "boost" .= boost,
          "disable_coord" .= disableCoord
        ]

instance FromJSON BoolQuery where
  parseJSON = withObject "BoolQuery" parse
    where
      parse o =
        BoolQuery
          <$> o .:? "must" .!= []
          <*> o .:? "filter" .!= []
          <*> o .:? "must_not" .!= []
          <*> o .:? "should" .!= []
          <*> o .:? "minimum_should_match"
          <*> o .:? "boost"
          <*> o .:? "disable_coord"

mkBoolQuery :: [Query] -> [Filter] -> [Query] -> [Query] -> BoolQuery
mkBoolQuery must filt mustNot should =
  BoolQuery must filt mustNot should Nothing Nothing Nothing

data BoostingQuery = BoostingQuery
  { positiveQuery :: Query,
    negativeQuery :: Query,
    negativeBoost :: Boost
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON BoostingQuery where
  toJSON (BoostingQuery bqPositiveQuery bqNegativeQuery bqNegativeBoost) =
    object
      [ "positive" .= bqPositiveQuery,
        "negative" .= bqNegativeQuery,
        "negative_boost" .= bqNegativeBoost
      ]

instance FromJSON BoostingQuery where
  parseJSON = withObject "BoostingQuery" parse
    where
      parse o =
        BoostingQuery
          <$> o .: "positive"
          <*> o .: "negative"
          <*> o .: "negative_boost"

data RangeExecution
  = RangeExecutionIndex
  | RangeExecutionFielddata
  deriving stock (Eq, Show, Generic)

-- index for smaller ranges, fielddata for longer ranges
instance ToJSON RangeExecution where
  toJSON RangeExecutionIndex = "index"
  toJSON RangeExecutionFielddata = "fielddata"

instance FromJSON RangeExecution where
  parseJSON = withText "RangeExecution" parse
    where
      parse "index" = pure RangeExecutionIndex
      parse "fielddata" = pure RangeExecutionFielddata
      parse t = error ("Unrecognized RangeExecution " <> show t)

data Term = Term
  { termField :: Key,
    termValue :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Term where
  toJSON (Term field value) =
    object
      [ "term"
          .= object
            [field .= value]
      ]

instance FromJSON Term where
  parseJSON = withObject "Term" parse
    where
      parse o = do
        termObj <- o .: "term"
        case HM.toList termObj of
          [(fn, v)] -> Term fn <$> parseJSON v
          _ -> fail "Expected object with 1 field-named key"

data BoolMatch
  = MustMatch Term Cache
  | MustNotMatch Term Cache
  | ShouldMatch [Term] Cache
  deriving stock (Eq, Show, Generic)

instance ToJSON BoolMatch where
  toJSON (MustMatch term cache) =
    object
      [ "must" .= term,
        "_cache" .= cache
      ]
  toJSON (MustNotMatch term cache) =
    object
      [ "must_not" .= term,
        "_cache" .= cache
      ]
  toJSON (ShouldMatch terms cache) =
    object
      [ "should" .= fmap toJSON terms,
        "_cache" .= cache
      ]

instance FromJSON BoolMatch where
  parseJSON = withObject "BoolMatch" parse
    where
      parse o =
        mustMatch
          `taggedWith` "must"
          <|> mustNotMatch
            `taggedWith` "must_not"
          <|> shouldMatch
            `taggedWith` "should"
        where
          taggedWith parser k = parser =<< o .: k
          mustMatch t = MustMatch t <$> o .:? "_cache" .!= defaultCache
          mustNotMatch t = MustNotMatch t <$> o .:? "_cache" .!= defaultCache
          shouldMatch t = ShouldMatch t <$> o .:? "_cache" .!= defaultCache

-- "memory" or "indexed"
data GeoFilterType
  = GeoFilterMemory
  | GeoFilterIndexed
  deriving stock (Eq, Show, Generic)

instance ToJSON GeoFilterType where
  toJSON GeoFilterMemory = String "memory"
  toJSON GeoFilterIndexed = String "indexed"

instance FromJSON GeoFilterType where
  parseJSON = withText "GeoFilterType" parse
    where
      parse "memory" = pure GeoFilterMemory
      parse "indexed" = pure GeoFilterIndexed
      parse t = fail ("Unrecognized GeoFilterType: " <> show t)

data LatLon = LatLon
  { lat :: Double,
    lon :: Double
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON LatLon where
  toJSON (LatLon lLat lLon) =
    object
      [ "lat" .= lLat,
        "lon" .= lLon
      ]

instance FromJSON LatLon where
  parseJSON = withObject "LatLon" parse
    where
      parse o =
        LatLon
          <$> o .: "lat"
          <*> o .: "lon"

data GeoBoundingBox = GeoBoundingBox
  { topLeft :: LatLon,
    bottomRight :: LatLon
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON GeoBoundingBox where
  toJSON (GeoBoundingBox gbbTopLeft gbbBottomRight) =
    object
      [ "top_left" .= gbbTopLeft,
        "bottom_right" .= gbbBottomRight
      ]

instance FromJSON GeoBoundingBox where
  parseJSON = withObject "GeoBoundingBox" parse
    where
      parse o =
        GeoBoundingBox
          <$> o .: "top_left"
          <*> o .: "bottom_right"

data GeoBoundingBoxConstraint = GeoBoundingBoxConstraint
  { geoBBField :: FieldName,
    constraintBox :: GeoBoundingBox,
    bbConstraintcache :: Cache,
    geoType :: GeoFilterType
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON GeoBoundingBoxConstraint where
  toJSON
    ( GeoBoundingBoxConstraint
        (FieldName gbbcGeoBBField)
        gbbcConstraintBox
        cache
        type'
      ) =
      object
        [ fromText gbbcGeoBBField .= gbbcConstraintBox,
          "_cache" .= cache,
          "type" .= type'
        ]

instance FromJSON GeoBoundingBoxConstraint where
  parseJSON = withObject "GeoBoundingBoxConstraint" parse
    where
      parse o = case X.toList (deleteSeveral ["type", "_cache"] o) of
        [(fn, v)] ->
          GeoBoundingBoxConstraint (FieldName $ toText fn)
            <$> parseJSON v
            <*> o .:? "_cache" .!= defaultCache
            <*> o .: "type"
        _ -> fail "Could not find field name for GeoBoundingBoxConstraint"

data GeoPoint = GeoPoint
  { geoField :: FieldName,
    latLon :: LatLon
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON GeoPoint where
  toJSON (GeoPoint (FieldName geoPointField) geoPointLatLon) =
    object [fromText geoPointField .= geoPointLatLon]

data DistanceUnit
  = Miles
  | Yards
  | Feet
  | Inches
  | Kilometers
  | Meters
  | Centimeters
  | Millimeters
  | NauticalMiles
  deriving stock (Eq, Show, Generic)

showDistanceUnit :: DistanceUnit -> Text
showDistanceUnit x =
  case x of
    Miles -> "mi"
    Yards -> "yd"
    Feet -> "ft"
    Inches -> "in"
    Kilometers -> "km"
    Meters -> "m"
    Centimeters -> "cm"
    Millimeters -> "mm"
    NauticalMiles -> "nmi"

instance ToJSON DistanceUnit where
  toJSON = String . showDistanceUnit

instance FromJSON DistanceUnit where
  parseJSON = withText "DistanceUnit" parse
    where
      parse "mi" = pure Miles
      parse "yd" = pure Yards
      parse "ft" = pure Feet
      parse "in" = pure Inches
      parse "km" = pure Kilometers
      parse "m" = pure Meters
      parse "cm" = pure Centimeters
      parse "mm" = pure Millimeters
      parse "nmi" = pure NauticalMiles
      parse u = fail ("Unrecognized DistanceUnit: " <> show u)

data DistanceType
  = Arc
  | SloppyArc -- doesn't exist <1.0
  | Plane
  deriving stock (Eq, Show, Generic)

instance ToJSON DistanceType where
  toJSON Arc = String "arc"
  toJSON SloppyArc = String "sloppy_arc"
  toJSON Plane = String "plane"

instance FromJSON DistanceType where
  parseJSON = withText "DistanceType" parse
    where
      parse "arc" = pure Arc
      parse "sloppy_arc" = pure SloppyArc
      parse "plane" = pure Plane
      parse t = fail ("Unrecognized DistanceType: " <> show t)

data OptimizeBbox
  = OptimizeGeoFilterType GeoFilterType
  | NoOptimizeBbox
  deriving stock (Eq, Show, Generic)

instance ToJSON OptimizeBbox where
  toJSON NoOptimizeBbox = String "none"
  toJSON (OptimizeGeoFilterType gft) = toJSON gft

instance FromJSON OptimizeBbox where
  parseJSON v =
    withText "NoOptimizeBbox" parseNoOptimize v
      <|> parseOptimize v
    where
      parseNoOptimize "none" = pure NoOptimizeBbox
      parseNoOptimize _ = mzero
      parseOptimize = fmap OptimizeGeoFilterType . parseJSON

data Distance = Distance
  { coefficient :: Double,
    unit :: DistanceUnit
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Distance where
  toJSON (Distance dCoefficient dUnit) =
    String boltedTogether
    where
      coefText = showText dCoefficient
      boltedTogether = mappend coefText $ showDistanceUnit dUnit

instance FromJSON Distance where
  parseJSON = withText "Distance" parse
    where
      parse t =
        Distance
          <$> parseCoeff nT
          <*> parseJSON (String unitT)
        where
          (nT, unitT) = T.span validForNumber t
          -- may be a better way to do this
          validForNumber '-' = True
          validForNumber '.' = True
          validForNumber 'e' = True
          validForNumber c = isNumber c
          parseCoeff "" = fail "Empty string cannot be parsed as number"
          parseCoeff s = return (read (T.unpack s))

data DistanceRange = DistanceRange
  { distanceFrom :: Distance,
    distanceTo :: Distance
  }
  deriving stock (Eq, Show, Generic)

type TemplateQueryValue = Text

newtype TemplateQueryKeyValuePairs
  = TemplateQueryKeyValuePairs (X.KeyMap TemplateQueryValue)
  deriving stock (Eq, Show)

instance ToJSON TemplateQueryKeyValuePairs where
  toJSON (TemplateQueryKeyValuePairs x) = Object $ String <$> x

instance FromJSON TemplateQueryKeyValuePairs where
  parseJSON (Object o) =
    pure . TemplateQueryKeyValuePairs $ X.mapMaybe getValue o
    where
      getValue (String x) = Just x
      getValue _ = Nothing
  parseJSON _ =
    fail "error parsing TemplateQueryKeyValuePairs"

-- | 'Cache' is for telling ES whether it should cache a 'Filter' not.
--   'Query's cannot be cached.
type Cache = Bool -- caching on/off

defaultCache :: Cache
defaultCache = False

data FunctionScoreQuery = FunctionScoreQuery
  { functionScoreQuery :: Maybe Query,
    functionScoreBoost :: Maybe Boost,
    functionScoreFunctions :: FunctionScoreFunctions,
    functionScoreMaxBoost :: Maybe Boost,
    functionScoreBoostMode :: Maybe BoostMode,
    functionScoreMinScore :: Score,
    functionScoreScoreMode :: Maybe ScoreMode
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON FunctionScoreQuery where
  toJSON (FunctionScoreQuery query boost fns maxBoost boostMode minScore scoreMode) =
    omitNulls base
    where
      base =
        functionScoreFunctionsPair fns
          : [ "query" .= query,
              "boost" .= boost,
              "max_boost" .= maxBoost,
              "boost_mode" .= boostMode,
              "min_score" .= minScore,
              "score_mode" .= scoreMode
            ]

instance FromJSON FunctionScoreQuery where
  parseJSON = withObject "FunctionScoreQuery" parse
    where
      parse o =
        FunctionScoreQuery
          <$> o .:? "query"
          <*> o .:? "boost"
          <*> ( singleFunction o
                  <|> multipleFunctions
                    `taggedWith` "functions"
              )
          <*> o .:? "max_boost"
          <*> o .:? "boost_mode"
          <*> o .:? "min_score"
          <*> o .:? "score_mode"
        where
          taggedWith parser k = parser =<< o .: k
      singleFunction = fmap FunctionScoreSingle . parseFunctionScoreFunction
      multipleFunctions = pure . FunctionScoreMultiple

data FunctionScoreFunctions
  = FunctionScoreSingle FunctionScoreFunction
  | FunctionScoreMultiple (NonEmpty ComponentFunctionScoreFunction)
  deriving stock (Eq, Show, Generic)

data ComponentFunctionScoreFunction = ComponentFunctionScoreFunction
  { componentScoreFunctionFilter :: Maybe Filter,
    componentScoreFunction :: FunctionScoreFunction,
    componentScoreFunctionWeight :: Maybe Weight
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ComponentFunctionScoreFunction where
  toJSON (ComponentFunctionScoreFunction filter' fn weight) =
    omitNulls base
    where
      base =
        functionScoreFunctionPair fn
          : [ "filter" .= filter',
              "weight" .= weight
            ]

instance FromJSON ComponentFunctionScoreFunction where
  parseJSON = withObject "ComponentFunctionScoreFunction" parse
    where
      parse o =
        ComponentFunctionScoreFunction
          <$> o .:? "filter"
          <*> parseFunctionScoreFunction o
          <*> o .:? "weight"

functionScoreFunctionsPair :: FunctionScoreFunctions -> (Key, Value)
functionScoreFunctionsPair (FunctionScoreSingle fn) =
  functionScoreFunctionPair fn
functionScoreFunctionsPair (FunctionScoreMultiple componentFns) =
  ("functions", toJSON componentFns)

data InnerHits = InnerHits
  { innerHitsFrom :: Maybe Integer,
    innerHitsSize :: Maybe Integer
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON InnerHits where
  toJSON (InnerHits ihFrom ihSize) =
    omitNulls
      [ "from" .= ihFrom,
        "size" .= ihSize
      ]

instance FromJSON InnerHits where
  parseJSON = withObject "InnerHits" parse
    where
      parse o =
        InnerHits
          <$> o .:? "from"
          <*> o .:? "size"
