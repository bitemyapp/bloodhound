module Database.V1.Bloodhound.Internal.Aggregation where

import           Bloodhound.Import

import           Data.Text
import qualified Data.Text         as T


type Aggregations = M.Map Text Aggregation

emptyAggregations :: Aggregations
emptyAggregations = M.empty

mkAggregations :: Text -> Aggregation -> Aggregations
mkAggregations name aggregation = M.insert name aggregation emptyAggregations

data Aggregation = TermsAgg TermsAggregation
                 | CardinalityAgg CardinalityAggregation
                 | DateHistogramAgg DateHistogramAggregation
                 | ValueCountAgg ValueCountAggregation
                 | FilterAgg FilterAggregation
                 | DateRangeAgg DateRangeAggregation
                 | MissingAgg MissingAggregation
                 | TopHitsAgg TopHitsAggregation
  deriving (Eq, Read, Show, Generic, Typeable)


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

data TopHitsAggregation = TopHitsAggregation
  { taFrom :: Maybe From
  , taSize :: Maybe Size
  , taSort :: Maybe Sort
  } deriving (Eq, Read, Show)

data MissingAggregation = MissingAggregation
  { maField :: Text
  } deriving (Eq, Read, Show, Generic, Typeable)

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
                                    } deriving (Eq, Read, Show, Generic, Typeable)

data CardinalityAggregation = CardinalityAggregation { cardinalityField   :: FieldName,
                                                       precisionThreshold :: Maybe Int
                                                     } deriving (Eq, Read, Show, Generic, Typeable)

data DateHistogramAggregation = DateHistogramAggregation { dateField      :: FieldName
                                                         , dateInterval   :: Interval
                                                         , dateFormat     :: Maybe Text
                                                         -- pre and post deprecated in 1.5
                                                         , datePreZone    :: Maybe Text
                                                         , datePostZone   :: Maybe Text
                                                         , datePreOffset  :: Maybe Text
                                                         , datePostOffset :: Maybe Text
                                                         , dateAggs       :: Maybe Aggregations
                                                         } deriving (Eq, Read, Show, Generic, Typeable)


data DateRangeAggregation = DateRangeAggregation { draField  :: FieldName
                                                 , draFormat :: Maybe Text
                                                 , draRanges :: NonEmpty DateRangeAggRange
                                                 } deriving (Eq, Read, Show, Generic, Typeable)

data DateRangeAggRange = DateRangeFrom DateMathExpr
                       | DateRangeTo DateMathExpr
                       | DateRangeFromAndTo DateMathExpr DateMathExpr deriving (Eq, Read, Show, Generic, Typeable)

-- | See <https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-metrics-valuecount-aggregation.html> for more information.
data ValueCountAggregation = FieldValueCount FieldName
                           | ScriptValueCount Script deriving (Eq, Read, Show, Generic, Typeable)

-- | Single-bucket filter aggregations. See <https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-bucket-filter-aggregation.html#search-aggregations-bucket-filter-aggregation> for more information.
data FilterAggregation = FilterAggregation { faFilter :: Filter
                                           , faAggs   :: Maybe Aggregations} deriving (Eq, Read, Show, Generic, Typeable)

mkTermsAggregation :: Text -> TermsAggregation
mkTermsAggregation t = TermsAggregation (Left t) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

mkTermsScriptAggregation :: Text -> TermsAggregation
mkTermsScriptAggregation t = TermsAggregation (Right t) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

mkDateHistogram :: FieldName -> Interval -> DateHistogramAggregation
mkDateHistogram t i = DateHistogramAggregation t i Nothing Nothing Nothing Nothing Nothing Nothing

mkCardinalityAggregation :: FieldName -> CardinalityAggregation
mkCardinalityAggregation t = CardinalityAggregation t Nothing

