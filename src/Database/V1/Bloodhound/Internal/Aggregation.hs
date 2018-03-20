{-# LANGUAGE RecordWildCards #-}

module Database.V1.Bloodhound.Internal.Aggregation where

import           Bloodhound.Import

import qualified Data.HashMap.Strict                      as HM
import qualified Data.Map.Strict                          as M
import           Data.Text
import qualified Data.Text                                as T

import           Database.V1.Bloodhound.Internal.Newtypes
import           Database.V1.Bloodhound.Internal.Query
import           Database.V1.Bloodhound.Internal.Sort


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
  deriving (Eq, Show)


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
  } deriving (Eq, Show)

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

data CardinalityAggregation = CardinalityAggregation { cardinalityField   :: FieldName,
                                                       precisionThreshold :: Maybe Int
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


data DateRangeAggregation = DateRangeAggregation { draField  :: FieldName
                                                 , draFormat :: Maybe Text
                                                 , draRanges :: NonEmpty DateRangeAggRange
                                                 } deriving (Eq, Show)

data DateRangeAggRange = DateRangeFrom DateMathExpr
                       | DateRangeTo DateMathExpr
                       | DateRangeFromAndTo DateMathExpr DateMathExpr deriving (Eq, Show)

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

mkCardinalityAggregation :: FieldName -> CardinalityAggregation
mkCardinalityAggregation t = CardinalityAggregation t Nothing

data TermInclusion = TermInclusion Text
                   | TermPattern Text Text deriving (Eq, Show)

instance ToJSON TermInclusion where
  toJSON (TermInclusion x) = toJSON x
  toJSON (TermPattern pattern flags) =
    omitNulls [ "pattern" .= pattern
              , "flags"   .= flags]

data TermOrder = TermOrder
  { termSortField :: Text
  , termSortOrder :: SortOrder } deriving (Eq, Show)

instance ToJSON TermOrder where
  toJSON (TermOrder termSortField termSortOrder) =
    object [termSortField .= termSortOrder]


data ExecutionHint = Ordinals
                   | GlobalOrdinals
                   | GlobalOrdinalsHash
                   | GlobalOrdinalsLowCardinality
                   | Map deriving (Eq, Show)

instance ToJSON ExecutionHint where
  toJSON Ordinals                     = "ordinals"
  toJSON GlobalOrdinals               = "global_ordinals"
  toJSON GlobalOrdinalsHash           = "global_ordinals_hash"
  toJSON GlobalOrdinalsLowCardinality = "global_ordinals_low_cardinality"
  toJSON Map                          = "map"


-- | See <https://www.elastic.co/guide/en/elasticsearch/reference/current/common-options.html#date-math> for more information.
data DateMathExpr =
  DateMathExpr DateMathAnchor [DateMathModifier]
  deriving (Eq, Show)

instance ToJSON DateMathExpr where
  toJSON (DateMathExpr a mods) = String (fmtA a <> mconcat (fmtMod <$> mods))
    where fmtA DMNow         = "now"
          fmtA (DMDate date) = (T.pack $ showGregorian date) <> "||"
          fmtMod (AddTime n u)      = "+" <> showText n <> fmtU u
          fmtMod (SubtractTime n u) = "-" <> showText n <> fmtU u
          fmtMod (RoundDownTo u)    = "/" <> fmtU u
          fmtU DMYear   = "y"
          fmtU DMMonth  = "M"
          fmtU DMWeek   = "w"
          fmtU DMDay    = "d"
          fmtU DMHour   = "h"
          fmtU DMMinute = "m"
          fmtU DMSecond = "s"

-- | Starting point for a date range. This along with the 'DateMathModifiers' gets you the date ES will start from.
data DateMathAnchor =
    DMNow
  | DMDate Day
  deriving (Eq, Show)

data DateMathModifier =
    AddTime Int DateMathUnit
  | SubtractTime Int DateMathUnit
  | RoundDownTo DateMathUnit
  deriving (Eq, Show)

data DateMathUnit =
    DMYear
  | DMMonth
  | DMWeek
  | DMDay
  | DMHour
  | DMMinute
  | DMSecond
  deriving (Eq, Show)
