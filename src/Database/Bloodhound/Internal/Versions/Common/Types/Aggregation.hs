{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Bloodhound.Internal.Versions.Common.Types.Aggregation where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as X
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Database.Bloodhound.Internal.Utils.Imports
import Database.Bloodhound.Internal.Versions.Common.Types.Highlight (HitHighlight)
import Database.Bloodhound.Internal.Versions.Common.Types.Newtypes
import Database.Bloodhound.Internal.Versions.Common.Types.Query
import Database.Bloodhound.Internal.Versions.Common.Types.Sort
import Database.Bloodhound.Internal.Versions.Common.Types.Units

type Aggregations = M.Map Key Aggregation

emptyAggregations :: Aggregations
emptyAggregations = M.empty

mkAggregations :: Key -> Aggregation -> Aggregations
mkAggregations name aggregation = M.insert name aggregation emptyAggregations

data Aggregation
  = TermsAgg TermsAggregation
  | CardinalityAgg CardinalityAggregation
  | DateHistogramAgg DateHistogramAggregation
  | ValueCountAgg ValueCountAggregation
  | FilterAgg FilterAggregation
  | DateRangeAgg DateRangeAggregation
  | MissingAgg MissingAggregation
  | TopHitsAgg TopHitsAggregation
  | StatsAgg StatisticsAggregation
  | SumAgg SumAggregation
  | AvgAgg AvgAggregation
  | MedianAbsoluteDeviationAgg MedianAbsoluteDeviationAggregation
  deriving stock (Eq, Show)

aggregationTermsAggPrism :: Prism' Aggregation TermsAggregation
aggregationTermsAggPrism = prism TermsAgg extract
  where
    extract s =
      case s of
        TermsAgg x -> Right x
        _ -> Left s

aggregationCardinalityAggPrism :: Prism' Aggregation CardinalityAggregation
aggregationCardinalityAggPrism = prism CardinalityAgg extract
  where
    extract s =
      case s of
        CardinalityAgg x -> Right x
        _ -> Left s

aggregationDateHistogramAggPrism :: Prism' Aggregation DateHistogramAggregation
aggregationDateHistogramAggPrism = prism DateHistogramAgg extract
  where
    extract s =
      case s of
        DateHistogramAgg x -> Right x
        _ -> Left s

aggregationValueCountAggPrism :: Prism' Aggregation ValueCountAggregation
aggregationValueCountAggPrism = prism ValueCountAgg extract
  where
    extract s =
      case s of
        ValueCountAgg x -> Right x
        _ -> Left s

aggregationFilterAggPrism :: Prism' Aggregation FilterAggregation
aggregationFilterAggPrism = prism FilterAgg extract
  where
    extract s =
      case s of
        FilterAgg x -> Right x
        _ -> Left s

aggregationDateRangeAggPrism :: Prism' Aggregation DateRangeAggregation
aggregationDateRangeAggPrism = prism DateRangeAgg extract
  where
    extract s =
      case s of
        DateRangeAgg x -> Right x
        _ -> Left s

aggregationMissingAggPrism :: Prism' Aggregation MissingAggregation
aggregationMissingAggPrism = prism MissingAgg extract
  where
    extract s =
      case s of
        MissingAgg x -> Right x
        _ -> Left s

aggregationTopHitsAggPrism :: Prism' Aggregation TopHitsAggregation
aggregationTopHitsAggPrism = prism TopHitsAgg extract
  where
    extract s =
      case s of
        TopHitsAgg x -> Right x
        _ -> Left s

aggregationStatsAggPrism :: Prism' Aggregation StatisticsAggregation
aggregationStatsAggPrism = prism StatsAgg extract
  where
    extract s =
      case s of
        StatsAgg x -> Right x
        _ -> Left s

aggregationSumAggPrism :: Prism' Aggregation SumAggregation
aggregationSumAggPrism = prism SumAgg extract
  where
    extract s =
      case s of
        SumAgg x -> Right x
        _ -> Left s

aggregationAvgAggPrism :: Prism' Aggregation AvgAggregation
aggregationAvgAggPrism = prism AvgAgg extract
  where
    extract s =
      case s of
        AvgAgg x -> Right x
        _ -> Left s

aggregationMedianAbsoluteDeviationAggPrism :: Prism' Aggregation MedianAbsoluteDeviationAggregation
aggregationMedianAbsoluteDeviationAggPrism = prism MedianAbsoluteDeviationAgg extract
  where
    extract s =
      case s of
        MedianAbsoluteDeviationAgg x -> Right x
        _ -> Left s

instance ToJSON Aggregation where
  toJSON (TermsAgg (TermsAggregation term include exclude order minDocCount size shardSize collectMode executionHint termAggs)) =
    omitNulls
      [ "terms"
          .= omitNulls
            [ toJSON' term,
              "include" .= include,
              "exclude" .= exclude,
              "order" .= order,
              "min_doc_count" .= minDocCount,
              "size" .= size,
              "shard_size" .= shardSize,
              "collect_mode" .= collectMode,
              "execution_hint" .= executionHint
            ],
        "aggs" .= termAggs
      ]
    where
      toJSON' x = case x of Left y -> "field" .= y; Right y -> "script" .= y
  toJSON (CardinalityAgg (CardinalityAggregation field precisionThreshold)) =
    object
      [ "cardinality"
          .= omitNulls
            [ "field" .= field,
              "precisionThreshold" .= precisionThreshold
            ]
      ]
  toJSON
    ( DateHistogramAgg
        ( DateHistogramAggregation
            field
            interval
            format
            preZone
            postZone
            preOffset
            postOffset
            dateHistoAggs
          )
      ) =
      omitNulls
        [ "date_histogram"
            .= omitNulls
              [ "field" .= field,
                "interval" .= interval,
                "format" .= format,
                "pre_zone" .= preZone,
                "post_zone" .= postZone,
                "pre_offset" .= preOffset,
                "post_offset" .= postOffset
              ],
          "aggs" .= dateHistoAggs
        ]
  toJSON (ValueCountAgg a) = object ["value_count" .= v]
    where
      v = case a of
        (FieldValueCount (FieldName n)) ->
          object ["field" .= n]
        (ScriptValueCount s) ->
          object ["script" .= s]
  toJSON (FilterAgg (FilterAggregation filt ags)) =
    omitNulls
      [ "filter" .= filt,
        "aggs" .= ags
      ]
  toJSON (DateRangeAgg a) =
    object
      [ "date_range" .= a
      ]
  toJSON (MissingAgg (MissingAggregation {..})) =
    object ["missing" .= object ["field" .= maField]]
  toJSON (TopHitsAgg (TopHitsAggregation mfrom msize msort)) =
    omitNulls
      [ "top_hits"
          .= omitNulls
            [ "size" .= msize,
              "from" .= mfrom,
              "sort" .= msort
            ]
      ]
  toJSON (StatsAgg (StatisticsAggregation typ field)) =
    object [stType .= omitNulls ["field" .= field]]
    where
      stType
        | typ == Basic = "stats"
        | otherwise = "extended_stats"
  toJSON (SumAgg (SumAggregation (FieldName n))) =
    omitNulls ["sum" .= omitNulls ["field" .= n]]
  toJSON (AvgAgg (AvgAggregation (FieldName n) m)) =
    omitNulls ["avg" .= omitNulls ["field" .= n], "missing" .= m]
  toJSON (MedianAbsoluteDeviationAgg (MedianAbsoluteDeviationAggregation (FieldName n) m)) =
    omitNulls ["median_absolute_deviation" .= omitNulls ["field" .= n], "missing" .= m]

data TopHitsAggregation = TopHitsAggregation
  { taFrom :: Maybe From,
    taSize :: Maybe Size,
    taSort :: Maybe Sort
  }
  deriving stock (Eq, Show)

topHitsAggregationFromLens :: Lens' TopHitsAggregation (Maybe From)
topHitsAggregationFromLens = lens taFrom (\x y -> x {taFrom = y})

topHitsAggregationSizeLens :: Lens' TopHitsAggregation (Maybe Size)
topHitsAggregationSizeLens = lens taSize (\x y -> x {taSize = y})

topHitsAggregationSortLens :: Lens' TopHitsAggregation (Maybe Sort)
topHitsAggregationSortLens = lens taSort (\x y -> x {taSort = y})

data MissingAggregation = MissingAggregation
  { maField :: Text
  }
  deriving stock (Eq, Show)

missingAggregationFieldLens :: Lens' MissingAggregation Text
missingAggregationFieldLens = lens maField (\x y -> x {maField = y})

data TermsAggregation = TermsAggregation
  { term :: Either Text Text,
    termInclude :: Maybe TermInclusion,
    termExclude :: Maybe TermInclusion,
    termOrder :: Maybe TermOrder,
    termMinDocCount :: Maybe Int,
    termSize :: Maybe Int,
    termShardSize :: Maybe Int,
    termCollectMode :: Maybe CollectionMode,
    termExecutionHint :: Maybe ExecutionHint,
    termAggs :: Maybe Aggregations
  }
  deriving stock (Eq, Show)

termAggregationTermLens :: Lens' TermsAggregation (Either Text Text)
termAggregationTermLens = lens term (\x y -> x {term = y})

termAggregationIncludeLens :: Lens' TermsAggregation (Maybe TermInclusion)
termAggregationIncludeLens = lens termInclude (\x y -> x {termInclude = y})

termAggregationExcludeLens :: Lens' TermsAggregation (Maybe TermInclusion)
termAggregationExcludeLens = lens termExclude (\x y -> x {termExclude = y})

termAggregationOrderLens :: Lens' TermsAggregation (Maybe TermOrder)
termAggregationOrderLens = lens termOrder (\x y -> x {termOrder = y})

termAggregationMinDocCountLens :: Lens' TermsAggregation (Maybe Int)
termAggregationMinDocCountLens = lens termMinDocCount (\x y -> x {termMinDocCount = y})

termAggregationSizeLens :: Lens' TermsAggregation (Maybe Int)
termAggregationSizeLens = lens termSize (\x y -> x {termSize = y})

termAggregationShardSizeLens :: Lens' TermsAggregation (Maybe Int)
termAggregationShardSizeLens = lens termShardSize (\x y -> x {termShardSize = y})

termAggregationCollectModeLens :: Lens' TermsAggregation (Maybe CollectionMode)
termAggregationCollectModeLens = lens termCollectMode (\x y -> x {termCollectMode = y})

termAggregationExecutionHintLens :: Lens' TermsAggregation (Maybe ExecutionHint)
termAggregationExecutionHintLens = lens termExecutionHint (\x y -> x {termExecutionHint = y})

termAggregationAggsLens :: Lens' TermsAggregation (Maybe Aggregations)
termAggregationAggsLens = lens termAggs (\x y -> x {termAggs = y})

data CardinalityAggregation = CardinalityAggregation
  { cardinalityField :: FieldName,
    precisionThreshold :: Maybe Int
  }
  deriving stock (Eq, Show)

cardinalityAggregationFieldLens :: Lens' CardinalityAggregation FieldName
cardinalityAggregationFieldLens = lens cardinalityField (\x y -> x {cardinalityField = y})

cardinalityAggregationPrecisionThresholdLens :: Lens' CardinalityAggregation (Maybe Int)
cardinalityAggregationPrecisionThresholdLens = lens precisionThreshold (\x y -> x {precisionThreshold = y})

data DateHistogramAggregation = DateHistogramAggregation
  { dateField :: FieldName,
    dateInterval :: Interval,
    dateFormat :: Maybe Text,
    -- pre and post deprecated in 1.5
    datePreZone :: Maybe Text,
    datePostZone :: Maybe Text,
    datePreOffset :: Maybe Text,
    datePostOffset :: Maybe Text,
    dateAggs :: Maybe Aggregations
  }
  deriving stock (Eq, Show)

dateHistogramAggregationFieldLens :: Lens' DateHistogramAggregation FieldName
dateHistogramAggregationFieldLens = lens dateField (\x y -> x {dateField = y})

dateHistogramAggregationIntervalLens :: Lens' DateHistogramAggregation Interval
dateHistogramAggregationIntervalLens = lens dateInterval (\x y -> x {dateInterval = y})

dateHistogramAggregationFormatLens :: Lens' DateHistogramAggregation (Maybe Text)
dateHistogramAggregationFormatLens = lens dateFormat (\x y -> x {dateFormat = y})

dateHistogramAggregationPreZoneLens :: Lens' DateHistogramAggregation (Maybe Text)
dateHistogramAggregationPreZoneLens = lens datePreZone (\x y -> x {datePreZone = y})

dateHistogramAggregationPostZoneLens :: Lens' DateHistogramAggregation (Maybe Text)
dateHistogramAggregationPostZoneLens = lens datePostZone (\x y -> x {datePostZone = y})

dateHistogramAggregationPreOffsetLens :: Lens' DateHistogramAggregation (Maybe Text)
dateHistogramAggregationPreOffsetLens = lens datePreOffset (\x y -> x {datePreOffset = y})

dateHistogramAggregationPostOffsetLens :: Lens' DateHistogramAggregation (Maybe Text)
dateHistogramAggregationPostOffsetLens = lens datePostOffset (\x y -> x {datePostOffset = y})

dateHistogramAggregationAggsLens :: Lens' DateHistogramAggregation (Maybe Aggregations)
dateHistogramAggregationAggsLens = lens dateAggs (\x y -> x {dateAggs = y})

data DateRangeAggregation = DateRangeAggregation
  { draField :: FieldName,
    draFormat :: Maybe Text,
    draRanges :: NonEmpty DateRangeAggRange
  }
  deriving stock (Eq, Show)

instance ToJSON DateRangeAggregation where
  toJSON DateRangeAggregation {..} =
    omitNulls
      [ "field" .= draField,
        "format" .= draFormat,
        "ranges" .= toList draRanges
      ]

dateRangeAggregationFieldLens :: Lens' DateRangeAggregation FieldName
dateRangeAggregationFieldLens = lens draField (\x y -> x {draField = y})

dateRangeAggregationFormatLens :: Lens' DateRangeAggregation (Maybe Text)
dateRangeAggregationFormatLens = lens draFormat (\x y -> x {draFormat = y})

dateRangeAggregationRangesLens :: Lens' DateRangeAggregation (NonEmpty DateRangeAggRange)
dateRangeAggregationRangesLens = lens draRanges (\x y -> x {draRanges = y})

data DateRangeAggRange
  = DateRangeFrom DateMathExpr
  | DateRangeTo DateMathExpr
  | DateRangeFromAndTo DateMathExpr DateMathExpr
  deriving stock (Eq, Show)

dateRangeAggRangeDateRangeFromPrism :: Prism' DateRangeAggRange DateMathExpr
dateRangeAggRangeDateRangeFromPrism = prism DateRangeFrom extract
  where
    extract s =
      case s of
        DateRangeFrom x -> Right x
        _ -> Left s

dateRangeAggRangeDateRangeToPrism :: Prism' DateRangeAggRange DateMathExpr
dateRangeAggRangeDateRangeToPrism = prism DateRangeTo extract
  where
    extract s =
      case s of
        DateRangeTo x -> Right x
        _ -> Left s

dateRangeAggRangeDateRangeFromAndToPrism :: Prism' DateRangeAggRange (DateMathExpr, DateMathExpr)
dateRangeAggRangeDateRangeFromAndToPrism = prism (uncurry DateRangeFromAndTo) extract
  where
    extract s =
      case s of
        DateRangeFromAndTo x y -> Right (x, y)
        _ -> Left s

instance ToJSON DateRangeAggRange where
  toJSON (DateRangeFrom e) = object ["from" .= e]
  toJSON (DateRangeTo e) = object ["to" .= e]
  toJSON (DateRangeFromAndTo f t) = object ["from" .= f, "to" .= t]

-- | See <https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-metrics-valuecount-aggregation.html> for more information.
data ValueCountAggregation
  = FieldValueCount FieldName
  | ScriptValueCount Script
  deriving stock (Eq, Show)

-- | Single-bucket filter aggregations. See <https://www.elastic.co/guide/en/elasticsearch/reference/current/search-aggregations-bucket-filter-aggregation.html#search-aggregations-bucket-filter-aggregation> for more information.
data FilterAggregation = FilterAggregation
  { faFilter :: Filter,
    faAggs :: Maybe Aggregations
  }
  deriving stock (Eq, Show)

filterAggregationFilterLens :: Lens' FilterAggregation Filter
filterAggregationFilterLens = lens faFilter (\x y -> x {faFilter = y})

filterAggregationAggsLens :: Lens' FilterAggregation (Maybe Aggregations)
filterAggregationAggsLens = lens faAggs (\x y -> x {faAggs = y})

data StatisticsAggregation = StatisticsAggregation
  { statsType :: StatsType,
    statsField :: FieldName
  }
  deriving stock (Eq, Show)

statisticsAggregationTypeLens :: Lens' StatisticsAggregation StatsType
statisticsAggregationTypeLens = lens statsType (\x y -> x {statsType = y})

statisticsAggregationFieldLens :: Lens' StatisticsAggregation FieldName
statisticsAggregationFieldLens = lens statsField (\x y -> x {statsField = y})

data StatsType
  = Basic
  | Extended
  deriving stock (Eq, Show)

newtype SumAggregation = SumAggregation {sumAggregationField :: FieldName}
  deriving stock (Eq, Show)

data AvgAggregation = AvgAggregation
  { avgAggregationField :: FieldName,
    avgAggregationMissing :: Maybe Missing
  }
  deriving stock (Eq, Show)

avgAggregationFieldLens :: Lens' AvgAggregation FieldName
avgAggregationFieldLens = lens avgAggregationField (\x y -> x {avgAggregationField = y})

avgAggregationMissingLens :: Lens' AvgAggregation (Maybe Missing)
avgAggregationMissingLens = lens avgAggregationMissing (\x y -> x {avgAggregationMissing = y})

data MedianAbsoluteDeviationAggregation = MedianAbsoluteDeviationAggregation
  { medianAbsoluteDeviationAggregationField :: FieldName,
    medianAbsoluteDeviationAggregationMissing :: Maybe Missing
  }
  deriving stock (Eq, Show)

medianAbsoluteDeviationAggregationFieldLens :: Lens' MedianAbsoluteDeviationAggregation FieldName
medianAbsoluteDeviationAggregationFieldLens = lens medianAbsoluteDeviationAggregationField (\x y -> x {medianAbsoluteDeviationAggregationField = y})

medianAbsoluteDeviationAggregationMissingLens :: Lens' MedianAbsoluteDeviationAggregation (Maybe Missing)
medianAbsoluteDeviationAggregationMissingLens = lens medianAbsoluteDeviationAggregationMissing (\x y -> x {medianAbsoluteDeviationAggregationMissing = y})

mkTermsAggregation :: Text -> TermsAggregation
mkTermsAggregation t =
  TermsAggregation
    (Left t)
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing

mkTermsScriptAggregation :: Text -> TermsAggregation
mkTermsScriptAggregation t = TermsAggregation (Right t) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

mkDateHistogram :: FieldName -> Interval -> DateHistogramAggregation
mkDateHistogram t i = DateHistogramAggregation t i Nothing Nothing Nothing Nothing Nothing Nothing

mkCardinalityAggregation :: FieldName -> CardinalityAggregation
mkCardinalityAggregation t = CardinalityAggregation t Nothing

mkStatsAggregation :: FieldName -> StatisticsAggregation
mkStatsAggregation = StatisticsAggregation Basic

mkExtendedStatsAggregation :: FieldName -> StatisticsAggregation
mkExtendedStatsAggregation = StatisticsAggregation Extended

type AggregationResults = M.Map Key Value

class BucketAggregation a where
  key :: a -> BucketValue
  docCount :: a -> Int
  aggs :: a -> Maybe AggregationResults

data Bucket a = Bucket
  { buckets :: [a]
  }
  deriving (Read, Show)

instance (FromJSON a) => FromJSON (Bucket a) where
  parseJSON (Object v) =
    Bucket
      <$> v .: "buckets"
  parseJSON _ = mempty

bucketsLens :: Lens' (Bucket a) [a]
bucketsLens = lens buckets (\x y -> x {buckets = y})

data BucketValue
  = TextValue Text
  | ScientificValue Scientific
  | BoolValue Bool
  deriving (Read, Show)

instance FromJSON BucketValue where
  parseJSON (String t) = return $ TextValue t
  parseJSON (Number s) = return $ ScientificValue s
  parseJSON (Bool b) = return $ BoolValue b
  parseJSON _ = mempty

data TermInclusion
  = TermInclusion Text
  | TermPattern Text Text
  deriving stock (Eq, Show)

instance ToJSON TermInclusion where
  toJSON (TermInclusion x) = toJSON x
  toJSON (TermPattern pattern flags) =
    omitNulls
      [ "pattern" .= pattern,
        "flags" .= flags
      ]

data TermOrder = TermOrder
  { termSortField :: Text,
    termSortOrder :: SortOrder
  }
  deriving stock (Eq, Show)

instance ToJSON TermOrder where
  toJSON (TermOrder termSortField termSortOrder) =
    object [fromText termSortField .= termSortOrder]

termOrderSortFieldLens :: Lens' TermOrder Text
termOrderSortFieldLens = lens termSortField (\x y -> x {termSortField = y})

termOrderSortOrderLens :: Lens' TermOrder SortOrder
termOrderSortOrderLens = lens termSortOrder (\x y -> x {termSortOrder = y})

data CollectionMode
  = BreadthFirst
  | DepthFirst
  deriving stock (Eq, Show)

instance ToJSON CollectionMode where
  toJSON BreadthFirst = "breadth_first"
  toJSON DepthFirst = "depth_first"

data ExecutionHint
  = GlobalOrdinals
  | Map
  deriving stock (Eq, Show)

instance ToJSON ExecutionHint where
  toJSON GlobalOrdinals = "global_ordinals"
  toJSON Map = "map"

-- | See <https://www.elastic.co/guide/en/elasticsearch/reference/current/common-options.html#date-math> for more information.
data DateMathExpr
  = DateMathExpr DateMathAnchor [DateMathModifier]
  deriving stock (Eq, Show)

instance ToJSON DateMathExpr where
  toJSON (DateMathExpr a mods) = String (fmtA a <> mconcat (fmtMod <$> mods))
    where
      fmtA DMNow = "now"
      fmtA (DMDate date) = (T.pack $ showGregorian date) <> "||"
      fmtMod (AddTime n u) = "+" <> showText n <> fmtU u
      fmtMod (SubtractTime n u) = "-" <> showText n <> fmtU u
      fmtMod (RoundDownTo u) = "/" <> fmtU u
      fmtU DMYear = "y"
      fmtU DMMonth = "M"
      fmtU DMWeek = "w"
      fmtU DMDay = "d"
      fmtU DMHour = "h"
      fmtU DMMinute = "m"
      fmtU DMSecond = "s"

-- | Starting point for a date range. This along with the 'DateMathModifiers' gets you the date ES will start from.
data DateMathAnchor
  = DMNow
  | DMDate Day
  deriving stock (Eq, Show)

data DateMathModifier
  = AddTime Int DateMathUnit
  | SubtractTime Int DateMathUnit
  | RoundDownTo DateMathUnit
  deriving stock (Eq, Show)

data DateMathUnit
  = DMYear
  | DMMonth
  | DMWeek
  | DMDay
  | DMHour
  | DMMinute
  | DMSecond
  deriving stock (Eq, Show)

data TermsResult = TermsResult
  { termKey :: BucketValue,
    termsDocCount :: Int,
    termsAggs :: Maybe AggregationResults
  }
  deriving (Read, Show)

instance FromJSON TermsResult where
  parseJSON (Object v) =
    TermsResult
      <$> v .: "key"
      <*> v .: "doc_count"
      <*> pure (getNamedSubAgg v ["key", "doc_count"])
  parseJSON _ = mempty

instance BucketAggregation TermsResult where
  key = termKey
  docCount = termsDocCount
  aggs = termsAggs

termsResultKeyLens :: Lens' TermsResult BucketValue
termsResultKeyLens = lens termKey (\x y -> x {termKey = y})

termsResultDocCountLens :: Lens' TermsResult Int
termsResultDocCountLens = lens termsDocCount (\x y -> x {termsDocCount = y})

termsResultAggsLens :: Lens' TermsResult (Maybe AggregationResults)
termsResultAggsLens = lens termsAggs (\x y -> x {termsAggs = y})

data DateHistogramResult = DateHistogramResult
  { dateKey :: Int,
    dateKeyStr :: Maybe Text,
    dateDocCount :: Int,
    dateHistogramAggs :: Maybe AggregationResults
  }
  deriving (Show)

instance FromJSON DateHistogramResult where
  parseJSON (Object v) =
    DateHistogramResult
      <$> v .: "key"
      <*> v .:? "key_as_string"
      <*> v .: "doc_count"
      <*> ( pure $
              getNamedSubAgg
                v
                [ "key",
                  "doc_count",
                  "key_as_string"
                ]
          )
  parseJSON _ = mempty

instance BucketAggregation DateHistogramResult where
  key = TextValue . showText . dateKey
  docCount = dateDocCount
  aggs = dateHistogramAggs

dateHistogramResultKeyLens :: Lens' DateHistogramResult Int
dateHistogramResultKeyLens = lens dateKey (\x y -> x {dateKey = y})

dateHistogramResultKeyStrLens :: Lens' DateHistogramResult (Maybe Text)
dateHistogramResultKeyStrLens = lens dateKeyStr (\x y -> x {dateKeyStr = y})

dateHistogramResultDocCountLens :: Lens' DateHistogramResult Int
dateHistogramResultDocCountLens = lens dateDocCount (\x y -> x {dateDocCount = y})

dateHistogramResultAggsLens :: Lens' DateHistogramResult (Maybe AggregationResults)
dateHistogramResultAggsLens = lens dateHistogramAggs (\x y -> x {dateHistogramAggs = y})

data DateRangeResult = DateRangeResult
  { dateRangeKey :: Text,
    dateRangeFrom :: Maybe UTCTime,
    dateRangeFromAsString :: Maybe Text,
    dateRangeTo :: Maybe UTCTime,
    dateRangeToAsString :: Maybe Text,
    dateRangeDocCount :: Int,
    dateRangeAggs :: Maybe AggregationResults
  }
  deriving stock (Eq, Show)

instance FromJSON DateRangeResult where
  parseJSON = withObject "DateRangeResult" parse
    where
      parse v =
        DateRangeResult
          <$> v .: "key"
          <*> (fmap posixMS <$> v .:? "from")
          <*> v .:? "from_as_string"
          <*> (fmap posixMS <$> v .:? "to")
          <*> v .:? "to_as_string"
          <*> v .: "doc_count"
          <*> ( pure $
                  getNamedSubAgg
                    v
                    [ "key",
                      "from",
                      "from_as_string",
                      "to",
                      "to_as_string",
                      "doc_count"
                    ]
              )

instance BucketAggregation DateRangeResult where
  key = TextValue . dateRangeKey
  docCount = dateRangeDocCount
  aggs = dateRangeAggs

dateRangeResultKeyLens :: Lens' DateRangeResult Text
dateRangeResultKeyLens = lens dateRangeKey (\x y -> x {dateRangeKey = y})

dateRangeResultFromLens :: Lens' DateRangeResult (Maybe UTCTime)
dateRangeResultFromLens = lens dateRangeFrom (\x y -> x {dateRangeFrom = y})

dateRangeResultFromAsStringLens :: Lens' DateRangeResult (Maybe Text)
dateRangeResultFromAsStringLens = lens dateRangeFromAsString (\x y -> x {dateRangeFromAsString = y})

dateRangeResultToLens :: Lens' DateRangeResult (Maybe UTCTime)
dateRangeResultToLens = lens dateRangeTo (\x y -> x {dateRangeTo = y})

dateRangeResultToAsStringLens :: Lens' DateRangeResult (Maybe Text)
dateRangeResultToAsStringLens = lens dateRangeToAsString (\x y -> x {dateRangeToAsString = y})

dateRangeResultDocCountLens :: Lens' DateRangeResult Int
dateRangeResultDocCountLens = lens dateRangeDocCount (\x y -> x {dateRangeDocCount = y})

dateRangeResultAggsLens :: Lens' DateRangeResult (Maybe AggregationResults)
dateRangeResultAggsLens = lens dateRangeAggs (\x y -> x {dateRangeAggs = y})

toTerms :: Key -> AggregationResults -> Maybe (Bucket TermsResult)
toTerms = toAggResult

toDateHistogram :: Key -> AggregationResults -> Maybe (Bucket DateHistogramResult)
toDateHistogram = toAggResult

toMissing :: Key -> AggregationResults -> Maybe MissingResult
toMissing = toAggResult

toTopHits :: (FromJSON a) => Key -> AggregationResults -> Maybe (TopHitResult a)
toTopHits = toAggResult

toAggResult :: (FromJSON a) => Key -> AggregationResults -> Maybe a
toAggResult t a = M.lookup t a >>= deserialize
  where
    deserialize = parseMaybe parseJSON

-- Try to get an AggregationResults when we don't know the
-- field name. We filter out the known keys to try to minimize the noise.
getNamedSubAgg :: Object -> [Key] -> Maybe AggregationResults
getNamedSubAgg o knownKeys = maggRes
  where
    unknownKeys = X.filterWithKey (\k _ -> k `notElem` knownKeys) o
    maggRes
      | X.null unknownKeys = Nothing
      | otherwise = Just . M.fromList $ X.toList unknownKeys

data MissingResult = MissingResult
  { missingDocCount :: Int
  }
  deriving (Show)

instance FromJSON MissingResult where
  parseJSON = withObject "MissingResult" parse
    where
      parse v = MissingResult <$> v .: "doc_count"

data TopHitResult a = TopHitResult
  { tarHits :: (SearchHits a)
  }
  deriving stock (Eq, Show)

instance (FromJSON a) => FromJSON (TopHitResult a) where
  parseJSON (Object v) =
    TopHitResult
      <$> v .: "hits"
  parseJSON _ = fail "Failure in FromJSON (TopHitResult a)"

data HitsTotalRelation = HTR_EQ | HTR_GTE deriving stock (Eq, Show)

instance FromJSON HitsTotalRelation where
  parseJSON (String "eq") = pure HTR_EQ
  parseJSON (String "gte") = pure HTR_GTE
  parseJSON _ = empty

data HitsTotal = HitsTotal
  { value :: Int,
    relation :: HitsTotalRelation
  }
  deriving stock (Eq, Show)

instance FromJSON HitsTotal where
  parseJSON (Object v) =
    HitsTotal
      <$> v .: "value"
      <*> v .: "relation"
  parseJSON _ = empty

instance Semigroup HitsTotal where
  (HitsTotal ta HTR_EQ) <> (HitsTotal tb HTR_EQ) = HitsTotal (ta + tb) HTR_EQ
  (HitsTotal ta HTR_GTE) <> (HitsTotal tb _) = HitsTotal (ta + tb) HTR_GTE
  (HitsTotal ta _) <> (HitsTotal tb HTR_GTE) = HitsTotal (ta + tb) HTR_GTE

hitsTotalValueLens :: Lens' HitsTotal Int
hitsTotalValueLens = lens value (\x y -> x {value = y})

hitsTotalRelationLens :: Lens' HitsTotal HitsTotalRelation
hitsTotalRelationLens = lens relation (\x y -> x {relation = y})

data SearchHits a = SearchHits
  { hitsTotal :: HitsTotal,
    maxScore :: Score,
    hits :: [Hit a]
  }
  deriving stock (Eq, Show)

instance (FromJSON a) => FromJSON (SearchHits a) where
  parseJSON (Object v) =
    SearchHits
      <$> v .: "total"
      <*> v .: "max_score"
      <*> v .: "hits"
  parseJSON _ = empty

instance Semigroup (SearchHits a) where
  (SearchHits ta ma ha) <> (SearchHits tb mb hb) =
    SearchHits (ta <> tb) (max ma mb) (ha <> hb)

instance Monoid (SearchHits a) where
  mempty = SearchHits (HitsTotal 0 HTR_EQ) Nothing mempty
  mappend = (<>)

searchHitsHitsTotalLens :: Lens' (SearchHits a) HitsTotal
searchHitsHitsTotalLens = lens hitsTotal (\x y -> x {hitsTotal = y})

searchHitsMaxScoreLens :: Lens' (SearchHits a) Score
searchHitsMaxScoreLens = lens maxScore (\x y -> x {maxScore = y})

searchHitsHitsLens :: Lens' (SearchHits a) [Hit a]
searchHitsHitsLens = lens hits (\x y -> x {hits = y})

type SearchAfterKey = [Aeson.Value]

data Hit a = Hit
  { hitIndex :: IndexName,
    hitDocId :: DocId,
    hitScore :: Score,
    hitSource :: Maybe a,
    hitSort :: Maybe SearchAfterKey,
    hitFields :: Maybe HitFields,
    hitHighlight :: Maybe HitHighlight,
    hitInnerHits :: Maybe (X.KeyMap (TopHitResult Value))
  }
  deriving stock (Eq, Show)

instance (FromJSON a) => FromJSON (Hit a) where
  parseJSON (Object v) =
    Hit
      <$> v .: "_index"
      <*> v .: "_id"
      <*> v .: "_score"
      <*> v .:? "_source"
      <*> v .:? "sort"
      <*> v .:? "fields"
      <*> v .:? "highlight"
      <*> v .:? "inner_hits"
  parseJSON _ = empty

hitIndexLens :: Lens' (Hit a) IndexName
hitIndexLens = lens hitIndex (\x y -> x {hitIndex = y})

hitDocIdLens :: Lens' (Hit a) DocId
hitDocIdLens = lens hitDocId (\x y -> x {hitDocId = y})

hitScoreLens :: Lens' (Hit a) Score
hitScoreLens = lens hitScore (\x y -> x {hitScore = y})

hitSourceLens :: Lens' (Hit a) (Maybe a)
hitSourceLens = lens hitSource (\x y -> x {hitSource = y})

hitSortLens :: Lens' (Hit a) (Maybe SearchAfterKey)
hitSortLens = lens hitSort (\x y -> x {hitSort = y})

hitFieldsLens :: Lens' (Hit a) (Maybe HitFields)
hitFieldsLens = lens hitFields (\x y -> x {hitFields = y})

hitHighlightLens :: Lens' (Hit a) (Maybe HitHighlight)
hitHighlightLens = lens hitHighlight (\x y -> x {hitHighlight = y})

hitInnerHitsLens :: Lens' (Hit a) (Maybe (X.KeyMap (TopHitResult Value)))
hitInnerHitsLens = lens hitInnerHits (\x y -> x {hitInnerHits = y})
