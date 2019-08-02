{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Database.V5.Bloodhound.Internal.Sort where

import           Bloodhound.Import

import           Database.V5.Bloodhound.Internal.Newtypes
import           Database.V5.Bloodhound.Internal.Query

{-| 'SortMode' prescribes how to handle sorting array/multi-valued fields.

http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/search-request-sort.html#_sort_mode_option
-}
data SortMode = SortMin
              | SortMax
              | SortSum
              | SortAvg deriving (Eq, Show)

instance ToJSON SortMode where
  toJSON SortMin = String "min"
  toJSON SortMax = String "max"
  toJSON SortSum = String "sum"
  toJSON SortAvg = String "avg"

{-| 'mkSort' defaults everything but the 'FieldName' and the 'SortOrder' so
    that you can concisely describe the usual kind of 'SortSpec's you want.
-}
mkSort :: FieldName -> SortOrder -> DefaultSort
mkSort fieldName sOrder = DefaultSort fieldName sOrder Nothing Nothing Nothing Nothing

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
data SortSpec =
    DefaultSortSpec DefaultSort
  | GeoDistanceSortSpec SortOrder GeoPoint DistanceUnit
  deriving (Eq, Show)

instance ToJSON SortSpec where
  toJSON (DefaultSortSpec
          (DefaultSort (FieldName dsSortFieldName) dsSortOrder dsIgnoreUnmapped
           dsSortMode dsMissingSort dsNestedFilter)) =
    object [dsSortFieldName .= omitNulls base] where
      base = [ "order" .= dsSortOrder
             , "unmapped_type" .= dsIgnoreUnmapped
             , "mode" .= dsSortMode
             , "missing" .= dsMissingSort
             , "nested_filter" .= dsNestedFilter ]

  toJSON (GeoDistanceSortSpec gdsSortOrder (GeoPoint (FieldName field) gdsLatLon) units) =
    object [ "unit" .= units
           , field .= gdsLatLon
           , "order" .= gdsSortOrder ]

{-| 'DefaultSort' is usually the kind of 'SortSpec' you'll want. There's a
    'mkSort' convenience function for when you want to specify only the most
    common parameters.

    The `ignoreUnmapped`, when `Just` field is used to set the elastic 'unmapped_type'

<http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/search-request-sort.html#search-request-sort>
-}
data DefaultSort =
  DefaultSort { sortFieldName  :: FieldName
              , sortOrder      :: SortOrder
                                  -- default False
              , ignoreUnmapped :: Maybe Text
              , sortMode       :: Maybe SortMode
              , missingSort    :: Maybe Missing
              , nestedFilter   :: Maybe Filter } deriving (Eq, Show)

{-| 'SortOrder' is 'Ascending' or 'Descending', as you might expect. These get
    encoded into "asc" or "desc" when turned into JSON.

<http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/search-request-sort.html#search-request-sort>
-}
data SortOrder = Ascending
               | Descending deriving (Eq, Show)

instance ToJSON SortOrder where
  toJSON Ascending  = String "asc"
  toJSON Descending = String "desc"

{-| 'Missing' prescribes how to handle missing fields. A missing field can be
    sorted last, first, or using a custom value as a substitute.

<http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/search-request-sort.html#_missing_values>
-}
data Missing = LastMissing
             | FirstMissing
             | CustomMissing Text deriving (Eq, Show)

instance ToJSON Missing where
  toJSON LastMissing         = String "_last"
  toJSON FirstMissing        = String "_first"
  toJSON (CustomMissing txt) = String txt
