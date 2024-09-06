{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Bloodhound.Internal.Versions.Common.Types.Search
  ( Exclude (..),
    ExpandWildcards (..),
    GetTemplateScript (..),
    Include (..),
    Pattern (..),
    PatternOrPatterns (..),
    ScrollId (..),
    Search (..),
    SearchResult (..),
    SearchTemplate (..),
    SearchTemplateId (..),
    SearchTemplateSource (..),
    SearchType (..),
    Source (..),
    TimeUnits (..),
    TrackSortScores,
    unpackId,

    -- * Optics
    tookLens,
    timedOutLens,
    shardsLens,
    searchHitsLens,
    aggregationsLens,
    scrollIdLens,
    suggestLens,
    pitIdLens,
    getTemplateScriptLangLens,
    getTemplateScriptSourceLens,
    getTemplateScriptOptionsLens,
    getTemplateScriptIdLens,
    getTemplateScriptFoundLens,
  )
where

import qualified Data.HashMap.Strict as HM
import Database.Bloodhound.Client.Cluster
import Database.Bloodhound.Internal.Utils.Imports
import Database.Bloodhound.Internal.Versions.Common.Types.Aggregation
import Database.Bloodhound.Internal.Versions.Common.Types.Highlight
import Database.Bloodhound.Internal.Versions.Common.Types.Newtypes
import Database.Bloodhound.Internal.Versions.Common.Types.PointInTime
import Database.Bloodhound.Internal.Versions.Common.Types.Query
import Database.Bloodhound.Internal.Versions.Common.Types.Sort
import Database.Bloodhound.Internal.Versions.Common.Types.Suggest

-- | 'unpackId' is a silly convenience function that gets used once.
unpackId :: DocId -> Text
unpackId (DocId docId) = docId

type TrackSortScores = Bool

data Search = Search
  { queryBody :: Maybe Query,
    filterBody :: Maybe Filter,
    sortBody :: Maybe Sort,
    aggBody :: Maybe Aggregations,
    highlight :: Maybe Highlights,
    -- default False
    trackSortScores :: TrackSortScores,
    from :: From,
    size :: Size,
    searchType :: SearchType,
    searchAfterKey :: Maybe SearchAfterKey,
    fields :: Maybe [FieldName],
    scriptFields :: Maybe ScriptFields,
    source :: Maybe Source,
    -- | Only one Suggestion request / response per Search is supported.
    suggestBody :: Maybe Suggest,
    pointInTime :: Maybe PointInTime
  }
  deriving stock (Eq, Show)

instance ToJSON Search where
  toJSON
    ( Search
        mquery
        sFilter
        sort
        searchAggs
        highlight
        sTrackSortScores
        sFrom
        sSize
        _
        sAfter
        sFields
        sScriptFields
        sSource
        sSuggest
        pPointInTime
      ) =
      omitNulls
        [ "query" .= query',
          "sort" .= sort,
          "aggregations" .= searchAggs,
          "highlight" .= highlight,
          "from" .= sFrom,
          "size" .= sSize,
          "track_scores" .= sTrackSortScores,
          "search_after" .= sAfter,
          "fields" .= sFields,
          "script_fields" .= sScriptFields,
          "_source" .= sSource,
          "suggest" .= sSuggest,
          "pit" .= pPointInTime
        ]
      where
        query' = case sFilter of
          Nothing -> mquery
          Just x ->
            Just
              . QueryBoolQuery
              $ mkBoolQuery
                (maybeToList mquery)
                [x]
                []
                []

data SearchType
  = SearchTypeQueryThenFetch
  | SearchTypeDfsQueryThenFetch
  deriving stock (Eq, Show)

instance ToJSON SearchType where
  toJSON SearchTypeQueryThenFetch = String "query_then_fetch"
  toJSON SearchTypeDfsQueryThenFetch = String "dfs_query_then_fetch"

instance FromJSON SearchType where
  parseJSON (String "query_then_fetch") = pure $ SearchTypeQueryThenFetch
  parseJSON (String "dfs_query_then_fetch") = pure $ SearchTypeDfsQueryThenFetch
  parseJSON _ = empty

data Source
  = NoSource
  | SourcePatterns PatternOrPatterns
  | SourceIncludeExclude Include Exclude
  deriving stock (Eq, Show)

instance ToJSON Source where
  toJSON NoSource = toJSON False
  toJSON (SourcePatterns patterns) = toJSON patterns
  toJSON (SourceIncludeExclude incl excl) = object ["includes" .= incl, "excludes" .= excl]

data PatternOrPatterns
  = PopPattern Pattern
  | PopPatterns [Pattern]
  deriving stock (Eq, Read, Show)

instance ToJSON PatternOrPatterns where
  toJSON (PopPattern pattern) = toJSON pattern
  toJSON (PopPatterns patterns) = toJSON patterns

data Include = Include [Pattern] deriving stock (Eq, Read, Show)

data Exclude = Exclude [Pattern] deriving stock (Eq, Read, Show)

instance ToJSON Include where
  toJSON (Include patterns) = toJSON patterns

instance ToJSON Exclude where
  toJSON (Exclude patterns) = toJSON patterns

newtype Pattern = Pattern Text deriving stock (Eq, Read, Show)

instance ToJSON Pattern where
  toJSON (Pattern pattern) = toJSON pattern

data SearchResult a = SearchResult
  { took :: Int,
    timedOut :: Bool,
    shards :: ShardResult,
    searchHits :: SearchHits a,
    aggregations :: Maybe AggregationResults,
    -- | Only one Suggestion request / response per
    --   Search is supported.
    scrollId :: Maybe ScrollId,
    suggest :: Maybe NamedSuggestionResponse,
    pitId :: Maybe Text
  }
  deriving stock (Eq, Show)

instance (FromJSON a) => FromJSON (SearchResult a) where
  parseJSON (Object v) =
    SearchResult
      <$> v
        .: "took"
      <*> v
        .: "timed_out"
      <*> v
        .: "_shards"
      <*> v
        .: "hits"
      <*> v
        .:? "aggregations"
      <*> v
        .:? "_scroll_id"
      <*> v
        .:? "suggest"
      <*> v
        .:? "pit_id"
  parseJSON _ = empty

tookLens :: Lens' (SearchResult a) Int
tookLens = lens took (\x y -> x {took = y})

timedOutLens :: Lens' (SearchResult a) Bool
timedOutLens = lens timedOut (\x y -> x {timedOut = y})

shardsLens :: Lens' (SearchResult a) ShardResult
shardsLens = lens shards (\x y -> x {shards = y})

searchHitsLens :: Lens' (SearchResult a) (SearchHits a)
searchHitsLens = lens searchHits (\x y -> x {searchHits = y})

aggregationsLens :: Lens' (SearchResult a) (Maybe AggregationResults)
aggregationsLens = lens aggregations (\x y -> x {aggregations = y})

scrollIdLens :: Lens' (SearchResult a) (Maybe ScrollId)
scrollIdLens = lens scrollId (\x y -> x {scrollId = y})

suggestLens :: Lens' (SearchResult a) (Maybe NamedSuggestionResponse)
suggestLens = lens suggest (\x y -> x {suggest = y})

pitIdLens :: Lens' (SearchResult a) (Maybe Text)
pitIdLens = lens pitId (\x y -> x {pitId = y})

newtype ScrollId
  = ScrollId Text
  deriving newtype (Eq, Ord, Show, ToJSON, FromJSON)

newtype SearchTemplateId = SearchTemplateId Text deriving stock (Eq, Show)

instance ToJSON SearchTemplateId where
  toJSON (SearchTemplateId x) = toJSON x

newtype SearchTemplateSource = SearchTemplateSource Text deriving stock (Eq, Show)

instance ToJSON SearchTemplateSource where
  toJSON (SearchTemplateSource x) = toJSON x

instance FromJSON SearchTemplateSource where
  parseJSON (String s) = pure $ SearchTemplateSource s
  parseJSON _ = empty

data ExpandWildcards
  = ExpandWildcardsAll
  | ExpandWildcardsOpen
  | ExpandWildcardsClosed
  | ExpandWildcardsNone
  deriving stock (Eq, Show)

instance ToJSON ExpandWildcards where
  toJSON ExpandWildcardsAll = String "all"
  toJSON ExpandWildcardsOpen = String "open"
  toJSON ExpandWildcardsClosed = String "closed"
  toJSON ExpandWildcardsNone = String "none"

instance FromJSON ExpandWildcards where
  parseJSON (String "all") = pure $ ExpandWildcardsAll
  parseJSON (String "open") = pure $ ExpandWildcardsOpen
  parseJSON (String "closed") = pure $ ExpandWildcardsClosed
  parseJSON (String "none") = pure $ ExpandWildcardsNone
  parseJSON _ = empty

data TimeUnits
  = TimeUnitDays
  | TimeUnitHours
  | TimeUnitMinutes
  | TimeUnitSeconds
  | TimeUnitMilliseconds
  | TimeUnitMicroseconds
  | TimeUnitNanoseconds
  deriving stock (Eq, Show)

instance ToJSON TimeUnits where
  toJSON TimeUnitDays = String "d"
  toJSON TimeUnitHours = String "h"
  toJSON TimeUnitMinutes = String "m"
  toJSON TimeUnitSeconds = String "s"
  toJSON TimeUnitMilliseconds = String "ms"
  toJSON TimeUnitMicroseconds = String "micros"
  toJSON TimeUnitNanoseconds = String "nanos"

instance FromJSON TimeUnits where
  parseJSON (String "d") = pure $ TimeUnitDays
  parseJSON (String "h") = pure $ TimeUnitHours
  parseJSON (String "m") = pure $ TimeUnitMinutes
  parseJSON (String "s") = pure $ TimeUnitSeconds
  parseJSON (String "ms") = pure $ TimeUnitMilliseconds
  parseJSON (String "micros") = pure $ TimeUnitMicroseconds
  parseJSON (String "nanos") = pure $ TimeUnitNanoseconds
  parseJSON _ = empty

data SearchTemplate = SearchTemplate
  { searchTemplate :: Either SearchTemplateId SearchTemplateSource,
    params :: TemplateQueryKeyValuePairs,
    explainSearchTemplate :: Maybe Bool,
    profileSearchTemplate :: Maybe Bool
  }
  deriving stock (Eq, Show)

instance ToJSON SearchTemplate where
  toJSON SearchTemplate {..} =
    omitNulls
      [ either ("id" .=) ("source" .=) searchTemplate,
        "params" .= params,
        "explain" .= explainSearchTemplate,
        "profile" .= profileSearchTemplate
      ]

data GetTemplateScript = GetTemplateScript
  { getTemplateScriptLang :: Maybe Text,
    getTemplateScriptSource :: Maybe SearchTemplateSource,
    getTemplateScriptOptions :: Maybe (HM.HashMap Text Text),
    getTemplateScriptId :: Text,
    getTemplateScriptFound :: Bool
  }
  deriving stock (Eq, Show)

instance FromJSON GetTemplateScript where
  parseJSON (Object v) = do
    script <- v .:? "script"
    maybe
      (GetTemplateScript Nothing Nothing Nothing <$> v .: "_id" <*> v .: "found")
      ( \s ->
          GetTemplateScript
            <$> s
              .:? "lang"
            <*> s
              .:? "source"
            <*> s
              .:? "options"
            <*> v
              .: "_id"
            <*> v
              .: "found"
      )
      script
  parseJSON _ = empty

getTemplateScriptLangLens :: Lens' GetTemplateScript (Maybe Text)
getTemplateScriptLangLens = lens getTemplateScriptLang (\x y -> x {getTemplateScriptLang = y})

getTemplateScriptSourceLens :: Lens' GetTemplateScript (Maybe SearchTemplateSource)
getTemplateScriptSourceLens = lens getTemplateScriptSource (\x y -> x {getTemplateScriptSource = y})

getTemplateScriptOptionsLens :: Lens' GetTemplateScript (Maybe (HM.HashMap Text Text))
getTemplateScriptOptionsLens = lens getTemplateScriptOptions (\x y -> x {getTemplateScriptOptions = y})

getTemplateScriptIdLens :: Lens' GetTemplateScript Text
getTemplateScriptIdLens = lens getTemplateScriptId (\x y -> x {getTemplateScriptId = y})

getTemplateScriptFoundLens :: Lens' GetTemplateScript Bool
getTemplateScriptFoundLens = lens getTemplateScriptFound (\x y -> x {getTemplateScriptFound = y})
