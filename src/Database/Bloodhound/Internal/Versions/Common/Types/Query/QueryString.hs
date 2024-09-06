{-# LANGUAGE OverloadedStrings #-}

module Database.Bloodhound.Internal.Versions.Common.Types.Query.QueryString
  ( QueryStringQuery (..),
    mkQueryStringQuery,
  )
where

import Database.Bloodhound.Internal.Utils.Imports
import Database.Bloodhound.Internal.Versions.Common.Types.Newtypes
import Database.Bloodhound.Internal.Versions.Common.Types.Query.Commons
import GHC.Generics

-- use_dis_max and tie_breaker when fields are plural?
data QueryStringQuery = QueryStringQuery
  { queryStringQuery :: QueryString,
    queryStringDefaultField :: Maybe FieldName,
    queryStringOperator :: Maybe BooleanOperator,
    queryStringAnalyzer :: Maybe Analyzer,
    queryStringAllowLeadingWildcard :: Maybe AllowLeadingWildcard,
    queryStringLowercaseExpanded :: Maybe LowercaseExpanded,
    queryStringEnablePositionIncrements :: Maybe EnablePositionIncrements,
    queryStringFuzzyMaxExpansions :: Maybe MaxExpansions,
    queryStringFuzziness :: Maybe Fuzziness,
    queryStringFuzzyPrefixLength :: Maybe PrefixLength,
    queryStringPhraseSlop :: Maybe PhraseSlop,
    queryStringBoost :: Maybe Boost,
    queryStringAnalyzeWildcard :: Maybe AnalyzeWildcard,
    queryStringGeneratePhraseQueries :: Maybe GeneratePhraseQueries,
    queryStringMinimumShouldMatch :: Maybe MinimumMatch,
    queryStringLenient :: Maybe Lenient,
    queryStringLocale :: Maybe Locale
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON QueryStringQuery where
  toJSON
    ( QueryStringQuery
        qsQueryString
        qsDefaultField
        qsOperator
        qsAnalyzer
        qsAllowWildcard
        qsLowercaseExpanded
        qsEnablePositionIncrements
        qsFuzzyMaxExpansions
        qsFuzziness
        qsFuzzyPrefixLength
        qsPhraseSlop
        qsBoost
        qsAnalyzeWildcard
        qsGeneratePhraseQueries
        qsMinimumShouldMatch
        qsLenient
        qsLocale
      ) =
      omitNulls base
      where
        base =
          [ "query" .= qsQueryString,
            "default_field" .= qsDefaultField,
            "default_operator" .= qsOperator,
            "analyzer" .= qsAnalyzer,
            "allow_leading_wildcard" .= qsAllowWildcard,
            "lowercase_expanded_terms" .= qsLowercaseExpanded,
            "enable_position_increments" .= qsEnablePositionIncrements,
            "fuzzy_max_expansions" .= qsFuzzyMaxExpansions,
            "fuzziness" .= qsFuzziness,
            "fuzzy_prefix_length" .= qsFuzzyPrefixLength,
            "phrase_slop" .= qsPhraseSlop,
            "boost" .= qsBoost,
            "analyze_wildcard" .= qsAnalyzeWildcard,
            "auto_generate_phrase_queries" .= qsGeneratePhraseQueries,
            "minimum_should_match" .= qsMinimumShouldMatch,
            "lenient" .= qsLenient,
            "locale" .= qsLocale
          ]

instance FromJSON QueryStringQuery where
  parseJSON = withObject "QueryStringQuery" parse
    where
      parse o =
        QueryStringQuery
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

mkQueryStringQuery :: QueryString -> QueryStringQuery
mkQueryStringQuery qs =
  QueryStringQuery
    qs
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
