{-# LANGUAGE OverloadedStrings #-}

module Database.Bloodhound.Internal.Versions.Common.Types.Query.Match
  ( MatchQuery (..),
    MatchQueryType (..),
    MultiMatchQuery (..),
    MultiMatchQueryType (..),
    mkMatchQuery,
    mkMultiMatchQuery,

    -- * Optics
    matchQueryFieldLens,
    matchQueryQueryStringLens,
    matchQueryOperatorLens,
    matchQueryZeroTermsLens,
    matchQueryCutoffFrequencyLens,
    matchQueryMatchTypeLens,
    matchQueryAnalyzerLens,
    matchQueryMaxExpansionsLens,
    matchQueryLenientLens,
    matchQueryBoostLens,
    matchQueryMinimumShouldMatchLens,
    matchQueryFuzzinessLens,
    multiMatchQueryFieldsLens,
    multiMatchQueryStringLens,
    multiMatchQueryOperatorLens,
    multiMatchQueryZeroTermsLens,
    multiMatchQueryTiebreakerLens,
    multiMatchQueryTypeLens,
    multiMatchQueryCutoffFrequencyLens,
    multiMatchQueryAnalyzerLens,
    multiMatchQueryMaxExpansionsLens,
    multiMatchQueryLenientLens,
  )
where

import Database.Bloodhound.Internal.Utils.Imports
import Database.Bloodhound.Internal.Versions.Common.Types.Newtypes
import Database.Bloodhound.Internal.Versions.Common.Types.Query.Commons
import GHC.Generics

data MatchQuery = MatchQuery
  { matchQueryField :: FieldName,
    matchQueryQueryString :: QueryString,
    matchQueryOperator :: BooleanOperator,
    matchQueryZeroTerms :: ZeroTermsQuery,
    matchQueryCutoffFrequency :: Maybe CutoffFrequency,
    matchQueryMatchType :: Maybe MatchQueryType,
    matchQueryAnalyzer :: Maybe Analyzer,
    matchQueryMaxExpansions :: Maybe MaxExpansions,
    matchQueryLenient :: Maybe Lenient,
    matchQueryBoost :: Maybe Boost,
    matchQueryMinimumShouldMatch :: Maybe Text,
    matchQueryFuzziness :: Maybe Fuzziness
  }
  deriving stock (Eq, Show, Generic)

matchQueryFieldLens :: Lens' MatchQuery FieldName
matchQueryFieldLens = lens matchQueryField (\x y -> x {matchQueryField = y})

matchQueryQueryStringLens :: Lens' MatchQuery QueryString
matchQueryQueryStringLens = lens matchQueryQueryString (\x y -> x {matchQueryQueryString = y})

matchQueryOperatorLens :: Lens' MatchQuery BooleanOperator
matchQueryOperatorLens = lens matchQueryOperator (\x y -> x {matchQueryOperator = y})

matchQueryZeroTermsLens :: Lens' MatchQuery ZeroTermsQuery
matchQueryZeroTermsLens = lens matchQueryZeroTerms (\x y -> x {matchQueryZeroTerms = y})

matchQueryCutoffFrequencyLens :: Lens' MatchQuery (Maybe CutoffFrequency)
matchQueryCutoffFrequencyLens = lens matchQueryCutoffFrequency (\x y -> x {matchQueryCutoffFrequency = y})

matchQueryMatchTypeLens :: Lens' MatchQuery (Maybe MatchQueryType)
matchQueryMatchTypeLens = lens matchQueryMatchType (\x y -> x {matchQueryMatchType = y})

matchQueryAnalyzerLens :: Lens' MatchQuery (Maybe Analyzer)
matchQueryAnalyzerLens = lens matchQueryAnalyzer (\x y -> x {matchQueryAnalyzer = y})

matchQueryMaxExpansionsLens :: Lens' MatchQuery (Maybe MaxExpansions)
matchQueryMaxExpansionsLens = lens matchQueryMaxExpansions (\x y -> x {matchQueryMaxExpansions = y})

matchQueryLenientLens :: Lens' MatchQuery (Maybe Lenient)
matchQueryLenientLens = lens matchQueryLenient (\x y -> x {matchQueryLenient = y})

matchQueryBoostLens :: Lens' MatchQuery (Maybe Boost)
matchQueryBoostLens = lens matchQueryBoost (\x y -> x {matchQueryBoost = y})

matchQueryMinimumShouldMatchLens :: Lens' MatchQuery (Maybe Text)
matchQueryMinimumShouldMatchLens = lens matchQueryMinimumShouldMatch (\x y -> x {matchQueryMinimumShouldMatch = y})

matchQueryFuzzinessLens :: Lens' MatchQuery (Maybe Fuzziness)
matchQueryFuzzinessLens = lens matchQueryFuzziness (\x y -> x {matchQueryFuzziness = y})

instance ToJSON MatchQuery where
  toJSON
    ( MatchQuery
        (FieldName fieldName)
        (QueryString mqQueryString)
        booleanOperator
        zeroTermsQuery
        cutoffFrequency
        matchQueryType
        analyzer
        maxExpansions
        lenient
        boost
        minShouldMatch
        mqFuzziness
      ) =
      object [fromText fieldName .= omitNulls base]
      where
        base =
          [ "query" .= mqQueryString,
            "operator" .= booleanOperator,
            "zero_terms_query" .= zeroTermsQuery,
            "cutoff_frequency" .= cutoffFrequency,
            "type" .= matchQueryType,
            "analyzer" .= analyzer,
            "max_expansions" .= maxExpansions,
            "lenient" .= lenient,
            "boost" .= boost,
            "minimum_should_match" .= minShouldMatch,
            "fuzziness" .= mqFuzziness
          ]

instance FromJSON MatchQuery where
  parseJSON = withObject "MatchQuery" parse
    where
      parse = fieldTagged $ \fn o ->
        MatchQuery fn
          <$> o .: "query"
          <*> o .: "operator"
          <*> o .: "zero_terms_query"
          <*> o .:? "cutoff_frequency"
          <*> o .:? "type"
          <*> o .:? "analyzer"
          <*> o .:? "max_expansions"
          <*> o .:? "lenient"
          <*> o .:? "boost"
          <*> o .:? "minimum_should_match"
          <*> o .:? "fuzziness"

-- | 'mkMatchQuery' is a convenience function that defaults the less common parameters,
--   enabling you to provide only the 'FieldName' and 'QueryString' to make a 'MatchQuery'
mkMatchQuery :: FieldName -> QueryString -> MatchQuery
mkMatchQuery field query = MatchQuery field query Or ZeroTermsNone Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data MatchQueryType
  = MatchPhrase
  | MatchPhrasePrefix
  deriving stock (Eq, Show, Generic)

instance ToJSON MatchQueryType where
  toJSON MatchPhrase = "phrase"
  toJSON MatchPhrasePrefix = "phrase_prefix"

instance FromJSON MatchQueryType where
  parseJSON = withText "MatchQueryType" parse
    where
      parse "phrase" = pure MatchPhrase
      parse "phrase_prefix" = pure MatchPhrasePrefix
      parse t = fail ("Unexpected MatchQueryType: " <> show t)

data MultiMatchQuery = MultiMatchQuery
  { multiMatchQueryFields :: [FieldName],
    multiMatchQueryString :: QueryString,
    multiMatchQueryOperator :: BooleanOperator,
    multiMatchQueryZeroTerms :: ZeroTermsQuery,
    multiMatchQueryTiebreaker :: Maybe Tiebreaker,
    multiMatchQueryType :: Maybe MultiMatchQueryType,
    multiMatchQueryCutoffFrequency :: Maybe CutoffFrequency,
    multiMatchQueryAnalyzer :: Maybe Analyzer,
    multiMatchQueryMaxExpansions :: Maybe MaxExpansions,
    multiMatchQueryLenient :: Maybe Lenient
  }
  deriving stock (Eq, Show, Generic)

multiMatchQueryFieldsLens :: Lens' MultiMatchQuery [FieldName]
multiMatchQueryFieldsLens = lens multiMatchQueryFields (\x y -> x {multiMatchQueryFields = y})

multiMatchQueryStringLens :: Lens' MultiMatchQuery QueryString
multiMatchQueryStringLens = lens multiMatchQueryString (\x y -> x {multiMatchQueryString = y})

multiMatchQueryOperatorLens :: Lens' MultiMatchQuery BooleanOperator
multiMatchQueryOperatorLens = lens multiMatchQueryOperator (\x y -> x {multiMatchQueryOperator = y})

multiMatchQueryZeroTermsLens :: Lens' MultiMatchQuery ZeroTermsQuery
multiMatchQueryZeroTermsLens = lens multiMatchQueryZeroTerms (\x y -> x {multiMatchQueryZeroTerms = y})

multiMatchQueryTiebreakerLens :: Lens' MultiMatchQuery (Maybe Tiebreaker)
multiMatchQueryTiebreakerLens = lens multiMatchQueryTiebreaker (\x y -> x {multiMatchQueryTiebreaker = y})

multiMatchQueryTypeLens :: Lens' MultiMatchQuery (Maybe MultiMatchQueryType)
multiMatchQueryTypeLens = lens multiMatchQueryType (\x y -> x {multiMatchQueryType = y})

multiMatchQueryCutoffFrequencyLens :: Lens' MultiMatchQuery (Maybe CutoffFrequency)
multiMatchQueryCutoffFrequencyLens = lens multiMatchQueryCutoffFrequency (\x y -> x {multiMatchQueryCutoffFrequency = y})

multiMatchQueryAnalyzerLens :: Lens' MultiMatchQuery (Maybe Analyzer)
multiMatchQueryAnalyzerLens = lens multiMatchQueryAnalyzer (\x y -> x {multiMatchQueryAnalyzer = y})

multiMatchQueryMaxExpansionsLens :: Lens' MultiMatchQuery (Maybe MaxExpansions)
multiMatchQueryMaxExpansionsLens = lens multiMatchQueryMaxExpansions (\x y -> x {multiMatchQueryMaxExpansions = y})

multiMatchQueryLenientLens :: Lens' MultiMatchQuery (Maybe Lenient)
multiMatchQueryLenientLens = lens multiMatchQueryLenient (\x y -> x {multiMatchQueryLenient = y})

instance ToJSON MultiMatchQuery where
  toJSON
    ( MultiMatchQuery
        fields
        (QueryString query)
        boolOp
        ztQ
        tb
        mmqt
        cf
        analyzer
        maxEx
        lenient
      ) =
      object ["multi_match" .= omitNulls base]
      where
        base =
          [ "fields" .= fmap toJSON fields,
            "query" .= query,
            "operator" .= boolOp,
            "zero_terms_query" .= ztQ,
            "tie_breaker" .= tb,
            "type" .= mmqt,
            "cutoff_frequency" .= cf,
            "analyzer" .= analyzer,
            "max_expansions" .= maxEx,
            "lenient" .= lenient
          ]

instance FromJSON MultiMatchQuery where
  parseJSON = withObject "MultiMatchQuery" parse
    where
      parse raw = do
        o <- raw .: "multi_match"
        MultiMatchQuery
          <$> o .:? "fields" .!= []
          <*> o .: "query"
          <*> o .: "operator"
          <*> o .: "zero_terms_query"
          <*> o .:? "tie_breaker"
          <*> o .:? "type"
          <*> o .:? "cutoff_frequency"
          <*> o .:? "analyzer"
          <*> o .:? "max_expansions"
          <*> o .:? "lenient"

-- | 'mkMultiMatchQuery' is a convenience function that defaults the less common parameters,
--   enabling you to provide only the list of 'FieldName's and 'QueryString' to
--   make a 'MultiMatchQuery'.
mkMultiMatchQuery :: [FieldName] -> QueryString -> MultiMatchQuery
mkMultiMatchQuery matchFields query =
  MultiMatchQuery
    matchFields
    query
    Or
    ZeroTermsNone
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing

data MultiMatchQueryType
  = MultiMatchBestFields
  | MultiMatchMostFields
  | MultiMatchCrossFields
  | MultiMatchPhrase
  | MultiMatchPhrasePrefix
  deriving stock (Eq, Show, Generic)

instance ToJSON MultiMatchQueryType where
  toJSON MultiMatchBestFields = "best_fields"
  toJSON MultiMatchMostFields = "most_fields"
  toJSON MultiMatchCrossFields = "cross_fields"
  toJSON MultiMatchPhrase = "phrase"
  toJSON MultiMatchPhrasePrefix = "phrase_prefix"

instance FromJSON MultiMatchQueryType where
  parseJSON = withText "MultiMatchPhrasePrefix" parse
    where
      parse "best_fields" = pure MultiMatchBestFields
      parse "most_fields" = pure MultiMatchMostFields
      parse "cross_fields" = pure MultiMatchCrossFields
      parse "phrase" = pure MultiMatchPhrase
      parse "phrase_prefix" = pure MultiMatchPhrasePrefix
      parse t = fail ("Unexpected MultiMatchPhrasePrefix: " <> show t)
