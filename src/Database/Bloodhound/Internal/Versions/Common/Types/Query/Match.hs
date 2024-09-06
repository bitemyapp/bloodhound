{-# LANGUAGE OverloadedStrings #-}

module Database.Bloodhound.Internal.Versions.Common.Types.Query.Match
  ( MatchQuery (..),
    MatchQueryType (..),
    MultiMatchQuery (..),
    MultiMatchQueryType (..),
    mkMatchQuery,
    mkMultiMatchQuery,
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
--    enabling you to provide only the 'FieldName' and 'QueryString' to make a 'MatchQuery'
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
--    enabling you to provide only the list of 'FieldName's and 'QueryString' to
--    make a 'MultiMatchQuery'.
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
