{-# LANGUAGE OverloadedStrings #-}

-- TODO Fuzziness ???
module Database.Bloodhound.Internal.Versions.Common.Types.Query.Fuzzy
  ( FuzzyQuery (..),
    FuzzyLikeFieldQuery (..),
    FuzzyLikeThisQuery (..),
  )
where

import Database.Bloodhound.Internal.Utils.Imports
import Database.Bloodhound.Internal.Versions.Common.Types.Newtypes
import Database.Bloodhound.Internal.Versions.Common.Types.Query.Commons
import GHC.Generics

data FuzzyQuery = FuzzyQuery
  { fuzzyQueryField :: FieldName,
    fuzzyQueryValue :: Text,
    fuzzyQueryPrefixLength :: PrefixLength,
    fuzzyQueryMaxExpansions :: MaxExpansions,
    fuzzyQueryFuzziness :: Fuzziness,
    fuzzyQueryBoost :: Maybe Boost
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON FuzzyQuery where
  toJSON
    ( FuzzyQuery
        (FieldName fieldName)
        queryText
        prefixLength
        maxEx
        fuzziness
        boost
      ) =
      object [fromText fieldName .= omitNulls base]
      where
        base =
          [ "value" .= queryText,
            "fuzziness" .= fuzziness,
            "prefix_length" .= prefixLength,
            "boost" .= boost,
            "max_expansions" .= maxEx
          ]

instance FromJSON FuzzyQuery where
  parseJSON = withObject "FuzzyQuery" parse
    where
      parse = fieldTagged $ \fn o ->
        FuzzyQuery fn
          <$> o .: "value"
          <*> o .: "prefix_length"
          <*> o .: "max_expansions"
          <*> o .: "fuzziness"
          <*> o .:? "boost"

data FuzzyLikeFieldQuery = FuzzyLikeFieldQuery
  { fuzzyLikeField :: FieldName,
    -- anaphora is good for the soul.
    fuzzyLikeFieldText :: Text,
    fuzzyLikeFieldMaxQueryTerms :: MaxQueryTerms,
    fuzzyLikeFieldIgnoreTermFrequency :: IgnoreTermFrequency,
    fuzzyLikeFieldFuzziness :: Fuzziness,
    fuzzyLikeFieldPrefixLength :: PrefixLength,
    fuzzyLikeFieldBoost :: Boost,
    fuzzyLikeFieldAnalyzer :: Maybe Analyzer
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON FuzzyLikeFieldQuery where
  toJSON
    ( FuzzyLikeFieldQuery
        (FieldName fieldName)
        fieldText
        maxTerms
        ignoreFreq
        fuzziness
        prefixLength
        boost
        analyzer
      ) =
      object
        [ fromText fieldName
            .= omitNulls
              [ "like_text" .= fieldText,
                "max_query_terms" .= maxTerms,
                "ignore_tf" .= ignoreFreq,
                "fuzziness" .= fuzziness,
                "prefix_length" .= prefixLength,
                "analyzer" .= analyzer,
                "boost" .= boost
              ]
        ]

instance FromJSON FuzzyLikeFieldQuery where
  parseJSON = withObject "FuzzyLikeFieldQuery" parse
    where
      parse = fieldTagged $ \fn o ->
        FuzzyLikeFieldQuery fn
          <$> o .: "like_text"
          <*> o .: "max_query_terms"
          <*> o .: "ignore_tf"
          <*> o .: "fuzziness"
          <*> o .: "prefix_length"
          <*> o .: "boost"
          <*> o .:? "analyzer"

data FuzzyLikeThisQuery = FuzzyLikeThisQuery
  { fuzzyLikeFields :: [FieldName],
    fuzzyLikeText :: Text,
    fuzzyLikeMaxQueryTerms :: MaxQueryTerms,
    fuzzyLikeIgnoreTermFrequency :: IgnoreTermFrequency,
    fuzzyLikeFuzziness :: Fuzziness,
    fuzzyLikePrefixLength :: PrefixLength,
    fuzzyLikeBoost :: Boost,
    fuzzyLikeAnalyzer :: Maybe Analyzer
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON FuzzyLikeThisQuery where
  toJSON
    ( FuzzyLikeThisQuery
        fields
        text
        maxTerms
        ignoreFreq
        fuzziness
        prefixLength
        boost
        analyzer
      ) =
      omitNulls base
      where
        base =
          [ "fields" .= fields,
            "like_text" .= text,
            "max_query_terms" .= maxTerms,
            "ignore_tf" .= ignoreFreq,
            "fuzziness" .= fuzziness,
            "prefix_length" .= prefixLength,
            "analyzer" .= analyzer,
            "boost" .= boost
          ]

instance FromJSON FuzzyLikeThisQuery where
  parseJSON = withObject "FuzzyLikeThisQuery" parse
    where
      parse o =
        FuzzyLikeThisQuery
          <$> o .:? "fields" .!= []
          <*> o .: "like_text"
          <*> o .: "max_query_terms"
          <*> o .: "ignore_tf"
          <*> o .: "fuzziness"
          <*> o .: "prefix_length"
          <*> o .: "boost"
          <*> o .:? "analyzer"
