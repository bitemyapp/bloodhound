{-# LANGUAGE OverloadedStrings #-}

module Database.Bloodhound.Internal.Versions.Common.Types.Query.Fuzzy
  ( FuzzyQuery (..),
    FuzzyLikeFieldQuery (..),
    FuzzyLikeThisQuery (..),

    -- * Optics
    fuzzyQueryFieldLens,
    fuzzyQueryValueLens,
    fuzzyQueryPrefixLengthLens,
    fuzzyQueryMaxExpansionsLens,
    fuzzyQueryFuzzinessLens,
    fuzzyQueryBoostLens,
    fuzzyLikeFieldQueryFieldLens,
    fuzzyLikeFieldQueryTextLens,
    fuzzyLikeFieldQueryMaxQueryTermsLens,
    fuzzyLikeFieldQueryIgnoreTermFrequencyLens,
    fuzzyLikeFieldQueryFuzzinessLens,
    fuzzyLikeFieldQueryPrefixLengthLens,
    fuzzyLikeFieldQueryBoostLens,
    fuzzyLikeFieldQueryAnalyzerLens,
    fuzzyLikeThisQueryFieldsLens,
    fuzzyLikeThisQueryTextLens,
    fuzzyLikeThisQueryMaxQueryTermsLens,
    fuzzyLikeThisQueryIgnoreTermFrequencyLens,
    fuzzyLikeThisQueryFuzzinessLens,
    fuzzyLikeThisQueryPrefixLengthLens,
    fuzzyLikeThisQueryBoostLens,
    fuzzyLikeThisQueryAnalyzerLens,
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

fuzzyQueryFieldLens :: Lens' FuzzyQuery FieldName
fuzzyQueryFieldLens = lens fuzzyQueryField (\x y -> x {fuzzyQueryField = y})

fuzzyQueryValueLens :: Lens' FuzzyQuery Text
fuzzyQueryValueLens = lens fuzzyQueryValue (\x y -> x {fuzzyQueryValue = y})

fuzzyQueryPrefixLengthLens :: Lens' FuzzyQuery PrefixLength
fuzzyQueryPrefixLengthLens = lens fuzzyQueryPrefixLength (\x y -> x {fuzzyQueryPrefixLength = y})

fuzzyQueryMaxExpansionsLens :: Lens' FuzzyQuery MaxExpansions
fuzzyQueryMaxExpansionsLens = lens fuzzyQueryMaxExpansions (\x y -> x {fuzzyQueryMaxExpansions = y})

fuzzyQueryFuzzinessLens :: Lens' FuzzyQuery Fuzziness
fuzzyQueryFuzzinessLens = lens fuzzyQueryFuzziness (\x y -> x {fuzzyQueryFuzziness = y})

fuzzyQueryBoostLens :: Lens' FuzzyQuery (Maybe Boost)
fuzzyQueryBoostLens = lens fuzzyQueryBoost (\x y -> x {fuzzyQueryBoost = y})

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

fuzzyLikeFieldQueryFieldLens :: Lens' FuzzyLikeFieldQuery FieldName
fuzzyLikeFieldQueryFieldLens = lens fuzzyLikeField (\x y -> x {fuzzyLikeField = y})

fuzzyLikeFieldQueryTextLens :: Lens' FuzzyLikeFieldQuery Text
fuzzyLikeFieldQueryTextLens = lens fuzzyLikeFieldText (\x y -> x {fuzzyLikeFieldText = y})

fuzzyLikeFieldQueryMaxQueryTermsLens :: Lens' FuzzyLikeFieldQuery MaxQueryTerms
fuzzyLikeFieldQueryMaxQueryTermsLens = lens fuzzyLikeFieldMaxQueryTerms (\x y -> x {fuzzyLikeFieldMaxQueryTerms = y})

fuzzyLikeFieldQueryIgnoreTermFrequencyLens :: Lens' FuzzyLikeFieldQuery IgnoreTermFrequency
fuzzyLikeFieldQueryIgnoreTermFrequencyLens = lens fuzzyLikeFieldIgnoreTermFrequency (\x y -> x {fuzzyLikeFieldIgnoreTermFrequency = y})

fuzzyLikeFieldQueryFuzzinessLens :: Lens' FuzzyLikeFieldQuery Fuzziness
fuzzyLikeFieldQueryFuzzinessLens = lens fuzzyLikeFieldFuzziness (\x y -> x {fuzzyLikeFieldFuzziness = y})

fuzzyLikeFieldQueryPrefixLengthLens :: Lens' FuzzyLikeFieldQuery PrefixLength
fuzzyLikeFieldQueryPrefixLengthLens = lens fuzzyLikeFieldPrefixLength (\x y -> x {fuzzyLikeFieldPrefixLength = y})

fuzzyLikeFieldQueryBoostLens :: Lens' FuzzyLikeFieldQuery Boost
fuzzyLikeFieldQueryBoostLens = lens fuzzyLikeFieldBoost (\x y -> x {fuzzyLikeFieldBoost = y})

fuzzyLikeFieldQueryAnalyzerLens :: Lens' FuzzyLikeFieldQuery (Maybe Analyzer)
fuzzyLikeFieldQueryAnalyzerLens = lens fuzzyLikeFieldAnalyzer (\x y -> x {fuzzyLikeFieldAnalyzer = y})

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

fuzzyLikeThisQueryFieldsLens :: Lens' FuzzyLikeThisQuery [FieldName]
fuzzyLikeThisQueryFieldsLens = lens fuzzyLikeFields (\x y -> x {fuzzyLikeFields = y})

fuzzyLikeThisQueryTextLens :: Lens' FuzzyLikeThisQuery Text
fuzzyLikeThisQueryTextLens = lens fuzzyLikeText (\x y -> x {fuzzyLikeText = y})

fuzzyLikeThisQueryMaxQueryTermsLens :: Lens' FuzzyLikeThisQuery MaxQueryTerms
fuzzyLikeThisQueryMaxQueryTermsLens = lens fuzzyLikeMaxQueryTerms (\x y -> x {fuzzyLikeMaxQueryTerms = y})

fuzzyLikeThisQueryIgnoreTermFrequencyLens :: Lens' FuzzyLikeThisQuery IgnoreTermFrequency
fuzzyLikeThisQueryIgnoreTermFrequencyLens = lens fuzzyLikeIgnoreTermFrequency (\x y -> x {fuzzyLikeIgnoreTermFrequency = y})

fuzzyLikeThisQueryFuzzinessLens :: Lens' FuzzyLikeThisQuery Fuzziness
fuzzyLikeThisQueryFuzzinessLens = lens fuzzyLikeFuzziness (\x y -> x {fuzzyLikeFuzziness = y})

fuzzyLikeThisQueryPrefixLengthLens :: Lens' FuzzyLikeThisQuery PrefixLength
fuzzyLikeThisQueryPrefixLengthLens = lens fuzzyLikePrefixLength (\x y -> x {fuzzyLikePrefixLength = y})

fuzzyLikeThisQueryBoostLens :: Lens' FuzzyLikeThisQuery Boost
fuzzyLikeThisQueryBoostLens = lens fuzzyLikeBoost (\x y -> x {fuzzyLikeBoost = y})

fuzzyLikeThisQueryAnalyzerLens :: Lens' FuzzyLikeThisQuery (Maybe Analyzer)
fuzzyLikeThisQueryAnalyzerLens = lens fuzzyLikeAnalyzer (\x y -> x {fuzzyLikeAnalyzer = y})

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
