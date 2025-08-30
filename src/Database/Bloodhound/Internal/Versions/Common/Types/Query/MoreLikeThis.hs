{-# LANGUAGE OverloadedStrings #-}

module Database.Bloodhound.Internal.Versions.Common.Types.Query.MoreLikeThis
  ( MoreLikeThisQuery (..),

    -- * Optics
    moreLikeThisQueryTextLens,
    moreLikeThisQueryFieldsLens,
    moreLikeThisQueryPercentMatchLens,
    moreLikeThisQueryMinimumTermFreqLens,
    moreLikeThisQueryMaxQueryTermsLens,
    moreLikeThisQueryStopWordsLens,
    moreLikeThisQueryMinDocFrequencyLens,
    moreLikeThisQueryMaxDocFrequencyLens,
    moreLikeThisQueryMinWordLengthLens,
    moreLikeThisQueryMaxWordLengthLens,
    moreLikeThisQueryBoostTermsLens,
    moreLikeThisQueryBoostLens,
    moreLikeThisQueryAnalyzerLens,
  )
where

import Database.Bloodhound.Internal.Utils.Imports
import Database.Bloodhound.Internal.Versions.Common.Types.Newtypes
import GHC.Generics

data MoreLikeThisQuery = MoreLikeThisQuery
  { moreLikeThisText :: Text,
    moreLikeThisFields :: Maybe (NonEmpty FieldName),
    -- default 0.3 (30%)
    moreLikeThisPercentMatch :: Maybe PercentMatch,
    moreLikeThisMinimumTermFreq :: Maybe MinimumTermFrequency,
    moreLikeThisMaxQueryTerms :: Maybe MaxQueryTerms,
    moreLikeThisStopWords :: Maybe (NonEmpty StopWord),
    moreLikeThisMinDocFrequency :: Maybe MinDocFrequency,
    moreLikeThisMaxDocFrequency :: Maybe MaxDocFrequency,
    moreLikeThisMinWordLength :: Maybe MinWordLength,
    moreLikeThisMaxWordLength :: Maybe MaxWordLength,
    moreLikeThisBoostTerms :: Maybe BoostTerms,
    moreLikeThisBoost :: Maybe Boost,
    moreLikeThisAnalyzer :: Maybe Analyzer
  }
  deriving stock (Eq, Show, Generic)

moreLikeThisQueryTextLens :: Lens' MoreLikeThisQuery (Text)
moreLikeThisQueryTextLens = lens moreLikeThisText (\x y -> x {moreLikeThisText = y})

moreLikeThisQueryFieldsLens :: Lens' MoreLikeThisQuery (Maybe (NonEmpty FieldName))
moreLikeThisQueryFieldsLens = lens moreLikeThisFields (\x y -> x {moreLikeThisFields = y})

moreLikeThisQueryPercentMatchLens :: Lens' MoreLikeThisQuery (Maybe PercentMatch)
moreLikeThisQueryPercentMatchLens = lens moreLikeThisPercentMatch (\x y -> x {moreLikeThisPercentMatch = y})

moreLikeThisQueryMinimumTermFreqLens :: Lens' MoreLikeThisQuery (Maybe MinimumTermFrequency)
moreLikeThisQueryMinimumTermFreqLens = lens moreLikeThisMinimumTermFreq (\x y -> x {moreLikeThisMinimumTermFreq = y})

moreLikeThisQueryMaxQueryTermsLens :: Lens' MoreLikeThisQuery (Maybe MaxQueryTerms)
moreLikeThisQueryMaxQueryTermsLens = lens moreLikeThisMaxQueryTerms (\x y -> x {moreLikeThisMaxQueryTerms = y})

moreLikeThisQueryStopWordsLens :: Lens' MoreLikeThisQuery (Maybe (NonEmpty StopWord))
moreLikeThisQueryStopWordsLens = lens moreLikeThisStopWords (\x y -> x {moreLikeThisStopWords = y})

moreLikeThisQueryMinDocFrequencyLens :: Lens' MoreLikeThisQuery (Maybe MinDocFrequency)
moreLikeThisQueryMinDocFrequencyLens = lens moreLikeThisMinDocFrequency (\x y -> x {moreLikeThisMinDocFrequency = y})

moreLikeThisQueryMaxDocFrequencyLens :: Lens' MoreLikeThisQuery (Maybe MaxDocFrequency)
moreLikeThisQueryMaxDocFrequencyLens = lens moreLikeThisMaxDocFrequency (\x y -> x {moreLikeThisMaxDocFrequency = y})

moreLikeThisQueryMinWordLengthLens :: Lens' MoreLikeThisQuery (Maybe MinWordLength)
moreLikeThisQueryMinWordLengthLens = lens moreLikeThisMinWordLength (\x y -> x {moreLikeThisMinWordLength = y})

moreLikeThisQueryMaxWordLengthLens :: Lens' MoreLikeThisQuery (Maybe MaxWordLength)
moreLikeThisQueryMaxWordLengthLens = lens moreLikeThisMaxWordLength (\x y -> x {moreLikeThisMaxWordLength = y})

moreLikeThisQueryBoostTermsLens :: Lens' MoreLikeThisQuery (Maybe BoostTerms)
moreLikeThisQueryBoostTermsLens = lens moreLikeThisBoostTerms (\x y -> x {moreLikeThisBoostTerms = y})

moreLikeThisQueryBoostLens :: Lens' MoreLikeThisQuery (Maybe Boost)
moreLikeThisQueryBoostLens = lens moreLikeThisBoost (\x y -> x {moreLikeThisBoost = y})

moreLikeThisQueryAnalyzerLens :: Lens' MoreLikeThisQuery (Maybe Analyzer)
moreLikeThisQueryAnalyzerLens = lens moreLikeThisAnalyzer (\x y -> x {moreLikeThisAnalyzer = y})

instance ToJSON MoreLikeThisQuery where
  toJSON
    ( MoreLikeThisQuery
        text
        fields
        percent
        mtf
        mqt
        stopwords
        mindf
        maxdf
        minwl
        maxwl
        boostTerms
        boost
        analyzer
      ) =
      omitNulls base
      where
        base =
          [ "like_text" .= text,
            "fields" .= fields,
            "percent_terms_to_match" .= percent,
            "min_term_freq" .= mtf,
            "max_query_terms" .= mqt,
            "stop_words" .= stopwords,
            "min_doc_freq" .= mindf,
            "max_doc_freq" .= maxdf,
            "min_word_length" .= minwl,
            "max_word_length" .= maxwl,
            "boost_terms" .= boostTerms,
            "boost" .= boost,
            "analyzer" .= analyzer
          ]

instance FromJSON MoreLikeThisQuery where
  parseJSON = withObject "MoreLikeThisQuery" parse
    where
      parse o =
        MoreLikeThisQuery
          <$> o .: "like_text"
          -- <*> (optionalNE =<< o .:? "fields")
          <*> o .:? "fields"
          <*> o .:? "percent_terms_to_match"
          <*> o .:? "min_term_freq"
          <*> o .:? "max_query_terms"
          -- <*> (optionalNE =<< o .:? "stop_words")
          <*> o .:? "stop_words"
          <*> o .:? "min_doc_freq"
          <*> o .:? "max_doc_freq"
          <*> o .:? "min_word_length"
          <*> o .:? "max_word_length"
          <*> o .:? "boost_terms"
          <*> o .:? "boost"
          <*> o .:? "analyzer"
