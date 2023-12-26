{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Bloodhound.Internal.Versions.Common.Types.Query.MoreLikeThis
  ( MoreLikeThisQuery (..),
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
  deriving (Eq, Show, Generic)

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
