{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Bloodhound.Internal.Versions.Common.Types.Query.MoreLikeThisField
  ( MoreLikeThisFieldQuery (..),
  )
where

import Database.Bloodhound.Internal.Utils.Imports
import Database.Bloodhound.Internal.Versions.Common.Types.Newtypes
import Database.Bloodhound.Internal.Versions.Common.Types.Query.Commons
import GHC.Generics

data MoreLikeThisFieldQuery = MoreLikeThisFieldQuery
  { moreLikeThisFieldText :: Text,
    moreLikeThisFieldFields :: FieldName,
    -- default 0.3 (30%)
    moreLikeThisFieldPercentMatch :: Maybe PercentMatch,
    moreLikeThisFieldMinimumTermFreq :: Maybe MinimumTermFrequency,
    moreLikeThisFieldMaxQueryTerms :: Maybe MaxQueryTerms,
    moreLikeThisFieldStopWords :: Maybe (NonEmpty StopWord),
    moreLikeThisFieldMinDocFrequency :: Maybe MinDocFrequency,
    moreLikeThisFieldMaxDocFrequency :: Maybe MaxDocFrequency,
    moreLikeThisFieldMinWordLength :: Maybe MinWordLength,
    moreLikeThisFieldMaxWordLength :: Maybe MaxWordLength,
    moreLikeThisFieldBoostTerms :: Maybe BoostTerms,
    moreLikeThisFieldBoost :: Maybe Boost,
    moreLikeThisFieldAnalyzer :: Maybe Analyzer
  }
  deriving (Eq, Show, Generic)

instance ToJSON MoreLikeThisFieldQuery where
  toJSON
    ( MoreLikeThisFieldQuery
        text
        (FieldName fieldName)
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
      object [fromText fieldName .= omitNulls base]
      where
        base =
          [ "like_text" .= text,
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

instance FromJSON MoreLikeThisFieldQuery where
  parseJSON = withObject "MoreLikeThisFieldQuery" parse
    where
      parse = fieldTagged $ \fn o ->
        MoreLikeThisFieldQuery
          <$> o .: "like_text"
          <*> pure fn
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
