{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Bloodhound.Internal.Versions.Common.Types.Query.CommonTerms
  ( CommonMinimumMatch (..),
    CommonTermsQuery (..),
    MinimumMatchHighLow (..),
  )
where

import Database.Bloodhound.Internal.Utils.Imports
import Database.Bloodhound.Internal.Versions.Common.Types.Newtypes
import Database.Bloodhound.Internal.Versions.Common.Types.Query.Commons
import GHC.Generics

data CommonTermsQuery = CommonTermsQuery
  { commonField :: FieldName,
    commonQuery :: QueryString,
    commonCutoffFrequency :: CutoffFrequency,
    commonLowFreqOperator :: BooleanOperator,
    commonHighFreqOperator :: BooleanOperator,
    commonMinimumShouldMatch :: Maybe CommonMinimumMatch,
    commonBoost :: Maybe Boost,
    commonAnalyzer :: Maybe Analyzer,
    commonDisableCoord :: Maybe DisableCoord
  }
  deriving (Eq, Show, Generic)

instance ToJSON CommonTermsQuery where
  toJSON
    ( CommonTermsQuery
        (FieldName fieldName)
        (QueryString query)
        cf
        lfo
        hfo
        msm
        boost
        analyzer
        disableCoord
      ) =
      object [fromText fieldName .= omitNulls base]
      where
        base =
          [ "query" .= query,
            "cutoff_frequency" .= cf,
            "low_freq_operator" .= lfo,
            "minimum_should_match" .= msm,
            "boost" .= boost,
            "analyzer" .= analyzer,
            "disable_coord" .= disableCoord,
            "high_freq_operator" .= hfo
          ]

instance FromJSON CommonTermsQuery where
  parseJSON = withObject "CommonTermsQuery" parse
    where
      parse = fieldTagged $ \fn o ->
        CommonTermsQuery fn
          <$> o .: "query"
          <*> o .: "cutoff_frequency"
          <*> o .: "low_freq_operator"
          <*> o .: "high_freq_operator"
          <*> o .:? "minimum_should_match"
          <*> o .:? "boost"
          <*> o .:? "analyzer"
          <*> o .:? "disable_coord"

data CommonMinimumMatch
  = CommonMinimumMatchHighLow MinimumMatchHighLow
  | CommonMinimumMatch MinimumMatch
  deriving (Eq, Show, Generic)

instance ToJSON CommonMinimumMatch where
  toJSON (CommonMinimumMatch mm) = toJSON mm
  toJSON (CommonMinimumMatchHighLow (MinimumMatchHighLow lowF highF)) =
    object
      [ "low_freq" .= lowF,
        "high_freq" .= highF
      ]

instance FromJSON CommonMinimumMatch where
  parseJSON v =
    parseMinimum v
      <|> parseMinimumHighLow v
    where
      parseMinimum = fmap CommonMinimumMatch . parseJSON
      parseMinimumHighLow =
        fmap CommonMinimumMatchHighLow
          . withObject
            "CommonMinimumMatchHighLow"
            ( \o ->
                MinimumMatchHighLow
                  <$> o .: "low_freq"
                  <*> o .: "high_freq"
            )

data MinimumMatchHighLow = MinimumMatchHighLow
  { lowFreq :: MinimumMatch,
    highFreq :: MinimumMatch
  }
  deriving (Eq, Show, Generic)
