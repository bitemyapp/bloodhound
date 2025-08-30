{-# LANGUAGE OverloadedStrings #-}

module Database.Bloodhound.Internal.Versions.Common.Types.Query.CommonTerms
  ( CommonMinimumMatch (..),
    CommonTermsQuery (..),
    MinimumMatchHighLow (..),

    -- * Optics
    commonTermsQueryFieldLens,
    commonTermsQueryQueryLens,
    commonTermsQueryCutoffFrequencyLens,
    commonTermsQueryLowFreqOperatorLens,
    commonTermsQueryHighFreqOperatorLens,
    commonTermsQueryMinimumShouldMatchLens,
    commonTermsQueryBoostLens,
    commonTermsQueryAnalyzerLens,
    commonTermsQueryDisableCoordLens,
    commonMinimumMatchHighLowPrism,
    commonMinimumMatchPrism,
    minimumMatchHighLowLowFreqLens,
    minimumMatchHighLowHighFreqLens,
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
  deriving stock (Eq, Show, Generic)

commonTermsQueryFieldLens :: Lens' CommonTermsQuery FieldName
commonTermsQueryFieldLens = lens commonField (\x y -> x {commonField = y})

commonTermsQueryQueryLens :: Lens' CommonTermsQuery QueryString
commonTermsQueryQueryLens = lens commonQuery (\x y -> x {commonQuery = y})

commonTermsQueryCutoffFrequencyLens :: Lens' CommonTermsQuery CutoffFrequency
commonTermsQueryCutoffFrequencyLens = lens commonCutoffFrequency (\x y -> x {commonCutoffFrequency = y})

commonTermsQueryLowFreqOperatorLens :: Lens' CommonTermsQuery BooleanOperator
commonTermsQueryLowFreqOperatorLens = lens commonLowFreqOperator (\x y -> x {commonLowFreqOperator = y})

commonTermsQueryHighFreqOperatorLens :: Lens' CommonTermsQuery BooleanOperator
commonTermsQueryHighFreqOperatorLens = lens commonHighFreqOperator (\x y -> x {commonHighFreqOperator = y})

commonTermsQueryMinimumShouldMatchLens :: Lens' CommonTermsQuery (Maybe CommonMinimumMatch)
commonTermsQueryMinimumShouldMatchLens = lens commonMinimumShouldMatch (\x y -> x {commonMinimumShouldMatch = y})

commonTermsQueryBoostLens :: Lens' CommonTermsQuery (Maybe Boost)
commonTermsQueryBoostLens = lens commonBoost (\x y -> x {commonBoost = y})

commonTermsQueryAnalyzerLens :: Lens' CommonTermsQuery (Maybe Analyzer)
commonTermsQueryAnalyzerLens = lens commonAnalyzer (\x y -> x {commonAnalyzer = y})

commonTermsQueryDisableCoordLens :: Lens' CommonTermsQuery (Maybe DisableCoord)
commonTermsQueryDisableCoordLens = lens commonDisableCoord (\x y -> x {commonDisableCoord = y})

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
  deriving stock (Eq, Show, Generic)

commonMinimumMatchHighLowPrism :: Prism' CommonMinimumMatch MinimumMatchHighLow
commonMinimumMatchHighLowPrism = prism CommonMinimumMatchHighLow extract
  where
    extract cmm =
      case cmm of
        CommonMinimumMatchHighLow x -> Right x
        _ -> Left cmm

commonMinimumMatchPrism :: Prism' CommonMinimumMatch MinimumMatch
commonMinimumMatchPrism = prism CommonMinimumMatch extract
  where
    extract cmm =
      case cmm of
        CommonMinimumMatch x -> Right x
        _ -> Left cmm

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
  deriving stock (Eq, Show, Generic)

minimumMatchHighLowLowFreqLens :: Lens' MinimumMatchHighLow MinimumMatch
minimumMatchHighLowLowFreqLens = lens lowFreq (\x y -> x {lowFreq = y})

minimumMatchHighLowHighFreqLens :: Lens' MinimumMatchHighLow MinimumMatch
minimumMatchHighLowHighFreqLens = lens highFreq (\x y -> x {highFreq = y})
