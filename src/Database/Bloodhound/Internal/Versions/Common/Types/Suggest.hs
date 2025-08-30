{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Bloodhound.Internal.Versions.Common.Types.Suggest where

import qualified Data.Aeson.KeyMap as X
import Database.Bloodhound.Internal.Utils.Imports
import Database.Bloodhound.Internal.Versions.Common.Types.Newtypes
import Database.Bloodhound.Internal.Versions.Common.Types.Query (Query, TemplateQueryKeyValuePairs)
import GHC.Generics

data Suggest = Suggest
  { suggestText :: Text,
    suggestName :: Text,
    suggestType :: SuggestType
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON Suggest where
  toJSON Suggest {..} =
    object
      [ "text" .= suggestText,
        fromText suggestName .= suggestType
      ]

instance FromJSON Suggest where
  parseJSON (Object o) = do
    suggestText' <- o .: "text"
    let dropTextList =
          X.toList $
            X.filterWithKey (\x _ -> x /= "text") o
    suggestName' <-
      case dropTextList of
        [(x, _)] -> return x
        _ -> fail "error parsing Suggest field name"
    suggestType' <- o .: suggestName'
    return $ Suggest suggestText' (toText suggestName') suggestType'
  parseJSON x = typeMismatch "Suggest" x

suggestTextLens :: Lens' Suggest Text
suggestTextLens = lens suggestText (\x y -> x {suggestText = y})

suggestNameLens :: Lens' Suggest Text
suggestNameLens = lens suggestName (\x y -> x {suggestName = y})

suggestTypeLens :: Lens' Suggest SuggestType
suggestTypeLens = lens suggestType (\x y -> x {suggestType = y})

data SuggestType
  = SuggestTypePhraseSuggester PhraseSuggester
  deriving stock (Eq, Show, Generic)

instance ToJSON SuggestType where
  toJSON (SuggestTypePhraseSuggester x) =
    object ["phrase" .= x]

instance FromJSON SuggestType where
  parseJSON = withObject "SuggestType" parse
    where
      parse o = phraseSuggester `taggedWith` "phrase"
        where
          taggedWith parser k = parser =<< o .: k
          phraseSuggester = pure . SuggestTypePhraseSuggester

data PhraseSuggester = PhraseSuggester
  { phraseSuggesterField :: FieldName,
    phraseSuggesterGramSize :: Maybe Int,
    phraseSuggesterRealWordErrorLikelihood :: Maybe Int,
    phraseSuggesterConfidence :: Maybe Int,
    phraseSuggesterMaxErrors :: Maybe Int,
    phraseSuggesterSeparator :: Maybe Text,
    phraseSuggesterSize :: Maybe Size,
    phraseSuggesterAnalyzer :: Maybe Analyzer,
    phraseSuggesterShardSize :: Maybe Int,
    phraseSuggesterHighlight :: Maybe PhraseSuggesterHighlighter,
    phraseSuggesterCollate :: Maybe PhraseSuggesterCollate,
    phraseSuggesterCandidateGenerators :: [DirectGenerators]
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON PhraseSuggester where
  toJSON PhraseSuggester {..} =
    omitNulls
      [ "field" .= phraseSuggesterField,
        "gram_size" .= phraseSuggesterGramSize,
        "real_word_error_likelihood"
          .= phraseSuggesterRealWordErrorLikelihood,
        "confidence" .= phraseSuggesterConfidence,
        "max_errors" .= phraseSuggesterMaxErrors,
        "separator" .= phraseSuggesterSeparator,
        "size" .= phraseSuggesterSize,
        "analyzer" .= phraseSuggesterAnalyzer,
        "shard_size" .= phraseSuggesterShardSize,
        "highlight" .= phraseSuggesterHighlight,
        "collate" .= phraseSuggesterCollate,
        "direct_generator"
          .= phraseSuggesterCandidateGenerators
      ]

instance FromJSON PhraseSuggester where
  parseJSON = withObject "PhraseSuggester" parse
    where
      parse o =
        PhraseSuggester
          <$> o .: "field"
          <*> o .:? "gram_size"
          <*> o .:? "real_word_error_likelihood"
          <*> o .:? "confidence"
          <*> o .:? "max_errors"
          <*> o .:? "separator"
          <*> o .:? "size"
          <*> o .:? "analyzer"
          <*> o .:? "shard_size"
          <*> o .:? "highlight"
          <*> o .:? "collate"
          <*> o .:? "direct_generator" .!= []

suggestTypePhraseSuggesterLens :: Lens' SuggestType PhraseSuggester
suggestTypePhraseSuggesterLens = lens (\(SuggestTypePhraseSuggester x) -> x) (const SuggestTypePhraseSuggester)

mkPhraseSuggester :: FieldName -> PhraseSuggester
mkPhraseSuggester fName =
  PhraseSuggester
    fName
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
    []

phraseSuggesterFieldLens :: Lens' PhraseSuggester FieldName
phraseSuggesterFieldLens = lens phraseSuggesterField (\x y -> x {phraseSuggesterField = y})

phraseSuggesterGramSizeLens :: Lens' PhraseSuggester (Maybe Int)
phraseSuggesterGramSizeLens = lens phraseSuggesterGramSize (\x y -> x {phraseSuggesterGramSize = y})

phraseSuggesterRealWordErrorLikelihoodLens :: Lens' PhraseSuggester (Maybe Int)
phraseSuggesterRealWordErrorLikelihoodLens = lens phraseSuggesterRealWordErrorLikelihood (\x y -> x {phraseSuggesterRealWordErrorLikelihood = y})

phraseSuggesterConfidenceLens :: Lens' PhraseSuggester (Maybe Int)
phraseSuggesterConfidenceLens = lens phraseSuggesterConfidence (\x y -> x {phraseSuggesterConfidence = y})

phraseSuggesterMaxErrorsLens :: Lens' PhraseSuggester (Maybe Int)
phraseSuggesterMaxErrorsLens = lens phraseSuggesterMaxErrors (\x y -> x {phraseSuggesterMaxErrors = y})

phraseSuggesterSeparatorLens :: Lens' PhraseSuggester (Maybe Text)
phraseSuggesterSeparatorLens = lens phraseSuggesterSeparator (\x y -> x {phraseSuggesterSeparator = y})

phraseSuggesterSizeLens :: Lens' PhraseSuggester (Maybe Size)
phraseSuggesterSizeLens = lens phraseSuggesterSize (\x y -> x {phraseSuggesterSize = y})

phraseSuggesterAnalyzerLens :: Lens' PhraseSuggester (Maybe Analyzer)
phraseSuggesterAnalyzerLens = lens phraseSuggesterAnalyzer (\x y -> x {phraseSuggesterAnalyzer = y})

phraseSuggesterShardSizeLens :: Lens' PhraseSuggester (Maybe Int)
phraseSuggesterShardSizeLens = lens phraseSuggesterShardSize (\x y -> x {phraseSuggesterShardSize = y})

phraseSuggesterHighlightLens :: Lens' PhraseSuggester (Maybe PhraseSuggesterHighlighter)
phraseSuggesterHighlightLens = lens phraseSuggesterHighlight (\x y -> x {phraseSuggesterHighlight = y})

phraseSuggesterCollateLens :: Lens' PhraseSuggester (Maybe PhraseSuggesterCollate)
phraseSuggesterCollateLens = lens phraseSuggesterCollate (\x y -> x {phraseSuggesterCollate = y})

phraseSuggesterCandidateGeneratorsLens :: Lens' PhraseSuggester [DirectGenerators]
phraseSuggesterCandidateGeneratorsLens = lens phraseSuggesterCandidateGenerators (\x y -> x {phraseSuggesterCandidateGenerators = y})

data PhraseSuggesterHighlighter = PhraseSuggesterHighlighter
  { phraseSuggesterHighlighterPreTag :: Text,
    phraseSuggesterHighlighterPostTag :: Text
  }
  deriving stock (Eq, Show, Generic)

phraseSuggesterHighlighterPreTagLens :: Lens' PhraseSuggesterHighlighter Text
phraseSuggesterHighlighterPreTagLens = lens phraseSuggesterHighlighterPreTag (\x y -> x {phraseSuggesterHighlighterPreTag = y})

phraseSuggesterHighlighterPostTagLens :: Lens' PhraseSuggesterHighlighter Text
phraseSuggesterHighlighterPostTagLens = lens phraseSuggesterHighlighterPostTag (\x y -> x {phraseSuggesterHighlighterPostTag = y})

instance ToJSON PhraseSuggesterHighlighter where
  toJSON PhraseSuggesterHighlighter {..} =
    object
      [ "pre_tag" .= phraseSuggesterHighlighterPreTag,
        "post_tag" .= phraseSuggesterHighlighterPostTag
      ]

instance FromJSON PhraseSuggesterHighlighter where
  parseJSON = withObject "PhraseSuggesterHighlighter" parse
    where
      parse o =
        PhraseSuggesterHighlighter
          <$> o .: "pre_tag"
          <*> o .: "post_tag"

data PhraseSuggesterCollate = PhraseSuggesterCollate
  { phraseSuggesterCollateTemplateQuery :: Query,
    phraseSuggesterCollateParams :: TemplateQueryKeyValuePairs,
    phraseSuggesterCollatePrune :: Bool
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON PhraseSuggesterCollate where
  toJSON PhraseSuggesterCollate {..} =
    object
      [ "query"
          .= object
            [ "source" .= phraseSuggesterCollateTemplateQuery
            ],
        "params" .= phraseSuggesterCollateParams,
        "prune" .= phraseSuggesterCollatePrune
      ]

instance FromJSON PhraseSuggesterCollate where
  parseJSON (Object o) = do
    query' <- o .: "query"
    inline' <- query' .: "source"
    params' <- o .: "params"
    prune' <- o .:? "prune" .!= False
    return $ PhraseSuggesterCollate inline' params' prune'
  parseJSON x = typeMismatch "PhraseSuggesterCollate" x

phraseSuggesterCollateTemplateQueryLens :: Lens' PhraseSuggesterCollate Query
phraseSuggesterCollateTemplateQueryLens = lens phraseSuggesterCollateTemplateQuery (\x y -> x {phraseSuggesterCollateTemplateQuery = y})

phraseSuggesterCollateParamsLens :: Lens' PhraseSuggesterCollate TemplateQueryKeyValuePairs
phraseSuggesterCollateParamsLens = lens phraseSuggesterCollateParams (\x y -> x {phraseSuggesterCollateParams = y})

phraseSuggesterCollatePruneLens :: Lens' PhraseSuggesterCollate Bool
phraseSuggesterCollatePruneLens = lens phraseSuggesterCollatePrune (\x y -> x {phraseSuggesterCollatePrune = y})

data SuggestOptions = SuggestOptions
  { suggestOptionsText :: Text,
    suggestOptionsScore :: Double,
    suggestOptionsFreq :: Maybe Int,
    suggestOptionsHighlighted :: Maybe Text
  }
  deriving stock (Eq, Read, Show)

instance FromJSON SuggestOptions where
  parseJSON = withObject "SuggestOptions" parse
    where
      parse o =
        SuggestOptions
          <$> o .: "text"
          <*> o .: "score"
          <*> o .:? "freq"
          <*> o .:? "highlighted"

suggestOptionsTextLens :: Lens' SuggestOptions Text
suggestOptionsTextLens = lens suggestOptionsText (\x y -> x {suggestOptionsText = y})

suggestOptionsScoreLens :: Lens' SuggestOptions Double
suggestOptionsScoreLens = lens suggestOptionsScore (\x y -> x {suggestOptionsScore = y})

suggestOptionsFreqLens :: Lens' SuggestOptions (Maybe Int)
suggestOptionsFreqLens = lens suggestOptionsFreq (\x y -> x {suggestOptionsFreq = y})

suggestOptionsHighlightedLens :: Lens' SuggestOptions (Maybe Text)
suggestOptionsHighlightedLens = lens suggestOptionsHighlighted (\x y -> x {suggestOptionsHighlighted = y})

data SuggestResponse = SuggestResponse
  { suggestResponseText :: Text,
    suggestResponseOffset :: Int,
    suggestResponseLength :: Int,
    suggestResponseOptions :: [SuggestOptions]
  }
  deriving stock (Eq, Read, Show)

instance FromJSON SuggestResponse where
  parseJSON = withObject "SuggestResponse" parse
    where
      parse o =
        SuggestResponse
          <$> o .: "text"
          <*> o .: "offset"
          <*> o .: "length"
          <*> o .: "options"

suggestResponseTextLens :: Lens' SuggestResponse Text
suggestResponseTextLens = lens suggestResponseText (\x y -> x {suggestResponseText = y})

suggestResponseOffsetLens :: Lens' SuggestResponse Int
suggestResponseOffsetLens = lens suggestResponseOffset (\x y -> x {suggestResponseOffset = y})

suggestResponseLengthLens :: Lens' SuggestResponse Int
suggestResponseLengthLens = lens suggestResponseLength (\x y -> x {suggestResponseLength = y})

suggestResponseOptionsLens :: Lens' SuggestResponse [SuggestOptions]
suggestResponseOptionsLens = lens suggestResponseOptions (\x y -> x {suggestResponseOptions = y})

data NamedSuggestionResponse = NamedSuggestionResponse
  { nsrName :: Text,
    nsrResponses :: [SuggestResponse]
  }
  deriving stock (Eq, Read, Show)

instance FromJSON NamedSuggestionResponse where
  parseJSON (Object o) = do
    suggestionName' <- case X.toList o of
      [(x, _)] -> return x
      _ -> fail "error parsing NamedSuggestionResponse name"
    suggestionResponses' <- o .: suggestionName'
    return $ NamedSuggestionResponse (toText suggestionName') suggestionResponses'
  parseJSON x = typeMismatch "NamedSuggestionResponse" x

namedSuggestionResponseNameLens :: Lens' NamedSuggestionResponse Text
namedSuggestionResponseNameLens = lens nsrName (\x y -> x {nsrName = y})

namedSuggestionResponseResponsesLens :: Lens' NamedSuggestionResponse [SuggestResponse]
namedSuggestionResponseResponsesLens = lens nsrResponses (\x y -> x {nsrResponses = y})

data DirectGeneratorSuggestModeTypes
  = DirectGeneratorSuggestModeMissing
  | DirectGeneratorSuggestModePopular
  | DirectGeneratorSuggestModeAlways
  deriving stock (Eq, Show, Generic)

instance ToJSON DirectGeneratorSuggestModeTypes where
  toJSON DirectGeneratorSuggestModeMissing = "missing"
  toJSON DirectGeneratorSuggestModePopular = "popular"
  toJSON DirectGeneratorSuggestModeAlways = "always"

instance FromJSON DirectGeneratorSuggestModeTypes where
  parseJSON = withText "DirectGeneratorSuggestModeTypes" parse
    where
      parse "missing" =
        pure DirectGeneratorSuggestModeMissing
      parse "popular" =
        pure DirectGeneratorSuggestModePopular
      parse "always" =
        pure DirectGeneratorSuggestModeAlways
      parse f =
        fail ("Unexpected DirectGeneratorSuggestModeTypes: " <> show f)

data DirectGenerators = DirectGenerators
  { directGeneratorsField :: FieldName,
    directGeneratorsSize :: Maybe Int,
    directGeneratorSuggestMode :: DirectGeneratorSuggestModeTypes,
    directGeneratorMaxEdits :: Maybe Double,
    directGeneratorPrefixLength :: Maybe Int,
    directGeneratorMinWordLength :: Maybe Int,
    directGeneratorMaxInspections :: Maybe Int,
    directGeneratorMinDocFreq :: Maybe Double,
    directGeneratorMaxTermFreq :: Maybe Double,
    directGeneratorPreFilter :: Maybe Text,
    directGeneratorPostFilter :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON DirectGenerators where
  toJSON DirectGenerators {..} =
    omitNulls
      [ "field" .= directGeneratorsField,
        "size" .= directGeneratorsSize,
        "suggest_mode" .= directGeneratorSuggestMode,
        "max_edits" .= directGeneratorMaxEdits,
        "prefix_length" .= directGeneratorPrefixLength,
        "min_word_length" .= directGeneratorMinWordLength,
        "max_inspections" .= directGeneratorMaxInspections,
        "min_doc_freq" .= directGeneratorMinDocFreq,
        "max_term_freq" .= directGeneratorMaxTermFreq,
        "pre_filter" .= directGeneratorPreFilter,
        "post_filter" .= directGeneratorPostFilter
      ]

instance FromJSON DirectGenerators where
  parseJSON = withObject "DirectGenerators" parse
    where
      parse o =
        DirectGenerators
          <$> o .: "field"
          <*> o .:? "size"
          <*> o .: "suggest_mode"
          <*> o .:? "max_edits"
          <*> o .:? "prefix_length"
          <*> o .:? "min_word_length"
          <*> o .:? "max_inspections"
          <*> o .:? "min_doc_freq"
          <*> o .:? "max_term_freq"
          <*> o .:? "pre_filter"
          <*> o .:? "post_filter"

mkDirectGenerators :: FieldName -> DirectGenerators
mkDirectGenerators fn =
  DirectGenerators
    fn
    Nothing
    DirectGeneratorSuggestModeMissing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing

directGeneratorsFieldLens :: Lens' DirectGenerators FieldName
directGeneratorsFieldLens = lens directGeneratorsField (\x y -> x {directGeneratorsField = y})

directGeneratorsSizeLens :: Lens' DirectGenerators (Maybe Int)
directGeneratorsSizeLens = lens directGeneratorsSize (\x y -> x {directGeneratorsSize = y})

directGeneratorSuggestModeLens :: Lens' DirectGenerators DirectGeneratorSuggestModeTypes
directGeneratorSuggestModeLens = lens directGeneratorSuggestMode (\x y -> x {directGeneratorSuggestMode = y})

directGeneratorMaxEditsLens :: Lens' DirectGenerators (Maybe Double)
directGeneratorMaxEditsLens = lens directGeneratorMaxEdits (\x y -> x {directGeneratorMaxEdits = y})

directGeneratorPrefixLengthLens :: Lens' DirectGenerators (Maybe Int)
directGeneratorPrefixLengthLens = lens directGeneratorPrefixLength (\x y -> x {directGeneratorPrefixLength = y})

directGeneratorMinWordLengthLens :: Lens' DirectGenerators (Maybe Int)
directGeneratorMinWordLengthLens = lens directGeneratorMinWordLength (\x y -> x {directGeneratorMinWordLength = y})

directGeneratorMaxInspectionsLens :: Lens' DirectGenerators (Maybe Int)
directGeneratorMaxInspectionsLens = lens directGeneratorMaxInspections (\x y -> x {directGeneratorMaxInspections = y})

directGeneratorMinDocFreqLens :: Lens' DirectGenerators (Maybe Double)
directGeneratorMinDocFreqLens = lens directGeneratorMinDocFreq (\x y -> x {directGeneratorMinDocFreq = y})

directGeneratorMaxTermFreqLens :: Lens' DirectGenerators (Maybe Double)
directGeneratorMaxTermFreqLens = lens directGeneratorMaxTermFreq (\x y -> x {directGeneratorMaxTermFreq = y})

directGeneratorPreFilterLens :: Lens' DirectGenerators (Maybe Text)
directGeneratorPreFilterLens = lens directGeneratorPreFilter (\x y -> x {directGeneratorPreFilter = y})

directGeneratorPostFilterLens :: Lens' DirectGenerators (Maybe Text)
directGeneratorPostFilterLens = lens directGeneratorPostFilter (\x y -> x {directGeneratorPostFilter = y})
