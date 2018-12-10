{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Database.V1.Bloodhound.Internal.Suggest where


import           Bloodhound.Import

import qualified Data.HashMap.Strict                      as HM

import           Database.V1.Bloodhound.Internal.Newtypes
import           Database.V1.Bloodhound.Internal.Query

data Suggest = Suggest { suggestText :: Text
                       , suggestName :: Text
                       , suggestType :: SuggestType
                       }
 deriving (Show, Eq)

instance ToJSON Suggest where
  toJSON Suggest{..} = object [ "text" .= suggestText
                              , suggestName .= suggestType
                              ]

instance FromJSON Suggest where
  parseJSON (Object o) = do
    suggestText' <- o .: "text"
    let dropTextList = HM.toList $ HM.filterWithKey (\x _ -> x /= "text") o
    suggestName' <- case dropTextList of
                        [(x, _)] -> return x
                        _        -> fail "error parsing Suggest field name"
    suggestType' <- o .: suggestName'
    return $ Suggest suggestText' suggestName' suggestType'
  parseJSON x = typeMismatch "Suggest" x

data SuggestType = SuggestTypePhraseSuggester PhraseSuggester
  deriving (Show, Eq)

instance ToJSON SuggestType where
  toJSON (SuggestTypePhraseSuggester x) = object ["phrase" .= x]

instance FromJSON SuggestType where
  parseJSON = withObject "SuggestType" parse
    where parse o = phraseSuggester `taggedWith` "phrase"
           where taggedWith parser k = parser =<< o .: k
                 phraseSuggester = pure . SuggestTypePhraseSuggester

data PhraseSuggester =
  PhraseSuggester { phraseSuggesterField :: FieldName
                  , phraseSuggesterGramSize :: Maybe Int
                  , phraseSuggesterRealWordErrorLikelihood :: Maybe Int
                  , phraseSuggesterConfidence :: Maybe Int
                  , phraseSuggesterMaxErrors :: Maybe Int
                  , phraseSuggesterSeparator :: Maybe Text
                  , phraseSuggesterSize :: Maybe Size
                  , phraseSuggesterAnalyzer :: Maybe Analyzer
                  , phraseSuggesterShardSize :: Maybe Int
                  , phraseSuggesterHighlight :: Maybe PhraseSuggesterHighlighter
                  , phraseSuggesterCollate :: Maybe PhraseSuggesterCollate
                  , phraseSuggesterCandidateGenerators :: [DirectGenerators]
                  }
  deriving (Show, Eq)

instance ToJSON PhraseSuggester where
  toJSON PhraseSuggester{..} = omitNulls [ "field" .= phraseSuggesterField
                                         , "gram_size" .= phraseSuggesterGramSize
                                         , "real_word_error_likelihood" .= phraseSuggesterRealWordErrorLikelihood
                                         , "confidence" .= phraseSuggesterConfidence
                                         , "max_errors" .= phraseSuggesterMaxErrors
                                         , "separator" .= phraseSuggesterSeparator
                                         , "size" .= phraseSuggesterSize
                                         , "analyzer" .= phraseSuggesterAnalyzer
                                         , "shard_size" .= phraseSuggesterShardSize
                                         , "highlight" .= phraseSuggesterHighlight
                                         , "collate" .= phraseSuggesterCollate
                                         , "direct_generator" .= phraseSuggesterCandidateGenerators
                                        ]

instance FromJSON PhraseSuggester where
  parseJSON = withObject "PhraseSuggester" parse
    where parse o = PhraseSuggester
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

mkPhraseSuggester :: FieldName -> PhraseSuggester
mkPhraseSuggester fName =
  PhraseSuggester fName Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing []

data PhraseSuggesterHighlighter =
  PhraseSuggesterHighlighter { phraseSuggesterHighlighterPreTag  :: Text
                             , phraseSuggesterHighlighterPostTag :: Text
                             }
  deriving (Show, Eq)

instance ToJSON PhraseSuggesterHighlighter where
  toJSON PhraseSuggesterHighlighter{..} =
            object [ "pre_tag" .= phraseSuggesterHighlighterPreTag
                   , "post_tag" .= phraseSuggesterHighlighterPostTag
                   ]

instance FromJSON PhraseSuggesterHighlighter where
  parseJSON = withObject "PhraseSuggesterHighlighter" parse
    where parse o = PhraseSuggesterHighlighter
                      <$> o .: "pre_tag"
                      <*> o .: "post_tag"

data PhraseSuggesterCollate =
  PhraseSuggesterCollate { phraseSuggesterCollateTemplateQuery :: TemplateQueryInline
                         , phraseSuggesterCollatePrune :: Bool
                         }
  deriving (Show, Eq)

instance ToJSON PhraseSuggesterCollate where
  toJSON PhraseSuggesterCollate{..} = object [ "query" .= object
                                              [ "inline" .= (inline phraseSuggesterCollateTemplateQuery)
                                              ]
                                             , "params" .= (params phraseSuggesterCollateTemplateQuery)
                                             , "prune" .= phraseSuggesterCollatePrune
                                             ]

instance FromJSON PhraseSuggesterCollate where
  parseJSON (Object o) = do
    query' <- o .: "query"
    inline' <- query' .: "inline"
    params' <- o .: "params"
    prune' <- o .:? "prune" .!= False
    return $ PhraseSuggesterCollate (TemplateQueryInline inline' params') prune'
  parseJSON x = typeMismatch "PhraseSuggesterCollate" x

data DirectGenerators = DirectGenerators
  { directGeneratorsField         :: FieldName
  , directGeneratorsSize          :: Maybe Int
  , directGeneratorSuggestMode    :: DirectGeneratorSuggestModeTypes
  , directGeneratorMaxEdits       :: Maybe Double
  , directGeneratorPrefixLength   :: Maybe Int
  , directGeneratorMinWordLength  :: Maybe Int
  , directGeneratorMaxInspections :: Maybe Int
  , directGeneratorMinDocFreq     :: Maybe Double
  , directGeneratorMaxTermFreq    :: Maybe Double
  , directGeneratorPreFilter      :: Maybe Text
  , directGeneratorPostFilter     :: Maybe Text
  }
  deriving (Show, Eq)


instance ToJSON DirectGenerators where
  toJSON DirectGenerators{..} = omitNulls [ "field" .= directGeneratorsField
                                         , "size" .= directGeneratorsSize
                                         , "suggest_mode" .= directGeneratorSuggestMode
                                         , "max_edits" .= directGeneratorMaxEdits
                                         , "prefix_length" .= directGeneratorPrefixLength
                                         , "min_word_length" .= directGeneratorMinWordLength
                                         , "max_inspections" .= directGeneratorMaxInspections
                                         , "min_doc_freq" .= directGeneratorMinDocFreq
                                         , "max_term_freq" .= directGeneratorMaxTermFreq
                                         , "pre_filter" .= directGeneratorPreFilter
                                         , "post_filter" .= directGeneratorPostFilter
                                        ]

instance FromJSON DirectGenerators where
  parseJSON = withObject "DirectGenerators" parse
    where parse o = DirectGenerators
                      <$> o .: "field"
                      <*> o .:? "size"
                      <*> o .:  "suggest_mode"
                      <*> o .:? "max_edits"
                      <*> o .:? "prefix_length"
                      <*> o .:? "min_word_length"
                      <*> o .:? "max_inspections"
                      <*> o .:? "min_doc_freq"
                      <*> o .:? "max_term_freq"
                      <*> o .:? "pre_filter"
                      <*> o .:? "post_filter"

mkDirectGenerators :: FieldName -> DirectGenerators
mkDirectGenerators fn = DirectGenerators fn Nothing DirectGeneratorSuggestModeMissing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data DirectGeneratorSuggestModeTypes = DirectGeneratorSuggestModeMissing
                                | DirectGeneratorSuggestModePopular
                                | DirectGeneratorSuggestModeAlways
  deriving (Show, Eq)

instance ToJSON DirectGeneratorSuggestModeTypes where
  toJSON DirectGeneratorSuggestModeMissing = "missing"
  toJSON DirectGeneratorSuggestModePopular = "popular"
  toJSON DirectGeneratorSuggestModeAlways  = "always"

instance FromJSON DirectGeneratorSuggestModeTypes where
  parseJSON = withText "DirectGeneratorSuggestModeTypes" parse
    where parse "missing"        = pure DirectGeneratorSuggestModeMissing
          parse "popular"       = pure DirectGeneratorSuggestModePopular
          parse "always"        = pure DirectGeneratorSuggestModeAlways
          parse f            = fail ("Unexpected DirectGeneratorSuggestModeTypes: " <> show f)

data SuggestOptions =
  SuggestOptions { suggestOptionsText        :: Text
                 , suggestOptionsScore       :: Double
                 , suggestOptionsFreq        :: Maybe Int
                 , suggestOptionsHighlighted :: Maybe Text
                 }
  deriving (Eq, Show)

instance FromJSON SuggestOptions where
  parseJSON = withObject "SuggestOptions" parse
    where parse o = SuggestOptions
                    <$> o .: "text"
                    <*> o .: "score"
                    <*> o .:? "freq"
                    <*> o .:? "highlighted"

data SuggestResponse =
  SuggestResponse { suggestResponseText    :: Text
                  , suggestResponseOffset  :: Int
                  , suggestResponseLength  :: Int
                  , suggestResponseOptions :: [SuggestOptions]
                  }
  deriving (Eq, Show)

instance FromJSON SuggestResponse where
  parseJSON = withObject "SuggestResponse" parse
    where parse o = SuggestResponse
                    <$> o .: "text"
                    <*> o .: "offset"
                    <*> o .: "length"
                    <*> o .: "options"

data NamedSuggestionResponse =
  NamedSuggestionResponse { nsrName      :: Text
                          , nsrResponses :: [SuggestResponse]
                          }
  deriving (Eq, Show)

instance FromJSON NamedSuggestionResponse where
  parseJSON (Object o) = do
    suggestionName' <- case HM.toList o of
                        [(x, _)] -> return x
                        _ -> fail "error parsing NamedSuggestionResponse name"
    suggestionResponses' <- o .: suggestionName'
    return $ NamedSuggestionResponse suggestionName' suggestionResponses'

  parseJSON x = typeMismatch "NamedSuggestionResponse" x
