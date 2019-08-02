{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Database.V5.Bloodhound.Internal.Analysis where

import           Bloodhound.Import

import qualified Data.Map.Strict as M
import qualified Data.Text as T

import           Database.V5.Bloodhound.Internal.Newtypes
import           Database.V5.Bloodhound.Internal.StringlyTyped

data Analysis = Analysis
  { analysisAnalyzer :: M.Map Text AnalyzerDefinition
  , analysisTokenizer :: M.Map Text TokenizerDefinition
  , analysisTokenFilter :: M.Map Text TokenFilterDefinition
  , analysisCharFilter :: M.Map Text CharFilterDefinition
  } deriving (Eq, Show)

instance ToJSON Analysis where
  toJSON (Analysis analyzer tokenizer tokenFilter charFilter) = object
    [ "analyzer" .= analyzer
    , "tokenizer" .= tokenizer
    , "filter" .= tokenFilter
    , "char_filter" .= charFilter
    ]

instance FromJSON Analysis where
  parseJSON = withObject "Analysis" $ \m -> Analysis
    <$> m .: "analyzer"
    <*> m .:? "tokenizer" .!= M.empty
    <*> m .:? "filter" .!= M.empty
    <*> m .:? "char_filter" .!= M.empty

newtype Tokenizer =
  Tokenizer Text
  deriving (Eq, Show, ToJSON, FromJSON)

data AnalyzerDefinition = AnalyzerDefinition
  { analyzerDefinitionTokenizer :: Maybe Tokenizer
  , analyzerDefinitionFilter :: [TokenFilter]
  , analyzerDefinitionCharFilter :: [CharFilter]
  } deriving (Eq,Show)

instance ToJSON AnalyzerDefinition where
  toJSON (AnalyzerDefinition tokenizer tokenFilter charFilter) =
    object $ catMaybes
    [ fmap ("tokenizer" .=) tokenizer
    , Just $ "filter" .= tokenFilter
    , Just $ "char_filter" .= charFilter
    ]

instance FromJSON AnalyzerDefinition where
  parseJSON = withObject "AnalyzerDefinition" $ \m -> AnalyzerDefinition
    <$> m .:? "tokenizer"
    <*> m .:? "filter" .!= []
    <*> m .:? "char_filter" .!= []

-- | Character filters are used to preprocess the stream of characters
--   before it is passed to the tokenizer.
data CharFilterDefinition
  = CharFilterDefinitionMapping (M.Map Text Text)
  | CharFilterDefinitionPatternReplace
    { charFilterDefinitionPatternReplacePattern :: Text
    , charFilterDefinitionPatternReplaceReplacement :: Text
    , charFilterDefinitionPatternReplaceFlags :: Maybe Text
    }
  deriving (Eq, Show)

instance ToJSON CharFilterDefinition where
  toJSON (CharFilterDefinitionMapping ms) = object
    [ "type" .= ("mapping" :: Text)
    , "mappings" .= [a <> " => " <> b | (a, b) <- M.toList ms] ]
  toJSON (CharFilterDefinitionPatternReplace pat repl flags) = object $
    [ "type" .= ("pattern_replace" :: Text)
    , "pattern" .= pat
    , "replacement" .= repl
    ] ++ maybe [] (\f -> ["flags" .= f]) flags

instance FromJSON CharFilterDefinition where
  parseJSON = withObject "CharFilterDefinition" $ \m -> do
    t <- m .: "type"
    case (t :: Text) of
      "mapping" -> CharFilterDefinitionMapping . M.fromList <$> ms
        where
          ms = m .: "mappings" >>= mapM parseMapping
          parseMapping kv = case T.splitOn "=>" kv of
            (k:vs) -> pure (T.strip k, T.strip $ T.concat vs)
            _ -> fail "mapping is not of the format key => value"
      "pattern_replace" -> CharFilterDefinitionPatternReplace
        <$> m .: "pattern" <*> m .: "replacement" <*> m .:? "flags"
      _ -> fail ("unrecognized character filter type: " ++ T.unpack t)

newtype TokenizerDefinition =
  TokenizerDefinitionNgram Ngram
  deriving (Eq,Show)

instance ToJSON TokenizerDefinition where
  toJSON x = case x of
    TokenizerDefinitionNgram (Ngram minGram maxGram tokenChars) -> object
      [ "type" .= ("ngram" :: Text)
      , "min_gram" .= minGram
      , "max_gram" .= maxGram
      , "token_chars" .= tokenChars
      ]

instance FromJSON TokenizerDefinition where
  parseJSON = withObject "TokenizerDefinition" $ \m -> do
    typ <- m .: "type" :: Parser Text
    case typ of
      "ngram" -> fmap TokenizerDefinitionNgram $ Ngram
        <$> fmap unStringlyTypedInt (m .: "min_gram")
        <*> fmap unStringlyTypedInt (m .: "max_gram")
        <*> m .: "token_chars"
      _ -> fail "invalid TokenizerDefinition"

data Ngram = Ngram
  { ngramMinGram :: Int
  , ngramMaxGram :: Int
  , ngramTokenChars :: [TokenChar]
  } deriving (Eq,Show)

data TokenChar =
    TokenLetter
  | TokenDigit
  | TokenWhitespace
  | TokenPunctuation
  | TokenSymbol
  deriving (Eq,Show)

instance ToJSON TokenChar where
  toJSON t = String $ case t of
    TokenLetter -> "letter"
    TokenDigit -> "digit"
    TokenWhitespace -> "whitespace"
    TokenPunctuation -> "punctuation"
    TokenSymbol -> "symbol"

instance FromJSON TokenChar where
  parseJSON = withText "TokenChar" $ \t -> case t of
    "letter" -> return TokenLetter
    "digit" -> return TokenDigit
    "whitespace" -> return TokenWhitespace
    "punctuation" -> return TokenPunctuation
    "symbol" -> return TokenSymbol
    _ -> fail "invalid TokenChar"

-- | Token filters are used to create custom analyzers.
data TokenFilterDefinition
  = TokenFilterDefinitionLowercase (Maybe Language)
  | TokenFilterDefinitionUppercase (Maybe Language)
  | TokenFilterDefinitionApostrophe
  | TokenFilterDefinitionReverse
  | TokenFilterDefinitionSnowball Language
  | TokenFilterDefinitionShingle Shingle
  | TokenFilterDefinitionStemmer Language
  | TokenFilterDefinitionStop (Either Language [StopWord])
  deriving (Eq, Show)

instance ToJSON TokenFilterDefinition where
  toJSON x = case x of
    TokenFilterDefinitionLowercase mlang -> object $ catMaybes
      [ Just $ "type" .= ("lowercase" :: Text)
      , fmap (\lang -> "language" .= languageToText lang) mlang
      ]
    TokenFilterDefinitionUppercase mlang -> object $ catMaybes
      [ Just $ "type" .= ("uppercase" :: Text)
      , fmap (\lang -> "language" .= languageToText lang) mlang
      ]
    TokenFilterDefinitionApostrophe -> object
      [ "type" .= ("apostrophe" :: Text)
      ]
    TokenFilterDefinitionReverse -> object
      [ "type" .= ("reverse" :: Text)
      ]
    TokenFilterDefinitionSnowball lang -> object
      [ "type" .= ("snowball" :: Text)
      , "language" .= languageToText lang
      ]
    TokenFilterDefinitionShingle s -> object
      [ "type" .= ("shingle" :: Text)
      , "max_shingle_size" .= shingleMaxSize s
      , "min_shingle_size" .= shingleMinSize s
      , "output_unigrams" .= shingleOutputUnigrams s
      , "output_unigrams_if_no_shingles" .= shingleOutputUnigramsIfNoShingles s
      , "token_separator" .= shingleTokenSeparator s
      , "filler_token" .= shingleFillerToken s
      ]
    TokenFilterDefinitionStemmer lang -> object
      [ "type" .= ("stemmer" :: Text)
      , "language" .= languageToText lang
      ]
    TokenFilterDefinitionStop stop -> object
      [ "type" .= ("stop" :: Text)
      , "stopwords" .= case stop of
          Left lang -> String $ "_" <> languageToText lang <> "_"
          Right stops -> toJSON stops
      ]

instance FromJSON TokenFilterDefinition where
  parseJSON = withObject "TokenFilterDefinition" $ \m -> do
    t <- m .: "type"
    case (t :: Text) of
      "reverse" -> return TokenFilterDefinitionReverse
      "apostrophe" -> return TokenFilterDefinitionApostrophe
      "lowercase" -> TokenFilterDefinitionLowercase
        <$> m .:? "language"
      "uppercase" -> TokenFilterDefinitionUppercase
        <$> m .:? "language"
      "snowball" -> TokenFilterDefinitionSnowball
        <$> m .: "language"
      "shingle" -> fmap TokenFilterDefinitionShingle $ Shingle
        <$> (fmap.fmap) unStringlyTypedInt (m .:? "max_shingle_size") .!= 2
        <*> (fmap.fmap) unStringlyTypedInt (m .:? "min_shingle_size") .!= 2
        <*> (fmap.fmap) unStringlyTypedBool (m .:? "output_unigrams") .!= True
        <*> (fmap.fmap) unStringlyTypedBool (m .:? "output_unigrams_if_no_shingles") .!= False
        <*> m .:? "token_separator" .!= " "
        <*> m .:? "filler_token" .!= "_"
      "stemmer" -> TokenFilterDefinitionStemmer
        <$> m .: "language"
      "stop" -> do
        stop <- m .: "stopwords"
        stop' <- case stop of
          String lang ->
              fmap Left
            . parseJSON
            . String
            . T.drop 1
            . T.dropEnd 1 $ lang
          _ -> Right <$> parseJSON stop
        return (TokenFilterDefinitionStop stop')
      _ -> fail ("unrecognized token filter type: " ++ T.unpack t)

-- | The set of languages that can be passed to various analyzers,
--   filters, etc. in Elasticsearch. Most data types in this module
--   that have a 'Language' field are actually only actually to
--   handle a subset of these languages. Consult the official
--   Elasticsearch documentation to see what is actually supported.
data Language
  = Arabic
  | Armenian
  | Basque
  | Bengali
  | Brazilian
  | Bulgarian
  | Catalan
  | Cjk
  | Czech
  | Danish
  | Dutch
  | English
  | Finnish
  | French
  | Galician
  | German
  | German2
  | Greek
  | Hindi
  | Hungarian
  | Indonesian
  | Irish
  | Italian
  | Kp
  | Latvian
  | Lithuanian
  | Lovins
  | Norwegian
  | Persian
  | Porter
  | Portuguese
  | Romanian
  | Russian
  | Sorani
  | Spanish
  | Swedish
  | Thai
  | Turkish
  deriving (Eq, Show)

instance ToJSON Language where
  toJSON = String . languageToText

instance FromJSON Language where
  parseJSON = withText "Language" $ \t -> case languageFromText t of
    Nothing -> fail "not a supported Elasticsearch language"
    Just lang -> return lang

languageToText :: Language -> Text
languageToText x = case x of
  Arabic -> "arabic"
  Armenian -> "armenian"
  Basque -> "basque"
  Bengali -> "bengali"
  Brazilian -> "brazilian"
  Bulgarian -> "bulgarian"
  Catalan -> "catalan"
  Cjk -> "cjk"
  Czech -> "czech"
  Danish -> "danish"
  Dutch -> "dutch"
  English -> "english"
  Finnish -> "finnish"
  French -> "french"
  Galician -> "galician"
  German -> "german"
  German2 -> "german2"
  Greek -> "greek"
  Hindi -> "hindi"
  Hungarian -> "hungarian"
  Indonesian -> "indonesian"
  Irish -> "irish"
  Italian -> "italian"
  Kp -> "kp"
  Latvian -> "latvian"
  Lithuanian -> "lithuanian"
  Lovins -> "lovins"
  Norwegian -> "norwegian"
  Persian -> "persian"
  Porter -> "porter"
  Portuguese -> "portuguese"
  Romanian -> "romanian"
  Russian -> "russian"
  Sorani -> "sorani"
  Spanish -> "spanish"
  Swedish -> "swedish"
  Thai -> "thai"
  Turkish -> "turkish"

languageFromText :: Text -> Maybe Language
languageFromText x = case x of
  "arabic" -> Just Arabic
  "armenian" -> Just Armenian
  "basque" -> Just Basque
  "bengali" -> Just Bengali
  "brazilian" -> Just Brazilian
  "bulgarian" -> Just Bulgarian
  "catalan" -> Just Catalan
  "cjk" -> Just Cjk
  "czech" -> Just Czech
  "danish" -> Just Danish
  "dutch" -> Just Dutch
  "english" -> Just English
  "finnish" -> Just Finnish
  "french" -> Just French
  "galician" -> Just Galician
  "german" -> Just German
  "german2" -> Just German2
  "greek" -> Just Greek
  "hindi" -> Just Hindi
  "hungarian" -> Just Hungarian
  "indonesian" -> Just Indonesian
  "irish" -> Just Irish
  "italian" -> Just Italian
  "kp" -> Just Kp
  "latvian" -> Just Latvian
  "lithuanian" -> Just Lithuanian
  "lovins" -> Just Lovins
  "norwegian" -> Just Norwegian
  "persian" -> Just Persian
  "porter" -> Just Porter
  "portuguese" -> Just Portuguese
  "romanian" -> Just Romanian
  "russian" -> Just Russian
  "sorani" -> Just Sorani
  "spanish" -> Just Spanish
  "swedish" -> Just Swedish
  "thai" -> Just Thai
  "turkish" -> Just Turkish
  _ -> Nothing

data Shingle = Shingle
  { shingleMaxSize :: Int
  , shingleMinSize :: Int
  , shingleOutputUnigrams :: Bool
  , shingleOutputUnigramsIfNoShingles :: Bool
  , shingleTokenSeparator :: Text
  , shingleFillerToken :: Text
  } deriving (Eq, Show)
