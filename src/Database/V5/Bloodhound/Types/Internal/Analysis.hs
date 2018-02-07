{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Database.V5.Bloodhound.Types.Internal.Analysis where

import           Data.Aeson
import           Data.Aeson.Types (Parser)
import qualified Data.Map.Strict as M
import           Data.Maybe         (catMaybes)
import           Data.Text          (Text)

import           Database.V5.Bloodhound.Types.Internal.StringlyTyped

data Analysis = Analysis
  { analysisAnalyzer :: M.Map Text AnalyzerDefinition
  , analysisTokenizer :: M.Map Text TokenizerDefinition
  } deriving (Eq, Show)

instance ToJSON Analysis where
  toJSON (Analysis analyzer tokenizer) = object
    [ "analyzer" .= analyzer
    , "tokenizer" .= tokenizer
    ]

instance FromJSON Analysis where
  parseJSON = withObject "Analysis" $ \m -> Analysis
    <$> m .: "analyzer"
    <*> m .: "tokenizer"

newtype Tokenizer =
  Tokenizer Text
  deriving (Eq, Show, ToJSON, FromJSON)

data AnalyzerDefinition = AnalyzerDefinition
  { analyzerDefinitionTokenizer :: Maybe Tokenizer
  } deriving (Eq,Show)

instance ToJSON AnalyzerDefinition where
  toJSON (AnalyzerDefinition tokenizer) = object $ catMaybes
    [ fmap ("tokenizer" .=) tokenizer
    ]

instance FromJSON AnalyzerDefinition where
  parseJSON = withObject "AnalyzerDefinition" $ \m -> AnalyzerDefinition
    <$> m .:? "tokenizer"
  

data TokenizerDefinition
  = TokenizerDefinitionNgram Ngram
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
        <$> (fmap unStringlyTypedInt (m .: "min_gram"))
        <*> (fmap unStringlyTypedInt (m .: "max_gram"))
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
