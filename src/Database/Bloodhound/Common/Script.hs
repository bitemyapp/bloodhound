{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Database.Bloodhound.Common.Script where

import           Bloodhound.Import

import           Data.Aeson.KeyMap
import           GHC.Generics

import           Database.Bloodhound.Internal.Newtypes

newtype ScriptFields =
  ScriptFields (KeyMap ScriptFieldValue)
  deriving (Eq, Show)

type ScriptFieldValue = Value

data ScriptSource = ScriptId Text
  | ScriptInline Text deriving (Eq, Show, Generic)

data Script =
  Script { scriptLanguage :: Maybe ScriptLanguage
         , scriptSource   :: ScriptSource
         , scriptParams   :: Maybe ScriptParams
         } deriving (Eq, Show, Generic)

newtype ScriptLanguage =
  ScriptLanguage Text deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype ScriptParams =
  ScriptParams (KeyMap ScriptParamValue)
  deriving (Eq, Show)

type ScriptParamValue = Value

data BoostMode =
    BoostModeMultiply
  | BoostModeReplace
  | BoostModeSum
  | BoostModeAvg
  | BoostModeMax
  | BoostModeMin deriving (Eq, Show, Generic)

data ScoreMode =
    ScoreModeMultiply
  | ScoreModeSum
  | ScoreModeAvg
  | ScoreModeFirst
  | ScoreModeMax
  | ScoreModeMin deriving (Eq, Show, Generic)

data FunctionScoreFunction =
    FunctionScoreFunctionScript Script
  | FunctionScoreFunctionRandom Seed
  | FunctionScoreFunctionFieldValueFactor FieldValueFactor
  deriving (Eq, Show, Generic)

newtype Weight =
  Weight Float deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype Seed =
  Seed Float deriving (Eq, Show, Generic, FromJSON, ToJSON)

data FieldValueFactor =
  FieldValueFactor { fieldValueFactorField    :: FieldName
                   , fieldValueFactor         :: Maybe Factor
                   , fieldValueFactorModifier :: Maybe FactorModifier
                   , fieldValueFactorMissing  :: Maybe FactorMissingFieldValue
                   } deriving (Eq, Show, Generic)

newtype Factor =
  Factor Float deriving (Eq, Show, Generic, FromJSON, ToJSON)

data FactorModifier =
  FactorModifierNone
  | FactorModifierLog
  | FactorModifierLog1p
  | FactorModifierLog2p
  | FactorModifierLn
  | FactorModifierLn1p
  | FactorModifierLn2p
  | FactorModifierSquare
  | FactorModifierSqrt
  | FactorModifierReciprocal deriving (Eq, Show, Generic)

newtype FactorMissingFieldValue =
  FactorMissingFieldValue Float deriving (Eq, Show, Generic, FromJSON, ToJSON)

instance ToJSON BoostMode where
  toJSON BoostModeMultiply = "multiply"
  toJSON BoostModeReplace  = "replace"
  toJSON BoostModeSum      = "sum"
  toJSON BoostModeAvg      = "avg"
  toJSON BoostModeMax      = "max"
  toJSON BoostModeMin      = "min"

instance FromJSON BoostMode where
  parseJSON = withText "BoostMode" parse
    where parse "multiply" = pure BoostModeMultiply
          parse "replace"  = pure BoostModeReplace
          parse "sum"      = pure BoostModeSum
          parse "avg"      = pure BoostModeAvg
          parse "max"      = pure BoostModeMax
          parse "min"      = pure BoostModeMin
          parse bm         = fail ("Unexpected BoostMode: " <> show bm)

instance ToJSON ScoreMode where
  toJSON ScoreModeMultiply = "multiply"
  toJSON ScoreModeSum      = "sum"
  toJSON ScoreModeFirst    = "first"
  toJSON ScoreModeAvg      = "avg"
  toJSON ScoreModeMax      = "max"
  toJSON ScoreModeMin      = "min"

instance FromJSON ScoreMode where
  parseJSON = withText "ScoreMode" parse
    where parse "multiply" = pure ScoreModeMultiply
          parse "sum"      = pure ScoreModeSum
          parse "first"    = pure ScoreModeFirst
          parse "avg"      = pure ScoreModeAvg
          parse "max"      = pure ScoreModeMax
          parse "min"      = pure ScoreModeMin
          parse sm         = fail ("Unexpected ScoreMode: " <> show sm)

functionScoreFunctionPair :: FunctionScoreFunction -> (Key, Value)
functionScoreFunctionPair (FunctionScoreFunctionScript functionScoreScript) =
  ("script_score", toJSON functionScoreScript)
functionScoreFunctionPair (FunctionScoreFunctionRandom seed) =
  ("random_score", omitNulls [ "seed" .= seed ])
functionScoreFunctionPair (FunctionScoreFunctionFieldValueFactor fvf) =
  ("field_value_factor", toJSON fvf)

parseFunctionScoreFunction :: Object -> Parser FunctionScoreFunction
parseFunctionScoreFunction o =
  singleScript `taggedWith` "script_score"
  <|> singleRandom `taggedWith` "random_score"
  <|> singleFieldValueFactor `taggedWith` "field_value_factor"
  where taggedWith parser k = parser =<< o .: k
        singleScript = pure . FunctionScoreFunctionScript
        singleRandom o' = FunctionScoreFunctionRandom <$> o' .: "seed"
        singleFieldValueFactor = pure . FunctionScoreFunctionFieldValueFactor

instance ToJSON ScriptFields where
  toJSON (ScriptFields x) = Object x

instance FromJSON ScriptFields where
  parseJSON (Object o) = pure (ScriptFields o)
  parseJSON _          = fail "error parsing ScriptFields"

instance ToJSON Script where
  toJSON script =
    object [ "script" .= omitNulls (base script) ]
    where
      base (Script lang (ScriptInline source) params) =
        ["lang" .= lang, "source" .= source, "params" .= params]
      base (Script lang (ScriptId id_) params) =
        ["lang" .= lang, "id" .= id_, "params" .= params]

instance FromJSON Script where
  parseJSON = withObject "Script" parse
    where 
      parseSource o = do
        inline <- o .:? "source"
        id_ <- o .:? "id"
        return $ case (inline,id_) of
          (Just x,Nothing) -> ScriptInline x
          (Nothing,Just x) -> ScriptId x
          (Nothing,Nothing) -> error "Script has to be either stored or inlined"
          (Just _,Just _) -> error "Script can't both be stored and inlined at the same time"
      parse o = o .: "script" >>= \o' -> Script
        <$> o' .:? "lang"
        <*> parseSource o'
        <*> o' .:? "params"

instance ToJSON ScriptParams where
  toJSON (ScriptParams x) = Object x

instance FromJSON ScriptParams where
  parseJSON (Object o) = pure (ScriptParams o)
  parseJSON _          = fail "error parsing ScriptParams"

instance ToJSON FieldValueFactor where
  toJSON (FieldValueFactor field factor modifier missing) =
    omitNulls base
    where base = [ "field"    .= field
                 , "factor"   .= factor
                 , "modifier" .= modifier
                 , "missing"  .= missing ]

instance FromJSON FieldValueFactor where
  parseJSON = withObject "FieldValueFactor" parse
    where parse o = FieldValueFactor
                    <$> o .: "field"
                    <*> o .:? "factor"
                    <*> o .:? "modifier"
                    <*> o .:? "missing"

instance ToJSON FactorModifier where
  toJSON FactorModifierNone       = "none"
  toJSON FactorModifierLog        = "log"
  toJSON FactorModifierLog1p      = "log1p"
  toJSON FactorModifierLog2p      = "log2p"
  toJSON FactorModifierLn         = "ln"
  toJSON FactorModifierLn1p       = "ln1p"
  toJSON FactorModifierLn2p       = "ln2p"
  toJSON FactorModifierSquare     = "square"
  toJSON FactorModifierSqrt       = "sqrt"
  toJSON FactorModifierReciprocal = "reciprocal"

instance FromJSON FactorModifier where
  parseJSON = withText "FactorModifier" parse
    where parse "none"       = pure FactorModifierNone
          parse "log"        = pure FactorModifierLog
          parse "log1p"      = pure FactorModifierLog1p
          parse "log2p"      = pure FactorModifierLog2p
          parse "ln"         = pure FactorModifierLn
          parse "ln1p"       = pure FactorModifierLn1p
          parse "ln2p"       = pure FactorModifierLn2p
          parse "square"     = pure FactorModifierSquare
          parse "sqrt"       = pure FactorModifierSqrt
          parse "reciprocal" = pure FactorModifierReciprocal
          parse fm           = fail ("Unexpected FactorModifier: " <> show fm)
