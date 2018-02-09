{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Database.Bloodhound.Common.Script where

import Bloodhound.Import

import qualified Data.HashMap.Strict as HM

import           Database.V5.Bloodhound.Internal.Newtypes

newtype ScriptFields =
  ScriptFields (HM.HashMap ScriptFieldName ScriptFieldValue)
  deriving (Eq, Show)

type ScriptFieldName = Text
type ScriptFieldValue = Value

data Script =
  Script { scriptLanguage :: Maybe ScriptLanguage
         , scriptInline   :: Maybe ScriptInline
         , scriptStored   :: Maybe ScriptId
         , scriptParams   :: Maybe ScriptParams
         } deriving (Eq, Show)

newtype ScriptLanguage =
  ScriptLanguage Text deriving (Eq, Show, FromJSON, ToJSON)

newtype ScriptInline =
  ScriptInline Text deriving (Eq, Show, FromJSON, ToJSON)

newtype ScriptId =
  ScriptId Text deriving (Eq, Show, FromJSON, ToJSON)

newtype ScriptParams =
  ScriptParams (HM.HashMap ScriptParamName ScriptParamValue)
  deriving (Eq, Show)

type ScriptParamName = Text
type ScriptParamValue = Value

data BoostMode =
    BoostModeMultiply
  | BoostModeReplace
  | BoostModeSum
  | BoostModeAvg
  | BoostModeMax
  | BoostModeMin deriving (Eq, Show)

data ScoreMode =
    ScoreModeMultiply
  | ScoreModeSum
  | ScoreModeAvg
  | ScoreModeFirst
  | ScoreModeMax
  | ScoreModeMin deriving (Eq, Show)

data FunctionScoreFunction =
    FunctionScoreFunctionScript Script
  | FunctionScoreFunctionRandom Seed
  | FunctionScoreFunctionFieldValueFactor FieldValueFactor
  deriving (Eq, Show)

newtype Weight =
  Weight Float deriving (Eq, Show, FromJSON, ToJSON)

newtype Seed =
  Seed Float deriving (Eq, Show, FromJSON, ToJSON)

data FieldValueFactor =
  FieldValueFactor { fieldValueFactorField    :: FieldName
                   , fieldValueFactor         :: Maybe Factor
                   , fieldValueFactorModifier :: Maybe FactorModifier
                   , fieldValueFactorMissing  :: Maybe FactorMissingFieldValue
                   } deriving (Eq, Show)

newtype Factor =
  Factor Float deriving (Eq, Show, FromJSON, ToJSON)

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
  | FactorModifierReciprocal deriving (Eq, Show)

newtype FactorMissingFieldValue =
  FactorMissingFieldValue Float deriving (Eq, Show, FromJSON, ToJSON)

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

functionScoreFunctionPair :: FunctionScoreFunction -> (Text, Value)
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
  toJSON (Script lang inline stored params) =
    object [ "script" .= omitNulls base ]
    where base = [ "lang"   .= lang
                 , "inline" .= inline
                 , "stored" .= stored
                 , "params" .= params ]

instance FromJSON Script where
  parseJSON = withObject "Script" parse
    where parse o = o .: "script" >>= \o' ->
                      Script
                      <$> o' .:? "lang"
                      <*> o' .:? "inline"
                      <*> o' .:? "stored"
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
