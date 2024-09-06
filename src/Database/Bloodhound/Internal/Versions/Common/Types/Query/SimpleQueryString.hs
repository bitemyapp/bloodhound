{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Bloodhound.Internal.Versions.Common.Types.Query.SimpleQueryString
  ( FieldOrFields (..),
    SimpleQueryFlag (..),
    SimpleQueryStringQuery (..),
  )
where

import Database.Bloodhound.Internal.Utils.Imports
import Database.Bloodhound.Internal.Versions.Common.Types.Newtypes
import Database.Bloodhound.Internal.Versions.Common.Types.Query.Commons
import GHC.Generics

data SimpleQueryStringQuery = SimpleQueryStringQuery
  { simpleQueryStringQuery :: QueryString,
    simpleQueryStringField :: Maybe FieldOrFields,
    simpleQueryStringOperator :: Maybe BooleanOperator,
    simpleQueryStringAnalyzer :: Maybe Analyzer,
    simpleQueryStringFlags :: Maybe (NonEmpty SimpleQueryFlag),
    simpleQueryStringLowercaseExpanded :: Maybe LowercaseExpanded,
    simpleQueryStringLocale :: Maybe Locale
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON SimpleQueryStringQuery where
  toJSON SimpleQueryStringQuery {..} =
    omitNulls (base ++ maybeAdd)
    where
      base = ["query" .= simpleQueryStringQuery]
      maybeAdd =
        [ "fields" .= simpleQueryStringField,
          "default_operator" .= simpleQueryStringOperator,
          "analyzer" .= simpleQueryStringAnalyzer,
          "flags" .= simpleQueryStringFlags,
          "lowercase_expanded_terms" .= simpleQueryStringLowercaseExpanded,
          "locale" .= simpleQueryStringLocale
        ]

instance FromJSON SimpleQueryStringQuery where
  parseJSON = withObject "SimpleQueryStringQuery" parse
    where
      parse o =
        SimpleQueryStringQuery
          <$> o .: "query"
          <*> o .:? "fields"
          <*> o .:? "default_operator"
          <*> o .:? "analyzer"
          <*> (parseFlags <$> o .:? "flags")
          <*> o .:? "lowercase_expanded_terms"
          <*> o .:? "locale"
      parseFlags (Just (x : xs)) = Just (x :| xs)
      parseFlags _ = Nothing

data SimpleQueryFlag
  = SimpleQueryAll
  | SimpleQueryNone
  | SimpleQueryAnd
  | SimpleQueryOr
  | SimpleQueryPrefix
  | SimpleQueryPhrase
  | SimpleQueryPrecedence
  | SimpleQueryEscape
  | SimpleQueryWhitespace
  | SimpleQueryFuzzy
  | SimpleQueryNear
  | SimpleQuerySlop
  deriving stock (Eq, Show, Generic)

instance ToJSON SimpleQueryFlag where
  toJSON SimpleQueryAll = "ALL"
  toJSON SimpleQueryNone = "NONE"
  toJSON SimpleQueryAnd = "AND"
  toJSON SimpleQueryOr = "OR"
  toJSON SimpleQueryPrefix = "PREFIX"
  toJSON SimpleQueryPhrase = "PHRASE"
  toJSON SimpleQueryPrecedence = "PRECEDENCE"
  toJSON SimpleQueryEscape = "ESCAPE"
  toJSON SimpleQueryWhitespace = "WHITESPACE"
  toJSON SimpleQueryFuzzy = "FUZZY"
  toJSON SimpleQueryNear = "NEAR"
  toJSON SimpleQuerySlop = "SLOP"

instance FromJSON SimpleQueryFlag where
  parseJSON = withText "SimpleQueryFlag" parse
    where
      parse "ALL" = pure SimpleQueryAll
      parse "NONE" = pure SimpleQueryNone
      parse "AND" = pure SimpleQueryAnd
      parse "OR" = pure SimpleQueryOr
      parse "PREFIX" = pure SimpleQueryPrefix
      parse "PHRASE" = pure SimpleQueryPhrase
      parse "PRECEDENCE" = pure SimpleQueryPrecedence
      parse "ESCAPE" = pure SimpleQueryEscape
      parse "WHITESPACE" = pure SimpleQueryWhitespace
      parse "FUZZY" = pure SimpleQueryFuzzy
      parse "NEAR" = pure SimpleQueryNear
      parse "SLOP" = pure SimpleQuerySlop
      parse f = fail ("Unexpected SimpleQueryFlag: " <> show f)

data FieldOrFields
  = FofField FieldName
  | FofFields (NonEmpty FieldName)
  deriving stock (Eq, Show, Generic)

instance ToJSON FieldOrFields where
  toJSON (FofField fieldName) =
    toJSON fieldName
  toJSON (FofFields fieldNames) =
    toJSON fieldNames

instance FromJSON FieldOrFields where
  parseJSON v =
    FofField <$> parseJSON v
      <|> FofFields <$> (parseNEJSON =<< parseJSON v)
