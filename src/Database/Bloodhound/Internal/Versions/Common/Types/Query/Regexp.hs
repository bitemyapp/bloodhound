{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Bloodhound.Internal.Versions.Common.Types.Query.Regexp
  ( Regexp (..),
    RegexpFlag (..),
    RegexpFlags (..),
    RegexpQuery (..),
  )
where

import qualified Data.Text as T
import Database.Bloodhound.Internal.Utils.Imports
import Database.Bloodhound.Internal.Versions.Common.Types.Newtypes
import Database.Bloodhound.Internal.Versions.Common.Types.Query.Commons
import GHC.Generics

data RegexpQuery = RegexpQuery
  { regexpQueryField :: FieldName,
    regexpQuery :: Regexp,
    regexpQueryFlags :: RegexpFlags,
    regexpQueryBoost :: Maybe Boost
  }
  deriving (Eq, Show, Generic)

instance ToJSON RegexpQuery where
  toJSON
    ( RegexpQuery
        (FieldName rqQueryField)
        (Regexp regexpQueryQuery)
        rqQueryFlags
        rqQueryBoost
      ) =
      object [fromText rqQueryField .= omitNulls base]
      where
        base =
          [ "value" .= regexpQueryQuery,
            "flags" .= rqQueryFlags,
            "boost" .= rqQueryBoost
          ]

instance FromJSON RegexpQuery where
  parseJSON = withObject "RegexpQuery" parse
    where
      parse = fieldTagged $ \fn o ->
        RegexpQuery fn
          <$> o .: "value"
          <*> o .: "flags"
          <*> o .:? "boost"

newtype Regexp = Regexp Text deriving (Eq, Show, Generic, FromJSON)

data RegexpFlags
  = AllRegexpFlags
  | NoRegexpFlags
  | SomeRegexpFlags (NonEmpty RegexpFlag)
  deriving (Eq, Show, Generic)

instance ToJSON RegexpFlags where
  toJSON AllRegexpFlags = String "ALL"
  toJSON NoRegexpFlags = String "NONE"
  toJSON (SomeRegexpFlags (h :| fs)) = String $ T.intercalate "|" flagStrs
    where
      flagStrs = map flagStr . nub $ h : fs
      flagStr AnyString = "ANYSTRING"
      flagStr Automaton = "AUTOMATON"
      flagStr Complement = "COMPLEMENT"
      flagStr Empty = "EMPTY"
      flagStr Intersection = "INTERSECTION"
      flagStr Interval = "INTERVAL"

instance FromJSON RegexpFlags where
  parseJSON = withText "RegexpFlags" parse
    where
      parse "ALL" = pure AllRegexpFlags
      parse "NONE" = pure NoRegexpFlags
      parse t = SomeRegexpFlags <$> parseNEJSON (String <$> T.splitOn "|" t)

data RegexpFlag
  = AnyString
  | Automaton
  | Complement
  | Empty
  | Intersection
  | Interval
  deriving (Eq, Show, Generic)

instance FromJSON RegexpFlag where
  parseJSON = withText "RegexpFlag" parse
    where
      parse "ANYSTRING" = pure AnyString
      parse "AUTOMATON" = pure Automaton
      parse "COMPLEMENT" = pure Complement
      parse "EMPTY" = pure Empty
      parse "INTERSECTION" = pure Intersection
      parse "INTERVAL" = pure Interval
      parse f = fail ("Unknown RegexpFlag: " <> show f)
