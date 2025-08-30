{-# LANGUAGE OverloadedStrings #-}

module Database.Bloodhound.Internal.Versions.Common.Types.Query.Prefix
  ( PrefixQuery (..),

    -- * Optics
    prefixQueryFieldLens,
    prefixQueryPrefixValueLens,
    prefixQueryBoostLens,
  )
where

import Database.Bloodhound.Internal.Utils.Imports
import Database.Bloodhound.Internal.Versions.Common.Types.Newtypes
import Database.Bloodhound.Internal.Versions.Common.Types.Query.Commons
import GHC.Generics

data PrefixQuery = PrefixQuery
  { prefixQueryField :: FieldName,
    prefixQueryPrefixValue :: Text,
    prefixQueryBoost :: Maybe Boost
  }
  deriving stock (Eq, Show, Generic)

prefixQueryFieldLens :: Lens' PrefixQuery FieldName
prefixQueryFieldLens = lens prefixQueryField (\x y -> x {prefixQueryField = y})

prefixQueryPrefixValueLens :: Lens' PrefixQuery Text
prefixQueryPrefixValueLens = lens prefixQueryPrefixValue (\x y -> x {prefixQueryPrefixValue = y})

prefixQueryBoostLens :: Lens' PrefixQuery (Maybe Boost)
prefixQueryBoostLens = lens prefixQueryBoost (\x y -> x {prefixQueryBoost = y})

instance ToJSON PrefixQuery where
  toJSON (PrefixQuery (FieldName fieldName) queryValue boost) =
    object [fromText fieldName .= omitNulls base]
    where
      base =
        [ "value" .= queryValue,
          "boost" .= boost
        ]

instance FromJSON PrefixQuery where
  parseJSON = withObject "PrefixQuery" parse
    where
      parse = fieldTagged $ \fn o ->
        PrefixQuery fn
          <$> o .: "value"
          <*> o .:? "boost"
