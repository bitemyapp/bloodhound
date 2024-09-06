{-# LANGUAGE OverloadedStrings #-}

module Database.Bloodhound.Internal.Versions.Common.Types.Query.Wildcard
  ( WildcardQuery (..),
  )
where

import Database.Bloodhound.Internal.Utils.Imports
import Database.Bloodhound.Internal.Versions.Common.Types.Newtypes
import Database.Bloodhound.Internal.Versions.Common.Types.Query.Commons
import GHC.Generics

data WildcardQuery = WildcardQuery
  { wildcardQueryField :: FieldName,
    wildcardQuery :: Key,
    wildcardQueryBoost :: Maybe Boost
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON WildcardQuery where
  toJSON
    ( WildcardQuery
        (FieldName wcQueryField)
        wcQueryQuery
        wcQueryBoost
      ) =
      object [fromText wcQueryField .= omitNulls base]
      where
        base =
          [ "value" .= wcQueryQuery,
            "boost" .= wcQueryBoost
          ]

instance FromJSON WildcardQuery where
  parseJSON = withObject "WildcardQuery" parse
    where
      parse = fieldTagged $ \fn o ->
        WildcardQuery fn
          <$> o .: "value"
          <*> o .:? "boost"
