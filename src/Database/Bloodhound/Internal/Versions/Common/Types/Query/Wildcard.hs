{-# LANGUAGE OverloadedStrings #-}

module Database.Bloodhound.Internal.Versions.Common.Types.Query.Wildcard
  ( WildcardQuery (..),

    -- * Optics
    wildcardQueryFieldLens,
    wildcardQueryLens,
    wildcardQueryBoostLens,
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

wildcardQueryFieldLens :: Lens' WildcardQuery FieldName
wildcardQueryFieldLens = lens wildcardQueryField (\x y -> x {wildcardQueryField = y})

wildcardQueryLens :: Lens' WildcardQuery Key
wildcardQueryLens = lens wildcardQuery (\x y -> x {wildcardQuery = y})

wildcardQueryBoostLens :: Lens' WildcardQuery (Maybe Boost)
wildcardQueryBoostLens = lens wildcardQueryBoost (\x y -> x {wildcardQueryBoost = y})

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
