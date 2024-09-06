{-# LANGUAGE OverloadedStrings #-}

module Database.Bloodhound.Internal.Versions.Common.Types.Count
  ( CountQuery (..),
    CountResponse (..),
    CountShards (..),

    -- * Optics
    crCountLens,
    crShardsLens,
    csTotalLens,
    csSuccessfulLens,
    csFailedLens,
  )
where

import Data.Aeson
import Database.Bloodhound.Internal.Versions.Common.Types.Query
import Numeric.Natural
import Optics.Lens

newtype CountQuery = CountQuery {countQuery :: Query}
  deriving stock (Eq, Show)

instance ToJSON CountQuery where
  toJSON (CountQuery q) =
    object ["query" .= q]

data CountResponse = CountResponse
  { crCount :: Natural,
    crShards :: CountShards
  }
  deriving stock (Eq, Show)

instance FromJSON CountResponse where
  parseJSON =
    withObject "CountResponse" $
      \o ->
        CountResponse
          <$> o
            .: "count"
          <*> o
            .: "_shards"

crCountLens :: Lens' CountResponse Natural
crCountLens = lens crCount (\x y -> x {crCount = y})

crShardsLens :: Lens' CountResponse CountShards
crShardsLens = lens crShards (\x y -> x {crShards = y})

data CountShards = CountShards
  { csTotal :: Int,
    csSuccessful :: Int,
    csFailed :: Int
  }
  deriving stock (Eq, Show)

instance FromJSON CountShards where
  parseJSON =
    withObject "CountShards" $
      \o ->
        CountShards
          <$> o
            .: "total"
          <*> o
            .: "successful"
          <*> o
            .: "failed"

csTotalLens :: Lens' CountShards Int
csTotalLens = lens csTotal (\x y -> x {csTotal = y})

csSuccessfulLens :: Lens' CountShards Int
csSuccessfulLens = lens csSuccessful (\x y -> x {csSuccessful = y})

csFailedLens :: Lens' CountShards Int
csFailedLens = lens csFailed (\x y -> x {csFailed = y})
