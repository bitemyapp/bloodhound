{-# LANGUAGE OverloadedStrings #-}

module Database.Bloodhound.Internal.Versions.Common.Types.Count
  ( CountQuery (..),
    CountResponse (..),
    CountShards (..),

    -- * Optics
    countResponseCountLens,
    countResponseShardsLens,
    countShardsTotalLens,
    countShardsSuccessfulLens,
    countShardsFailedLens,
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

countResponseCountLens :: Lens' CountResponse Natural
countResponseCountLens = lens crCount (\x y -> x {crCount = y})

countResponseShardsLens :: Lens' CountResponse CountShards
countResponseShardsLens = lens crShards (\x y -> x {crShards = y})

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

countShardsTotalLens :: Lens' CountShards Int
countShardsTotalLens = lens csTotal (\x y -> x {csTotal = y})

countShardsSuccessfulLens :: Lens' CountShards Int
countShardsSuccessfulLens = lens csSuccessful (\x y -> x {csSuccessful = y})

countShardsFailedLens :: Lens' CountShards Int
countShardsFailedLens = lens csFailed (\x y -> x {csFailed = y})
