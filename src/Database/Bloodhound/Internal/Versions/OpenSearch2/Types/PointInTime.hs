{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Bloodhound.Internal.Versions.OpenSearch2.Types.PointInTime where

import Database.Bloodhound.Client.Cluster (ShardResult)
import Database.Bloodhound.Internal.Utils.Imports

data OpenPointInTimeResponse = OpenPointInTimeResponse
  { oos2PitId :: Text,
    oos2Shards :: ShardResult,
    oos2CreationTime :: POSIXTime
  }
  deriving (Eq, Show)

instance ToJSON OpenPointInTimeResponse where
  toJSON OpenPointInTimeResponse {..} =
    object ["pit_id" .= oos2PitId, "_shards" .= oos2Shards, "creation_time" .= oos2CreationTime]

instance FromJSON OpenPointInTimeResponse where
  parseJSON (Object o) =
    OpenPointInTimeResponse
      <$> o .: "pit_id"
      <*> o .: "_shards"
      <*> o .: "creation_time"
  parseJSON x = typeMismatch "OpenPointInTimeResponse" x

data ClosePointInTime = ClosePointInTime
  { cPitId :: Text
  }
  deriving (Eq, Show)

instance ToJSON ClosePointInTime where
  toJSON ClosePointInTime {..} =
    object ["id" .= cPitId]

instance FromJSON ClosePointInTime where
  parseJSON (Object o) = ClosePointInTime <$> o .: "id"
  parseJSON x = typeMismatch "ClosePointInTime" x

data ClosePointInTimeResponse = ClosePointInTimeResponse
  { succeeded :: Bool,
    numFreed :: Int
  }
  deriving (Eq, Show)

instance ToJSON ClosePointInTimeResponse where
  toJSON ClosePointInTimeResponse {..} =
    object
      [ "succeeded" .= succeeded,
        "num_freed" .= numFreed
      ]

instance FromJSON ClosePointInTimeResponse where
  parseJSON (Object o) = do
    succeeded' <- o .: "succeeded"
    numFreed' <- o .: "num_freed"
    return $ ClosePointInTimeResponse succeeded' numFreed'
  parseJSON x = typeMismatch "ClosePointInTimeResponse" x
