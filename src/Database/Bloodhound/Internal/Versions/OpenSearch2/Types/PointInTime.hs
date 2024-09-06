{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Bloodhound.Internal.Versions.OpenSearch2.Types.PointInTime where

import Database.Bloodhound.Internal.Utils.Imports
import Database.Bloodhound.Internal.Versions.Common.Types.Nodes (ShardResult)

data OpenPointInTimeResponse = OpenPointInTimeResponse
  { oos2PitId :: Text,
    oos2Shards :: ShardResult,
    oos2CreationTime :: POSIXTime
  }
  deriving stock (Eq, Show)

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

oos2PitIdLens :: Lens' OpenPointInTimeResponse Text
oos2PitIdLens = lens oos2PitId (\x y -> x { oos2PitId = y })

oos2ShardsLens :: Lens' OpenPointInTimeResponse ShardResult
oos2ShardsLens = lens oos2Shards (\x y -> x { oos2Shards = y })

oos2CreationTimeLens :: Lens' OpenPointInTimeResponse POSIXTime
oos2CreationTimeLens = lens oos2CreationTime (\x y -> x { oos2CreationTime = y })

data ClosePointInTime = ClosePointInTime
  { cPitId :: Text
  }
  deriving stock (Eq, Show)

instance ToJSON ClosePointInTime where
  toJSON ClosePointInTime {..} =
    object ["id" .= cPitId]

instance FromJSON ClosePointInTime where
  parseJSON (Object o) = ClosePointInTime <$> o .: "id"
  parseJSON x = typeMismatch "ClosePointInTime" x

cPitIdLens :: Lens' ClosePointInTime Text
cPitIdLens = lens cPitId (\x y -> x { cPitId = y })

data ClosePointInTimeResponse = ClosePointInTimeResponse
  { succeeded :: Bool,
    numFreed :: Int
  }
  deriving stock (Eq, Show)

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

succeededLens :: Lens' ClosePointInTimeResponse Bool
succeededLens = lens succeeded (\x y -> x { succeeded = y })

numFreedLens :: Lens' ClosePointInTimeResponse Int
numFreedLens = lens numFreed (\x y -> x { numFreed = y })
