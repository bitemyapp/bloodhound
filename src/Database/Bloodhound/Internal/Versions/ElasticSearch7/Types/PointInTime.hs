{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Bloodhound.Internal.Versions.ElasticSearch7.Types.PointInTime where

import Database.Bloodhound.Internal.Utils.Imports

data OpenPointInTimeResponse = OpenPointInTimeResponse
  { oPitId :: Text
  }
  deriving (Eq, Show)

instance ToJSON OpenPointInTimeResponse where
  toJSON OpenPointInTimeResponse {..} =
    object ["id" .= oPitId]

instance FromJSON OpenPointInTimeResponse where
  parseJSON (Object o) = OpenPointInTimeResponse <$> o .: "id"
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
