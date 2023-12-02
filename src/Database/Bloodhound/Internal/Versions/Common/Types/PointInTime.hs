{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Bloodhound.Internal.Versions.Common.Types.PointInTime where

import Database.Bloodhound.Internal.Utils.Imports

data PointInTime = PointInTime
  { pPitId :: Text,
    keepAlive :: Text
  }
  deriving (Eq, Show)

instance ToJSON PointInTime where
  toJSON PointInTime {..} =
    object
      [ "id" .= pPitId,
        "keep_alive" .= keepAlive
      ]

instance FromJSON PointInTime where
  parseJSON (Object o) = PointInTime <$> o .: "id" <*> o .: "keep_alive"
  parseJSON x = typeMismatch "PointInTime" x
