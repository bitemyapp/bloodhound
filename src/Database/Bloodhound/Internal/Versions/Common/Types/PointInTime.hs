{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Bloodhound.Internal.Versions.Common.Types.PointInTime where

import Database.Bloodhound.Internal.Utils.Imports

data PointInTime = PointInTime
  { pointInTimeId :: Text,
    pointInTimeKeepAlive :: Text
  }
  deriving (Eq, Show)

instance ToJSON PointInTime where
  toJSON PointInTime {..} =
    object
      [ "id" .= pointInTimeId,
        "keep_alive" .= pointInTimeKeepAlive
      ]

instance FromJSON PointInTime where
  parseJSON (Object o) = PointInTime <$> o .: "id" <*> o .: "keep_alive"
  parseJSON x = typeMismatch "PointInTime" x

pointInTimeIdLens :: Lens' PointInTime Text
pointInTimeIdLens = lens pointInTimeId (\x y -> x { pointInTimeId = y })

pointInTimeKeepAliveLens :: Lens' PointInTime Text
pointInTimeKeepAliveLens = lens pointInTimeKeepAlive (\x y -> x { pointInTimeKeepAlive = y })
