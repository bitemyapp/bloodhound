{-# LANGUAGE OverloadedStrings #-}
module Database.Bloodhound.Internal.Count
  (CountQuery (..), CountResponse(..), CountShards(..))
where

import           Data.Aeson
import           Database.Bloodhound.Internal.Query
import           Numeric.Natural

newtype CountQuery = CountQuery { countQuery :: Query }
  deriving (Eq, Show)

instance ToJSON CountQuery where
  toJSON (CountQuery q) =
    object ["query" .= q]

data CountResponse = CountResponse { crCount  :: Natural
                                   , crShards :: CountShards
                                   }
  deriving (Eq, Show)

instance FromJSON CountResponse where
  parseJSON =
    withObject "CountResponse"
    $ \o ->
        CountResponse
        <$> o .: "count"
        <*> o .: "_shards"

data CountShards = CountShards { csTotal      :: Int
                               , csSuccessful :: Int
                               , csSkipped    :: Int
                               , csFailed     :: Int
                               }
  deriving (Eq, Show)

instance FromJSON CountShards where
  parseJSON =
    withObject "CountShards"
    $ \o ->
        CountShards
        <$> o .: "total"
        <*> o .: "successful"
        <*> o .: "skipped"
        <*> o .: "failed"
