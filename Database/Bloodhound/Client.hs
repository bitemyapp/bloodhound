{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Bloodhound.Client where

import Control.Applicative
import Control.Monad (liftM)
import Data.Aeson
import Data.Aeson.TH (deriveJSON)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T
import Data.Time.Clock as DTC
import GHC.Generics (Generic)
import Network.HTTP.Conduit

data Version = Version { number          :: T.Text
                       , build_hash      :: T.Text
                       , build_timestamp :: DTC.UTCTime
                       , build_snapshot  :: Bool
                       , lucene_version  :: T.Text } deriving (Show, Generic)

data (FromJSON a, ToJSON a) => Status a =
                     Status { ok      :: Bool
                            , status  :: Int
                            , name    :: T.Text
                            , version :: a
                            , tagline :: T.Text } deriving (Show)

instance ToJSON Version
instance FromJSON Version

instance (FromJSON a, ToJSON a) => FromJSON (Status a) where
  parseJSON (Object v) = Status <$>
                         v .: "ok" <*>
                         v .: "status" <*>
                         v .: "name" <*>
                         v .: "version" <*>
                         v .: "tagline"
  parseJSON _          = empty

-- instance ToJSON (Status a)
-- instance FromJSON (Status a)

muhServer = "http://localhost:9200"

-- bloo <- (liftM responseBody $ parseUrl "http://localhost:9200/" >>= \r -> withManager (httpLbs r))

rootPath :: String
rootPath = rollUp $ ["/"]

-- Kinda hate this.
pathFromType :: String -> String -> String
pathFromType index docType = "/" ++ index ++ docType

-- Kinda hate this too.
rollUp :: [String] -> String
rollUp = Prelude.concat

getStatus :: String -> IO (Maybe (Status Version))
getStatus server = do
  request <- parseUrl $ server ++ rootPath
  response <- withManager $ httpLbs request
  return $ (decode $ responseBody response)

main = simpleHttp "http://localhost:9200/events/event/_search?q=hostname:localhost&size=1" >>= L.putStrLn
