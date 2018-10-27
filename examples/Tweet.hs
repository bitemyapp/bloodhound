{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main
    ( main
    ) where


import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (FromJSON (..), defaultOptions,
                                         genericParseJSON, genericToJSON,
                                         object, (.=))
import           Data.List.NonEmpty     (NonEmpty (..))
import           Data.Text              (Text)
import           Data.Time.Calendar     (Day (..))
import           Data.Time.Clock        (UTCTime (..), secondsToDiffTime)
import qualified Data.Vector            as V
import           Database.V5.Bloodhound
import           GHC.Generics           (Generic)
import           Network.HTTP.Client    (defaultManagerSettings)


data TweetMapping = TweetMapping deriving (Eq, Show)

instance ToJSON TweetMapping where
  toJSON TweetMapping =
    object
      [ "properties" .=
        object ["location" .= object ["type" .= ("geo_point" :: Text)]]
      ]

data Tweet = Tweet
  { user     :: Text
  , postDate :: UTCTime
  , message  :: Text
  , age      :: Int
  , location :: LatLon
  } deriving (Eq, Generic, Show)


exampleTweet :: Tweet
exampleTweet =
  Tweet
  { user = "bitemyapp"
  , postDate = UTCTime (ModifiedJulianDay 55000) (secondsToDiffTime 10)
  , message = "Use haskell!"
  , age = 10000
  , location = loc
  }
  where
    loc = LatLon {lat = 40.12, lon = -71.3}

instance ToJSON Tweet where
  toJSON = genericToJSON defaultOptions
instance FromJSON Tweet where
  parseJSON = genericParseJSON defaultOptions


main :: IO ()
main = runBH' $ do
  -- set up index
  _ <- createIndex indexSettings testIndex
  True <- indexExists testIndex
  _ <- putMapping testIndex testMapping TweetMapping

  -- create a tweet
  resp <- indexDocument testIndex testMapping defaultIndexDocumentSettings exampleTweet (DocId "1")
  liftIO (print resp)
  -- Response {responseStatus = Status {statusCode = 201, statusMessage = "Created"}, responseVersion = HTTP/1.1, responseHeaders = [("Content-Type","application/json; charset=UTF-8"),("Content-Length","74")], responseBody = "{\"_index\":\"twitter\",\"_type\":\"tweet\",\"_id\":\"1\",\"_version\":1,\"created\":true}", responseCookieJar = CJ {expose = []}, responseClose' = ResponseClose}

  -- bulk load
  let stream = V.fromList [BulkIndex testIndex testMapping (DocId "2") (toJSON exampleTweet)]
  _ <- bulk stream
  -- Bulk loads require an index refresh before new data is loaded.
  _ <- refreshIndex testIndex

  -- set up some aliases
  let aliasName = IndexName "twitter-alias"
  let iAlias = IndexAlias testIndex (IndexAliasName aliasName)
  let aliasRouting = Nothing
  let aliasFiltering = Nothing
  let aliasCreate = IndexAliasCreate aliasRouting aliasFiltering
  _ <- updateIndexAliases (AddAlias iAlias aliasCreate :| [])
  True <- indexExists aliasName

  -- create a template so that if we just write into an index named tweet-2017-01-02, for instance, the index will be automatically created with the given mapping. This is a great idea for any ongoing indices because it makes them much easier to manage and rotate.
  let idxTpl = IndexTemplate (TemplatePattern "tweet-*") (Just (IndexSettings (ShardCount 1) (ReplicaCount 1))) [object ["tweet" .= toJSON TweetMapping]]
  let templateName = TemplateName "tweet-tpl"
  _ <- putTemplate idxTpl templateName
  True <- templateExists templateName

  -- do a search
  let boost = Nothing
  let query = TermQuery (Term "user" "bitemyapp") boost
  let search = mkSearch (Just query) boost
  _ <- searchByType testIndex testMapping search

  -- clean up
  _ <- deleteTemplate templateName
  _ <- deleteIndex testIndex
  False <- indexExists testIndex

  return ()
  where
    testServer = Server "http://localhost:9200"
    runBH' = withBH defaultManagerSettings testServer
    testIndex = IndexName "twitter"
    testMapping = MappingName "tweet"
    indexSettings = IndexSettings (ShardCount 1) (ReplicaCount 0)
