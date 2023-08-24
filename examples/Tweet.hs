{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Main
  ( main,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), Value, object, (.=))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Time.Calendar (Day (..))
import Data.Time.Clock (UTCTime (..), secondsToDiffTime)
import qualified Data.Vector as V
import Database.Bloodhound
import GHC.Generics (Generic)
import Network.HTTP.Client (defaultManagerSettings)

data TweetMapping = TweetMapping deriving stock (Eq, Show)

instance ToJSON TweetMapping where
  toJSON TweetMapping =
    object
      [ "properties"
          .= object ["location" .= object ["type" .= ("geo_point" :: Text)]]
      ]

data Tweet = Tweet
  { user :: Text,
    postDate :: UTCTime,
    message :: Text,
    age :: Int,
    location :: LatLon
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

exampleTweet :: Tweet
exampleTweet =
  Tweet
    { user = "bitemyapp",
      postDate = UTCTime (ModifiedJulianDay 55000) (secondsToDiffTime 10),
      message = "Use haskell!",
      age = 10000,
      location = loc
    }
  where
    loc = LatLon {lat = 40.12, lon = -71.3}

main :: IO ()
main = runBH' $ do
  -- set up index
  _ <- createIndex indexSettings testIndex
  True <- indexExists testIndex
  _ <- putMapping @Value testIndex TweetMapping

  -- create a tweet
  resp <- indexDocument testIndex defaultIndexDocumentSettings exampleTweet (DocId "1")
  liftIO $ print resp
  {-
       IndexedDocument
         { idxDocIndex = "twitter"
         , idxDocType = "_doc"
         , idxDocId = "1"
         , idxDocVersion = 3
         , idxDocResult = "updated"
         , idxDocShards =
             ShardResult
               { shardTotal = 1
               , shardsSuccessful = 1
               , shardsSkipped = 0
               , shardsFailed = 0
               }
         , idxDocSeqNo = 2
         , idxDocPrimaryTerm = 1
         }
  -}

  -- bulk load
  let stream = V.fromList [BulkIndex testIndex (DocId "2") (toJSON exampleTweet)]
  _ <- bulk @IgnoredBody stream
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
  let idxTpl = IndexTemplate [IndexPattern "tweet-*"] (Just (IndexSettings (ShardCount 1) (ReplicaCount 1) defaultIndexMappingsLimits)) (toJSON TweetMapping)
  let templateName = TemplateName "tweet-tpl"
  tplResp <- putTemplate idxTpl templateName
  liftIO $ print tplResp
  {-
    Acknowledged { isAcknowledged = True }
  -}
  True <- templateExists templateName

  -- do a search
  let boost = Nothing
  let query = TermQuery (Term "user" "bitemyapp") boost
  let search = mkSearch (Just query) boost
  tweetResp <- searchByIndex @Tweet testIndex search
  liftIO $ print tweetResp
  {-
    SearchResult
      { took = 1
      , timedOut = False
      , shards =
          ShardResult
            { shardTotal = 1
            , shardsSuccessful = 1
            , shardsSkipped = 0
            , shardsFailed = 0
            }
      , searchHits =
          SearchHits
            { hitsTotal = HitsTotal { value = 2 , relation = HTR_EQ }
            , maxScore = Just 0.18232156
            , hits =
                [ Hit
                    { hitIndex = IndexName "twitter"
                    , hitDocId = DocId "1"
                    , hitScore = Just 0.18232156
                    , hitSource =
                        Just
                          Tweet
                            { user = "bitemyapp"
                            , postDate = 2009-06-18 00:00:10 UTC
                            , message = "Use haskell!"
                            , age = 10000
                            , location = LatLon { lat = 40.12 , lon = -71.3 }
                            }
                    , hitSort = Nothing
                    , hitFields = Nothing
                    , hitHighlight = Nothing
                    , hitInnerHits = Nothing
                    }
                , Hit
                    { hitIndex = IndexName "twitter"
                    , hitDocId = DocId "2"
                    , hitScore = Just 0.18232156
                    , hitSource =
                        Just
                          Tweet
                            { user = "bitemyapp"
                            , postDate = 2009-06-18 00:00:10 UTC
                            , message = "Use haskell!"
                            , age = 10000
                            , location = LatLon { lat = 40.12 , lon = -71.3 }
                            }
                    , hitSort = Nothing
                    , hitFields = Nothing
                    , hitHighlight = Nothing
                    , hitInnerHits = Nothing
                    }
                ]
            }
      , aggregations = Nothing
      , scrollId = Nothing
      , suggest = Nothing
      , pitId = Nothing
      }
  -}

  -- clean up
  _ <- deleteTemplate templateName
  _ <- deleteIndex testIndex
  False <- indexExists testIndex

  return ()
  where
    testServer = Server "http://localhost:9200"
    runBH' = withBH defaultManagerSettings testServer
    testIndex = IndexName "twitter"
    indexSettings = IndexSettings (ShardCount 1) (ReplicaCount 0) defaultIndexMappingsLimits
