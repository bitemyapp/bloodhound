{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}

module Test
  where

import Database.V5.Bloodhound
import           Data.Aeson
import           Data.Text                       (Text)
import           Data.Time.Clock                 (NominalDiffTime, UTCTime (..),
                                                  secondsToDiffTime)
import           GHC.Generics             
import           Data.Time.Calendar              (Day (..), fromGregorian)       
import           Network.HTTP.Client             hiding (Proxy)
import qualified Data.HashMap.Strict                   as HM

testServer  :: Server
testServer  = Server "http://localhost:9200"
testIndex   :: IndexName
testIndex   = IndexName "bloodhound-tests-twitter-1"
testMapping :: MappingName
testMapping = MappingName "tweet"
withTestEnv :: BH IO a -> IO a
withTestEnv = withBH defaultManagerSettings testServer
createExampleIndex :: (MonadBH m) => m Reply
createExampleIndex = createIndex (IndexSettings (ShardCount 1) (ReplicaCount 0)) testIndex
deleteExampleIndex :: (MonadBH m) => m Reply
deleteExampleIndex = deleteIndex testIndex
insertData :: BH IO Reply
insertData = do
  resetIndex
  insertData' defaultIndexDocumentSettings

insertData' :: IndexDocumentSettings -> BH IO Reply
insertData' ids = do
  r <- indexDocument testIndex testMapping ids exampleTweet (DocId "1")
  _ <- refreshIndex testIndex
  return r

searchTweet :: Search -> BH IO (Either EsError Tweet)
searchTweet search = do
  result <- searchTweets search
  let myTweet :: Either EsError Tweet
      myTweet = grabFirst result
  return myTweet

searchTweets :: Search -> BH IO (Either EsError (SearchResult Tweet))
searchTweets search = parseEsResponse =<< searchByIndex testIndex search

data Tweet = Tweet { user     :: Text
                   , postDate :: UTCTime
                   , message  :: Text
                   , age      :: Int
                   , location :: Location
                   , extra    :: Maybe Text }
           deriving (Eq, Generic, Show)

instance ToJSON   Tweet where
  toJSON = genericToJSON defaultOptions
instance FromJSON Tweet where
  parseJSON = genericParseJSON defaultOptions
instance ToJSON   Location where
  toJSON = genericToJSON defaultOptions
instance FromJSON Location where
  parseJSON = genericParseJSON defaultOptions

data Location = Location { lat :: Double
                         , lon :: Double } deriving (Eq, Generic, Show)

  
grabFirst :: Either EsError (SearchResult a) -> Either EsError a
grabFirst r =
  case fmap (hitSource . head . hits . searchHits) r of
    (Left e)         -> Left e
    (Right Nothing)  -> Left (EsError 500 "Source was missing")
    (Right (Just x)) -> Right x

resetIndex :: BH IO ()
resetIndex = do
  _ <- deleteExampleIndex
  _ <- createExampleIndex
  _ <- putMapping testIndex testMapping TweetMapping
  return ()

data TweetMapping = TweetMapping deriving (Eq, Show)

instance ToJSON TweetMapping where
  toJSON TweetMapping =
    object ["tweet" .=
      object ["properties" .=
        object [ "user"     .= object [ "type"    .= ("string" :: Text)
                                      , "fielddata" .= True
                                      ]
               -- Serializing the date as a date is breaking other tests, mysteriously.
               -- , "postDate" .= object [ "type"   .= ("date" :: Text)
               --                        , "format" .= ("YYYY-MM-dd`T`HH:mm:ss.SSSZZ" :: Text)]
               , "message"  .= object ["type" .= ("string" :: Text)]
               , "age"      .= object ["type" .= ("integer" :: Text)]
               , "location" .= object ["type" .= ("geo_point" :: Text)]
               , "extra"    .= object ["type" .= ("keyword" :: Text)]
               ]]]

exampleTweet :: Tweet
exampleTweet = Tweet { user     = "bitemyapp"
                     , postDate = UTCTime
                                  (ModifiedJulianDay 55000)
                                  (secondsToDiffTime 10)
                     , message  = "Use haskell!"
                     , age      = 10000
                     , location = Location 40.12 (-71.34)
                     , extra = Nothing }

one :: IO ()
one = do
  _ <- withTestEnv $ insertData
  let innerQuery = QueryMatchNoneQuery
  -- let innerQuery = QueryMatchQuery $ mkMatchQuery (FieldName "message") (QueryString "Use haskel")
  let phraseSuggester = mkPhraseSuggester (FieldName "message")
  let namedSuggester = Suggest "Use haskel" "suggest_name" (SuggestTypePhraseSuggester phraseSuggester)
  let search' = mkSearch (Just innerQuery) Nothing
  let search = search' { suggestBody = Just namedSuggester }
  print $ encode search
  -- myTweet <- withTestEnv $ searchTweet search
  {- liftIO $
    myTweet `shouldBe` Right exampleTweet -}
  -- print myTweet
  resp <- withTestEnv $ searchByIndex testIndex search
  parsed <- parseEsResponse resp :: IO (Either EsError (SearchResult Tweet))
  print parsed
  return ()
