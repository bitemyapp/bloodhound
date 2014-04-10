{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Main where

import Database.Bloodhound.Client
import Data.Aeson
import Data.DeriveTH
import Data.Either (Either(..))
import Data.Maybe (fromJust)
import Data.Time.Calendar (Day(..))
import Data.Time.Clock (secondsToDiffTime, UTCTime(..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Conduit
import qualified Network.HTTP.Types.Status as NHTS
import Test.Hspec


testServer = Server "http://localhost:9200"
testIndex = "twitter"
validateStatus resp expected = (NHTS.statusCode $ responseStatus resp) `shouldBe` (expected :: Int)
createExampleIndex = createIndex testServer defaultIndexSettings testIndex
deleteExampleIndex = deleteIndex testServer testIndex

data Tweet = Tweet { user :: Text
                   , postDate :: UTCTime
                   , message :: Text }
           deriving (Eq, Generic, Show)

instance ToJSON Tweet
instance FromJSON Tweet

exampleTweet = Tweet { user     = "bitemyapp"
                     , postDate = UTCTime
                                  (ModifiedJulianDay 55000)
                                  (secondsToDiffTime 10)
                     , message  = "Use haskell!" }

main :: IO ()
main = hspec $ do
  describe "index create/delete API" $ do
    it "creates and then deletes the requested index" $ do
      -- priming state.
      _ <- deleteExampleIndex
      resp <- createExampleIndex
      deleteResp <- deleteExampleIndex
      validateStatus resp 200
      validateStatus deleteResp 200
  describe "document API" $ do
    it "indexes, gets, and then deletes the generated document" $ do
      let tweet = exampleTweet
      let encoded = encode tweet
      _ <- deleteExampleIndex
      created <- createExampleIndex
      docCreated <- indexDocument (Server "http://localhost:9200") "twitter" "tweet" tweet "1"
      docInserted <- getDocument (Server "http://localhost:9200") "twitter" "tweet" "1"
      let newTweet = eitherDecode (responseBody docInserted) :: Either String (EsResult Tweet)
      deleted <- deleteExampleIndex
      Right tweet `shouldBe` fmap _source newTweet
