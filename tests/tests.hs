{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}

module Main where

import Database.Bloodhound.Client
import Data.Aeson
import Data.DeriveTH
import Data.Maybe (fromJust)
import Data.Time.Clock (UTCTime)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Conduit
import qualified Network.HTTP.Types.Status as NHTS
import System.Random
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Gen
import Test.QuickCheck.Instances


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
$(derive makeArbitrary ''Tweet)

newTweet :: IO Tweet
newTweet = do
  g <- newStdGen
  let seed = head (take 1 $ randoms g)
  return (unGen arbitrary (mkStdGen seed) 100)

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
      tweet <- newTweet
      let encoded = encode tweet
      created <- createExampleIndex
      docCreated <- indexDocument (Server "http://localhost:9200") "twitter" "tweet" tweet "1"
      docInserted <- getDocument (Server "http://localhost:9200") "twitter" "tweet" "1"
      let newTweet = decode (responseBody docInserted) :: Maybe Tweet
      deleted <- deleteExampleIndex
      (Just tweet) `shouldBe` newTweet
