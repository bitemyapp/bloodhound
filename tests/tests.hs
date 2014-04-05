module Main where

import Database.Bloodhound.Client
import Network.HTTP.Conduit
import qualified Network.HTTP.Types.Status as NHTS
import Test.Hspec


testServer = Server "http://localhost:9200"
testIndex = "twitter"
validateStatus resp expected = (NHTS.statusCode $ responseStatus resp) `shouldBe` (expected :: Int)

main :: IO ()
main = hspec $ do
  describe "index API" $ do
    it "creates and then deletes the requested index" $ do
      resp <- createIndex testServer defaultIndexSettings testIndex
      deleteResp <- deleteIndex testServer testIndex
      validateStatus resp 200
      validateStatus deleteResp 200
