module Test.ScanScroll
  ( spec,
  )
where

import Test.Common
import Test.Import
import Prelude

spec :: Spec
spec =
  describe "Scan & Scroll API" $
    it "returns documents using the scan&scroll API" $
      withTestEnv $ do
        _ <- insertData
        _ <- insertOther
        let search =
              ( mkSearch
                  (Just $ MatchAllQuery Nothing)
                  Nothing
              )
                { size = Size 1
                }
        regular_search <- searchTweet search
        scan_search' <- scanSearch testIndex search :: BH IO [Hit Tweet]
        let scan_search = map hitSource scan_search'
        liftIO $
          regular_search `shouldBe` Right exampleTweet -- Check that the size restrtiction is being honored
        liftIO $
          scan_search `shouldMatchList` [Just exampleTweet, Just otherTweet]
