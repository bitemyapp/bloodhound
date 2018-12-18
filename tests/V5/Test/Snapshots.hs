{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Snapshots (spec) where

import Test.Common
import Test.Import

import Data.Maybe (fromMaybe)
import qualified Data.List as L
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Network.HTTP.Types.Method as NHTM
import qualified Network.URI as URI

import Test.Generators ()

spec :: Spec
spec = do
  describe "FsSnapshotRepo" $
    prop "SnapshotRepo laws" $ \fsr ->
      fromGSnapshotRepo (toGSnapshotRepo fsr) === Right (fsr :: FsSnapshotRepo)

  describe "Snapshot repos" $ do
    it "always parses all snapshot repos API" $ when' canSnapshot $ withTestEnv $ do
      res <- getSnapshotRepos AllSnapshotRepos
      liftIO $ case res of
        Left e -> expectationFailure ("Expected a right but got Left " <> show e)
        Right _ -> return ()

    it "finds an existing list of repos" $ when' canSnapshot $ withTestEnv $ do
      let r1n = SnapshotRepoName "bloodhound-repo1"
      let r2n = SnapshotRepoName "bloodhound-repo2"
      withSnapshotRepo r1n $ \r1 ->
        withSnapshotRepo r2n $ \r2 -> do
          repos <- getSnapshotRepos (SnapshotRepoList (ExactRepo r1n :| [ExactRepo r2n]))
          liftIO $ case repos of
            Right xs -> do
              let srt = L.sortBy (comparing gSnapshotRepoName)
              srt xs `shouldBe` srt [r1, r2]
            Left e -> expectationFailure (show e)

    it "creates and updates with updateSnapshotRepo" $ when' canSnapshot $ withTestEnv $ do
      let r1n = SnapshotRepoName "bloodhound-repo1"
      withSnapshotRepo r1n $ \r1 -> do
        let Just (String dir) = HM.lookup "location" (gSnapshotRepoSettingsObject (gSnapshotRepoSettings r1))
        let noCompression = FsSnapshotRepo r1n (T.unpack dir) False Nothing Nothing Nothing
        resp <- updateSnapshotRepo defaultSnapshotRepoUpdateSettings noCompression
        liftIO (validateStatus resp 200)
        Right [roundtrippedNoCompression] <- getSnapshotRepos (SnapshotRepoList (ExactRepo r1n :| []))
        liftIO (roundtrippedNoCompression `shouldBe` toGSnapshotRepo noCompression)

    -- verify came around in 1.4 it seems
    it "can verify existing repos" $ when' canSnapshot $ when' (atleast es14) $ withTestEnv $ do
      let r1n = SnapshotRepoName "bloodhound-repo1"
      withSnapshotRepo r1n $ \_ -> do
        res <- verifySnapshotRepo r1n
        liftIO $ case res of
          Right (SnapshotVerification vs)
            | null vs -> expectationFailure "Expected nonempty set of verifying nodes"
            | otherwise -> return ()
          Left e -> expectationFailure (show e)

  describe "Snapshots" $ do
    it "always parses all snapshots API" $ when' canSnapshot $ withTestEnv $ do
      let r1n = SnapshotRepoName "bloodhound-repo1"
      withSnapshotRepo r1n $ \_ -> do
        res <- getSnapshots r1n AllSnapshots
        liftIO $ case res of
          Left e -> expectationFailure ("Expected a right but got Left " <> show e)
          Right _ -> return ()

    it "can parse a snapshot that it created" $ when' canSnapshot $ withTestEnv $ do
      let r1n = SnapshotRepoName "bloodhound-repo1"
      withSnapshotRepo r1n $ \_ -> do
        let s1n = SnapshotName "example-snapshot"
        withSnapshot r1n s1n $ do
          res <- getSnapshots r1n (SnapshotList (ExactSnap s1n :| []))
          liftIO $ case res of
            Right [snap]
              | snapInfoState snap == SnapshotSuccess &&
                snapInfoName snap == s1n -> return ()
              | otherwise -> expectationFailure (show snap)
            Right [] -> expectationFailure "There were no snapshots"
            Right snaps -> expectationFailure ("Expected 1 snapshot but got" <> show (length snaps))
            Left e -> expectationFailure (show e)

  describe "Snapshot restore" $ do
    it "can restore a snapshot that we create" $ when' canSnapshot $ withTestEnv $ do
      let r1n = SnapshotRepoName "bloodhound-repo1"
      withSnapshotRepo r1n $ \_ -> do
        let s1n = SnapshotName "example-snapshot"
        withSnapshot r1n s1n $ do
          let settings = defaultSnapshotRestoreSettings { snapRestoreWaitForCompletion = True }
          -- have to close an index to restore it
          resp1 <- closeIndex testIndex
          liftIO (validateStatus resp1 200)
          resp2 <- restoreSnapshot r1n s1n settings
          liftIO (validateStatus resp2 200)

    it "can restore and rename" $ when' canSnapshot $ withTestEnv $ do
      let r1n = SnapshotRepoName "bloodhound-repo1"
      withSnapshotRepo r1n $ \_ -> do
        let s1n = SnapshotName "example-snapshot"
        withSnapshot r1n s1n $ do
          let pat = RestoreRenamePattern "bloodhound-tests-twitter-(\\d+)"
          let replace = RRTLit "restored-" :| [RRSubWholeMatch]
          let expectedIndex = IndexName "restored-bloodhound-tests-twitter-1"
          oldEnoughForOverrides <- liftIO (atleast es15)
          let overrides = RestoreIndexSettings { restoreOverrideReplicas = Just (ReplicaCount 0) }
          let settings = defaultSnapshotRestoreSettings { snapRestoreWaitForCompletion = True
                                                        , snapRestoreRenamePattern = Just pat
                                                        , snapRestoreRenameReplacement = Just replace
                                                        , snapRestoreIndexSettingsOverrides = if oldEnoughForOverrides
                                                                                              then Just overrides
                                                                                              else Nothing
                                                        }
          -- have to close an index to restore it
          let go = do
                resp <- restoreSnapshot r1n s1n settings
                liftIO (validateStatus resp 200)
                exists <- indexExists expectedIndex
                liftIO (exists `shouldBe` True)
          go `finally` deleteIndex expectedIndex

-- | Get configured repo paths for snapshotting. Note that by default
-- this is not enabled and if we are over es 1.5, we won't be able to
-- test snapshotting. Note that this can and should be part of the
-- client functionality in a much less ad-hoc incarnation.
getRepoPaths :: IO [FilePath]
getRepoPaths = withTestEnv $ do
  bhe <- getBHEnv
  let Server s = bhServer bhe
  let tUrl = s <> "/" <> "_nodes"
  initReq <- parseRequest (URI.escapeURIString URI.isAllowedInURI (T.unpack tUrl))
  let req = setRequestIgnoreStatus $ initReq { method = NHTM.methodGet }
  Right (Object o) <- parseEsResponse =<< liftIO (httpLbs req (bhManager bhe))
  return $ fromMaybe mempty $ do
    Object nodes <- HM.lookup "nodes" o
    Object firstNode <- snd <$> headMay (HM.toList nodes)
    Object settings <- HM.lookup "settings" firstNode
    Object path <- HM.lookup "path" settings
    Array repo <- HM.lookup "repo" path
    return [ T.unpack t | String t <- V.toList repo]

-- | 1.5 and earlier don't care about repo paths
canSnapshot :: IO Bool
canSnapshot = do
  caresAboutRepos <- atleast es16
  repoPaths <- getRepoPaths
  return (not caresAboutRepos || not (null repoPaths))

withSnapshotRepo
    :: ( MonadMask m
       , MonadBH m
       )
    => SnapshotRepoName
    -> (GenericSnapshotRepo -> m a)
    -> m a
withSnapshotRepo srn@(SnapshotRepoName n) f = do
  repoPaths <- liftIO getRepoPaths
  -- we'll use the first repo path if available, otherwise system temp
  -- dir. Note that this will fail on ES > 1.6, so be sure you use
  -- @when' canSnapshot@.
  case repoPaths of
    (firstRepoPath:_) -> withTempDirectory firstRepoPath (T.unpack n) $ \dir -> bracket (alloc dir) free f
    [] -> withSystemTempDirectory (T.unpack n) $ \dir -> bracket (alloc dir) free f
  where
    alloc dir = do
      liftIO (setFileMode dir mode)
      let repo = FsSnapshotRepo srn "bloodhound-tests-backups" True Nothing Nothing Nothing
      resp <- updateSnapshotRepo defaultSnapshotRepoUpdateSettings repo
      liftIO (validateStatus resp 200)
      return (toGSnapshotRepo repo)
    mode = ownerModes `unionFileModes` groupModes `unionFileModes` otherModes
    free GenericSnapshotRepo {..} = do
      resp <- deleteSnapshotRepo gSnapshotRepoName
      liftIO (validateStatus resp 200)


withSnapshot
    :: ( MonadMask m
       , MonadBH m
       )
    => SnapshotRepoName
    -> SnapshotName
    -> m a
    -> m a
withSnapshot srn sn = bracket_ alloc free
  where
    alloc = do
      resp <- createSnapshot srn sn createSettings
      liftIO (validateStatus resp 200)
    -- We'll make this synchronous for testing purposes
    createSettings = defaultSnapshotCreateSettings { snapWaitForCompletion = True
                                                   , snapIndices = Just (IndexList (testIndex :| []))
                                                   -- We don't actually need to back up any data
                                                   }
    free =
      deleteSnapshot srn sn
