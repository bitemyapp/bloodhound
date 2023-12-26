{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Bloodhound.Internal.Versions.Common.Types.Snapshots
  ( FsSnapshotRepo (..),
    GenericSnapshotRepo (..),
    GenericSnapshotRepoSettings (..),
    RRGroupRefNum (..),
    RestoreIndexSettings (..),
    RestoreRenamePattern (..),
    RestoreRenameToken (..),
    SnapshotCreateSettings (..),
    SnapshotInfo (..),
    SnapshotNodeVerification (..),
    SnapshotPattern (..),
    SnapshotRepo (..),
    SnapshotRepoConversionError (..),
    SnapshotRepoName (..),
    SnapshotRepoPattern (..),
    SnapshotRepoSelection (..),
    SnapshotRepoType (..),
    SnapshotRepoUpdateSettings (..),
    SnapshotRestoreSettings (..),
    SnapshotSelection (..),
    SnapshotShardFailure (..),
    SnapshotState (..),
    SnapshotVerification (..),
    defaultSnapshotCreateSettings,
    defaultSnapshotRepoUpdateSettings,
    defaultSnapshotRestoreSettings,
    mkRRGroupRefNum,
  )
where

import Control.Monad.Catch
import qualified Data.Aeson.Key as X
import qualified Data.Aeson.KeyMap as X
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Database.Bloodhound.Internal.Utils.Imports
import Database.Bloodhound.Internal.Utils.StringlyTyped
import Database.Bloodhound.Internal.Versions.Common.Types.Indices
import Database.Bloodhound.Internal.Versions.Common.Types.Newtypes
import Database.Bloodhound.Internal.Versions.Common.Types.Nodes
import Database.Bloodhound.Internal.Versions.Common.Types.Units
import GHC.Generics

data SnapshotRepoSelection
  = SnapshotRepoList (NonEmpty SnapshotRepoPattern)
  | AllSnapshotRepos
  deriving (Eq, Show)

-- | Either specifies an exact repo name or one with globs in it,
-- e.g. @RepoPattern "foo*"@ __NOTE__: Patterns are not supported on ES < 1.7
data SnapshotRepoPattern
  = ExactRepo SnapshotRepoName
  | RepoPattern Text
  deriving (Eq, Show)

-- | The unique name of a snapshot repository.
newtype SnapshotRepoName = SnapshotRepoName {snapshotRepoName :: Text}
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | A generic representation of a snapshot repo. This is what gets
-- sent to and parsed from the server. For repo types enabled by
-- plugins that aren't exported by this library, consider making a
-- custom type which implements 'SnapshotRepo'. If it is a common repo
-- type, consider submitting a pull request to have it included in the
-- library proper
data GenericSnapshotRepo = GenericSnapshotRepo
  { gSnapshotRepoName :: SnapshotRepoName,
    gSnapshotRepoType :: SnapshotRepoType,
    gSnapshotRepoSettings :: GenericSnapshotRepoSettings
  }
  deriving (Eq, Show)

instance SnapshotRepo GenericSnapshotRepo where
  toGSnapshotRepo = id
  fromGSnapshotRepo = Right

newtype SnapshotRepoType = SnapshotRepoType {snapshotRepoType :: Text}
  deriving (Eq, Ord, Show, ToJSON, FromJSON)

-- | Opaque representation of snapshot repo settings. Instances of
-- 'SnapshotRepo' will produce this.
newtype GenericSnapshotRepoSettings = GenericSnapshotRepoSettings {gSnapshotRepoSettingsObject :: Object}
  deriving (Eq, Show, ToJSON)

-- Regardless of whether you send strongly typed json, my version of
-- ES sends back stringly typed json in the settings, e.g. booleans
-- as strings, so we'll try to convert them.
instance FromJSON GenericSnapshotRepoSettings where
  parseJSON = fmap (GenericSnapshotRepoSettings . fmap unStringlyTypeJSON) . parseJSON

-- | The result of running 'verifySnapshotRepo'.
newtype SnapshotVerification = SnapshotVerification
  { snapshotNodeVerifications :: [SnapshotNodeVerification]
  }
  deriving (Eq, Show)

instance FromJSON SnapshotVerification where
  parseJSON = withObject "SnapshotVerification" parse
    where
      parse o = do
        o2 <- o .: "nodes"
        SnapshotVerification <$> mapM (uncurry parse') (HM.toList o2)
      parse' rawFullId = withObject "SnapshotNodeVerification" $ \o ->
        SnapshotNodeVerification (FullNodeId rawFullId) <$> o .: "name"

-- | A node that has verified a snapshot
data SnapshotNodeVerification = SnapshotNodeVerification
  { snvFullId :: FullNodeId,
    snvNodeName :: NodeName
  }
  deriving (Eq, Show)

data SnapshotState
  = SnapshotInit
  | SnapshotStarted
  | SnapshotSuccess
  | SnapshotFailed
  | SnapshotAborted
  | SnapshotMissing
  | SnapshotWaiting
  deriving (Eq, Show)

instance FromJSON SnapshotState where
  parseJSON = withText "SnapshotState" parse
    where
      parse "INIT" = return SnapshotInit
      parse "STARTED" = return SnapshotStarted
      parse "SUCCESS" = return SnapshotSuccess
      parse "FAILED" = return SnapshotFailed
      parse "ABORTED" = return SnapshotAborted
      parse "MISSING" = return SnapshotMissing
      parse "WAITING" = return SnapshotWaiting
      parse t = fail ("Invalid snapshot state " <> T.unpack t)

data SnapshotRestoreSettings = SnapshotRestoreSettings
  { -- | Should the API call return immediately after initializing
    -- the restore or wait until completed? Note that if this is
    -- enabled, it could wait a long time, so you should adjust your
    -- 'ManagerSettings' accordingly to set long timeouts or
    -- explicitly handle timeouts.
    snapRestoreWaitForCompletion :: Bool,
    -- | Nothing will restore all indices in the snapshot. Just [] is
    -- permissable and will essentially be a no-op restore.
    snapRestoreIndices :: Maybe IndexSelection,
    -- | If set to True, any indices that do not exist will be ignored
    -- during snapshot rather than failing the restore.
    snapRestoreIgnoreUnavailable :: Bool,
    -- | If set to false, will ignore any global state in the snapshot
    -- and will not restore it.
    snapRestoreIncludeGlobalState :: Bool,
    -- | A regex pattern for matching indices. Used with
    -- 'snapRestoreRenameReplacement', the restore can reference the
    -- matched index and create a new index name upon restore.
    snapRestoreRenamePattern :: Maybe RestoreRenamePattern,
    -- | Expression of how index renames should be constructed.
    snapRestoreRenameReplacement :: Maybe (NonEmpty RestoreRenameToken),
    -- | If some indices fail to restore, should the process proceed?
    snapRestorePartial :: Bool,
    -- | Should the restore also restore the aliases captured in the
    -- snapshot.
    snapRestoreIncludeAliases :: Bool,
    -- | Settings to apply during the restore process. __NOTE:__ This
    -- option is not supported in ES < 1.5 and should be set to
    -- Nothing in that case.
    snapRestoreIndexSettingsOverrides :: Maybe RestoreIndexSettings,
    -- | This type could be more rich but it isn't clear which
    -- settings are allowed to be ignored during restore, so we're
    -- going with including this feature in a basic form rather than
    -- omitting it. One example here would be
    -- "index.refresh_interval". Any setting specified here will
    -- revert back to the server default during the restore process.
    snapRestoreIgnoreIndexSettings :: Maybe (NonEmpty Text)
  }
  deriving (Eq, Show)

newtype SnapshotRepoUpdateSettings = SnapshotRepoUpdateSettings
  { -- | After creation/update, synchronously check that nodes can
    -- write to this repo. Defaults to True. You may use False if you
    -- need a faster response and plan on verifying manually later
    -- with 'verifySnapshotRepo'.
    repoUpdateVerify :: Bool
  }
  deriving (Eq, Show)

-- | Reasonable defaults for repo creation/update
--
-- * repoUpdateVerify True
defaultSnapshotRepoUpdateSettings :: SnapshotRepoUpdateSettings
defaultSnapshotRepoUpdateSettings = SnapshotRepoUpdateSettings True

-- | A filesystem-based snapshot repo that ships with
-- Elasticsearch. This is an instance of 'SnapshotRepo' so it can be
-- used with 'updateSnapshotRepo'
data FsSnapshotRepo = FsSnapshotRepo
  { fsrName :: SnapshotRepoName,
    fsrLocation :: FilePath,
    fsrCompressMetadata :: Bool,
    -- | Size by which to split large files during snapshotting.
    fsrChunkSize :: Maybe Bytes,
    -- | Throttle node restore rate. If not supplied, defaults to 40mb/sec
    fsrMaxRestoreBytesPerSec :: Maybe Bytes,
    -- | Throttle node snapshot rate. If not supplied, defaults to 40mb/sec
    fsrMaxSnapshotBytesPerSec :: Maybe Bytes
  }
  deriving (Eq, Show, Generic)

instance SnapshotRepo FsSnapshotRepo where
  toGSnapshotRepo FsSnapshotRepo {..} =
    GenericSnapshotRepo fsrName fsRepoType (GenericSnapshotRepoSettings settings)
    where
      settings =
        X.fromList $
          [ X.fromText "location" .= fsrLocation,
            X.fromText "compress" .= fsrCompressMetadata
          ]
            ++ optionalPairs
      optionalPairs =
        catMaybes
          [ ("chunk_size" .=) <$> fsrChunkSize,
            ("max_restore_bytes_per_sec" .=) <$> fsrMaxRestoreBytesPerSec,
            ("max_snapshot_bytes_per_sec" .=) <$> fsrMaxSnapshotBytesPerSec
          ]
  fromGSnapshotRepo GenericSnapshotRepo {..}
    | gSnapshotRepoType == fsRepoType = do
        let o = gSnapshotRepoSettingsObject gSnapshotRepoSettings
        parseRepo $
          FsSnapshotRepo gSnapshotRepoName
            <$> o
              .: "location"
            <*> o
              .:? "compress"
              .!= False
            <*> o
              .:? "chunk_size"
            <*> o
              .:? "max_restore_bytes_per_sec"
            <*> o
              .:? "max_snapshot_bytes_per_sec"
    | otherwise = Left (RepoTypeMismatch fsRepoType gSnapshotRepoType)

parseRepo :: Parser a -> Either SnapshotRepoConversionError a
parseRepo parser = case parseEither (const parser) () of
  Left e -> Left (OtherRepoConversionError (T.pack e))
  Right a -> Right a

fsRepoType :: SnapshotRepoType
fsRepoType = SnapshotRepoType "fs"

-- | Law: fromGSnapshotRepo (toGSnapshotRepo r) == Right r
class SnapshotRepo r where
  toGSnapshotRepo :: r -> GenericSnapshotRepo
  fromGSnapshotRepo :: GenericSnapshotRepo -> Either SnapshotRepoConversionError r

data SnapshotRepoConversionError
  = -- | Expected type and actual type
    RepoTypeMismatch SnapshotRepoType SnapshotRepoType
  | OtherRepoConversionError Text
  deriving (Show, Eq)

instance Exception SnapshotRepoConversionError

data SnapshotCreateSettings = SnapshotCreateSettings
  { -- | Should the API call return immediately after initializing
    -- the snapshot or wait until completed? Note that if this is
    -- enabled it could wait a long time, so you should adjust your
    -- 'ManagerSettings' accordingly to set long timeouts or
    -- explicitly handle timeouts.
    snapWaitForCompletion :: Bool,
    -- | Nothing will snapshot all indices. Just [] is permissable and
    -- will essentially be a no-op snapshot.
    snapIndices :: Maybe IndexSelection,
    -- | If set to True, any matched indices that don't exist will be
    -- ignored. Otherwise it will be an error and fail.
    snapIgnoreUnavailable :: Bool,
    snapIncludeGlobalState :: Bool,
    -- | If some indices failed to snapshot (e.g. if not all primary
    -- shards are available), should the process proceed?
    snapPartial :: Bool
  }
  deriving (Eq, Show)

-- | Reasonable defaults for snapshot creation
--
-- * snapWaitForCompletion False
-- * snapIndices Nothing
-- * snapIgnoreUnavailable False
-- * snapIncludeGlobalState True
-- * snapPartial False
defaultSnapshotCreateSettings :: SnapshotCreateSettings
defaultSnapshotCreateSettings =
  SnapshotCreateSettings
    { snapWaitForCompletion = False,
      snapIndices = Nothing,
      snapIgnoreUnavailable = False,
      snapIncludeGlobalState = True,
      snapPartial = False
    }

data SnapshotSelection
  = SnapshotList (NonEmpty SnapshotPattern)
  | AllSnapshots
  deriving (Eq, Show)

-- | Either specifies an exact snapshot name or one with globs in it,
-- e.g. @SnapPattern "foo*"@ __NOTE__: Patterns are not supported on
-- ES < 1.7
data SnapshotPattern
  = ExactSnap SnapshotName
  | SnapPattern Text
  deriving (Eq, Show)

-- | General information about the state of a snapshot. Has some
-- redundancies with 'SnapshotStatus'
data SnapshotInfo = SnapshotInfo
  { snapInfoShards :: ShardResult,
    snapInfoFailures :: [SnapshotShardFailure],
    snapInfoDuration :: NominalDiffTime,
    snapInfoEndTime :: UTCTime,
    snapInfoStartTime :: UTCTime,
    snapInfoState :: SnapshotState,
    snapInfoIndices :: [IndexName],
    snapInfoName :: SnapshotName
  }
  deriving (Eq, Show)

instance FromJSON SnapshotInfo where
  parseJSON = withObject "SnapshotInfo" parse
    where
      parse o =
        SnapshotInfo
          <$> o
            .: "shards"
          <*> o
            .: "failures"
          <*> (unMS <$> o .: "duration_in_millis")
          <*> (posixMS <$> o .: "end_time_in_millis")
          <*> (posixMS <$> o .: "start_time_in_millis")
          <*> o
            .: "state"
          <*> o
            .: "indices"
          <*> o
            .: "snapshot"

data SnapshotShardFailure = SnapshotShardFailure
  { snapShardFailureIndex :: IndexName,
    snapShardFailureNodeId :: Maybe NodeName, -- I'm not 100% sure this isn't actually 'FullNodeId'
    snapShardFailureReason :: Text,
    snapShardFailureShardId :: ShardId
  }
  deriving (Eq, Show)

instance FromJSON SnapshotShardFailure where
  parseJSON = withObject "SnapshotShardFailure" parse
    where
      parse o =
        SnapshotShardFailure
          <$> o
            .: "index"
          <*> o
            .:? "node_id"
          <*> o
            .: "reason"
          <*> o
            .: "shard_id"

-- | Regex-stype pattern, e.g. "index_(.+)" to match index names
newtype RestoreRenamePattern = RestoreRenamePattern {rrPattern :: Text}
  deriving (Eq, Show, Ord, ToJSON)

-- | A single token in a index renaming scheme for a restore. These
-- are concatenated into a string before being sent to
-- Elasticsearch. Check out these Java
-- <https://docs.oracle.com/javase/7/docs/api/java/util/regex/Matcher.html docs> to find out more if you're into that sort of thing.
data RestoreRenameToken
  = -- | Just a literal string of characters
    RRTLit Text
  | -- | Equivalent to $0. The entire matched pattern, not any subgroup
    RRSubWholeMatch
  | -- | A specific reference to a group number
    RRSubGroup RRGroupRefNum
  deriving (Eq, Show)

-- | A group number for regex matching. Only values from 1-9 are
-- supported. Construct with 'mkRRGroupRefNum'
newtype RRGroupRefNum = RRGroupRefNum {rrGroupRefNum :: Int}
  deriving (Eq, Ord, Show)

instance Bounded RRGroupRefNum where
  minBound = RRGroupRefNum 1
  maxBound = RRGroupRefNum 9

-- | Only allows valid group number references (1-9).
mkRRGroupRefNum :: Int -> Maybe RRGroupRefNum
mkRRGroupRefNum i
  | i >= rrGroupRefNum minBound
      && i <= rrGroupRefNum maxBound =
      Just $ RRGroupRefNum i
  | otherwise = Nothing

-- | Reasonable defaults for snapshot restores
--
-- * snapRestoreWaitForCompletion False
-- * snapRestoreIndices Nothing
-- * snapRestoreIgnoreUnavailable False
-- * snapRestoreIncludeGlobalState True
-- * snapRestoreRenamePattern Nothing
-- * snapRestoreRenameReplacement Nothing
-- * snapRestorePartial False
-- * snapRestoreIncludeAliases True
-- * snapRestoreIndexSettingsOverrides Nothing
-- * snapRestoreIgnoreIndexSettings Nothing
defaultSnapshotRestoreSettings :: SnapshotRestoreSettings
defaultSnapshotRestoreSettings =
  SnapshotRestoreSettings
    { snapRestoreWaitForCompletion = False,
      snapRestoreIndices = Nothing,
      snapRestoreIgnoreUnavailable = False,
      snapRestoreIncludeGlobalState = True,
      snapRestoreRenamePattern = Nothing,
      snapRestoreRenameReplacement = Nothing,
      snapRestorePartial = False,
      snapRestoreIncludeAliases = True,
      snapRestoreIndexSettingsOverrides = Nothing,
      snapRestoreIgnoreIndexSettings = Nothing
    }

-- | Index settings that can be overridden. The docs only mention you
-- can update number of replicas, but there may be more. You
-- definitely cannot override shard count.
newtype RestoreIndexSettings = RestoreIndexSettings
  { restoreOverrideReplicas :: Maybe ReplicaCount
  }
  deriving (Eq, Show)

instance ToJSON RestoreIndexSettings where
  toJSON RestoreIndexSettings {..} = object prs
    where
      prs = catMaybes [("index.number_of_replicas" .=) <$> restoreOverrideReplicas]
