{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Bloodhound.Internal.Versions.Common.Types.Reindex where

import Data.Aeson
import Data.List.NonEmpty
import Data.Text (Text)
import Database.Bloodhound.Internal.Versions.Common.Types.Newtypes (IndexName)
import Database.Bloodhound.Internal.Versions.Common.Types.Query (Query)
import Database.Bloodhound.Internal.Versions.Common.Types.Script (ScriptLanguage)
import GHC.Generics

data ReindexRequest = ReindexRequest
  { reindexConflicts :: Maybe ReindexConflicts,
    reindexSource :: ReindexSource,
    reindexDest :: ReindexDest,
    reindexScript :: Maybe ReindexScript
  }
  deriving (Show, Eq, Generic)

instance ToJSON ReindexRequest where
  toJSON ReindexRequest {..} =
    object
      [ "conflicts" .= reindexConflicts,
        "source" .= reindexSource,
        "dest" .= reindexDest,
        "script" .= reindexScript
      ]

instance FromJSON ReindexRequest where
  parseJSON = withObject "ReindexRequest" $ \v ->
    ReindexRequest
      <$> v .:? "conflicts"
      <*> v .: "source"
      <*> v .: "dest"
      <*> v .:? "script"

data ReindexConflicts
  = ReindexAbortOnConflicts
  | ReindexProceedOnConflicts
  deriving (Show, Eq, Generic)

instance FromJSON ReindexConflicts where
  parseJSON = withText "ReindexConflicts" $ \case
    "abort" -> pure ReindexAbortOnConflicts
    "proceed" -> pure ReindexProceedOnConflicts
    s -> fail $ "Expected one of [abort, proceed], found: " <> show s

instance ToJSON ReindexConflicts where
  toJSON =
    String . \case
      ReindexAbortOnConflicts -> "abort"
      ReindexProceedOnConflicts -> "proceed"

-- | Elasticsearch also supports reindex from remote, it could be added here if required
data ReindexSource = ReindexSource
  { reindexSourceIndex :: NonEmpty IndexName,
    reindexSourceMaxDocs :: Maybe Int,
    reindexSourceQuery :: Maybe Query,
    reindexSourceSize :: Maybe Int,
    reindexSourceSlice :: Maybe ReindexSlice
  }
  deriving (Show, Eq, Generic)

instance ToJSON ReindexSource where
  toJSON ReindexSource {..} =
    object
      [ "index" .= reindexSourceIndex,
        "max_docs" .= reindexSourceMaxDocs,
        "query" .= reindexSourceQuery,
        "size" .= reindexSourceSize,
        "slice" .= reindexSourceSlice
      ]

instance FromJSON ReindexSource where
  parseJSON = withObject "ReindexSource" $ \v ->
    ReindexSource
      <$> v .: "index"
      <*> v .:? "max_docs"
      <*> v .:? "query"
      <*> v .:? "size"
      <*> v .:? "slice"

data ReindexSlice = ReindexSlice
  { reindexSliceId :: Maybe Int,
    reindexSliceMax :: Maybe Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON ReindexSlice where
  toJSON ReindexSlice {..} =
    object ["id" .= reindexSliceId, "max" .= reindexSliceMax]

instance FromJSON ReindexSlice where
  parseJSON = withObject "ReindexSlice" $ \v ->
    ReindexSlice <$> v .:? "id" <*> v .:? "max"

data ReindexDest = ReindexDest
  { reindexDestIndex :: IndexName,
    reindexDestVersionType :: Maybe VersionType,
    reindexDestOpType :: Maybe ReindexOpType
  }
  deriving (Show, Eq, Generic)

instance ToJSON ReindexDest where
  toJSON ReindexDest {..} =
    object
      [ "index" .= reindexDestIndex,
        "version_type" .= reindexDestVersionType,
        "op_type" .= reindexDestOpType
      ]

instance FromJSON ReindexDest where
  parseJSON = withObject "ReindexDest" $ \v ->
    ReindexDest
      <$> v .: "index"
      <*> v .:? "version_type"
      <*> v .:? "op_type"

data VersionType
  = VersionTypeInternal
  | VersionTypeExternal
  | VersionTypeExternalGT
  | VersionTypeExternalGTE
  deriving (Show, Eq, Generic)

instance ToJSON VersionType where
  toJSON =
    String . \case
      VersionTypeInternal -> "internal"
      VersionTypeExternal -> "external"
      VersionTypeExternalGT -> "external_gt"
      VersionTypeExternalGTE -> "external_gte"

instance FromJSON VersionType where
  parseJSON = withText "VersionType" $ \case
    "internal" -> pure VersionTypeInternal
    "external" -> pure VersionTypeExternal
    "external_gt" -> pure VersionTypeExternalGT
    "external_gte" -> pure VersionTypeExternalGTE
    s -> fail $ "Expected one of [internal, external, external_gt, external_gte], found: " <> show s

data ReindexOpType
  = OpCreate
  | OpIndex
  deriving (Show, Eq, Generic)

instance FromJSON ReindexOpType where
  parseJSON = withText "ReindexOpType" $ \case
    "create" -> pure OpCreate
    "index" -> pure OpIndex
    s -> fail $ "Expected one of [create, index], found: " <> show s

instance ToJSON ReindexOpType where
  toJSON OpCreate = String "create"
  toJSON OpIndex = String "index"

data ReindexScript = ReindexScript
  { reindexScriptLanguage :: ScriptLanguage,
    reindexScriptSource :: Text
  }
  deriving (Show, Eq, Generic)

instance ToJSON ReindexScript where
  toJSON ReindexScript {..} =
    object
      [ "language" .= reindexScriptLanguage,
        "source" .= reindexScriptSource
      ]

instance FromJSON ReindexScript where
  parseJSON = withObject "ReindexScript" $ \v ->
    ReindexScript
      <$> v .: "language"
      <*> v .: "source"

mkReindexRequest :: IndexName -> IndexName -> ReindexRequest
mkReindexRequest src dst =
  ReindexRequest
    { reindexSource =
        ReindexSource
          { reindexSourceIndex = src :| [],
            reindexSourceMaxDocs = Nothing,
            reindexSourceQuery = Nothing,
            reindexSourceSize = Nothing,
            reindexSourceSlice = Nothing
          },
      reindexDest =
        ReindexDest
          { reindexDestIndex = dst,
            reindexDestVersionType = Nothing,
            reindexDestOpType = Nothing
          },
      reindexConflicts = Nothing,
      reindexScript = Nothing
    }

data ReindexResponse = ReindexResponse
  { reindexResponseTook :: Maybe Int,
    reindexResponseUpdated :: Int,
    reindexResponseCreated :: Int,
    reindexResponseBatches :: Int,
    reindexResponseVersionConflicts :: Int,
    reindexResponseThrottledMillis :: Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON ReindexResponse where
  toJSON ReindexResponse {..} =
    object
      [ "took" .= reindexResponseTook,
        "updated" .= reindexResponseUpdated,
        "created" .= reindexResponseCreated,
        "batches" .= reindexResponseBatches,
        "version_conflicts" .= reindexResponseVersionConflicts,
        "throttled_millis" .= reindexResponseThrottledMillis
      ]

instance FromJSON ReindexResponse where
  parseJSON = withObject "ReindexResponse" $ \v ->
    ReindexResponse
      <$> v .:? "took"
      <*> v .: "updated"
      <*> v .: "created"
      <*> v .: "batches"
      <*> v .: "version_conflicts"
      <*> v .: "throttled_millis"
