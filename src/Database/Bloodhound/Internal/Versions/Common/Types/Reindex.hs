{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Database.Bloodhound.Internal.Versions.Common.Types.Reindex where

import Data.Aeson
import Data.List.NonEmpty
import Data.Text (Text)
import Database.Bloodhound.Internal.Utils.Imports (omitNulls)
import Database.Bloodhound.Internal.Versions.Common.Types.Newtypes (IndexName)
import Database.Bloodhound.Internal.Versions.Common.Types.Query (Query)
import Database.Bloodhound.Internal.Versions.Common.Types.Script (ScriptLanguage)
import GHC.Generics
import Optics.Lens

data ReindexRequest = ReindexRequest
  { reindexConflicts :: Maybe ReindexConflicts,
    reindexSource :: ReindexSource,
    reindexDest :: ReindexDest,
    reindexScript :: Maybe ReindexScript
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ReindexRequest where
  toJSON ReindexRequest {..} =
    omitNulls
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

reindexRequestConflictsLens :: Lens' ReindexRequest (Maybe ReindexConflicts)
reindexRequestConflictsLens = lens reindexConflicts (\x y -> x {reindexConflicts = y})

reindexRequestSourceLens :: Lens' ReindexRequest ReindexSource
reindexRequestSourceLens = lens reindexSource (\x y -> x {reindexSource = y})

reindexRequestDestLens :: Lens' ReindexRequest ReindexDest
reindexRequestDestLens = lens reindexDest (\x y -> x {reindexDest = y})

reindexRequestScriptLens :: Lens' ReindexRequest (Maybe ReindexScript)
reindexRequestScriptLens = lens reindexScript (\x y -> x {reindexScript = y})

data ReindexConflicts
  = ReindexAbortOnConflicts
  | ReindexProceedOnConflicts
  deriving stock (Eq, Show, Generic)

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
  deriving stock (Eq, Show, Generic)

instance ToJSON ReindexSource where
  toJSON ReindexSource {..} =
    omitNulls
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

reindexSourceIndexLens :: Lens' ReindexSource (NonEmpty IndexName)
reindexSourceIndexLens = lens reindexSourceIndex (\x y -> x {reindexSourceIndex = y})

reindexSourceMaxDocsLens :: Lens' ReindexSource (Maybe Int)
reindexSourceMaxDocsLens = lens reindexSourceMaxDocs (\x y -> x {reindexSourceMaxDocs = y})

reindexSourceQueryLens :: Lens' ReindexSource (Maybe Query)
reindexSourceQueryLens = lens reindexSourceQuery (\x y -> x {reindexSourceQuery = y})

reindexSourceSizeLens :: Lens' ReindexSource (Maybe Int)
reindexSourceSizeLens = lens reindexSourceSize (\x y -> x {reindexSourceSize = y})

reindexSourceSliceLens :: Lens' ReindexSource (Maybe ReindexSlice)
reindexSourceSliceLens = lens reindexSourceSlice (\x y -> x {reindexSourceSlice = y})

data ReindexSlice = ReindexSlice
  { reindexSliceId :: Maybe Int,
    reindexSliceMax :: Maybe Int
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ReindexSlice where
  toJSON ReindexSlice {..} =
    omitNulls ["id" .= reindexSliceId, "max" .= reindexSliceMax]

instance FromJSON ReindexSlice where
  parseJSON = withObject "ReindexSlice" $ \v ->
    ReindexSlice <$> v .:? "id" <*> v .:? "max"

reindexSliceIdLens :: Lens' ReindexSlice (Maybe Int)
reindexSliceIdLens = lens reindexSliceId (\x y -> x {reindexSliceId = y})

reindexSliceMaxLens :: Lens' ReindexSlice (Maybe Int)
reindexSliceMaxLens = lens reindexSliceMax (\x y -> x {reindexSliceMax = y})

data ReindexDest = ReindexDest
  { reindexDestIndex :: IndexName,
    reindexDestVersionType :: Maybe VersionType,
    reindexDestOpType :: Maybe ReindexOpType
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON ReindexDest where
  toJSON ReindexDest {..} =
    omitNulls
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

reindexDestIndexLens :: Lens' ReindexDest IndexName
reindexDestIndexLens = lens reindexDestIndex (\x y -> x {reindexDestIndex = y})

reindexDestVersionTypeLens :: Lens' ReindexDest (Maybe VersionType)
reindexDestVersionTypeLens = lens reindexDestVersionType (\x y -> x {reindexDestVersionType = y})

reindexDestOpTypeLens :: Lens' ReindexDest (Maybe ReindexOpType)
reindexDestOpTypeLens = lens reindexDestOpType (\x y -> x {reindexDestOpType = y})

data VersionType
  = VersionTypeInternal
  | VersionTypeExternal
  | VersionTypeExternalGT
  | VersionTypeExternalGTE
  deriving stock (Eq, Show, Generic)

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
  deriving stock (Eq, Show, Generic)

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
  deriving stock (Eq, Show, Generic)

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

reindexScriptLanguageLens :: Lens' ReindexScript ScriptLanguage
reindexScriptLanguageLens = lens reindexScriptLanguage (\x y -> x {reindexScriptLanguage = y})

reindexScriptSourceLens :: Lens' ReindexScript Text
reindexScriptSourceLens = lens reindexScriptSource (\x y -> x {reindexScriptSource = y})

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
  deriving stock (Eq, Show, Generic)

instance ToJSON ReindexResponse where
  toJSON ReindexResponse {..} =
    omitNulls
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

reindexResponseTookLens :: Lens' ReindexResponse (Maybe Int)
reindexResponseTookLens = lens reindexResponseTook (\x y -> x {reindexResponseTook = y})

reindexResponseUpdatedLens :: Lens' ReindexResponse Int
reindexResponseUpdatedLens = lens reindexResponseUpdated (\x y -> x {reindexResponseUpdated = y})

reindexResponseCreatedLens :: Lens' ReindexResponse Int
reindexResponseCreatedLens = lens reindexResponseCreated (\x y -> x {reindexResponseCreated = y})

reindexResponseBatchesLens :: Lens' ReindexResponse Int
reindexResponseBatchesLens = lens reindexResponseBatches (\x y -> x {reindexResponseBatches = y})

reindexResponseVersionConflictsLens :: Lens' ReindexResponse Int
reindexResponseVersionConflictsLens = lens reindexResponseVersionConflicts (\x y -> x {reindexResponseVersionConflicts = y})

reindexResponseThrottledMillisLens :: Lens' ReindexResponse Int
reindexResponseThrottledMillisLens = lens reindexResponseThrottledMillis (\x y -> x {reindexResponseThrottledMillis = y})
