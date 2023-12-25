{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Bloodhound.Internal.Versions.Common.Types.Reindex where

import Data.Aeson
import Data.List.NonEmpty
import Data.Text (Text)
import Database.Bloodhound.Internal.Utils.Imports (optionsDerivingStrippingPrefix)
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
  toEncoding = genericToEncoding $ optionsDerivingStrippingPrefix "reindex"

instance FromJSON ReindexRequest where
  parseJSON = genericParseJSON $ optionsDerivingStrippingPrefix "reindex"

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
  toEncoding = genericToEncoding $ optionsDerivingStrippingPrefix "reindexSource"

instance FromJSON ReindexSource where
  parseJSON = genericParseJSON $ optionsDerivingStrippingPrefix "reindexSource"

data ReindexSlice = ReindexSlice
  { reindexSliceId :: Maybe Int,
    reindexSliceMax :: Maybe Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON ReindexSlice where
  toEncoding = genericToEncoding $ optionsDerivingStrippingPrefix "reindexSlice"

instance FromJSON ReindexSlice where
  parseJSON = genericParseJSON $ optionsDerivingStrippingPrefix "reindexSlice"

data ReindexDest = ReindexDest
  { reindexDestIndex :: IndexName,
    reindexDestVersionType :: Maybe VersionType,
    reindexDestOpType :: Maybe ReindexOpType
  }
  deriving (Show, Eq, Generic)

instance ToJSON ReindexDest where
  toEncoding = genericToEncoding $ optionsDerivingStrippingPrefix "reindexDest"

instance FromJSON ReindexDest where
  parseJSON = genericParseJSON $ optionsDerivingStrippingPrefix "reindexDest"

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
  toEncoding = genericToEncoding $ optionsDerivingStrippingPrefix "reindexScript"

instance FromJSON ReindexScript where
  parseJSON = genericParseJSON $ optionsDerivingStrippingPrefix "reindexScript"

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
  toEncoding = genericToEncoding $ optionsDerivingStrippingPrefix "reindexResponse"

instance FromJSON ReindexResponse where
  parseJSON = genericParseJSON $ optionsDerivingStrippingPrefix "reindexResponse"
