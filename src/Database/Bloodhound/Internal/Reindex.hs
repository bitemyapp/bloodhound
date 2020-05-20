{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Database.Bloodhound.Internal.Reindex where

import           Data.Aeson
import           Data.List.NonEmpty
import           Data.Text                             (Text)
import           Database.Bloodhound.Common.Script     (ScriptLanguage)
import           Database.Bloodhound.Internal.Newtypes (IndexName)
import           Database.Bloodhound.Internal.Query    (Query)
import           Deriving.Aeson

data ReindexRequest = ReindexRequest { reindexConflicts :: Maybe ReindexConflicts,
                                       reindexSource    :: ReindexSource,
                                       reindexDest      :: ReindexDest,
                                       reindexScript    :: Maybe ReindexScript
                                     }
                      deriving (Show, Eq, Generic)
                      deriving (FromJSON, ToJSON)
                      via CustomJSON '[ OmitNothingFields
                                      , FieldLabelModifier (StripPrefix "reindex", CamelToSnake)
                                      ] ReindexRequest

data ReindexConflicts = ReindexAbortOnConflicts
                      | ReindexProceedOnConflicts
                      deriving (Show, Eq, Generic)

instance FromJSON ReindexConflicts where
  parseJSON = withText "ReindexConflicts" $ \case
    "abort" -> pure ReindexAbortOnConflicts
    "proceed" -> pure ReindexProceedOnConflicts
    s -> fail $ "Expected one of [abort, proceed], found: " <> show s

instance ToJSON ReindexConflicts where
  toJSON = String . \case
    ReindexAbortOnConflicts -> "abort"
    ReindexProceedOnConflicts -> "proceed"

-- | Elasticsearch also supports reindex from remote, it could be added here if required
data ReindexSource = ReindexSource { reindexSourceIndex   :: NonEmpty IndexName,
                                     reindexSourceMaxDocs :: Maybe Int,
                                     reindexSourceQuery   :: Maybe Query,
                                     reindexSourceSize    :: Maybe Int,
                                     reindexSourceSlice   :: Maybe ReindexSlice
                                   }
                      deriving (Show, Eq, Generic)
                      deriving (FromJSON, ToJSON)
                      via CustomJSON '[ OmitNothingFields
                                      , FieldLabelModifier (StripPrefix "reindexSource", CamelToSnake)
                                      ] ReindexSource

data ReindexSlice = ReindexSlice { reindexSliceId  :: Maybe Int,
                                   reindexSliceMax :: Maybe Int
                                 }
                  deriving (Show, Eq, Generic)
                  deriving (FromJSON, ToJSON)
                  via CustomJSON '[ OmitNothingFields
                                  , FieldLabelModifier (StripPrefix "reindexSlice", CamelToSnake)
                                  ] ReindexSlice

data ReindexDest = ReindexDest { reindexDestIndex       :: IndexName,
                                 reindexDestVersionType :: Maybe VersionType,
                                 reindexDestOpType      :: Maybe ReindexOpType
                               }
                 deriving (Show, Eq, Generic)
                 deriving (FromJSON, ToJSON)
                 via CustomJSON '[ OmitNothingFields
                                 , FieldLabelModifier (StripPrefix "reindexDest", CamelToSnake)
                                 ] ReindexDest

data VersionType = VersionTypeInternal
                 | VersionTypeExternal
                 | VersionTypeExternalGT
                 | VersionTypeExternalGTE
                 deriving (Show, Eq, Generic)

instance ToJSON VersionType where
  toJSON = String . \case
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

data ReindexOpType = OpCreate
                   | OpIndex
                   deriving (Show, Eq, Generic)

instance FromJSON ReindexOpType where
  parseJSON = withText "ReindexOpType" $ \case
    "create" -> pure OpCreate
    "index" -> pure OpIndex
    s -> fail $ "Expected one of [create, index], found: " <> show s

instance ToJSON ReindexOpType where
  toJSON OpCreate = String "create"
  toJSON OpIndex  = String "index"

data ReindexScript = ReindexScript { reindexScriptLanguage :: ScriptLanguage
                                   , reindexScriptSource   :: Text
                                   }
                   deriving (Show, Eq, Generic)
                   deriving (FromJSON, ToJSON)
                   via CustomJSON '[ OmitNothingFields
                                   , FieldLabelModifier (StripPrefix "reindexScript", CamelToSnake)
                                   ] ReindexScript

mkReindexRequest :: IndexName -> IndexName -> ReindexRequest
mkReindexRequest src dst =
  ReindexRequest { reindexSource =
                   ReindexSource { reindexSourceIndex   = src :| []
                                 , reindexSourceMaxDocs = Nothing
                                 , reindexSourceQuery   = Nothing
                                 , reindexSourceSize    = Nothing
                                 , reindexSourceSlice   = Nothing
                                 }
                 , reindexDest =
                   ReindexDest { reindexDestIndex        = dst
                               ,  reindexDestVersionType = Nothing
                               ,  reindexDestOpType      = Nothing
                               }
                 , reindexConflicts = Nothing
                 , reindexScript = Nothing
                 }

data ReindexResponse = ReindexResponse { reindexResponseTook             :: Int
                                       , reindexResponseUpdated          :: Int
                                       , reindexResponseCreated          :: Int
                                       , reindexResponseBatches          :: Int
                                       , reindexResponseVersionConflicts :: Int
                                       , reindexResponseRetries          :: Int
                                       , reindexResponseThrottledMillis  :: Int
                                       , reindexResponseFailures         :: Int
                                       }
                     deriving (Show, Eq, Generic)
                     deriving (FromJSON, ToJSON)
                     via CustomJSON '[ OmitNothingFields
                                     , FieldLabelModifier (StripPrefix "reindexResponse", CamelToSnake)
                                     ] ReindexResponse
