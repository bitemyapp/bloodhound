{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module : Database.Bloodhound.Dynamic.Client
-- Copyright : (C) 2014, 2018 Chris Allen
-- License : BSD-style (see the file LICENSE)
-- Maintainer : Chris Allen <cma@bitemyapp.com>
-- Stability : provisional
-- Portability : GHC
--
-- Dynamically route the query depending on the backend.
--
-- @
-- withFetchedBackendType $ \backend ->
--   pitSearch backend index search
-- @
module Database.Bloodhound.Dynamic.Client
  ( module Reexport,
    guessBackendType,
    withFetchedBackendType,
    pitSearch,
  )
where

import Data.Aeson
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Versions as Versions
import Database.Bloodhound.Client.Cluster
import Database.Bloodhound.Common.Client as Reexport
import qualified Database.Bloodhound.ElasticSearch7.Client as ClientES2
import qualified Database.Bloodhound.OpenSearch2.Client as ClientOS2
import Database.Bloodhound.OpenSearch2.Types
import Lens.Micro (toListOf)
import Prelude hiding (filter, head)

-- | Try to guess the current 'BackendType'
guessBackendType :: NodeInfo -> Maybe BackendType
guessBackendType nodeInfo =
  case listToMaybe $ toListOf Versions.major $ versionNumber $ nodeInfoESVersion nodeInfo of
    Just 7 -> Just ElasticSearch7
    Just 1 -> Just OpenSearch1
    Just 2 -> Just OpenSearch2
    _ -> Nothing

-- | Fetch the currently running backend and run backend-dependent code
withFetchedBackendType :: (MonadBH m) => (forall backend. SBackendType backend -> m a) -> m a
withFetchedBackendType f = do
  nodeInfo <- getNodesInfo LocalNode
  let backend = fromMaybe Dynamic $ listToMaybe (nodesInfo nodeInfo) >>= guessBackendType
  withDynamicBH backend $ MkBH . f

-- | 'pitSearch' uses the point in time (PIT) API of elastic, for a given
-- 'IndexName'. Requires Elasticsearch >=7.10 or OpenSearch >=2. Note that this will consume the
-- entire search result set and will be doing O(n) list appends so this may
-- not be suitable for large result sets. In that case, the point in time API
-- should be used directly with `openPointInTime` and `closePointInTime`.
--
-- Note that 'pitSearch' utilizes the 'search_after' parameter under the hood,
-- which requires a non-empty 'sortBody' field in the provided 'Search' value.
-- Otherwise, 'pitSearch' will fail to return all matching documents.
--
-- For more information see
-- <https://opensearch.org/docs/latest/search-plugins/point-in-time/>.
pitSearch ::
  forall a m backend.
  (FromJSON a, MonadBH m) =>
  SBackendType backend ->
  IndexName ->
  Search ->
  m [Hit a]
pitSearch backend indexName search =
  case backend of
    SElasticSearch7 -> unsafePerformBH @'ElasticSearch7 $ ClientES2.pitSearch indexName search
    SOpenSearch1 -> throwEsError $ EsError Nothing "pitSearch is not supported by OpenSearch1"
    SOpenSearch2 -> unsafePerformBH @'OpenSearch2 $ ClientOS2.pitSearch indexName search
