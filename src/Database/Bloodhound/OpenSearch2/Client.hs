{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Bloodhound.OpenSearch2.Client
  ( module Reexport,
    pitSearch,
    openPointInTime,
    closePointInTime,
  )
where

import Control.Monad
import Data.Aeson
import Data.Monoid
import Database.Bloodhound.Client.Cluster
import Database.Bloodhound.Common.Client as Reexport
import Database.Bloodhound.Internal.Client.BHRequest
import qualified Database.Bloodhound.OpenSearch2.Requests as Requests
import Database.Bloodhound.OpenSearch2.Types
import Prelude hiding (filter, head)

-- | 'pitSearch' uses the point in time (PIT) API of elastic, for a given
-- 'IndexName'. Note that this will consume the
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
  forall a m.
  (FromJSON a, MonadBH m, WithBackend OpenSearch2 m) =>
  IndexName ->
  Search ->
  m [Hit a]
pitSearch indexName search = do
  openResp <- openPointInTime indexName
  case openResp of
    Left e -> throwEsError e
    Right OpenPointInTimeResponse {..} -> do
      let searchPIT = search {pointInTime = Just (PointInTime oos2PitId "1m")}
      hits <- pitAccumulator searchPIT []
      closeResp <- closePointInTime (ClosePointInTime oos2PitId)
      case closeResp of
        Left _ -> return []
        Right (ClosePointInTimeResponse False _) ->
          error "failed to close point in time (PIT)"
        Right (ClosePointInTimeResponse True _) -> return hits
  where
    pitAccumulator :: Search -> [Hit a] -> m [Hit a]
    pitAccumulator search' oldHits = do
      resp <- tryPerformBHRequest $ Requests.searchAll search'
      case resp of
        Left _ -> return []
        Right searchResult -> case hits (searchHits searchResult) of
          [] -> return oldHits
          newHits -> case (hitSort $ last newHits, pitId searchResult) of
            (Nothing, Nothing) ->
              error "no point in time (PIT) ID or last sort value"
            (Just _, Nothing) -> error "no point in time (PIT) ID"
            (Nothing, _) -> return (oldHits <> newHits)
            (Just lastSort, Just pitId') -> do
              let newSearch =
                    search'
                      { pointInTime = Just (PointInTime pitId' "1m"),
                        searchAfterKey = Just lastSort
                      }
              pitAccumulator newSearch (oldHits <> newHits)

-- | 'openPointInTime' opens a point in time for an index given an 'IndexName'.
-- Note that the point in time should be closed with 'closePointInTime' as soon
-- as it is no longer needed.
--
-- For more information see
-- <https://opensearch.org/docs/latest/search-plugins/point-in-time/>.
openPointInTime ::
  (MonadBH m, WithBackend OpenSearch2 m) =>
  IndexName ->
  m (ParsedEsResponse OpenPointInTimeResponse)
openPointInTime indexName = performBHRequest $ Requests.openPointInTime indexName

-- | 'closePointInTime' closes a point in time given a 'ClosePointInTime'.
--
-- For more information see
-- <https://opensearch.org/docs/latest/search-plugins/point-in-time/>.
closePointInTime ::
  (MonadBH m, WithBackend OpenSearch2 m) =>
  ClosePointInTime ->
  m (ParsedEsResponse ClosePointInTimeResponse)
closePointInTime q = performBHRequest $ Requests.closePointInTime q
