{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Bloodhound.OpenSearch2.Requests
  ( module Reexport,
    openPointInTime,
    closePointInTime,
  )
where

import Data.Aeson
import Database.Bloodhound.Client.Cluster
import Database.Bloodhound.Common.Requests as Reexport
import Database.Bloodhound.Internal.Client.BHRequest
import Database.Bloodhound.Internal.Utils.Requests
import Database.Bloodhound.Internal.Versions.Common.Types.Newtypes
import Database.Bloodhound.OpenSearch2.Types
import Prelude hiding (filter, head)

-- | 'openPointInTime' opens a point in time for an index given an 'IndexName'.
-- Note that the point in time should be closed with 'closePointInTime' as soon
-- as it is no longer needed.
--
-- For more information see
-- <https://opensearch.org/docs/latest/search-plugins/point-in-time/>.
openPointInTime ::
  IndexName ->
  BHRequest StatusDependant (ParsedEsResponse OpenPointInTimeResponse)
openPointInTime indexName =
  withBHResponseParsedEsResponse $ post @StatusDependant [unIndexName indexName, "_search", "point_in_time?keep_alive=1m"] emptyBody

-- | 'closePointInTime' closes a point in time given a 'ClosePointInTime'.
--
-- For more information see
-- <https://opensearch.org/docs/latest/search-plugins/point-in-time/>.
closePointInTime ::
  ClosePointInTime ->
  BHRequest StatusDependant (ParsedEsResponse ClosePointInTimeResponse)
closePointInTime q = do
  withBHResponseParsedEsResponse $ deleteWithBody @StatusDependant ["_search", "point_in_time"] (encode q)
