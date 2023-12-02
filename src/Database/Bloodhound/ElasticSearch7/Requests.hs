{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Database.Bloodhound.ElasticSearch7.Requests
  ( module Reexport,
    openPointInTime,
    closePointInTime,
  )
where

import Data.Aeson
import Database.Bloodhound.Client.Cluster
import Database.Bloodhound.Common.Requests as Reexport
import Database.Bloodhound.ElasticSearch7.Types
import Database.Bloodhound.Internal.Utils.Requests
import Prelude hiding (filter, head)

-- | 'openPointInTime' opens a point in time for an index given an 'IndexName'.
-- Note that the point in time should be closed with 'closePointInTime' as soon
-- as it is no longer needed.
--
-- For more information see
-- <https://www.elastic.co/guide/en/elasticsearch/reference/current/point-in-time-api.html>.
openPointInTime ::
  IndexName ->
  BHRequest StatusDependant (ParsedEsResponse OpenPointInTimeResponse)
openPointInTime indexName =
  withBHResponseParsedEsResponse $ post @StatusDependant [unIndexName indexName, "_pit?keep_alive=1m"] emptyBody

-- | 'closePointInTime' closes a point in time given a 'ClosePointInTime'.
--
-- For more information see
-- <https://www.elastic.co/guide/en/elasticsearch/reference/current/point-in-time-api.html>.
closePointInTime ::
  ClosePointInTime ->
  BHRequest StatusDependant (ParsedEsResponse ClosePointInTimeResponse)
closePointInTime q = do
  withBHResponseParsedEsResponse $ deleteWithBody @StatusDependant ["_pit"] (encode q)
