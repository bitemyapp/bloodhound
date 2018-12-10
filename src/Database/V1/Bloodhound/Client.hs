{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-------------------------------------------------------------------------------
-- |
-- Module : Database.Bloodhound.Client
-- Copyright : (C) 2014 Chris Allen
-- License : BSD-style (see the file LICENSE)
-- Maintainer : Chris Allen <cma@bitemyapp.com
-- Stability : provisional
-- Portability : OverloadedStrings
--
-- Client side functions for talking to Elasticsearch servers.
--
-------------------------------------------------------------------------------

module Database.V1.Bloodhound.Client
       ( -- * Bloodhound client functions
         -- | The examples in this module assume the following code has been run.
         --   The :{ and :} will only work in GHCi. You'll only need the data types
         --   and typeclass instances for the functions that make use of them.

         -- $setup
         withBH
       -- ** Indices
       , createIndex
       , deleteIndex
       , updateIndexSettings
       , getIndexSettings
       , optimizeIndex
       , indexExists
       , openIndex
       , closeIndex
       , listIndices
       , waitForYellowIndex
       -- *** Index Aliases
       , updateIndexAliases
       , getIndexAliases
       -- *** Index Templates
       , putTemplate
       , templateExists
       , deleteTemplate
       -- ** Mapping
       , putMapping
       , deleteMapping
       -- ** Documents
       , indexDocument
       , updateDocument
       , getDocument
       , documentExists
       , deleteDocument
       -- ** Searching
       , searchAll
       , searchByIndex
       , searchByType
       , scanSearch
       , getInitialScroll
       , advanceScroll
       , refreshIndex
       , mkSearch
       , mkAggregateSearch
       , mkHighlightSearch
       , bulk
       , pageSearch
       , mkShardCount
       , mkReplicaCount
       , getStatus
       -- ** Snapshot/Restore
       -- *** Snapshot Repos
       , getSnapshotRepos
       , updateSnapshotRepo
       , verifySnapshotRepo
       , deleteSnapshotRepo
       -- *** Snapshots
       , createSnapshot
       , getSnapshots
       , deleteSnapshot
       -- *** Restoring Snapshots
       , restoreSnapshot
       -- ** Nodes
       , getNodesInfo
       , getNodesStats
       -- ** Request Utilities
       , encodeBulkOperations
       , encodeBulkOperation
       -- * Authentication
       , basicAuthHook
       -- * Reply-handling tools
       , isVersionConflict
       , isSuccess
       , isCreated
       , parseEsResponse
       )
       where

import qualified Blaze.ByteString.Builder     as BB
import           Control.Applicative          as A
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.ByteString.Lazy.Builder
import qualified Data.ByteString.Lazy.Char8   as L
import           Data.Foldable                (toList)
import qualified Data.HashMap.Strict          as HM
import           Data.Ix
import qualified Data.List                    as LS (filter, foldl')
import           Data.List.NonEmpty           (NonEmpty (..))
import           Data.Maybe                   (catMaybes, fromMaybe, isJust)
import           Data.Monoid
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import           Data.Time.Clock
import qualified Data.Vector                  as V
import           Network.HTTP.Client
import qualified Network.HTTP.Types.Method    as NHTM
import qualified Network.HTTP.Types.Status    as NHTS
import qualified Network.HTTP.Types.URI       as NHTU
import qualified Network.URI                  as URI
import           Prelude                      hiding (filter, head)

import           Database.V1.Bloodhound.Types

-- | 'mkShardCount' is a straight-forward smart constructor for 'ShardCount'
--   which rejects 'Int' values below 1 and above 1000.
mkShardCount :: Int -> Maybe ShardCount
mkShardCount n
  | n < 1 = Nothing
  | n > 1000 = Nothing
  | otherwise = Just (ShardCount n)

-- | 'mkReplicaCount' is a straight-forward smart constructor for 'ReplicaCount'
--   which rejects 'Int' values below 0 and above 1000.
mkReplicaCount :: Int -> Maybe ReplicaCount
mkReplicaCount n
  | n < 0 = Nothing
  | n > 1000 = Nothing -- ...
  | otherwise = Just (ReplicaCount n)

emptyBody :: L.ByteString
emptyBody = L.pack ""

dispatch :: MonadBH m
         => Method
         -> Text
         -> Maybe L.ByteString
         -> m Reply
dispatch dMethod url body = do
  initReq <- liftIO $ parseUrl' url
  reqHook <- bhRequestHook A.<$> getBHEnv
  let reqBody = RequestBodyLBS $ fromMaybe emptyBody body
  req <- liftIO
         $ reqHook
         $ setRequestIgnoreStatus
         $ initReq { method = dMethod
                   , requestBody = reqBody }
  mgr <- bhManager <$> getBHEnv
  liftIO $ httpLbs req mgr

joinPath' :: [Text] -> Text
joinPath' = T.intercalate "/"

joinPath :: MonadBH m => [Text] -> m Text
joinPath ps = do
  Server s <- bhServer <$> getBHEnv
  return $ joinPath' (s:ps)

appendSearchTypeParam :: Text -> SearchType -> Text
appendSearchTypeParam originalUrl st = addQuery params originalUrl
  where stText = "search_type"
        params
          | st == SearchTypeDfsQueryThenFetch = [(stText, Just "dfs_query_then_fetch")]
          | st == SearchTypeCount             = [(stText, Just "count")]
          | st == SearchTypeScan              = [(stText, Just "scan"), ("scroll", Just "1m")]
          | st == SearchTypeQueryAndFetch     = [(stText, Just "query_and_fetch")]
          | st == SearchTypeDfsQueryAndFetch  = [(stText, Just "dfs_query_and_fetch")]
        -- used to catch 'SearchTypeQueryThenFetch', which is also the default
          | otherwise                         = [(stText, Just "query_then_fetch")]

-- | Severely dumbed down query renderer. Assumes your data doesn't
-- need any encoding
addQuery :: [(Text, Maybe Text)] -> Text -> Text
addQuery q u = u <> rendered
  where
    rendered =
      T.decodeUtf8 $ BB.toByteString $ NHTU.renderQueryText prependQuestionMark q
    prependQuestionMark = True

bindM2 :: (Applicative m, Monad m) => (a -> b -> m c) -> m a -> m b -> m c
bindM2 f ma mb = join (f <$> ma <*> mb)

-- | Convenience function that sets up a manager and BHEnv and runs
-- the given set of bloodhound operations. Connections will be
-- pipelined automatically in accordance with the given manager
-- settings in IO. If you've got your own monad transformer stack, you
-- should use 'runBH' directly.
withBH :: ManagerSettings -> Server -> BH IO a -> IO a
withBH ms s f = do
  mgr <- newManager ms
  let env = mkBHEnv s mgr
  runBH env f

-- Shortcut functions for HTTP methods
delete :: MonadBH m => Text -> m Reply
delete = flip (dispatch NHTM.methodDelete) Nothing
get    :: MonadBH m => Text -> m Reply
get    = flip (dispatch NHTM.methodGet) Nothing
head   :: MonadBH m => Text -> m Reply
head   = flip (dispatch NHTM.methodHead) Nothing
put    :: MonadBH m => Text -> Maybe L.ByteString -> m Reply
put    = dispatch NHTM.methodPut
post   :: MonadBH m => Text -> Maybe L.ByteString -> m Reply
post   = dispatch NHTM.methodPost

-- indexDocument s ix name doc = put (root </> s </> ix </> name </> doc) (Just encode doc)
-- http://hackage.haskell.org/package/http-client-lens-0.1.0/docs/Network-HTTP-Client-Lens.html
-- https://github.com/supki/libjenkins/blob/master/src/Jenkins/Rest/Internal.hs

-- | 'getStatus' fetches the 'Status' of a 'Server'
getStatus :: MonadBH m => m (Maybe Status)
getStatus = do
  response <- get =<< url
  return $ decode (responseBody response)
  where
    url = joinPath []

-- | 'getSnapshotRepos' gets the definitions of a subset of the
-- defined snapshot repos.
getSnapshotRepos
    :: ( MonadBH m
       , MonadThrow m
       )
    => SnapshotRepoSelection
    -> m (Either EsError [GenericSnapshotRepo])
getSnapshotRepos sel = fmap (fmap unGSRs) . parseEsResponse =<< get =<< url
  where
    url = joinPath ["_snapshot", selectorSeg]
    selectorSeg = case sel of
                    AllSnapshotRepos -> "_all"
                    SnapshotRepoList (p :| ps) -> T.intercalate "," (renderPat <$> (p:ps))
    renderPat (RepoPattern t)                  = t
    renderPat (ExactRepo (SnapshotRepoName t)) = t


-- | Wrapper to extract the list of 'GenericSnapshotRepo' in the
-- format they're returned in
newtype GSRs = GSRs { unGSRs :: [GenericSnapshotRepo] }


instance FromJSON GSRs where
  parseJSON = withObject "Collection of GenericSnapshotRepo" parse
    where
      parse = fmap GSRs . mapM (uncurry go) . HM.toList
      go rawName = withObject "GenericSnapshotRepo" $ \o ->
        GenericSnapshotRepo (SnapshotRepoName rawName) <$> o .: "type"
                                                       <*> o .: "settings"


-- | Create or update a snapshot repo
updateSnapshotRepo
  :: ( MonadBH m
     , SnapshotRepo repo
     )
  => SnapshotRepoUpdateSettings
  -- ^ Use 'defaultSnapshotRepoUpdateSettings' if unsure
  -> repo
  -> m Reply
updateSnapshotRepo SnapshotRepoUpdateSettings {..} repo =
  bindM2 put url (return (Just body))
  where
    url = addQuery params <$> joinPath ["_snapshot", snapshotRepoName gSnapshotRepoName]
    params
      | repoUpdateVerify = []
      | otherwise        = [("verify", Just "false")]
    body = encode $ object [ "type" .= gSnapshotRepoType
                           , "settings" .= gSnapshotRepoSettings
                           ]
    GenericSnapshotRepo {..} = toGSnapshotRepo repo



-- | Verify if a snapshot repo is working. __NOTE:__ this API did not
-- make it into Elasticsearch until 1.4. If you use an older version,
-- you will get an error here.
verifySnapshotRepo
    :: ( MonadBH m
       , MonadThrow m
       )
    => SnapshotRepoName
    -> m (Either EsError SnapshotVerification)
verifySnapshotRepo (SnapshotRepoName n) =
  parseEsResponse =<< bindM2 post url (return Nothing)
  where
    url = joinPath ["_snapshot", n, "_verify"]


deleteSnapshotRepo :: MonadBH m => SnapshotRepoName -> m Reply
deleteSnapshotRepo (SnapshotRepoName n) = delete =<< url
  where
    url = joinPath ["_snapshot", n]


-- | Create and start a snapshot
createSnapshot
    :: (MonadBH m)
    => SnapshotRepoName
    -> SnapshotName
    -> SnapshotCreateSettings
    -> m Reply
createSnapshot (SnapshotRepoName repoName)
               (SnapshotName snapName)
               SnapshotCreateSettings {..} =
  bindM2 put url (return (Just body))
  where
    url = addQuery params <$> joinPath ["_snapshot", repoName, snapName]
    params = [("wait_for_completion", Just (boolQP snapWaitForCompletion))]
    body = encode $ object prs
    prs = catMaybes [ ("indices" .=) . indexSelectionName <$> snapIndices
                    , Just ("ignore_unavailable" .= snapIgnoreUnavailable)
                    , Just ("ignore_global_state" .= snapIncludeGlobalState)
                    , Just ("partial" .= snapPartial)
                    ]


indexSelectionName :: IndexSelection -> Text
indexSelectionName AllIndexes            = "_all"
indexSelectionName (IndexList (i :| is)) = T.intercalate "," (renderIndex <$> (i:is))
  where
    renderIndex (IndexName n) = n


-- | Get info about known snapshots given a pattern and repo name.
getSnapshots
    :: ( MonadBH m
       , MonadThrow m
       )
    => SnapshotRepoName
    -> SnapshotSelection
    -> m (Either EsError [SnapshotInfo])
getSnapshots (SnapshotRepoName repoName) sel =
  fmap (fmap unSIs) . parseEsResponse =<< get =<< url
  where
    url = joinPath ["_snapshot", repoName, snapPath]
    snapPath = case sel of
      AllSnapshots -> "_all"
      SnapshotList (s :| ss) -> T.intercalate "," (renderPath <$> (s:ss))
    renderPath (SnapPattern t)              = t
    renderPath (ExactSnap (SnapshotName t)) = t


newtype SIs = SIs { unSIs :: [SnapshotInfo] }


instance FromJSON SIs where
  parseJSON = withObject "Collection of SnapshotInfo" parse
    where
      parse o = SIs <$> o .: "snapshots"


-- | Delete a snapshot. Cancels if it is running.
deleteSnapshot :: MonadBH m => SnapshotRepoName -> SnapshotName -> m Reply
deleteSnapshot (SnapshotRepoName repoName) (SnapshotName snapName) =
  delete =<< url
  where
    url = joinPath ["_snapshot", repoName, snapName]


-- | Restore a snapshot to the cluster See
-- <https://www.elastic.co/guide/en/elasticsearch/reference/1.7/modules-snapshots.html#_restore>
-- for more details.
restoreSnapshot
    :: MonadBH m
    => SnapshotRepoName
    -> SnapshotName
    -> SnapshotRestoreSettings
    -- ^ Start with 'defaultSnapshotRestoreSettings' and customize
    -- from there for reasonable defaults.
    -> m Reply
restoreSnapshot (SnapshotRepoName repoName)
                (SnapshotName snapName)
                SnapshotRestoreSettings {..} = bindM2 post url (return (Just body))
  where
    url = addQuery params <$> joinPath ["_snapshot", repoName, snapName, "_restore"]
    params = [("wait_for_completion", Just (boolQP snapRestoreWaitForCompletion))]
    body = encode (object prs)


    prs = catMaybes [ ("indices" .=) . indexSelectionName <$> snapRestoreIndices
                 , Just ("ignore_unavailable" .= snapRestoreIgnoreUnavailable)
                 , Just ("include_global_state" .= snapRestoreIncludeGlobalState)
                 , ("rename_pattern" .=) <$> snapRestoreRenamePattern
                 , ("rename_replacement" .=) . renderTokens <$> snapRestoreRenameReplacement
                 , Just ("include_aliases" .= snapRestoreIncludeAliases)
                 , ("index_settings" .= ) <$> snapRestoreIndexSettingsOverrides
                 , ("ignore_index_settings" .= ) <$> snapRestoreIgnoreIndexSettings
                 ]
    renderTokens (t :| ts) = mconcat (renderToken <$> (t:ts))
    renderToken (RRTLit t)      = t
    renderToken RRSubWholeMatch = "$0"
    renderToken (RRSubGroup g)  = T.pack (show (rrGroupRefNum g))


getNodesInfo
    :: ( MonadBH m
       , MonadThrow m
       )
    => NodeSelection
    -> m (Either EsError NodesInfo)
getNodesInfo sel = parseEsResponse =<< get =<< url
  where
    url = joinPath ["_nodes", selectionSeg]
    selectionSeg = case sel of
      LocalNode -> "_local"
      NodeList (l :| ls) -> T.intercalate "," (selToSeg <$> (l:ls))
      AllNodes -> "_all"
    selToSeg (NodeByName (NodeName n))            = n
    selToSeg (NodeByFullNodeId (FullNodeId i))    = i
    selToSeg (NodeByHost (Server s))              = s
    selToSeg (NodeByAttribute (NodeAttrName a) v) = a <> ":" <> v

getNodesStats
    :: ( MonadBH m
       , MonadThrow m
       )
    => NodeSelection
    -> m (Either EsError NodesStats)
getNodesStats sel = parseEsResponse =<< get =<< url
  where
    url = joinPath ["_nodes", selectionSeg, "stats"]
    selectionSeg = case sel of
      LocalNode -> "_local"
      NodeList (l :| ls) -> T.intercalate "," (selToSeg <$> (l:ls))
      AllNodes -> "_all"
    selToSeg (NodeByName (NodeName n))            = n
    selToSeg (NodeByFullNodeId (FullNodeId i))    = i
    selToSeg (NodeByHost (Server s))              = s
    selToSeg (NodeByAttribute (NodeAttrName a) v) = a <> ":" <> v

-- | 'createIndex' will create an index given a 'Server', 'IndexSettings', and an 'IndexName'.
createIndex :: MonadBH m => IndexSettings -> IndexName -> m Reply
createIndex indexSettings (IndexName indexName) =
  bindM2 put url (return body)
  where url = joinPath [indexName]
        body = Just $ encode indexSettings


-- | 'deleteIndex' will delete an index given a 'Server', and an 'IndexName'.
deleteIndex :: MonadBH m => IndexName -> m Reply
deleteIndex (IndexName indexName) =
  delete =<< joinPath [indexName]

-- | 'updateIndexSettings' will apply a non-empty list of setting updates to an index
updateIndexSettings :: MonadBH m => NonEmpty UpdatableIndexSetting -> IndexName -> m Reply
updateIndexSettings updates (IndexName indexName) =
  bindM2 put url (return body)
  where
    url = joinPath [indexName, "_settings"]
    body = Just (encode jsonBody)
    jsonBody = Object (deepMerge [u | Object u <- toJSON <$> toList updates])


getIndexSettings :: (MonadBH m, MonadThrow m) => IndexName
                 -> m (Either EsError IndexSettingsSummary)
getIndexSettings (IndexName indexName) =
  parseEsResponse =<< get =<< url
  where
    url = joinPath [indexName, "_settings"]


-- | 'optimizeIndex' will optimize a single index, list of indexes or
-- all indexes. Note that this call will block until finishing but
-- will continue even if the request times out. Concurrent requests to
-- optimize an index while another is performing will block until the
-- previous one finishes. For more information see
-- <https://www.elastic.co/guide/en/elasticsearch/reference/1.7/indices-optimize.html>. Nothing
-- worthwhile comes back in the reply body, so matching on the status
-- should suffice.
--
-- 'optimizeIndex' with a maxNumSegments of 1 and onlyExpungeDeletes
-- to True is the main way to release disk space back to the OS being
-- held by deleted documents.
--
-- Note that this API was deprecated in Elasticsearch 2.1 for the
-- almost completely identical forcemerge API. Adding support to that
-- API would be trivial but due to the significant breaking changes,
-- this library cannot currently be used with >= 2.0, so that feature was omitted.
optimizeIndex :: MonadBH m => IndexSelection -> IndexOptimizationSettings -> m Reply
optimizeIndex ixs IndexOptimizationSettings {..} =
    bindM2 post url (return body)
  where url = addQuery params <$> joinPath [indexName, "_optimize"]
        params = catMaybes [ ("max_num_segments",) . Just . showText <$> maxNumSegments
                           , Just ("only_expunge_deletes", Just (boolQP onlyExpungeDeletes))
                           , Just ("flush", Just (boolQP flushAfterOptimize))
                           ]
        indexName = indexSelectionName ixs
        body = Nothing


deepMerge :: [Object] -> Object
deepMerge = LS.foldl' go mempty
  where go acc = LS.foldl' go' acc . HM.toList
        go' acc (k, v) = HM.insertWith merge k v acc
        merge (Object a) (Object b) = Object (deepMerge [a, b])
        merge _ b = b


statusCodeIs :: (Int, Int) -> Reply -> Bool
statusCodeIs r resp = inRange r $ NHTS.statusCode (responseStatus resp)

respIsTwoHunna :: Reply -> Bool
respIsTwoHunna = statusCodeIs (200, 299)

existentialQuery :: MonadBH m => Text -> m (Reply, Bool)
existentialQuery url = do
  reply <- head url
  return (reply, respIsTwoHunna reply)


-- | Tries to parse a response body as the expected type @a@ and
-- failing that tries to parse it as an EsError. All well-formed, JSON
-- responses from elasticsearch should fall into these two
-- categories. If they don't, a 'EsProtocolException' will be
-- thrown. If you encounter this, please report the full body it
-- reports along with your Elasticsearch verison.
parseEsResponse :: (MonadThrow m, FromJSON a) => Reply
                -> m (Either EsError a)
parseEsResponse reply
  | respIsTwoHunna reply = case eitherDecode body of
                             Right a -> return (Right a)
                             Left _ -> tryParseError
  | otherwise = tryParseError
  where body = responseBody reply
        tryParseError = case eitherDecode body of
                          Right e -> return (Left e)
                          -- this case should not be possible
                          Left _ -> explode
        explode = throwM (EsProtocolException body)

-- | 'indexExists' enables you to check if an index exists. Returns 'Bool'
--   in IO
indexExists :: MonadBH m => IndexName -> m Bool
indexExists (IndexName indexName) = do
  (_, exists) <- existentialQuery =<< joinPath [indexName]
  return exists

-- | 'refreshIndex' will force a refresh on an index. You must
-- do this if you want to read what you wrote.
refreshIndex :: MonadBH m => IndexName -> m Reply
refreshIndex (IndexName indexName) =
  bindM2 post url (return Nothing)
  where url = joinPath [indexName, "_refresh"]

-- | Block until the index becomes available for indexing
--   documents. This is useful for integration tests in which
--   indices are rapidly created and deleted.
waitForYellowIndex :: MonadBH m => IndexName -> m Reply
waitForYellowIndex (IndexName indexName) = get =<< url
  where url = addQuery q <$> joinPath ["_cluster","health",indexName]
        q = [("wait_for_status",Just "yellow"),("timeout",Just "10s")]

stringifyOCIndex :: OpenCloseIndex -> Text
stringifyOCIndex oci = case oci of
  OpenIndex  -> "_open"
  CloseIndex -> "_close"

openOrCloseIndexes :: MonadBH m => OpenCloseIndex -> IndexName -> m Reply
openOrCloseIndexes oci (IndexName indexName) =
  bindM2 post url (return Nothing)
  where ociString = stringifyOCIndex oci
        url = joinPath [indexName, ociString]

-- | 'openIndex' opens an index given a 'Server' and an 'IndexName'. Explained in further detail at
--   <http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/indices-open-close.html>
openIndex :: MonadBH m => IndexName -> m Reply
openIndex = openOrCloseIndexes OpenIndex

-- | 'closeIndex' closes an index given a 'Server' and an 'IndexName'. Explained in further detail at
--   <http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/indices-open-close.html>
closeIndex :: MonadBH m => IndexName -> m Reply
closeIndex = openOrCloseIndexes CloseIndex

-- | 'listIndices' returns a list of all index names on a given 'Server'
listIndices :: (MonadThrow m, MonadBH m) => m [IndexName]
listIndices =
  parse . responseBody =<< get =<< url
  where
    url = joinPath ["_cat/indices?format=json"]
    parse body = maybe (throwM (EsProtocolException body)) return $ do
      vals <- decode body
      forM vals $ \val ->
        case val of
          Object obj -> do
            indexVal <- HM.lookup "index" obj
            case indexVal of
              String txt -> Just (IndexName txt)
              _ -> Nothing
          _ -> Nothing

-- | 'updateIndexAliases' updates the server's index alias
-- table. Operations are atomic. Explained in further detail at
-- <https://www.elastic.co/guide/en/elasticsearch/reference/current/indices-aliases.html>
updateIndexAliases :: MonadBH m => NonEmpty IndexAliasAction -> m Reply
updateIndexAliases actions = bindM2 post url (return body)
  where url = joinPath ["_aliases"]
        body = Just (encode bodyJSON)
        bodyJSON = object [ "actions" .= toList actions]

-- | Get all aliases configured on the server.
getIndexAliases :: (MonadBH m, MonadThrow m)
                => m (Either EsError IndexAliasesSummary)
getIndexAliases = parseEsResponse =<< get =<< url
  where url = joinPath ["_aliases"]

-- | 'putTemplate' creates a template given an 'IndexTemplate' and a 'TemplateName'.
--   Explained in further detail at
--   <https://www.elastic.co/guide/en/elasticsearch/reference/1.7/indices-templates.html>
putTemplate :: MonadBH m => IndexTemplate -> TemplateName -> m Reply
putTemplate indexTemplate (TemplateName templateName) =
  bindM2 put url (return body)
  where url = joinPath ["_template", templateName]
        body = Just $ encode indexTemplate

-- | 'templateExists' checks to see if a template exists.
templateExists :: MonadBH m => TemplateName -> m Bool
templateExists (TemplateName templateName) = do
  (_, exists) <- existentialQuery =<< joinPath ["_template", templateName]
  return exists

-- | 'deleteTemplate' is an HTTP DELETE and deletes a template.
deleteTemplate :: MonadBH m => TemplateName -> m Reply
deleteTemplate (TemplateName templateName) =
  delete =<< joinPath ["_template", templateName]

-- | 'putMapping' is an HTTP PUT and has upsert semantics. Mappings are schemas
-- for documents in indexes.
putMapping :: (MonadBH m, ToJSON a) => IndexName
                 -> MappingName -> a -> m Reply
putMapping (IndexName indexName) (MappingName mappingName) mapping =
  bindM2 put url (return body)
  where url = joinPath [indexName, "_mapping", mappingName]
        -- "_mapping" and mappingName above were originally transposed
        -- erroneously. The correct API call is: "/INDEX/_mapping/MAPPING_NAME"
        body = Just $ encode mapping

-- | 'deleteMapping' is an HTTP DELETE and deletes a mapping for a given index.
-- Mappings are schemas for documents in indexes.
deleteMapping :: MonadBH m => IndexName -> MappingName -> m Reply
deleteMapping (IndexName indexName)
  (MappingName mappingName) =
  -- "_mapping" and mappingName below were originally transposed
  -- erroneously. The correct API call is: "/INDEX/_mapping/MAPPING_NAME"
  delete =<< joinPath [indexName, "_mapping", mappingName]

versionCtlParams :: IndexDocumentSettings -> [(Text, Maybe Text)]
versionCtlParams cfg =
  case idsVersionControl cfg of
    NoVersionControl -> []
    InternalVersion v -> versionParams v "internal"
    ExternalGT (ExternalDocVersion v) -> versionParams v "external_gt"
    ExternalGTE (ExternalDocVersion v) -> versionParams v "external_gte"
    ForceVersion (ExternalDocVersion v) -> versionParams v "force"
  where
    vt = showText . docVersionNumber
    versionParams v t = [ ("version", Just $ vt v)
                        , ("version_type", Just t)
                        ]

-- | 'indexDocument' is the primary way to save a single document in
--   Elasticsearch. The document itself is simply something we can
--   convert into a JSON 'Value'. The 'DocId' will function as the
--   primary key for the document.
indexDocument :: (ToJSON doc, MonadBH m) => IndexName -> MappingName
                 -> IndexDocumentSettings -> doc -> DocId -> m Reply
indexDocument (IndexName indexName)
  (MappingName mappingName) cfg document (DocId docId) =
  bindM2 put url (return body)
  where url = addQuery params <$> joinPath [indexName, mappingName, docId]
        parentParams = case idsParent cfg of
          Nothing -> []
          Just (DocumentParent (DocId p)) -> [ ("parent", Just p) ]
        params = versionCtlParams cfg ++ parentParams
        body = Just (encode document)

-- | 'updateDocument' provides a way to perform an partial update of a
-- an already indexed document.
updateDocument :: (ToJSON patch, MonadBH m) => IndexName -> MappingName
                  -> IndexDocumentSettings -> patch -> DocId -> m Reply
updateDocument (IndexName indexName)
  (MappingName mappingName) cfg patch (DocId docId) =
  bindM2 post url (return body)
  where url = addQuery (versionCtlParams cfg) <$>
              joinPath [indexName, mappingName, docId, "_update"]
        body = Just (encode $ object ["doc" .= toJSON patch])

-- | 'deleteDocument' is the primary way to delete a single document.
deleteDocument :: MonadBH m => IndexName -> MappingName
                  -> DocId -> m Reply
deleteDocument (IndexName indexName)
  (MappingName mappingName) (DocId docId) =
  delete =<< joinPath [indexName, mappingName, docId]

-- | 'bulk' uses
--    <http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/docs-bulk.html Elasticsearch's bulk API>
--    to perform bulk operations. The 'BulkOperation' data type encodes the
--    index\/update\/delete\/create operations. You pass a 'V.Vector' of 'BulkOperation's
--    and a 'Server' to 'bulk' in order to send those operations up to your Elasticsearch
--    server to be performed. I changed from [BulkOperation] to a Vector due to memory overhead.
bulk :: MonadBH m => V.Vector BulkOperation -> m Reply
bulk bulkOps = bindM2 post url (return body)
  where url = joinPath ["_bulk"]
        body = Just $ encodeBulkOperations bulkOps

-- | 'encodeBulkOperations' is a convenience function for dumping a vector of 'BulkOperation'
--   into an 'L.ByteString'
encodeBulkOperations :: V.Vector BulkOperation -> L.ByteString
encodeBulkOperations stream = collapsed where
  blobs = fmap encodeBulkOperation stream
  mashedTaters = mash (mempty :: Builder) blobs
  collapsed = toLazyByteString $ mappend mashedTaters (byteString "\n")

mash :: Builder -> V.Vector L.ByteString -> Builder
mash =
  V.foldl' (\b x -> b <> byteString "\n" <> lazyByteString x)

mkBulkStreamValue :: Text -> Text -> Text -> Text -> Value
mkBulkStreamValue operation indexName mappingName docId =
  object [operation .=
          object [ "_index" .= indexName
                 , "_type"  .= mappingName
                 , "_id"    .= docId]]

-- | 'encodeBulkOperation' is a convenience function for dumping a single 'BulkOperation'
--   into an 'L.ByteString'
encodeBulkOperation :: BulkOperation -> L.ByteString
encodeBulkOperation (BulkIndex (IndexName indexName)
                (MappingName mappingName)
                (DocId docId) value) = blob
    where metadata = mkBulkStreamValue "index" indexName mappingName docId
          blob = encode metadata `mappend` "\n" `mappend` encode value

encodeBulkOperation (BulkCreate (IndexName indexName)
                (MappingName mappingName)
                (DocId docId) value) = blob
    where metadata = mkBulkStreamValue "create" indexName mappingName docId
          blob = encode metadata `mappend` "\n" `mappend` encode value

encodeBulkOperation (BulkDelete (IndexName indexName)
                (MappingName mappingName)
                (DocId docId)) = blob
    where metadata = mkBulkStreamValue "delete" indexName mappingName docId
          blob = encode metadata

encodeBulkOperation (BulkUpdate (IndexName indexName)
                (MappingName mappingName)
                (DocId docId) value) = blob
    where metadata = mkBulkStreamValue "update" indexName mappingName docId
          doc = object ["doc" .= value]
          blob = encode metadata `mappend` "\n" `mappend` encode doc

-- | 'getDocument' is a straight-forward way to fetch a single document from
--   Elasticsearch using a 'Server', 'IndexName', 'MappingName', and a 'DocId'.
--   The 'DocId' is the primary key for your Elasticsearch document.
getDocument :: MonadBH m => IndexName -> MappingName
               -> DocId -> m Reply
getDocument (IndexName indexName)
  (MappingName mappingName) (DocId docId) =
  get =<< joinPath [indexName, mappingName, docId]

-- | 'documentExists' enables you to check if a document exists. Returns 'Bool'
--   in IO
documentExists :: MonadBH m => IndexName -> MappingName
               -> Maybe DocumentParent -> DocId -> m Bool
documentExists (IndexName indexName) (MappingName mappingName)
               parent (DocId docId) = do
  (_, exists) <- existentialQuery =<< url
  return exists
  where url = addQuery params <$> joinPath [indexName, mappingName, docId]
        parentParam = fmap (\(DocumentParent (DocId p)) -> p) parent
        params = LS.filter (\(_, v) -> isJust v) [("parent", parentParam)]

dispatchSearch :: MonadBH m => Text -> Search -> m Reply
dispatchSearch url search = post url' (Just (encode search))
  where url' = appendSearchTypeParam url (searchType search)

-- | 'searchAll', given a 'Search', will perform that search against all indexes
--   on an Elasticsearch server. Try to avoid doing this if it can be helped.
searchAll :: MonadBH m => Search -> m Reply
searchAll = bindM2 dispatchSearch url . return
  where url = joinPath ["_search"]

-- | 'searchByIndex', given a 'Search' and an 'IndexName', will perform that search
--   against all mappings within an index on an Elasticsearch server.
searchByIndex :: MonadBH m => IndexName -> Search -> m Reply
searchByIndex (IndexName indexName) = bindM2 dispatchSearch url . return
  where url = joinPath [indexName, "_search"]

-- | 'searchByType', given a 'Search', 'IndexName', and 'MappingName', will perform that
--   search against a specific mapping within an index on an Elasticsearch server.
searchByType :: MonadBH m => IndexName -> MappingName -> Search
                -> m Reply
searchByType (IndexName indexName)
  (MappingName mappingName) = bindM2 dispatchSearch url . return
  where url = joinPath [indexName, mappingName, "_search"]

-- | For a given search, request a scroll for efficient streaming of
-- search results. Note that the search is put into 'SearchTypeScan'
-- mode and thus results will not be sorted. Combine this with
-- 'advanceScroll' to efficiently stream through the full result set
getInitialScroll :: MonadBH m => IndexName -> MappingName -> Search -> m (Maybe ScrollId)
getInitialScroll (IndexName indexName) (MappingName mappingName) search = do
    let url = joinPath [indexName, mappingName, "_search"]
        search' = search { searchType = SearchTypeScan }
    resp' <- bindM2 dispatchSearch url (return search')
    let msr = decode' $ responseBody resp' :: Maybe (SearchResult ())
        msid = maybe Nothing scrollId msr
    return msid

scroll' :: (FromJSON a, MonadBH m, MonadThrow m) => Maybe ScrollId -> m ([Hit a], Maybe ScrollId)
scroll' Nothing = return ([], Nothing)
scroll' (Just sid) = do
    res <- advanceScroll sid 60
    case res of
      Right SearchResult {..} -> return (hits searchHits, scrollId)
      Left _ -> return ([], Nothing)

-- | Use the given scroll to fetch the next page of documents. If there are no
-- further pages, 'SearchResult.searchHits.hits' will be '[]'.
advanceScroll
  :: ( FromJSON a
     , MonadBH m
     , MonadThrow m
     )
  => ScrollId
  -> NominalDiffTime
  -- ^ How long should the snapshot of data be kept around? This timeout is updated every time 'advanceScroll' is used, so don't feel the need to set it to the entire duration of your search processing. Note that durations < 1s will be rounded up. Also note that 'NominalDiffTime' is an instance of Num so literals like 60 will be interpreted as seconds. 60s is a reasonable default.
  -> m (Either EsError (SearchResult a))
advanceScroll (ScrollId sid) scroll = do
  url <- joinPath ["_search/scroll?scroll=" <> scrollTime]
  parseEsResponse =<< post url (Just . L.fromStrict $ T.encodeUtf8 sid)
  where scrollTime = showText secs <> "s"
        secs :: Integer
        secs = round scroll

simpleAccumulator :: (FromJSON a, MonadBH m, MonadThrow m) => [Hit a] -> ([Hit a], Maybe ScrollId) -> m ([Hit a], Maybe ScrollId)
simpleAccumulator oldHits (newHits, Nothing) = return (oldHits ++ newHits, Nothing)
simpleAccumulator oldHits ([], _) = return (oldHits, Nothing)
simpleAccumulator oldHits (newHits, msid) = do
    (newHits', msid') <- scroll' msid
    simpleAccumulator (oldHits ++ newHits) (newHits', msid')

-- | 'scanSearch' uses the 'scan&scroll' API of elastic,
-- for a given 'IndexName' and 'MappingName'. Note that this will
-- consume the entire search result set and will be doing O(n) list
-- appends so this may not be suitable for large result sets. In that
-- case, 'getInitialScroll' and 'advanceScroll' are good low level
-- tools. You should be able to hook them up trivially to conduit,
-- pipes, or your favorite streaming IO abstraction of choice. Note
-- that ordering on the search would destroy performance and thus is
-- ignored.
scanSearch :: (FromJSON a, MonadBH m, MonadThrow m) => IndexName -> MappingName -> Search -> m [Hit a]
scanSearch indexName mappingName search = do
    msid <- getInitialScroll indexName mappingName search
    (hits, msid') <- scroll' msid
    (totalHits, _) <- simpleAccumulator [] (hits, msid')
    return totalHits

-- | 'mkSearch' is a helper function for defaulting additional fields of a 'Search'
--   to Nothing in case you only care about your 'Query' and 'Filter'. Use record update
--   syntax if you want to add things like aggregations or highlights while still using
--   this helper function.
mkSearch :: Maybe Query -> Maybe Filter -> Search
mkSearch query filter = Search query filter Nothing Nothing Nothing False (From 0) (Size 10) SearchTypeQueryThenFetch Nothing Nothing Nothing

-- | 'mkAggregateSearch' is a helper function that defaults everything in a 'Search' except for
--   the 'Query' and the 'Aggregation'.
mkAggregateSearch :: Maybe Query -> Aggregations -> Search
mkAggregateSearch query mkSearchAggs = Search query Nothing Nothing (Just mkSearchAggs) Nothing False (From 0) (Size 0) SearchTypeQueryThenFetch Nothing Nothing Nothing

-- | 'mkHighlightSearch' is a helper function that defaults everything in a 'Search' except for
--   the 'Query' and the 'Aggregation'.
mkHighlightSearch :: Maybe Query -> Highlights -> Search
mkHighlightSearch query searchHighlights = Search query Nothing Nothing Nothing (Just searchHighlights) False (From 0) (Size 10) SearchTypeQueryThenFetch Nothing Nothing Nothing

-- | 'pageSearch' is a helper function that takes a search and assigns the from
--    and size fields for the search. The from parameter defines the offset
--    from the first result you want to fetch. The size parameter allows you to
--    configure the maximum amount of hits to be returned.
pageSearch :: From     -- ^ The result offset
           -> Size     -- ^ The number of results to return
           -> Search  -- ^ The current seach
           -> Search  -- ^ The paged search
pageSearch resultOffset pageSize search = search { from = resultOffset, size = pageSize }

parseUrl' :: MonadThrow m => Text -> m Request
parseUrl' t = parseRequest (URI.escapeURIString URI.isAllowedInURI (T.unpack t))

-- | Was there an optimistic concurrency control conflict when
-- indexing a document?
isVersionConflict :: Reply -> Bool
isVersionConflict = statusCheck (== 409)

isSuccess :: Reply -> Bool
isSuccess = statusCheck (inRange (200, 299))

isCreated :: Reply -> Bool
isCreated = statusCheck (== 201)

statusCheck :: (Int -> Bool) -> Reply -> Bool
statusCheck prd = prd . NHTS.statusCode . responseStatus

-- | This is a hook that can be set via the 'bhRequestHook' function
-- that will authenticate all requests using an HTTP Basic
-- Authentication header. Note that it is *strongly* recommended that
-- this option only be used over an SSL connection.
--
-- >> (mkBHEnv myServer myManager) { bhRequestHook = basicAuthHook (EsUsername "myuser") (EsPassword "mypass") }
basicAuthHook :: Monad m => EsUsername -> EsPassword -> Request -> m Request
basicAuthHook (EsUsername u) (EsPassword p) = return . applyBasicAuth u' p'
  where u' = T.encodeUtf8 u
        p' = T.encodeUtf8 p


boolQP :: Bool -> Text
boolQP True  = "true"
boolQP False = "false"
