{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UndecidableInstances       #-}

module Database.V1.Bloodhound.Internal.Client where

import           Bloodhound.Import

import           Control.Applicative                      as A
import           Data.Text                                (Text)
import qualified Data.Version                             as Vers
import           Network.HTTP.Client
import           Text.Read                                (Read (..))
import qualified Text.Read                                as TR

import           Database.V1.Bloodhound.Internal.Newtypes


{-| Common environment for Elasticsearch calls. Connections will be
    pipelined according to the provided HTTP connection manager.
-}
data BHEnv = BHEnv { bhServer      :: Server
                   , bhManager     :: Manager
                   , bhRequestHook :: Request -> IO Request
                   -- ^ Low-level hook that is run before every request is sent. Used to implement custom authentication strategies. Defaults to 'return' with 'mkBHEnv'.
                   }

instance (Functor m, Applicative m, MonadIO m) => MonadBH (ReaderT BHEnv m) where
  getBHEnv = ask

{-| 'Server' is used with the client functions to point at the ES instance
-}
newtype Server = Server Text deriving (Eq, Show, FromJSON)

{-| All API calls to Elasticsearch operate within
    MonadBH
    . The idea is that it can be easily embedded in your
    own monad transformer stack. A default instance for a ReaderT and
    alias 'BH' is provided for the simple case.
-}
class (Functor m, A.Applicative m, MonadIO m) => MonadBH m where
  getBHEnv :: m BHEnv

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Aeson
-- >>> import Database.V1.Bloodhound
-- >>> let testServer = (Server "http://localhost:9200")
-- >>> let testIndex = IndexName "twitter"
-- >>> let testMapping = MappingName "tweet"
-- >>> let defaultIndexSettings = IndexSettings (ShardCount 3) (ReplicaCount 2)

-- defaultIndexSettings is exported by Database.Bloodhound as well
-- no trailing slashes in servers, library handles building the path.

-- | Create a 'BHEnv' with all optional fields defaulted. HTTP hook
-- will be a noop. You can use the exported fields to customize it further, e.g.:
--
-- >> (mkBHEnv myServer myManager) { bhRequestHook = customHook }
mkBHEnv :: Server -> Manager -> BHEnv
mkBHEnv s m = BHEnv s m return

newtype BH m a = BH {
      unBH :: ReaderT BHEnv m a
    } deriving ( Functor
               , A.Applicative
               , Monad
               , MonadIO
               , MonadState s
               , MonadWriter w
               , MonadError e
               , Alternative
               , MonadPlus
               , MonadFix
               , MonadThrow
               , MonadCatch
               , MonadMask)

instance MonadTrans BH where
  lift = BH . lift

instance (MonadReader r m) => MonadReader r (BH m) where
    ask = lift ask
    local f (BH (ReaderT m)) = BH $ ReaderT $ \r ->
      local f (m r)

instance (Functor m, Applicative m, MonadIO m) => MonadBH (BH m) where
  getBHEnv = BH getBHEnv

runBH :: BHEnv -> BH m a -> m a
runBH e f = runReaderT (unBH f) e

{-| 'Version' is embedded in 'Status' -}
data Version = Version { number          :: VersionNumber
                       , build_hash      :: BuildHash
                       , build_timestamp :: UTCTime
                       , build_snapshot  :: Bool
                       , lucene_version  :: VersionNumber } deriving (Eq, Show)

-- | Traditional software versioning number
newtype VersionNumber = VersionNumber { versionNumber :: Vers.Version
                                      } deriving (Eq, Show, Ord)

{-| 'Status' is a data type for describing the JSON body returned by
    Elasticsearch when you query its status. This was deprecated in 1.2.0.

   <http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/indices-status.html#indices-status>
-}

data Status = Status { ok      :: Maybe Bool
                     , status  :: Int
                     , name    :: Text
                     , version :: Version
                     , tagline :: Text } deriving (Eq, Show)

{-| 'IndexSettings' is used to configure the shards and replicas when you create
   an Elasticsearch Index.

   <http://www.elasticsearch.org/guide/en/elasticsearch/reference/current/indices-create-index.html>
-}

data IndexSettings =
  IndexSettings { indexShards   :: ShardCount
                , indexReplicas :: ReplicaCount } deriving (Eq, Show)

{-| 'defaultIndexSettings' is an 'IndexSettings' with 3 shards and 2 replicas. -}
defaultIndexSettings :: IndexSettings
defaultIndexSettings =  IndexSettings (ShardCount 3) (ReplicaCount 2)


{-| 'IndexOptimizationSettings' is used to configure index optimization. See
    <https://www.elastic.co/guide/en/elasticsearch/reference/1.7/indices-optimize.html>
    for more info.
-}
data IndexOptimizationSettings =
  IndexOptimizationSettings { maxNumSegments     :: Maybe Int
                            -- ^ Number of segments to optimize to. 1 will fully optimize the index. If omitted, the default behavior is to only optimize if the server deems it necessary.
                            , onlyExpungeDeletes :: Bool
                            -- ^ Should the optimize process only expunge segments with deletes in them? If the purpose of the optimization is to free disk space, this should be set to True.
                            , flushAfterOptimize :: Bool
                            -- ^ Should a flush be performed after the optimize.
                            } deriving (Eq, Show)


{-| 'defaultIndexOptimizationSettings' implements the default settings that
    ElasticSearch uses for index optimization. 'maxNumSegments' is Nothing,
    'onlyExpungeDeletes' is False, and flushAfterOptimize is True.
-}
defaultIndexOptimizationSettings :: IndexOptimizationSettings
defaultIndexOptimizationSettings = IndexOptimizationSettings Nothing False True

{-| 'UpdatableIndexSetting' are settings which may be updated after an index is created.

   <https://www.elastic.co/guide/en/elasticsearch/reference/current/indices-update-settings.html>
-}
data UpdatableIndexSetting = NumberOfReplicas ReplicaCount
                           -- ^ The number of replicas each shard has.
                           | AutoExpandReplicas ReplicaBounds
                           | BlocksReadOnly Bool
                           -- ^ Set to True to have the index read only. False to allow writes and metadata changes.
                           | BlocksRead Bool
                           -- ^ Set to True to disable read operations against the index.
                           | BlocksWrite Bool
                           -- ^ Set to True to disable write operations against the index.
                           | BlocksMetaData Bool
                           -- ^ Set to True to disable metadata operations against the index.
                           | RefreshInterval NominalDiffTime
                           -- ^ The async refresh interval of a shard
                           | IndexConcurrency Int
                           | FailOnMergeFailure Bool
                           | TranslogFlushThresholdOps Int
                           -- ^ When to flush on operations.
                           | TranslogFlushThresholdSize Bytes
                           -- ^ When to flush based on translog (bytes) size.
                           | TranslogFlushThresholdPeriod NominalDiffTime
                           -- ^ When to flush based on a period of not flushing.
                           | TranslogDisableFlush Bool
                           -- ^ Disables flushing. Note, should be set for a short interval and then enabled.
                           | CacheFilterMaxSize (Maybe Bytes)
                           -- ^ The maximum size of filter cache (per segment in shard).
                           | CacheFilterExpire (Maybe NominalDiffTime)
                           -- ^ The expire after access time for filter cache.
                           | GatewaySnapshotInterval NominalDiffTime
                           -- ^ The gateway snapshot interval (only applies to shared gateways).
                           | RoutingAllocationInclude (NonEmpty NodeAttrFilter)
                           -- ^ A node matching any rule will be allowed to host shards from the index.
                           | RoutingAllocationExclude (NonEmpty NodeAttrFilter)
                           -- ^ A node matching any rule will NOT be allowed to host shards from the index.
                           | RoutingAllocationRequire (NonEmpty NodeAttrFilter)
                           -- ^ Only nodes matching all rules will be allowed to host shards from the index.
                           | RoutingAllocationEnable AllocationPolicy
                           -- ^ Enables shard allocation for a specific index.
                           | RoutingAllocationShardsPerNode ShardCount
                           -- ^ Controls the total number of shards (replicas and primaries) allowed to be allocated on a single node.
                           | RecoveryInitialShards InitialShardCount
                           -- ^ When using local gateway a particular shard is recovered only if there can be allocated quorum shards in the cluster.
                           | GCDeletes NominalDiffTime
                           | TTLDisablePurge Bool
                           -- ^ Disables temporarily the purge of expired docs.
                           | TranslogFSType FSType
                           | IndexCompoundFormat CompoundFormat
                           | IndexCompoundOnFlush Bool
                           | WarmerEnabled Bool
                           deriving (Eq, Show)

data AllocationPolicy = AllocAll
                      -- ^ Allows shard allocation for all shards.
                      | AllocPrimaries
                      -- ^ Allows shard allocation only for primary shards.
                      | AllocNewPrimaries
                      -- ^ Allows shard allocation only for primary shards for new indices.
                      | AllocNone
                      -- ^ No shard allocation is allowed
                      deriving (Eq, Show)

data ReplicaBounds = ReplicasBounded Int Int
                   | ReplicasLowerBounded Int
                   | ReplicasUnbounded
                   deriving (Eq, Show)

-- | A measure of bytes used for various configurations. You may want
-- to use smart constructors like 'gigabytes' for larger values.
--
-- >>> gigabytes 9
-- Bytes 9000000000
--
-- >>> megabytes 9
-- Bytes 9000000
--
-- >>> kilobytes 9
-- Bytes 9000
newtype Bytes = Bytes Int deriving (Eq, Show, Ord, ToJSON, FromJSON)

gigabytes :: Int -> Bytes
gigabytes n = megabytes (1000 * n)


megabytes :: Int -> Bytes
megabytes n = kilobytes (1000 * n)


kilobytes :: Int -> Bytes
kilobytes n = Bytes (1000 * n)


data Interval = Year
              | Quarter
              | Month
              | Week
              | Day
              | Hour
              | Minute
              | Second deriving (Eq, Show)

instance ToJSON Interval where
  toJSON Year    = "year"
  toJSON Quarter = "quarter"
  toJSON Month   = "month"
  toJSON Week    = "week"
  toJSON Day     = "day"
  toJSON Hour    = "hour"
  toJSON Minute  = "minute"
  toJSON Second  = "second"

parseStringInterval :: (Monad m) => String -> m NominalDiffTime
parseStringInterval s = case span isNumber s of
  ("", _) -> fail "Invalid interval"
  (nS, unitS) -> case (readMay nS, readMay unitS) of
    (Just n, Just unit) -> return (fromInteger (n * unitNDT unit))
    (Nothing, _)        -> fail "Invalid interval number"
    (_, Nothing)        -> fail "Invalid interval unit"
  where
    unitNDT Seconds = 1
    unitNDT Minutes = 60
    unitNDT Hours   = 60 * 60
    unitNDT Days    = 24 * 60 * 60
    unitNDT Weeks   = 7 * 24 * 60 * 60

data TimeInterval = Weeks
                  | Days
                  | Hours
                  | Minutes
                  | Seconds deriving Eq

instance Show TimeInterval where
  show Weeks   = "w"
  show Days    = "d"
  show Hours   = "h"
  show Minutes = "m"
  show Seconds = "s"

instance Read TimeInterval where
  readPrec = f =<< TR.get
    where
      f 'w' = return Weeks
      f 'd' = return Days
      f 'h' = return Hours
      f 'm' = return Minutes
      f 's' = return Seconds
      f  _  = fail "TimeInterval expected one of w, d, h, m, s"

-- | Typically a 7 character hex string.
newtype BuildHash = BuildHash { buildHash :: Text }
                 deriving (Eq, Ord, Show, FromJSON, ToJSON)

data NodeAttrFilter = NodeAttrFilter
  { nodeAttrFilterName   :: NodeAttrName
  , nodeAttrFilterValues :: NonEmpty Text }
  deriving (Eq, Ord, Show)

newtype NodeAttrName = NodeAttrName Text deriving (Eq, Ord, Show)

data InitialShardCount = QuorumShards
                       | QuorumMinus1Shards
                       | FullShards
                       | FullMinus1Shards
                       | ExplicitShards Int
                       deriving (Eq, Show)

instance FromJSON InitialShardCount where
  parseJSON v = withText "InitialShardCount" parseText v
            <|> ExplicitShards <$> parseJSON v
    where parseText "quorum"   = pure QuorumShards
          parseText "quorum-1" = pure QuorumMinus1Shards
          parseText "full"     = pure FullShards
          parseText "full-1"   = pure FullMinus1Shards
          parseText _          = mzero

instance ToJSON InitialShardCount where
  toJSON QuorumShards       = String "quorum"
  toJSON QuorumMinus1Shards = String "quorum-1"
  toJSON FullShards         = String "full"
  toJSON FullMinus1Shards   = String "full-1"
  toJSON (ExplicitShards x) = toJSON x

data FSType = FSSimple
            | FSBuffered deriving (Eq, Show)

instance ToJSON FSType where
  toJSON FSSimple   = "simple"
  toJSON FSBuffered = "buffered"

instance FromJSON FSType where
  parseJSON = withText "FSType" parse
    where parse "simple"   = pure FSSimple
          parse "buffered" = pure FSBuffered
          parse t          = fail ("Invalid FSType: " <> show t)

data CompoundFormat = CompoundFileFormat Bool
                    | MergeSegmentVsTotalIndex Double
                    -- ^ percentage between 0 and 1 where 0 is false, 1 is true
                    deriving (Eq, Show)
