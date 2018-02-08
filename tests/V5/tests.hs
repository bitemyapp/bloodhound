{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
#if __GLASGOW_HASKELL__ < 800
{-# OPTIONS_GHC -fcontext-stack=100 #-}
#endif
#if __GLASGOW_HASKELL__ >= 802
{-# LANGUAGE MonoLocalBinds #-}
#endif
module Main where

import           Control.Applicative
import           Control.Error
import           Control.Exception               (evaluate)
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Aeson.Types                (parseEither)
import qualified Data.ByteString.Lazy.Char8      as BL8
import qualified Data.HashMap.Strict             as HM
import           Data.List                       (nub)
import qualified Data.List                       as L
import           Data.List.NonEmpty              (NonEmpty (..))
import qualified Data.List.NonEmpty              as NE
import qualified Data.Map.Strict                 as M
import           Data.Monoid
import           Data.Ord                        (comparing)
import           Data.Proxy
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Data.Time.Calendar              (Day (..), fromGregorian)
import           Data.Time.Clock                 (NominalDiffTime, UTCTime (..),
                                                  secondsToDiffTime)
import           Data.Typeable
import qualified Data.Vector                     as V
import qualified Data.Version                    as Vers
import           Database.V5.Bloodhound
import           GHC.Generics                    as G
import           Network.HTTP.Client             hiding (Proxy)
import qualified Network.HTTP.Types.Method       as NHTM
import qualified Network.HTTP.Types.Status       as NHTS
import qualified Network.URI                     as URI
import           Prelude                         hiding (filter)
import           System.IO.Temp
import           System.PosixCompat.Files
import           Test.Hspec
import           Test.QuickCheck.Property.Monoid (T (..), eq, prop_Monoid)

import           Test.Hspec.QuickCheck           (prop)
import           Test.QuickCheck

import qualified Generics.SOP as SOP
import qualified Generics.SOP.GGP as SOP

testServer  :: Server
testServer  = Server "http://localhost:9200"
testIndex   :: IndexName
testIndex   = IndexName "bloodhound-tests-twitter-1"
testMapping :: MappingName
testMapping = MappingName "tweet"

withTestEnv :: BH IO a -> IO a
withTestEnv = withBH defaultManagerSettings testServer

validateStatus :: Show body => Response body -> Int -> Expectation
validateStatus resp expected =
  if actual == expected
    then return ()
    else expectationFailure ("Expected " <> show expected <> " but got " <> show actual <> ": " <> show body)
  where
    actual = NHTS.statusCode (responseStatus resp)
    body = responseBody resp

createExampleIndex :: (MonadBH m) => m Reply
createExampleIndex = createIndex (IndexSettings (ShardCount 1) (ReplicaCount 0)) testIndex
deleteExampleIndex :: (MonadBH m) => m Reply
deleteExampleIndex = deleteIndex testIndex

es13 :: Vers.Version
es13 = Vers.Version [1, 3, 0] []

es12 :: Vers.Version
es12 = Vers.Version [1, 2, 0] []

es11 :: Vers.Version
es11 = Vers.Version [1, 1, 0] []

es14 :: Vers.Version
es14 = Vers.Version [1, 4, 0] []

es15 :: Vers.Version
es15 = Vers.Version [1, 5, 0] []

es16 :: Vers.Version
es16 = Vers.Version [1, 6, 0] []

es20 :: Vers.Version
es20 = Vers.Version [2, 0, 0] []

es50 :: Vers.Version
es50 = Vers.Version [5, 0, 0] []

getServerVersion :: IO (Maybe Vers.Version)
getServerVersion = fmap extractVersion <$> withTestEnv getStatus
  where
    extractVersion              = versionNumber . number . version

-- | Get configured repo paths for snapshotting. Note that by default
-- this is not enabled and if we are over es 1.5, we won't be able to
-- test snapshotting. Note that this can and should be part of the
-- client functionality in a much less ad-hoc incarnation.
getRepoPaths :: IO [FilePath]
getRepoPaths = withTestEnv $ do
  bhe <- getBHEnv
  let Server s = bhServer bhe
  let tUrl = s <> "/" <> "_nodes"
  initReq <- parseRequest (URI.escapeURIString URI.isAllowedInURI (T.unpack tUrl))
  let req = setRequestIgnoreStatus $ initReq { method = NHTM.methodGet }
  Right (Object o) <- parseEsResponse =<< liftIO (httpLbs req (bhManager bhe))
  return $ fromMaybe mempty $ do
    Object nodes <- HM.lookup "nodes" o
    Object firstNode <- snd <$> headMay (HM.toList nodes)
    Object settings <- HM.lookup "settings" firstNode
    Object path <- HM.lookup "path" settings
    Array repo <- HM.lookup "repo" path
    return [ T.unpack t | String t <- V.toList repo]

-- | 1.5 and earlier don't care about repo paths
canSnapshot :: IO Bool
canSnapshot = do
  caresAboutRepos <- atleast es16
  repoPaths <- getRepoPaths
  return (not caresAboutRepos || not (null (repoPaths)))

atleast :: Vers.Version -> IO Bool
atleast v = getServerVersion >>= \x -> return $ x >= Just v

atmost :: Vers.Version -> IO Bool
atmost v = getServerVersion >>= \x -> return $ x <= Just v

is :: Vers.Version -> IO Bool
is v = getServerVersion >>= \x -> return $ x == Just v

when' :: Monad m => m Bool -> m () -> m ()
when' b f = b >>= \x -> when x f

(==~) :: (ApproxEq a) => a -> a -> Property
a ==~ b = counterexample (showApproxEq a ++ " !=~ " ++ showApproxEq b) (a =~ b)

propJSON :: forall a. (Arbitrary a, ToJSON a, FromJSON a, Show a, ApproxEq a, Typeable a) => Proxy a -> Spec
propJSON _ = prop testName $ \(a :: a) ->
  let jsonStr = "via " <> BL8.unpack (encode a)
  in counterexample jsonStr (parseEither parseJSON (toJSON a) ==~ Right a)
  where testName = show ty <> " FromJSON/ToJSON roundtrips"
        ty = typeOf (undefined :: a)

data Location = Location { lat :: Double
                         , lon :: Double } deriving (Eq, Generic, Show)

data Tweet = Tweet { user     :: Text
                   , postDate :: UTCTime
                   , message  :: Text
                   , age      :: Int
                   , location :: Location
                   , extra    :: Maybe Text }
           deriving (Eq, Generic, Show)

instance ToJSON   Tweet where
  toJSON = genericToJSON defaultOptions
instance FromJSON Tweet where
  parseJSON = genericParseJSON defaultOptions
instance ToJSON   Location where
  toJSON = genericToJSON defaultOptions
instance FromJSON Location where
  parseJSON = genericParseJSON defaultOptions

data ParentMapping = ParentMapping deriving (Eq, Show)

instance ToJSON ParentMapping where
  toJSON ParentMapping =
    object ["properties" .=
      object [ "user"     .= object ["type"    .= ("string" :: Text)
                                    , "fielddata" .= True
                                    ]
            -- Serializing the date as a date is breaking other tests, mysteriously.
            -- , "postDate" .= object [ "type"   .= ("date" :: Text)
            --                        , "format" .= ("YYYY-MM-dd`T`HH:mm:ss.SSSZZ" :: Text)]
            , "message"  .= object ["type" .= ("string" :: Text)]
            , "age"      .= object ["type" .= ("integer" :: Text)]
            , "location" .= object ["type" .= ("geo_point" :: Text)]
            , "extra"    .= object ["type" .= ("keyword" :: Text)]
            ]]

data ChildMapping = ChildMapping deriving (Eq, Show)

instance ToJSON ChildMapping where
  toJSON ChildMapping =
    object ["_parent" .= object ["type" .= ("parent" :: Text)]
           , "properties" .=
                object [ "user"     .= object ["type"    .= ("string" :: Text)
                                              , "fielddata" .= True
                                              ]
                  -- Serializing the date as a date is breaking other tests, mysteriously.
                  -- , "postDate" .= object [ "type"   .= ("date" :: Text)
                  --                        , "format" .= ("YYYY-MM-dd`T`HH:mm:ss.SSSZZ" :: Text)]
                  , "message"  .= object ["type" .= ("string" :: Text)]
                  , "age"      .= object ["type" .= ("integer" :: Text)]
                  , "location" .= object ["type" .= ("geo_point" :: Text)]
                  , "extra"    .= object ["type" .= ("keyword" :: Text)]
                  ]]

data TweetMapping = TweetMapping deriving (Eq, Show)

instance ToJSON TweetMapping where
  toJSON TweetMapping =
    object ["tweet" .=
      object ["properties" .=
        object [ "user"     .= object [ "type"    .= ("string" :: Text)
                                      , "fielddata" .= True
                                      ]
               -- Serializing the date as a date is breaking other tests, mysteriously.
               -- , "postDate" .= object [ "type"   .= ("date" :: Text)
               --                        , "format" .= ("YYYY-MM-dd`T`HH:mm:ss.SSSZZ" :: Text)]
               , "message"  .= object ["type" .= ("string" :: Text)]
               , "age"      .= object ["type" .= ("integer" :: Text)]
               , "location" .= object ["type" .= ("geo_point" :: Text)]
               , "extra"    .= object ["type" .= ("keyword" :: Text)]
               ]]]

exampleTweet :: Tweet
exampleTweet = Tweet { user     = "bitemyapp"
                     , postDate = UTCTime
                                  (ModifiedJulianDay 55000)
                                  (secondsToDiffTime 10)
                     , message  = "Use haskell!"
                     , age      = 10000
                     , location = Location 40.12 (-71.34)
                     , extra = Nothing }

tweetWithExtra :: Tweet
tweetWithExtra = Tweet { user     = "bitemyapp"
                       , postDate = UTCTime
                                    (ModifiedJulianDay 55000)
                                    (secondsToDiffTime 10)
                       , message  = "Use haskell!"
                       , age      = 10000
                       , location = Location 40.12 (-71.34)
                       , extra = Just "blah blah" }

newAge :: Int
newAge = 31337

newUser :: Text
newUser = "someotherapp"

tweetPatch :: Value
tweetPatch =
  object [ "age" .= newAge
         , "user" .= newUser
         ]

patchedTweet :: Tweet
patchedTweet = exampleTweet{age = newAge, user = newUser}

otherTweet :: Tweet
otherTweet = Tweet { user     = "notmyapp"
                   , postDate = UTCTime
                                (ModifiedJulianDay 55000)
                                (secondsToDiffTime 11)
                   , message  = "Use haskell!"
                   , age      = 1000
                   , location = Location 40.12 (-71.34)
                   , extra = Nothing }

resetIndex :: BH IO ()
resetIndex = do
  _ <- deleteExampleIndex
  _ <- createExampleIndex
  _ <- putMapping testIndex testMapping TweetMapping
  return ()

insertData :: BH IO Reply
insertData = do
  resetIndex
  insertData' defaultIndexDocumentSettings

insertData' :: IndexDocumentSettings -> BH IO Reply
insertData' ids = do
  r <- indexDocument testIndex testMapping ids exampleTweet (DocId "1")
  _ <- refreshIndex testIndex
  return r

updateData :: BH IO Reply
updateData = do
  r <- updateDocument testIndex testMapping defaultIndexDocumentSettings tweetPatch (DocId "1")
  _ <- refreshIndex testIndex
  return r

insertOther :: BH IO ()
insertOther = do
  _ <- indexDocument testIndex testMapping defaultIndexDocumentSettings otherTweet (DocId "2")
  _ <- refreshIndex testIndex
  return ()

insertExtra :: BH IO ()
insertExtra = do
  _ <- indexDocument testIndex testMapping defaultIndexDocumentSettings tweetWithExtra (DocId "4")
  _ <- refreshIndex testIndex
  return ()

insertWithSpaceInId :: BH IO ()
insertWithSpaceInId = do
  _ <- indexDocument testIndex testMapping defaultIndexDocumentSettings exampleTweet (DocId "Hello World")
  _ <- refreshIndex testIndex
  return ()

searchTweet :: Search -> BH IO (Either EsError Tweet)
searchTweet search = do
  result <- searchTweets search
  let myTweet :: Either EsError Tweet
      myTweet = grabFirst result
  return myTweet

searchTweets :: Search -> BH IO (Either EsError (SearchResult Tweet))
searchTweets search = parseEsResponse =<< searchByIndex testIndex search

searchExpectNoResults :: Search -> BH IO ()
searchExpectNoResults search = do
  result <- searchTweets search
  let emptyHits = fmap (hits . searchHits) result
  liftIO $
    emptyHits `shouldBe` Right []

searchExpectAggs :: Search -> BH IO ()
searchExpectAggs search = do
  reply <- searchByIndex testIndex search
  let isEmpty x = return (M.null x)
  let result = decode (responseBody reply) :: Maybe (SearchResult Tweet)
  liftIO $
    (result >>= aggregations >>= isEmpty) `shouldBe` Just False

searchValidBucketAgg :: (BucketAggregation a, FromJSON a, Show a) =>
                        Search -> Text -> (Text -> AggregationResults -> Maybe (Bucket a)) -> BH IO ()
searchValidBucketAgg search aggKey extractor = do
  reply <- searchByIndex testIndex search
  let bucketDocs = docCount . head . buckets
  let result = decode (responseBody reply) :: Maybe (SearchResult Tweet)
  let count = result >>= aggregations >>= extractor aggKey >>= \x -> return (bucketDocs x)
  liftIO $
    count `shouldBe` Just 1

searchTermsAggHint :: [ExecutionHint] -> BH IO ()
searchTermsAggHint hints = do
      let terms hint = TermsAgg $ (mkTermsAggregation "user") { termExecutionHint = Just hint }
      let search hint = mkAggregateSearch Nothing $ mkAggregations "users" $ terms hint
      forM_ hints $ searchExpectAggs . search
      forM_ hints (\x -> searchValidBucketAgg (search x) "users" toTerms)

searchTweetHighlight :: Search -> BH IO (Either EsError (Maybe HitHighlight))
searchTweetHighlight search = do
  result <- searchTweets search
  let myHighlight = fmap (hitHighlight . head . hits . searchHits) result
  return myHighlight

searchExpectSource :: Source -> Either EsError Value -> BH IO ()
searchExpectSource src expected = do
  _ <- insertData
  let query = QueryMatchQuery $ mkMatchQuery (FieldName "_all") (QueryString "haskell")
  let search = (mkSearch (Just query) Nothing) { source = Just src }
  reply <- searchByIndex testIndex search
  result <- parseEsResponse reply
  let value = grabFirst result
  liftIO $
    value `shouldBe` expected

withSnapshotRepo
    :: ( MonadMask m
       , MonadBH m
       )
    => SnapshotRepoName
    -> (GenericSnapshotRepo -> m a)
    -> m a
withSnapshotRepo srn@(SnapshotRepoName n) f = do
  repoPaths <- liftIO getRepoPaths
  -- we'll use the first repo path if available, otherwise system temp
  -- dir. Note that this will fail on ES > 1.6, so be sure you use
  -- @when' canSnapshot@.
  case repoPaths of
    (firstRepoPath:_) -> withTempDirectory firstRepoPath (T.unpack n) $ \dir -> bracket (alloc dir) free f
    [] -> withSystemTempDirectory (T.unpack n) $ \dir -> bracket (alloc dir) free f
  where
    alloc dir = do
      liftIO (setFileMode dir mode)
      let repo = FsSnapshotRepo srn "bloodhound-tests-backups" True Nothing Nothing Nothing
      resp <- updateSnapshotRepo defaultSnapshotRepoUpdateSettings repo
      liftIO (validateStatus resp 200)
      return (toGSnapshotRepo repo)
    mode = ownerModes `unionFileModes` groupModes `unionFileModes` otherModes
    free GenericSnapshotRepo {..} = do
      resp <- deleteSnapshotRepo gSnapshotRepoName
      liftIO (validateStatus resp 200)


withSnapshot
    :: ( MonadMask m
       , MonadBH m
       )
    => SnapshotRepoName
    -> SnapshotName
    -> m a
    -> m a
withSnapshot srn sn = bracket_ alloc free
  where
    alloc = do
      resp <- createSnapshot srn sn createSettings
      liftIO (validateStatus resp 200)
    -- We'll make this synchronous for testing purposes
    createSettings = defaultSnapshotCreateSettings { snapWaitForCompletion = True
                                                   , snapIndices = Just (IndexList (testIndex :| []))
                                                   -- We don't actually need to back up any data
                                                   }
    free = do
      deleteSnapshot srn sn



data BulkTest = BulkTest { name :: Text } deriving (Eq, Generic, Show)
instance FromJSON BulkTest where
  parseJSON = genericParseJSON defaultOptions
instance ToJSON BulkTest where
  toJSON = genericToJSON defaultOptions

class GApproxEq f where
  gApproxEq :: f a -> f a -> Bool

-- | Unit type
instance GApproxEq U1 where
  gApproxEq U1 U1 = True

-- | Sum type, ensure same constructors, recurse
instance (GApproxEq a, GApproxEq b) => GApproxEq (a :+: b) where
  gApproxEq (L1 a) (L1 b) = gApproxEq a b
  gApproxEq (R1 a) (R1 b) = gApproxEq a b
  gApproxEq _ _           = False

-- | Product type, ensure each field is approx eq
instance (GApproxEq a, GApproxEq b) => GApproxEq (a :*: b) where
  gApproxEq (a1 :*: b1) (a2 :*: b2) = gApproxEq a1 a2 && gApproxEq b1 b2

-- | Value type, actually check the values for approx equality
instance (ApproxEq a) => GApproxEq (K1 i a) where
  gApproxEq (K1 a) (K1 b) = a =~ b

instance (GApproxEq f) => GApproxEq (M1 i t f) where
  gApproxEq (M1 a) (M1 b) = gApproxEq a b

-- | Typeclass for "equal where it matters". Use this to specify
-- less-strict equivalence for things such as lists that can wind up
-- in an unpredictable order
class ApproxEq a where
  (=~) :: a -> a -> Bool
  default (=~) :: (Generic a, GApproxEq (Rep a)) => a -> a -> Bool
  a =~ b = gApproxEq (G.from a) (G.from b)

  showApproxEq :: a -> String
  default showApproxEq :: (Show a) => a -> String
  showApproxEq = show

instance ApproxEq NominalDiffTime where (=~) = (==)
instance ApproxEq UTCTime where (=~) = (==)
instance ApproxEq Text where (=~) = (==)
instance ApproxEq Bool where (=~) = (==)
instance ApproxEq Int where (=~) = (==)
instance ApproxEq Double where (=~) = (==)
instance (ApproxEq a, Show a) => ApproxEq (NonEmpty a)
instance (ApproxEq a, Show a) => ApproxEq (Maybe a)
instance ApproxEq GeoPoint
instance ApproxEq Regexp
instance ApproxEq RangeValue
instance ApproxEq LessThan
instance ApproxEq LessThanEq
instance ApproxEq LessThanD
instance ApproxEq LessThanEqD
instance ApproxEq GreaterThan
instance ApproxEq GreaterThanEq
instance ApproxEq GreaterThanD
instance ApproxEq GreaterThanEqD
instance ApproxEq MinimumMatchHighLow
instance ApproxEq RegexpFlag
instance ApproxEq RegexpFlags
instance ApproxEq NullValue
instance ApproxEq Version
instance ApproxEq VersionNumber
instance ApproxEq DistanceRange
instance ApproxEq IndexName
instance ApproxEq MappingName
instance ApproxEq DocId
instance ApproxEq IndexAliasRouting
instance ApproxEq RoutingValue
instance ApproxEq ShardCount
instance ApproxEq ReplicaCount
instance ApproxEq TemplateName
instance ApproxEq TemplatePattern
instance ApproxEq QueryString
instance ApproxEq FieldName
instance ApproxEq CacheName
instance ApproxEq CacheKey
instance ApproxEq Existence
instance ApproxEq CutoffFrequency
instance ApproxEq Analyzer
instance ApproxEq Lenient
instance ApproxEq Tiebreaker
instance ApproxEq Boost
instance ApproxEq BoostTerms
instance ApproxEq MaxExpansions
instance ApproxEq MinimumMatch
instance ApproxEq DisableCoord
instance ApproxEq IgnoreTermFrequency
instance ApproxEq MinimumTermFrequency
instance ApproxEq MaxQueryTerms
instance ApproxEq Fuzziness
instance ApproxEq PrefixLength
instance ApproxEq TypeName
instance ApproxEq PercentMatch
instance ApproxEq StopWord
instance ApproxEq QueryPath
instance ApproxEq AllowLeadingWildcard
instance ApproxEq LowercaseExpanded
instance ApproxEq EnablePositionIncrements
instance ApproxEq AnalyzeWildcard
instance ApproxEq GeneratePhraseQueries
instance ApproxEq Locale
instance ApproxEq MaxWordLength
instance ApproxEq MinWordLength
instance ApproxEq PhraseSlop
instance ApproxEq MinDocFrequency
instance ApproxEq MaxDocFrequency
instance ApproxEq Filter
instance ApproxEq Query
instance ApproxEq SimpleQueryStringQuery
instance ApproxEq FieldOrFields
instance ApproxEq SimpleQueryFlag
instance ApproxEq RegexpQuery
instance ApproxEq QueryStringQuery
instance ApproxEq RangeQuery
instance ApproxEq PrefixQuery
instance ApproxEq NestedQuery
instance ApproxEq MoreLikeThisFieldQuery
instance ApproxEq MoreLikeThisQuery
instance ApproxEq IndicesQuery
instance ApproxEq HasParentQuery
instance ApproxEq HasChildQuery
instance ApproxEq FuzzyQuery
instance ApproxEq FuzzyLikeFieldQuery
instance ApproxEq FuzzyLikeThisQuery
instance ApproxEq DisMaxQuery
instance ApproxEq CommonTermsQuery
instance ApproxEq CommonMinimumMatch
instance ApproxEq BoostingQuery
instance ApproxEq BoolQuery
instance ApproxEq MatchQuery
instance ApproxEq MultiMatchQueryType
instance ApproxEq BooleanOperator
instance ApproxEq ZeroTermsQuery
instance ApproxEq MatchQueryType
instance ApproxEq AliasRouting
instance ApproxEq IndexAliasCreate
instance ApproxEq SearchAliasRouting
instance ApproxEq ScoreType
instance ApproxEq Distance
instance ApproxEq DistanceUnit
instance ApproxEq DistanceType
instance ApproxEq OptimizeBbox
instance ApproxEq GeoBoundingBoxConstraint
instance ApproxEq GeoFilterType
instance ApproxEq GeoBoundingBox
instance ApproxEq LatLon
instance ApproxEq RangeExecution
instance ApproxEq FSType
instance ApproxEq CompoundFormat
instance ApproxEq InitialShardCount
instance ApproxEq Bytes
instance ApproxEq ReplicaBounds
instance ApproxEq Term
instance ApproxEq BoolMatch
instance ApproxEq MultiMatchQuery
instance ApproxEq IndexSettings
instance ApproxEq AllocationPolicy
instance ApproxEq Char where
  (=~) = (==)
instance ApproxEq Vers.Version where
  (=~) = (==)
instance (ApproxEq a, Show a) => ApproxEq [a] where
  as =~ bs = and (zipWith (=~) as bs)
instance (ApproxEq l, Show l, ApproxEq r, Show r) => ApproxEq (Either l r) where
  Left a =~ Left b = a =~ b
  Right a =~ Right b = a =~ b
  _ =~ _ = False
  showApproxEq (Left x)  = "Left " <> showApproxEq x
  showApproxEq (Right x) = "Right " <> showApproxEq x
instance ApproxEq NodeAttrFilter
instance ApproxEq NodeAttrName
instance ApproxEq BuildHash
instance ApproxEq TemplateQueryKeyValuePairs where
  (=~) = (==)
instance ApproxEq TemplateQueryInline
instance ApproxEq Size
instance ApproxEq PhraseSuggesterHighlighter
instance ApproxEq PhraseSuggesterCollate
instance ApproxEq PhraseSuggester
instance ApproxEq SuggestType
instance ApproxEq Suggest
instance ApproxEq DirectGenerators
instance ApproxEq DirectGeneratorSuggestModeTypes

-- | Due to the way nodeattrfilters get serialized here, they may come
-- out in a different order, but they are morally equivalent
instance ApproxEq UpdatableIndexSetting where
  RoutingAllocationInclude a =~ RoutingAllocationInclude b =
    NE.sort a =~ NE.sort b
  RoutingAllocationExclude a =~ RoutingAllocationExclude b =
    NE.sort a =~ NE.sort b
  RoutingAllocationRequire a =~ RoutingAllocationRequire b =
    NE.sort a =~ NE.sort b
  a =~ b = a == b
  showApproxEq (RoutingAllocationInclude xs) = show (RoutingAllocationInclude (NE.sort xs))
  showApproxEq (RoutingAllocationExclude xs) = show (RoutingAllocationExclude (NE.sort xs))
  showApproxEq (RoutingAllocationRequire xs) = show (RoutingAllocationRequire (NE.sort xs))
  showApproxEq x = show x


noDuplicates :: Eq a => [a] -> Bool
noDuplicates xs = nub xs == xs

instance Arbitrary NominalDiffTime where
  arbitrary = fromInteger <$> arbitrary

#if !MIN_VERSION_QuickCheck(2,8,0)
instance (Arbitrary k, Ord k, Arbitrary v) => Arbitrary (M.Map k v) where
  arbitrary = M.fromList <$> arbitrary
#endif

instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary UTCTime where
  arbitrary = UTCTime
          <$> arbitrary
          <*> (fromRational . toRational <$> choose (0::Double, 86400))

instance Arbitrary Day where
    arbitrary = ModifiedJulianDay <$> (2000 +) <$> arbitrary
    shrink    = (ModifiedJulianDay <$>) . shrink . toModifiedJulianDay

#if !MIN_VERSION_QuickCheck(2,9,0)
instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = liftA2 (:|) arbitrary arbitrary
#endif

arbitraryScore :: Gen Score
arbitraryScore = fmap getPositive <$> arbitrary

instance (Arbitrary a, Typeable a) => Arbitrary (Hit a) where
  arbitrary = Hit <$> arbitrary
                  <*> arbitrary
                  <*> arbitrary
                  <*> arbitraryScore
                  <*> arbitrary
                  <*> arbitrary
  shrink = genericShrink


instance (Arbitrary a, Typeable a) => Arbitrary (SearchHits a) where
  arbitrary = reduceSize $ do
    tot <- getPositive <$> arbitrary
    score <- arbitraryScore
    hs <- arbitrary
    return $ SearchHits tot score hs
  shrink = genericShrink

reduceSize :: Gen a -> Gen a
reduceSize f = sized $ \n -> resize (n `div` 2) f

getSource :: EsResult a -> Maybe a
getSource = fmap _source . foundResult

grabFirst :: Either EsError (SearchResult a) -> Either EsError a
grabFirst r =
  case fmap (hitSource . head . hits . searchHits) r of
    (Left e)         -> Left e
    (Right Nothing)  -> Left (EsError 500 "Source was missing")
    (Right (Just x)) -> Right x

-------------------------------------------------------------------------------
arbitraryAlphaNum :: Gen Char
arbitraryAlphaNum = oneof [choose ('a', 'z')
                          ,choose ('A','Z')
                          , choose ('0', '9')]

instance Arbitrary RoutingValue where
  arbitrary = RoutingValue . T.pack <$> listOf1 arbitraryAlphaNum

instance Arbitrary AliasRouting where
  arbitrary = oneof [allAlias
                    ,one
                    ,theOther
                    ,both]
    where one = GranularAliasRouting
                <$> (Just <$> arbitrary)
                <*> pure Nothing
          theOther = GranularAliasRouting Nothing
                     <$> (Just <$> arbitrary)
          both = GranularAliasRouting
                 <$> (Just <$> arbitrary)
                 <*> (Just <$> arbitrary)
          allAlias = AllAliasRouting <$> arbitrary
  shrink = genericShrink


instance Arbitrary FieldName where
  arbitrary = FieldName . T.pack <$> listOf1 arbitraryAlphaNum
  shrink = genericShrink


#if MIN_VERSION_base(4,10,0)
-- Test.QuickCheck.Modifiers

qcNonEmptyToNonEmpty :: NonEmptyList a -> NonEmpty a
qcNonEmptyToNonEmpty (NonEmpty (a : xs)) = (a :| xs)
qcNonEmptyToNonEmpty (NonEmpty []) = error "NonEmpty was empty!"

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = qcNonEmptyToNonEmpty <$> arbitrary
#endif

instance Arbitrary RegexpFlags where
  arbitrary = oneof [ pure AllRegexpFlags
                    , pure NoRegexpFlags
                    , SomeRegexpFlags <$> genUniqueFlags
                    ]
    where genUniqueFlags = NE.fromList . nub <$> listOf1 arbitrary

  shrink = genericShrink


instance Arbitrary IndexAliasCreate where
  arbitrary = IndexAliasCreate <$> arbitrary <*> reduceSize arbitrary
  shrink = genericShrink

instance Arbitrary Query where
  arbitrary = reduceSize $ oneof [ TermQuery <$> arbitrary <*> arbitrary
                                 , TermsQuery <$> arbitrary <*> arbitrary
                                 , QueryMatchQuery <$> arbitrary
                                 , QueryMultiMatchQuery <$> arbitrary
                                 , QueryBoolQuery <$> arbitrary
                                 , QueryBoostingQuery <$> arbitrary
                                 , QueryCommonTermsQuery <$> arbitrary
                                 , ConstantScoreQuery <$> arbitrary <*> arbitrary
                                 , QueryDisMaxQuery <$> arbitrary
                                 , QueryFuzzyLikeThisQuery <$> arbitrary
                                 , QueryFuzzyLikeFieldQuery <$> arbitrary
                                 , QueryFuzzyQuery <$> arbitrary
                                 , QueryHasChildQuery <$> arbitrary
                                 , QueryHasParentQuery <$> arbitrary
                                 , IdsQuery <$> arbitrary <*> arbitrary
                                 , QueryIndicesQuery <$> arbitrary
                                 , MatchAllQuery <$> arbitrary
                                 , QueryMoreLikeThisQuery <$> arbitrary
                                 , QueryMoreLikeThisFieldQuery <$> arbitrary
                                 , QueryNestedQuery <$> arbitrary
                                 , QueryPrefixQuery <$> arbitrary
                                 , QueryQueryStringQuery <$> arbitrary
                                 , QuerySimpleQueryStringQuery <$> arbitrary
                                 , QueryRangeQuery <$> arbitrary
                                 , QueryRegexpQuery <$> arbitrary
                                 , QueryTemplateQueryInline <$> arbitrary
                                 ]
  shrink = genericShrink

instance Arbitrary Filter where
  arbitrary = Filter <$> arbitrary 
  shrink = genericShrink

instance Arbitrary ReplicaBounds where
  arbitrary = oneof [ replicasBounded
                    , replicasLowerBounded
                    , pure ReplicasUnbounded
                    ]
    where replicasBounded = do Positive a <- arbitrary
                               Positive b <- arbitrary
                               return (ReplicasBounded a b)
          replicasLowerBounded = do Positive a <- arbitrary
                                    return (ReplicasLowerBounded a)

instance Arbitrary NodeAttrName where
  arbitrary = NodeAttrName . T.pack <$> listOf1 arbitraryAlphaNum


instance Arbitrary NodeAttrFilter where
  arbitrary = do
    n <- arbitrary
    s:ss <- listOf1 (listOf1 arbitraryAlphaNum)
    let ts = T.pack <$> s :| ss
    return (NodeAttrFilter n ts)
  shrink = genericShrink

instance Arbitrary VersionNumber where
  arbitrary = mk . fmap getPositive . getNonEmpty <$> arbitrary
    where
      mk versions = VersionNumber (Vers.Version versions [])

instance Arbitrary TemplateQueryKeyValuePairs where
  arbitrary = TemplateQueryKeyValuePairs . HM.fromList <$> arbitrary
  shrink (TemplateQueryKeyValuePairs x) = map (TemplateQueryKeyValuePairs . HM.fromList) . shrink $ HM.toList x

instance Arbitrary IndexName where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary MappingName where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary DocId where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary Version where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary BuildHash where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary IndexAliasRouting where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary ShardCount where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary ReplicaCount where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary TemplateName where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary TemplatePattern where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary QueryString where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary CacheName where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary CacheKey where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary Existence where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary CutoffFrequency where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary Analyzer where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary MaxExpansions where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary Lenient where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary Tiebreaker where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary Boost where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary BoostTerms where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary MinimumMatch where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary DisableCoord where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary IgnoreTermFrequency where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary MinimumTermFrequency where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary MaxQueryTerms where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary Fuzziness where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary PrefixLength where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary TypeName where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary PercentMatch where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary StopWord where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary QueryPath where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary AllowLeadingWildcard where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary LowercaseExpanded where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary EnablePositionIncrements where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary AnalyzeWildcard where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary GeneratePhraseQueries where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary Locale where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary MaxWordLength where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary MinWordLength where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary PhraseSlop where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary MinDocFrequency where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary MaxDocFrequency where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary Regexp where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary SimpleQueryStringQuery where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary FieldOrFields where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary SimpleQueryFlag where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary RegexpQuery where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary QueryStringQuery where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary RangeQuery where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary RangeValue where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary PrefixQuery where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary NestedQuery where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary MoreLikeThisFieldQuery where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary MoreLikeThisQuery where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary IndicesQuery where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary HasParentQuery where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary HasChildQuery where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary FuzzyQuery where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary FuzzyLikeFieldQuery where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary FuzzyLikeThisQuery where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary DisMaxQuery where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary CommonTermsQuery where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary DistanceRange where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary MultiMatchQuery where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary LessThanD where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary LessThanEqD where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary GreaterThanD where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary GreaterThanEqD where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary LessThan where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary LessThanEq where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary GreaterThan where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary GreaterThanEq where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary GeoPoint where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary NullValue where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary MinimumMatchHighLow where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary CommonMinimumMatch where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary BoostingQuery where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary BoolQuery where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary MatchQuery where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary MultiMatchQueryType where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary BooleanOperator where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary ZeroTermsQuery where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary MatchQueryType where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary SearchAliasRouting where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary ScoreType where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary Distance where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary DistanceUnit where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary DistanceType where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary OptimizeBbox where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary GeoBoundingBoxConstraint where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary GeoFilterType where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary GeoBoundingBox where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary LatLon where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary RangeExecution where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary RegexpFlag where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary BoolMatch where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary Term where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary IndexSettings where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary TokenChar where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary Ngram where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary TokenizerDefinition where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary AnalyzerDefinition where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary TokenFilterDefinition where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary Shingle where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary Language where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary Analysis where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary Tokenizer where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary TokenFilter where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary UpdatableIndexSetting where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary Compression where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary Bytes where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary AllocationPolicy where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary InitialShardCount where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary FSType where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary CompoundFormat where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary FsSnapshotRepo where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary SnapshotRepoName where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary TemplateQueryInline where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary PhraseSuggesterCollate where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary PhraseSuggesterHighlighter where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary Size where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary PhraseSuggester where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary SuggestType where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary Suggest where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary DirectGenerators where arbitrary = sopArbitrary; shrink = genericShrink
instance Arbitrary DirectGeneratorSuggestModeTypes where arbitrary = sopArbitrary; shrink = genericShrink

newtype UpdatableIndexSetting' = UpdatableIndexSetting' UpdatableIndexSetting
                               deriving (Show, Eq, ToJSON, FromJSON, ApproxEq, Typeable)

instance Arbitrary UpdatableIndexSetting' where
  arbitrary = do
    settings <- arbitrary
    return $ UpdatableIndexSetting' $ case settings of
      RoutingAllocationInclude xs -> RoutingAllocationInclude (dropDuplicateAttrNames xs)
      RoutingAllocationExclude xs -> RoutingAllocationExclude (dropDuplicateAttrNames xs)
      RoutingAllocationRequire xs -> RoutingAllocationRequire (dropDuplicateAttrNames xs)
      x -> x
    where
      dropDuplicateAttrNames = NE.fromList . L.nubBy sameAttrName . NE.toList
      sameAttrName a b = nodeAttrFilterName a == nodeAttrFilterName b
  shrink (UpdatableIndexSetting' x) = map UpdatableIndexSetting' (genericShrink x)

main :: IO ()
main = hspec $ do

  describe "index create/delete API" $ do
    it "creates and then deletes the requested index" $ withTestEnv $ do
      -- priming state.
      _ <- deleteExampleIndex
      resp <- createExampleIndex
      deleteResp <- deleteExampleIndex
      liftIO $ do
        validateStatus resp 200
        validateStatus deleteResp 200

  describe "error parsing"  $ do
    it "can parse EsErrors for < 2.0" $ when' (atmost es16) $ withTestEnv $ do
      res <- getDocument (IndexName "bogus") (MappingName "also_bogus") (DocId "bogus_as_well")
      let errorResp = eitherDecode (responseBody res)
      liftIO (errorResp `shouldBe` Right (EsError 404 "IndexMissingException[[bogus] missing]"))

    it "can parse EsErrors for >= 2.0" $ when' (atleast es20) $ withTestEnv $ do
      res <- getDocument (IndexName "bogus") (MappingName "also_bogus") (DocId "bogus_as_well")
      let errorResp = eitherDecode (responseBody res)
      liftIO (errorResp `shouldBe` Right (EsError 404 "no such index"))

  describe "document API" $ do
    it "indexes, updates, gets, and then deletes the generated document" $ withTestEnv $ do
      _ <- insertData
      _ <- updateData
      docInserted <- getDocument testIndex testMapping (DocId "1")
      let newTweet = eitherDecode
                     (responseBody docInserted) :: Either String (EsResult Tweet)
      liftIO $ (fmap getSource newTweet `shouldBe` Right (Just patchedTweet))

    it "indexes, gets, and then deletes the generated document with a DocId containing a space" $ withTestEnv $ do
      _ <- insertWithSpaceInId
      docInserted <- getDocument testIndex testMapping (DocId "Hello World")
      let newTweet = eitherDecode
                     (responseBody docInserted) :: Either String (EsResult Tweet)
      liftIO $ (fmap getSource newTweet `shouldBe` Right (Just exampleTweet))

    it "produces a parseable result when looking up a bogus document" $ withTestEnv $ do
      doc <- getDocument testIndex testMapping  (DocId "bogus")
      let noTweet = eitherDecode
                    (responseBody doc) :: Either String (EsResult Tweet)
      liftIO $ fmap foundResult noTweet `shouldBe` Right Nothing

    it "can use optimistic concurrency control" $ withTestEnv $ do
      let ev = ExternalDocVersion minBound
      let cfg = defaultIndexDocumentSettings { idsVersionControl = ExternalGT ev }
      resetIndex
      res <- insertData' cfg
      liftIO $ isCreated res `shouldBe` True
      res' <- insertData' cfg
      liftIO $ isVersionConflict res' `shouldBe` True

    it "indexes two documents in a parent/child relationship and checks that the child exists" $ withTestEnv $ do
      resetIndex
      _ <- putMapping testIndex (MappingName "child") ChildMapping
      _ <- putMapping testIndex (MappingName "parent") ParentMapping
      _ <- indexDocument testIndex (MappingName "parent") defaultIndexDocumentSettings exampleTweet (DocId "1")
      let parent = (Just . DocumentParent . DocId) "1"
          ids = IndexDocumentSettings NoVersionControl parent
      _ <- indexDocument testIndex (MappingName "child") ids otherTweet (DocId "2")
      _ <- refreshIndex testIndex
      exists <- documentExists testIndex (MappingName "child") parent (DocId "2")
      liftIO $ exists `shouldBe` True

  describe "template API" $ do
    it "can create a template" $ withTestEnv $ do
      let idxTpl = IndexTemplate (TemplatePattern "tweet-*") (Just (IndexSettings (ShardCount 1) (ReplicaCount 1))) [toJSON TweetMapping]
      resp <- putTemplate idxTpl (TemplateName "tweet-tpl")
      liftIO $ validateStatus resp 200

    it "can detect if a template exists" $ withTestEnv $ do
      exists <- templateExists (TemplateName "tweet-tpl")
      liftIO $ exists `shouldBe` True

    it "can delete a template" $ withTestEnv $ do
      resp <- deleteTemplate (TemplateName "tweet-tpl")
      liftIO $ validateStatus resp 200

    it "can detect if a template doesn't exist" $ withTestEnv $ do
      exists <- templateExists (TemplateName "tweet-tpl")
      liftIO $ exists `shouldBe` False

  describe "bulk API" $ do
    it "inserts all documents we request" $ withTestEnv $ do
      _ <- insertData
      let firstTest = BulkTest "blah"
      let secondTest = BulkTest "bloo"
      let thirdTest = BulkTest "graffle"
      let firstDoc = BulkIndex testIndex
                     testMapping (DocId "2") (toJSON firstTest)
      let secondDoc = BulkCreate testIndex
                     testMapping (DocId "3") (toJSON secondTest)
      let thirdDoc = BulkCreateEncoding testIndex
                     testMapping (DocId "4") (toEncoding thirdTest)
      let stream = V.fromList [firstDoc, secondDoc, thirdDoc]
      _ <- bulk stream
      _ <- refreshIndex testIndex
      fDoc <- getDocument testIndex testMapping (DocId "2")
      sDoc <- getDocument testIndex testMapping (DocId "3")
      tDoc <- getDocument testIndex testMapping (DocId "4")
      let maybeFirst  = eitherDecode $ responseBody fDoc :: Either String (EsResult BulkTest)
      let maybeSecond = eitherDecode $ responseBody sDoc :: Either String (EsResult BulkTest)
      let maybeThird = eitherDecode $ responseBody tDoc :: Either String (EsResult BulkTest)
      liftIO $ do
        fmap getSource maybeFirst `shouldBe` Right (Just firstTest)
        fmap getSource maybeSecond `shouldBe` Right (Just secondTest)
        fmap getSource maybeThird `shouldBe` Right (Just thirdTest)


  describe "query API" $ do
    it "returns document for term query and identity filter" $ withTestEnv $ do
      _ <- insertData
      let query = TermQuery (Term "user" "bitemyapp") Nothing
      let filter = Filter $ MatchAllQuery Nothing
      let search = mkSearch (Just query) (Just filter)
      myTweet <- searchTweet search
      liftIO $
        myTweet `shouldBe` Right exampleTweet

    it "handles constant score queries" $ withTestEnv $ do
      _ <- insertData
      let query = TermsQuery "user" ("bitemyapp" :| [])
      let cfQuery = ConstantScoreQuery query (Boost 1.0)
      let filter = Filter $ MatchAllQuery Nothing
      let search = mkSearch (Just cfQuery) (Just filter)
      myTweet <- searchTweet search
      liftIO $
        myTweet `shouldBe` Right exampleTweet

    it "returns document for terms query and identity filter" $ withTestEnv $ do
      _ <- insertData
      let query = TermsQuery "user" ("bitemyapp" :| [])
      let filter = Filter $ MatchAllQuery Nothing
      let search = mkSearch (Just query) (Just filter)
      myTweet <- searchTweet search
      liftIO $
        myTweet `shouldBe` Right exampleTweet

    it "returns document for match query" $ withTestEnv $ do
      _ <- insertData
      let query = QueryMatchQuery $ mkMatchQuery (FieldName "user") (QueryString "bitemyapp")
      let search = mkSearch (Just query) Nothing
      myTweet <- searchTweet search
      liftIO $
        myTweet `shouldBe` Right exampleTweet

    it "returns document for multi-match query" $ withTestEnv $ do
      _ <- insertData
      let flds = [FieldName "user", FieldName "message"]
      let query = QueryMultiMatchQuery $ mkMultiMatchQuery flds (QueryString "bitemyapp")
      let search = mkSearch (Just query) Nothing
      myTweet <- searchTweet search
      liftIO $
        myTweet `shouldBe` Right exampleTweet

    it "returns document for multi-match query with a custom tiebreaker" $ withTestEnv $ do
      _ <- insertData
      let tiebreaker = Just $ Tiebreaker 0.3
          flds = [FieldName "user", FieldName "message"]
          multiQuery' = mkMultiMatchQuery flds (QueryString "bitemyapp")
          query =  QueryMultiMatchQuery $ multiQuery' { multiMatchQueryTiebreaker = tiebreaker }
          search = mkSearch (Just query) Nothing
      myTweet <- searchTweet search
      liftIO $
        myTweet `shouldBe` Right exampleTweet

    it "returns document for bool query" $ withTestEnv $ do
      _ <- insertData
      let innerQuery = QueryMatchQuery $
                       mkMatchQuery (FieldName "user") (QueryString "bitemyapp")
      let query = QueryBoolQuery $
                  mkBoolQuery [innerQuery] [] [] []
      let search = mkSearch (Just query) Nothing
      myTweet <- searchTweet search
      liftIO $
        myTweet `shouldBe` Right exampleTweet

    it "returns document for boosting query" $ withTestEnv $ do
      _ <- insertData
      let posQuery = QueryMatchQuery $ mkMatchQuery (FieldName "user") (QueryString "bitemyapp")
      let negQuery = QueryMatchQuery $ mkMatchQuery (FieldName "user") (QueryString "notmyapp")
      let query = QueryBoostingQuery $ BoostingQuery posQuery negQuery (Boost 0.2)
      let search = mkSearch (Just query) Nothing
      myTweet <- searchTweet search
      liftIO $
        myTweet `shouldBe` Right exampleTweet

    it "returns document for common terms query" $ withTestEnv $ do
      _ <- insertData
      let query = QueryCommonTermsQuery $
                  CommonTermsQuery (FieldName "user")
                  (QueryString "bitemyapp")
                  (CutoffFrequency 0.0001)
                  Or Or Nothing Nothing Nothing Nothing
      let search = mkSearch (Just query) Nothing
      myTweet <- searchTweet search
      liftIO $
        myTweet `shouldBe` Right exampleTweet

    it "returns document for for inline template query" $ withTestEnv $ do
      _ <- insertData
      let innerQuery = QueryMatchQuery $
                         mkMatchQuery (FieldName "{{userKey}}")
                                      (QueryString "{{bitemyappKey}}")
          templateParams = TemplateQueryKeyValuePairs $ HM.fromList
                            [ ("userKey", "user")
                            , ("bitemyappKey", "bitemyapp")
                            ]
          templateQuery = QueryTemplateQueryInline $
                            TemplateQueryInline innerQuery templateParams
          search = mkSearch (Just templateQuery) Nothing
      myTweet <- searchTweet search
      liftIO $ myTweet `shouldBe` Right exampleTweet


  describe "sorting" $ do
    it "returns documents in the right order" $ withTestEnv $ do
      _ <- insertData
      _ <- insertOther
      let sortSpec = DefaultSortSpec $ mkSort (FieldName "age") Ascending
      let search = Search Nothing
                   Nothing (Just [sortSpec]) Nothing Nothing
                   False (From 0) (Size 10) SearchTypeQueryThenFetch Nothing Nothing
                   Nothing
      result <- searchTweets search
      let myTweet = grabFirst result
      liftIO $
        myTweet `shouldBe` Right otherTweet

  describe "Aggregation API" $ do
    it "returns term aggregation results" $ withTestEnv $ do
      _ <- insertData
      let terms = TermsAgg $ mkTermsAggregation "user"
      let search = mkAggregateSearch Nothing $ mkAggregations "users" terms
      searchExpectAggs search
      searchValidBucketAgg search "users" toTerms

    it "return sub-aggregation results" $ withTestEnv $ do
      _ <- insertData
      let subaggs = mkAggregations "age_agg" . TermsAgg $ mkTermsAggregation "age"
          agg = TermsAgg $ (mkTermsAggregation "user") { termAggs = Just subaggs}
          search = mkAggregateSearch Nothing $ mkAggregations "users" agg
      reply <- searchByIndex testIndex search
      let result = decode (responseBody reply) :: Maybe (SearchResult Tweet)
          usersAggResults = result >>= aggregations >>= toTerms "users"
          subAggResults = usersAggResults >>= (listToMaybe . buckets) >>= termsAggs >>= toTerms "age_agg"
          subAddResultsExists = isJust subAggResults
      liftIO $ (subAddResultsExists) `shouldBe` True

    it "returns cardinality aggregation results" $ withTestEnv $ do
      _ <- insertData
      let cardinality = CardinalityAgg $ mkCardinalityAggregation $ FieldName "user"
      let search = mkAggregateSearch Nothing $ mkAggregations "users" cardinality
      let search' = search { Database.V5.Bloodhound.from = From 0, size = Size 0 }
      searchExpectAggs search'
      let docCountPair k n = (k, object ["value" .= Number n])
      res <- searchTweets search'
      liftIO $
        fmap aggregations res `shouldBe` Right (Just (M.fromList [ docCountPair "users" 1]))

    it "returns stats aggregation results" $ withTestEnv $ do
      _ <- insertData
      let stats = StatsAgg $ mkStatsAggregation $ FieldName "age"
      let search = mkAggregateSearch Nothing $ mkAggregations "users" stats
      let search' = search { Database.V5.Bloodhound.from = From 0, size = Size 0 }
      searchExpectAggs search'
      let statsAggRes k n = (k, object [ "max" .= Number n
                                       , "avg" .= Number n
                                       , "count" .= Number 1
                                       , "min" .= Number n
                                       , "sum" .= Number n])
      res <- searchTweets search'
      liftIO $
        fmap aggregations res `shouldBe` Right (Just (M.fromList [ statsAggRes "users" 10000]))

    it "can give collection hint parameters to term aggregations" $ when' (atleast es13) $ withTestEnv $ do
      _ <- insertData
      let terms = TermsAgg $ (mkTermsAggregation "user") { termCollectMode = Just BreadthFirst }
      let search = mkAggregateSearch Nothing $ mkAggregations "users" terms
      searchExpectAggs search
      searchValidBucketAgg search "users" toTerms

    -- One of these fails with 1.7.3
    it "can give execution hint parameters to term aggregations" $ when' (atmost es11) $ withTestEnv $ do
      _ <- insertData
      searchTermsAggHint [Map, Ordinals]

    it "can give execution hint parameters to term aggregations" $ when' (is es12) $ withTestEnv $ do
      _ <- insertData
      searchTermsAggHint [GlobalOrdinals, GlobalOrdinalsHash, GlobalOrdinalsLowCardinality, Map, Ordinals]

    it "can give execution hint parameters to term aggregations" $ when' (atleast es12) $ withTestEnv $ do
      _ <- insertData
      searchTermsAggHint [GlobalOrdinals, GlobalOrdinalsHash, GlobalOrdinalsLowCardinality, Map]
    -- One of the above.

    it "can execute value_count aggregations" $ withTestEnv $ do
      _ <- insertData
      _ <- insertOther
      let ags = mkAggregations "user_count" (ValueCountAgg (FieldValueCount (FieldName "user"))) <>
                mkAggregations "bogus_count" (ValueCountAgg (FieldValueCount (FieldName "bogus")))
      let search = mkAggregateSearch Nothing ags
      let docCountPair k n = (k, object ["value" .= Number n])
      res <- searchTweets search
      liftIO $
        fmap aggregations res `shouldBe` Right (Just (M.fromList [ docCountPair "user_count" 2
                                                                 , docCountPair "bogus_count" 0
                                                                 ]))

    it "can execute date_range aggregations" $ withTestEnv $ do
      let now = fromGregorian 2015 3 14
      let ltAMonthAgo = UTCTime (fromGregorian 2015 3 1) 0
      let ltAWeekAgo = UTCTime (fromGregorian 2015 3 10) 0
      let oldDoc = exampleTweet { postDate = ltAMonthAgo }
      let newDoc = exampleTweet { postDate = ltAWeekAgo }
      _ <- indexDocument testIndex testMapping defaultIndexDocumentSettings oldDoc (DocId "1")
      _ <- indexDocument testIndex testMapping defaultIndexDocumentSettings newDoc (DocId "2")
      _ <- refreshIndex testIndex
      let thisMonth = DateRangeFrom (DateMathExpr (DMDate now) [SubtractTime 1 DMMonth])
      let thisWeek = DateRangeFrom (DateMathExpr (DMDate now) [SubtractTime 1 DMWeek])
      let agg = DateRangeAggregation (FieldName "postDate") Nothing (thisMonth :| [thisWeek])
      let ags = mkAggregations "date_ranges" (DateRangeAgg agg)
      let search = mkAggregateSearch Nothing ags
      res <- searchTweets search
      liftIO $ hitsTotal . searchHits <$> res `shouldBe` Right 2
      let bucks = do magrs <- fmapL show (aggregations <$> res)
                     agrs <- note "no aggregations returned" magrs
                     rawBucks <- note "no date_ranges aggregation" $ M.lookup "date_ranges" agrs
                     parseEither parseJSON rawBucks
      let fromMonthT = UTCTime (fromGregorian 2015 2 14) 0
      let fromWeekT = UTCTime (fromGregorian 2015 3 7) 0
      liftIO $ buckets <$> bucks `shouldBe` Right [ DateRangeResult "2015-02-14T00:00:00.000Z-*"
                                                                    (Just fromMonthT)
                                                                    (Just "2015-02-14T00:00:00.000Z")
                                                                    Nothing
                                                                    Nothing
                                                                    2
                                                                    Nothing
                                                  , DateRangeResult "2015-03-07T00:00:00.000Z-*"
                                                                    (Just fromWeekT)
                                                                    (Just "2015-03-07T00:00:00.000Z")
                                                                    Nothing
                                                                    Nothing
                                                                    1
                                                                    Nothing
                                     ]

    it "returns date histogram aggregation results" $ withTestEnv $ do
      _ <- insertData
      let histogram = DateHistogramAgg $ mkDateHistogram (FieldName "postDate") Minute
      let search = mkAggregateSearch Nothing (mkAggregations "byDate" histogram)
      searchExpectAggs search
      searchValidBucketAgg search "byDate" toDateHistogram

    it "can execute missing aggregations" $ withTestEnv $ do
      _ <- insertData
      _ <- insertExtra
      let ags = mkAggregations "missing_agg" (MissingAgg (MissingAggregation "extra"))
      let search = mkAggregateSearch Nothing ags
      let docCountPair k n = (k, object ["doc_count" .= Number n])
      res <- searchTweets search
      liftIO $
        fmap aggregations res `shouldBe` Right (Just (M.fromList [docCountPair "missing_agg" 1]))

  describe "Highlights API" $ do

    it "returns highlight from query when there should be one" $ withTestEnv $ do
      _ <- insertData
      _ <- insertOther
      let query = QueryMatchQuery $ mkMatchQuery (FieldName "message") (QueryString "haskell")
      let testHighlight = Highlights Nothing [FieldHighlight (FieldName "message") Nothing]

      let search = mkHighlightSearch (Just query) testHighlight
      myHighlight <- searchTweetHighlight search
      liftIO $
        myHighlight `shouldBe` Right (Just (M.fromList [("message",["Use <em>haskell</em>!"])]))

    it "doesn't return highlight from a query when it shouldn't" $ withTestEnv $ do
      _ <- insertData
      _ <- insertOther
      let query = QueryMatchQuery $ mkMatchQuery (FieldName "message") (QueryString "haskell")
      let testHighlight = Highlights Nothing [FieldHighlight (FieldName "user") Nothing]

      let search = mkHighlightSearch (Just query) testHighlight
      myHighlight <- searchTweetHighlight search
      liftIO $
        myHighlight `shouldBe` Right Nothing

  describe "Source filtering" $ do

    it "doesn't include source when sources are disabled" $ withTestEnv $ do
      searchExpectSource
        NoSource
        (Left (EsError 500 "Source was missing"))

    it "includes a source" $ withTestEnv $ do
      searchExpectSource
        (SourcePatterns (PopPattern (Pattern "message")))
        (Right (Object (HM.fromList [("message", String "Use haskell!")])))

    it "includes sources" $ withTestEnv $ do
      searchExpectSource
        (SourcePatterns (PopPatterns [Pattern "user", Pattern "message"]))
        (Right (Object (HM.fromList [("user",String "bitemyapp"),("message", String "Use haskell!")])))

    it "includes source patterns" $ withTestEnv $ do
      searchExpectSource
        (SourcePatterns (PopPattern (Pattern "*ge")))
        (Right (Object (HM.fromList [("age", Number 10000),("message", String "Use haskell!")])))

    it "excludes source patterns" $ withTestEnv $ do
      searchExpectSource
        (SourceIncludeExclude (Include [])
        (Exclude [Pattern "l*", Pattern "*ge", Pattern "postDate", Pattern "extra"]))
        (Right (Object (HM.fromList [("user",String "bitemyapp")])))

  describe "ToJSON RegexpFlags" $ do
    it "generates the correct JSON for AllRegexpFlags" $
      toJSON AllRegexpFlags `shouldBe` String "ALL"

    it "generates the correct JSON for NoRegexpFlags" $
      toJSON NoRegexpFlags `shouldBe` String "NONE"

    it "generates the correct JSON for SomeRegexpFlags" $
      let flags = AnyString :| [ Automaton
                               , Complement
                               , Empty
                               , Intersection
                               , Interval ]
      in toJSON (SomeRegexpFlags flags) `shouldBe` String "ANYSTRING|AUTOMATON|COMPLEMENT|EMPTY|INTERSECTION|INTERVAL"

    prop "removes duplicates from flags" $ \(flags :: RegexpFlags) ->
      let String str = toJSON flags
          flagStrs   = T.splitOn "|" str
      in noDuplicates flagStrs

  describe "omitNulls" $ do
    it "checks that omitNulls drops list elements when it should" $
       let dropped = omitNulls $ [ "test1" .= (toJSON ([] :: [Int]))
                                 , "test2" .= (toJSON ("some value" :: Text))]
       in dropped `shouldBe` Object (HM.fromList [("test2", String "some value")])

    it "checks that omitNulls doesn't drop list elements when it shouldn't" $
       let notDropped = omitNulls $ [ "test1" .= (toJSON ([1] :: [Int]))
                                    , "test2" .= (toJSON ("some value" :: Text))]
       in notDropped `shouldBe` Object (HM.fromList [ ("test1", Array (V.fromList [Number 1.0]))
                                                 , ("test2", String "some value")])
    it "checks that omitNulls drops non list elements when it should" $
       let dropped = omitNulls $ [ "test1" .= (toJSON Null)
                                 , "test2" .= (toJSON ("some value" :: Text))]
       in dropped `shouldBe` Object (HM.fromList [("test2", String "some value")])
    it "checks that omitNulls doesn't drop non list elements when it shouldn't" $
       let notDropped = omitNulls $ [ "test1" .= (toJSON (1 :: Int))
                                    , "test2" .= (toJSON ("some value" :: Text))]
       in notDropped `shouldBe` Object (HM.fromList [ ("test1", Number 1.0)
                                                   , ("test2", String "some value")])
  describe "Monoid (SearchHits a)" $ do
    prop "abides the monoid laws" $ eq $
      prop_Monoid (T :: T (SearchHits ()))

  describe "mkDocVersion" $ do
    prop "can never construct an out of range docVersion" $ \i ->
      let res = mkDocVersion i
      in case res of
        Nothing -> property True
        Just dv -> (dv >= minBound) .&&.
                   (dv <= maxBound) .&&.
                   docVersionNumber dv === i

  describe "FsSnapshotRepo" $ do
    prop "SnapshotRepo laws" $ \fsr ->
      fromGSnapshotRepo (toGSnapshotRepo fsr) === Right (fsr :: FsSnapshotRepo)

  describe "snapshot repos" $ do
    it "always parses all snapshot repos API" $ when' canSnapshot $ withTestEnv $ do
      res <- getSnapshotRepos AllSnapshotRepos
      liftIO $ case res of
        Left e -> expectationFailure ("Expected a right but got Left " <> show e)
        Right _ -> return ()

    it "finds an existing list of repos" $ when' canSnapshot $ withTestEnv $ do
      let r1n = SnapshotRepoName "bloodhound-repo1"
      let r2n = SnapshotRepoName "bloodhound-repo2"
      withSnapshotRepo r1n $ \r1 ->
        withSnapshotRepo r2n $ \r2 -> do
          repos <- getSnapshotRepos (SnapshotRepoList (ExactRepo r1n :| [ExactRepo r2n]))
          liftIO $ case repos of
            Right xs -> do
              let srt = L.sortBy (comparing gSnapshotRepoName)
              srt xs `shouldBe` srt [r1, r2]
            Left e -> expectationFailure (show e)

    it "creates and updates with updateSnapshotRepo" $ when' canSnapshot $ withTestEnv $ do
      let r1n = SnapshotRepoName "bloodhound-repo1"
      withSnapshotRepo r1n $ \r1 -> do
        let Just (String dir) = HM.lookup "location" (gSnapshotRepoSettingsObject (gSnapshotRepoSettings r1))
        let noCompression = FsSnapshotRepo r1n (T.unpack dir) False Nothing Nothing Nothing
        resp <- updateSnapshotRepo defaultSnapshotRepoUpdateSettings noCompression
        liftIO (validateStatus resp 200)
        Right [roundtrippedNoCompression] <- getSnapshotRepos (SnapshotRepoList (ExactRepo r1n :| []))
        liftIO (roundtrippedNoCompression `shouldBe` toGSnapshotRepo noCompression)

    -- verify came around in 1.4 it seems
    it "can verify existing repos" $ when' canSnapshot $ when' (atleast es14) $ withTestEnv $ do
      let r1n = SnapshotRepoName "bloodhound-repo1"
      withSnapshotRepo r1n $ \_ -> do
        res <- verifySnapshotRepo r1n
        liftIO $ case res of
          Right (SnapshotVerification vs)
            | null vs -> expectationFailure "Expected nonempty set of verifying nodes"
            | otherwise -> return ()
          Left e -> expectationFailure (show e)

  describe "snapshots" $ do
    it "always parses all snapshots API" $ when' canSnapshot $ withTestEnv $ do
      let r1n = SnapshotRepoName "bloodhound-repo1"
      withSnapshotRepo r1n $ \_ -> do
        res <- getSnapshots r1n AllSnapshots
        liftIO $ case res of
          Left e -> expectationFailure ("Expected a right but got Left " <> show e)
          Right _ -> return ()

    it "can parse a snapshot that it created" $ when' canSnapshot $ withTestEnv $ do
      let r1n = SnapshotRepoName "bloodhound-repo1"
      withSnapshotRepo r1n $ \_ -> do
        let s1n = SnapshotName "example-snapshot"
        withSnapshot r1n s1n $ do
          res <- getSnapshots r1n (SnapshotList (ExactSnap s1n :| []))
          liftIO $ case res of
            Right [snap]
              | snapInfoState snap == SnapshotSuccess &&
                snapInfoName snap == s1n -> return ()
              | otherwise -> expectationFailure (show snap)
            Right [] -> expectationFailure "There were no snapshots"
            Right snaps -> expectationFailure ("Expected 1 snapshot but got" <> show (length snaps))
            Left e -> expectationFailure (show e)

  describe "snapshot restore" $ do
    it "can restore a snapshot that we create" $ when' canSnapshot $ withTestEnv $ do
      let r1n = SnapshotRepoName "bloodhound-repo1"
      withSnapshotRepo r1n $ \_ -> do
        let s1n = SnapshotName "example-snapshot"
        withSnapshot r1n s1n $ do
          let settings = defaultSnapshotRestoreSettings { snapRestoreWaitForCompletion = True }
          -- have to close an index to restore it
          resp1 <- closeIndex testIndex
          liftIO (validateStatus resp1 200)
          resp2 <- restoreSnapshot r1n s1n settings
          liftIO (validateStatus resp2 200)

    it "can restore and rename" $ when' canSnapshot $ withTestEnv $ do
      let r1n = SnapshotRepoName "bloodhound-repo1"
      withSnapshotRepo r1n $ \_ -> do
        let s1n = SnapshotName "example-snapshot"
        withSnapshot r1n s1n $ do
          let pat = RestoreRenamePattern "bloodhound-tests-twitter-(\\d+)"
          let replace = RRTLit "restored-" :| [RRSubWholeMatch]
          let expectedIndex = IndexName "restored-bloodhound-tests-twitter-1"
          oldEnoughForOverrides <- liftIO (atleast es15)
          let overrides = RestoreIndexSettings { restoreOverrideReplicas = Just (ReplicaCount 0) }
          let settings = defaultSnapshotRestoreSettings { snapRestoreWaitForCompletion = True
                                                        , snapRestoreRenamePattern = Just pat
                                                        , snapRestoreRenameReplacement = Just replace
                                                        , snapRestoreIndexSettingsOverrides = if oldEnoughForOverrides
                                                                                              then Just overrides
                                                                                              else Nothing
                                                        }
          -- have to close an index to restore it
          let go = do
                resp <- restoreSnapshot r1n s1n settings
                liftIO (validateStatus resp 200)
                exists <- indexExists expectedIndex
                liftIO (exists `shouldBe` True)
          go `finally` deleteIndex expectedIndex

  describe "getNodesInfo" $ do
     it "fetches the responding node when LocalNode is used" $ withTestEnv $ do
       res <- getNodesInfo LocalNode
       liftIO $ case res of
         -- This is really just a smoke test for response
         -- parsing. Node info is so variable, there's not much I can
         -- assert here.
         Right NodesInfo {..} -> length nodesInfo `shouldBe` 1
         Left e -> expectationFailure ("Expected NodesInfo but got " <> show e)

  describe "getNodesStats" $ do
     it "fetches the responding node when LocalNode is used" $ withTestEnv $ do
       res <- getNodesStats LocalNode
       liftIO $ case res of
         -- This is really just a smoke test for response
         -- parsing. Node stats is so variable, there's not much I can
         -- assert here.
         Right NodesStats {..} -> length nodesStats `shouldBe` 1
         Left e -> expectationFailure ("Expected NodesStats but got " <> show e)

  describe "Enum DocVersion" $ do
    it "follows the laws of Enum, Bounded" $ do
      evaluate (succ maxBound :: DocVersion) `shouldThrow` anyErrorCall
      evaluate (pred minBound :: DocVersion) `shouldThrow` anyErrorCall
      evaluate (toEnum 0 :: DocVersion) `shouldThrow` anyErrorCall
      evaluate (toEnum 9200000000000000001 :: DocVersion) `shouldThrow` anyErrorCall
      enumFrom (pred maxBound :: DocVersion) `shouldBe` [pred maxBound, maxBound]
      enumFrom (pred maxBound :: DocVersion) `shouldBe` [pred maxBound, maxBound]
      enumFromThen minBound (pred maxBound :: DocVersion) `shouldBe` [minBound, pred maxBound]

  describe "scan&scroll API" $ do
    it "returns documents using the scan&scroll API" $ withTestEnv $ do
      _ <- insertData
      _ <- insertOther
      let search = (mkSearch (Just $ MatchAllQuery Nothing) Nothing) { size = (Size 1) }
      regular_search <- searchTweet search
      scan_search' <- scanSearch testIndex testMapping search :: BH IO [Hit Tweet]
      let scan_search = map hitSource scan_search'
      liftIO $
        regular_search `shouldBe` Right exampleTweet -- Check that the size restrtiction is being honored
      liftIO $
        scan_search `shouldMatchList` [Just exampleTweet, Just otherTweet]

  describe "index aliases" $ do
    let aname = IndexAliasName (IndexName "bloodhound-tests-twitter-1-alias")
    let alias = IndexAlias (testIndex) aname
    let create = IndexAliasCreate Nothing Nothing
    let action = AddAlias alias create
    it "handles the simple case of aliasing an existing index" $ do
      withTestEnv $ do
        resetIndex
        resp <- updateIndexAliases (action :| [])
        liftIO $ validateStatus resp 200
      let cleanup = withTestEnv (updateIndexAliases (RemoveAlias alias :| []))
      (do aliases <- withTestEnv getIndexAliases
          let expected = IndexAliasSummary alias create
          case aliases of
            Right (IndexAliasesSummary summs) ->
              L.find ((== alias) . indexAliasSummaryAlias) summs `shouldBe` Just expected
            Left e -> expectationFailure ("Expected an IndexAliasesSummary but got " <> show e)) `finally` cleanup
    it "allows alias deletion" $ do
      aliases <- withTestEnv $ do
        resetIndex
        resp <- updateIndexAliases (action :| [])
        liftIO $ validateStatus resp 200
        deleteIndexAlias aname
        getIndexAliases
      let expected = IndexAliasSummary alias create
      case aliases of
        Right (IndexAliasesSummary summs) ->
          L.find ((== aname) . indexAlias . indexAliasSummaryAlias) summs `shouldBe` Nothing
        Left e -> expectationFailure ("Expected an IndexAliasesSummary but got " <> show e)

  describe "Index Listing" $ do
    it "returns a list of index names" $ withTestEnv $ do
      _ <- createExampleIndex
      ixns <- listIndices
      liftIO (ixns `shouldContain` [testIndex])

  describe "Index Settings" $ do
    it "persists settings" $ withTestEnv $ do
      _ <- deleteExampleIndex
      _ <- createExampleIndex
      let updates = BlocksWrite False :| []
      updateResp <- updateIndexSettings updates testIndex
      liftIO $ validateStatus updateResp 200
      getResp <- getIndexSettings testIndex
      liftIO $
        getResp `shouldBe` Right (IndexSettingsSummary
                                    testIndex
                                    (IndexSettings (ShardCount 1) (ReplicaCount 0))
                                    (NE.toList updates))

    it "allows total fields to be set" $ when' (atleast es50) $ withTestEnv $ do
      _ <- deleteExampleIndex
      _ <- createExampleIndex
      let updates = MappingTotalFieldsLimit 2500 :| []
      updateResp <- updateIndexSettings updates testIndex
      liftIO $ validateStatus updateResp 200
      getResp <- getIndexSettings testIndex
      liftIO $
        getResp `shouldBe` Right (IndexSettingsSummary
                                    testIndex
                                    (IndexSettings (ShardCount 1) (ReplicaCount 0))
                                    (NE.toList updates))

    it "accepts customer analyzers" $ when' (atleast es50) $ withTestEnv $ do
      _ <- deleteExampleIndex
      let analysis = Analysis
            (M.singleton "ex_analyzer"
              ( AnalyzerDefinition
                (Just (Tokenizer "ex_tokenizer"))
                (map TokenFilter
                  [ "ex_filter_lowercase","ex_filter_uppercase","ex_filter_apostrophe"
                  , "ex_filter_reverse","ex_filter_snowball"
                  , "ex_filter_shingle"
                  ]
                )
              )
            )
            (M.singleton "ex_tokenizer"
              ( TokenizerDefinitionNgram
                ( Ngram 3 4 [TokenLetter,TokenDigit])
              )
            )
            (M.fromList
              [ ("ex_filter_lowercase",TokenFilterDefinitionLowercase (Just Greek))
              , ("ex_filter_uppercase",TokenFilterDefinitionUppercase Nothing)
              , ("ex_filter_apostrophe",TokenFilterDefinitionApostrophe)
              , ("ex_filter_reverse",TokenFilterDefinitionReverse)
              , ("ex_filter_snowball",TokenFilterDefinitionSnowball English)
              , ("ex_filter_shingle",TokenFilterDefinitionShingle (Shingle 3 3 True False " " "_"))
              ]
            )
          updates = [AnalysisSetting analysis]
      createResp <- createIndexWith (updates ++ [NumberOfReplicas (ReplicaCount 0)]) 1 testIndex
      liftIO $ validateStatus createResp 200
      getResp <- getIndexSettings testIndex
      liftIO $
        getResp `shouldBe` Right (IndexSettingsSummary
                                    testIndex
                                    (IndexSettings (ShardCount 1) (ReplicaCount 0))
                                    updates
                                 )

    it "accepts default compression codec" $ when' (atleast es50) $ withTestEnv $ do
      _ <- deleteExampleIndex
      let updates = [CompressionSetting CompressionDefault]
      createResp <- createIndexWith (updates ++ [NumberOfReplicas (ReplicaCount 0)]) 1 testIndex
      liftIO $ validateStatus createResp 200
      getResp <- getIndexSettings testIndex
      liftIO $ getResp `shouldBe` Right
        (IndexSettingsSummary testIndex (IndexSettings (ShardCount 1) (ReplicaCount 0)) updates)

    it "accepts best compression codec" $ when' (atleast es50) $ withTestEnv $ do
      _ <- deleteExampleIndex
      let updates = [CompressionSetting CompressionBest]
      createResp <- createIndexWith (updates ++ [NumberOfReplicas (ReplicaCount 0)]) 1 testIndex
      liftIO $ validateStatus createResp 200
      getResp <- getIndexSettings testIndex
      liftIO $ getResp `shouldBe` Right
        (IndexSettingsSummary testIndex (IndexSettings (ShardCount 1) (ReplicaCount 0)) updates)


  describe "Index Optimization" $ do
    it "returns a successful response upon completion" $ withTestEnv $ do
      _ <- createExampleIndex
      resp <- forceMergeIndex (IndexList (testIndex :| [])) defaultForceMergeIndexSettings
      liftIO $ validateStatus resp 200

  describe "Suggest" $ do
    it "returns a search suggestion using the phrase suggester" $ withTestEnv $ do
      _ <- insertData
      let query = QueryMatchNoneQuery
          phraseSuggester = mkPhraseSuggester (FieldName "message")
          namedSuggester = Suggest "Use haskel" "suggest_name" (SuggestTypePhraseSuggester phraseSuggester)
          search' = mkSearch (Just query) Nothing
          search = search' { suggestBody = Just namedSuggester }
          expectedText = Just "use haskell"
      resp <- searchByIndex testIndex search
      parsed <- parseEsResponse resp :: BH IO (Either EsError (SearchResult Tweet))
      case parsed of
        Left e -> liftIO $ expectationFailure ("Expected an search suggestion but got " <> show e)
        Right sr -> liftIO $ (suggestOptionsText . head . suggestResponseOptions . head . nsrResponses  <$> suggest sr) `shouldBe` expectedText

  describe "JSON instances" $ do
    propJSON (Proxy :: Proxy Version)
    propJSON (Proxy :: Proxy IndexName)
    propJSON (Proxy :: Proxy MappingName)
    propJSON (Proxy :: Proxy DocId)
    propJSON (Proxy :: Proxy IndexAliasRouting)
    propJSON (Proxy :: Proxy RoutingValue)
    propJSON (Proxy :: Proxy ShardCount)
    propJSON (Proxy :: Proxy ReplicaCount)
    propJSON (Proxy :: Proxy TemplateName)
    propJSON (Proxy :: Proxy TemplatePattern)
    propJSON (Proxy :: Proxy QueryString)
    propJSON (Proxy :: Proxy FieldName)
    propJSON (Proxy :: Proxy CacheName)
    propJSON (Proxy :: Proxy CacheKey)
    propJSON (Proxy :: Proxy Existence)
    propJSON (Proxy :: Proxy CutoffFrequency)
    propJSON (Proxy :: Proxy Analyzer)
    propJSON (Proxy :: Proxy MaxExpansions)
    propJSON (Proxy :: Proxy Lenient)
    propJSON (Proxy :: Proxy Tiebreaker)
    propJSON (Proxy :: Proxy Boost)
    propJSON (Proxy :: Proxy BoostTerms)
    propJSON (Proxy :: Proxy MinimumMatch)
    propJSON (Proxy :: Proxy DisableCoord)
    propJSON (Proxy :: Proxy IgnoreTermFrequency)
    propJSON (Proxy :: Proxy MinimumTermFrequency)
    propJSON (Proxy :: Proxy MaxQueryTerms)
    propJSON (Proxy :: Proxy Fuzziness)
    propJSON (Proxy :: Proxy PrefixLength)
    propJSON (Proxy :: Proxy TypeName)
    propJSON (Proxy :: Proxy PercentMatch)
    propJSON (Proxy :: Proxy StopWord)
    propJSON (Proxy :: Proxy QueryPath)
    propJSON (Proxy :: Proxy AllowLeadingWildcard)
    propJSON (Proxy :: Proxy LowercaseExpanded)
    propJSON (Proxy :: Proxy EnablePositionIncrements)
    propJSON (Proxy :: Proxy AnalyzeWildcard)
    propJSON (Proxy :: Proxy GeneratePhraseQueries)
    propJSON (Proxy :: Proxy Locale)
    propJSON (Proxy :: Proxy MaxWordLength)
    propJSON (Proxy :: Proxy MinWordLength)
    propJSON (Proxy :: Proxy PhraseSlop)
    propJSON (Proxy :: Proxy MinDocFrequency)
    propJSON (Proxy :: Proxy MaxDocFrequency)
    propJSON (Proxy :: Proxy Filter)
    propJSON (Proxy :: Proxy Query)
    propJSON (Proxy :: Proxy SimpleQueryStringQuery)
    propJSON (Proxy :: Proxy FieldOrFields)
    propJSON (Proxy :: Proxy SimpleQueryFlag)
    propJSON (Proxy :: Proxy RegexpQuery)
    propJSON (Proxy :: Proxy QueryStringQuery)
    propJSON (Proxy :: Proxy RangeQuery)
    propJSON (Proxy :: Proxy PrefixQuery)
    propJSON (Proxy :: Proxy NestedQuery)
    propJSON (Proxy :: Proxy MoreLikeThisFieldQuery)
    propJSON (Proxy :: Proxy MoreLikeThisQuery)
    propJSON (Proxy :: Proxy IndicesQuery)
    propJSON (Proxy :: Proxy HasParentQuery)
    propJSON (Proxy :: Proxy HasChildQuery)
    propJSON (Proxy :: Proxy FuzzyQuery)
    propJSON (Proxy :: Proxy FuzzyLikeFieldQuery)
    propJSON (Proxy :: Proxy FuzzyLikeThisQuery)
    propJSON (Proxy :: Proxy DisMaxQuery)
    propJSON (Proxy :: Proxy CommonTermsQuery)
    propJSON (Proxy :: Proxy CommonMinimumMatch)
    propJSON (Proxy :: Proxy BoostingQuery)
    propJSON (Proxy :: Proxy BoolQuery)
    propJSON (Proxy :: Proxy MatchQuery)
    propJSON (Proxy :: Proxy MultiMatchQueryType)
    propJSON (Proxy :: Proxy BooleanOperator)
    propJSON (Proxy :: Proxy ZeroTermsQuery)
    propJSON (Proxy :: Proxy MatchQueryType)
    propJSON (Proxy :: Proxy AliasRouting)
    propJSON (Proxy :: Proxy IndexAliasCreate)
    propJSON (Proxy :: Proxy SearchAliasRouting)
    propJSON (Proxy :: Proxy ScoreType)
    propJSON (Proxy :: Proxy Distance)
    propJSON (Proxy :: Proxy DistanceUnit)
    propJSON (Proxy :: Proxy DistanceType)
    propJSON (Proxy :: Proxy OptimizeBbox)
    propJSON (Proxy :: Proxy GeoBoundingBoxConstraint)
    propJSON (Proxy :: Proxy GeoFilterType)
    propJSON (Proxy :: Proxy GeoBoundingBox)
    propJSON (Proxy :: Proxy LatLon)
    propJSON (Proxy :: Proxy RangeExecution)
    prop "RegexpFlags FromJSON/ToJSON roundtrips, removing dups " $ \rfs ->
      let expected = case rfs of
                       SomeRegexpFlags fs -> SomeRegexpFlags (NE.fromList (nub (NE.toList fs)))
                       x -> x
      in parseEither parseJSON (toJSON rfs) === Right expected
    propJSON (Proxy :: Proxy BoolMatch)
    propJSON (Proxy :: Proxy Term)
    propJSON (Proxy :: Proxy MultiMatchQuery)
    propJSON (Proxy :: Proxy IndexSettings)
    propJSON (Proxy :: Proxy UpdatableIndexSetting')
    propJSON (Proxy :: Proxy ReplicaBounds)
    propJSON (Proxy :: Proxy Bytes)
    propJSON (Proxy :: Proxy AllocationPolicy)
    propJSON (Proxy :: Proxy InitialShardCount)
    propJSON (Proxy :: Proxy FSType)
    propJSON (Proxy :: Proxy CompoundFormat)
    propJSON (Proxy :: Proxy TemplateQueryInline)
    propJSON (Proxy :: Proxy Suggest)
    propJSON (Proxy :: Proxy DirectGenerators)
    propJSON (Proxy :: Proxy DirectGeneratorSuggestModeTypes)

-- Temporary solution for lacking of generic derivation of Arbitrary
-- We use generics-sop, as it's much more concise than directly using GHC.Generics
--
-- This will be unneeded after https://github.com/nick8325/quickcheck/pull/112
-- is merged and released
sopArbitrary :: forall a. (Generic a, SOP.GTo a, SOP.All SOP.SListI (SOP.GCode a), SOP.All2 Arbitrary (SOP.GCode a)) => Gen a
sopArbitrary = fmap SOP.gto sopArbitrary'

sopArbitrary' :: forall xss. (SOP.All SOP.SListI xss, SOP.All2 Arbitrary xss) => Gen (SOP.SOP SOP.I xss)
sopArbitrary' = SOP.hsequence =<< elements (SOP.apInjs_POP $ SOP.hcpure p arbitrary)
  where
    p :: Proxy Arbitrary
    p = Proxy
