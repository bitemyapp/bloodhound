{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import           Control.Applicative
import           Control.Error
import           Control.Exception
import           Control.Monad
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Aeson.Types                (parseEither)
import qualified Data.ByteString.Lazy.Char8      as BL8
import           Data.DeriveTH
import qualified Data.HashMap.Strict             as HM
import           Data.List                       (nub)
import qualified Data.List                       as L
import           Data.List.NonEmpty              (NonEmpty (..))
import qualified Data.List.NonEmpty              as NE
import qualified Data.Map.Strict                 as M
import           Data.Monoid
import           Data.Proxy
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Data.Time.Calendar              (Day (..), fromGregorian)
import           Data.Time.Clock                 (NominalDiffTime, UTCTime (..),
                                                  secondsToDiffTime)
import           Data.Typeable
import qualified Data.Vector                     as V
import           Database.Bloodhound
import           GHC.Generics                    as G
import           Network.HTTP.Client             hiding (Proxy)
import qualified Network.HTTP.Types.Status       as NHTS
import           Prelude                         hiding (filter)
import           Test.Hspec
import           Test.QuickCheck.Property.Monoid (T (..), eq, prop_Monoid)

import           Test.Hspec.QuickCheck           (prop)
import           Test.QuickCheck

testServer  :: Server
testServer  = Server "http://localhost:9200"
testIndex   :: IndexName
testIndex   = IndexName "bloodhound-tests-twitter-1"
testMapping :: MappingName
testMapping = MappingName "tweet"

withTestEnv :: BH IO a -> IO a
withTestEnv = withBH defaultManagerSettings testServer

validateStatus :: Response body -> Int -> Expectation
validateStatus resp expected =
  (NHTS.statusCode $ responseStatus resp)
  `shouldBe` (expected :: Int)

createExampleIndex :: BH IO Reply
createExampleIndex = createIndex (IndexSettings (ShardCount 1) (ReplicaCount 0)) testIndex
deleteExampleIndex :: BH IO Reply
deleteExampleIndex = deleteIndex testIndex

data ServerVersion = ServerVersion Int Int Int deriving (Show, Eq, Ord)

es13 :: ServerVersion
es13 = ServerVersion 1 3 0

es12 :: ServerVersion
es12 = ServerVersion 1 2 0

es11 :: ServerVersion
es11 = ServerVersion 1 1 0

serverBranch :: ServerVersion -> ServerVersion
serverBranch (ServerVersion majorVer minorVer patchVer) =
  ServerVersion majorVer minorVer patchVer

mkServerVersion :: [Int] -> Maybe ServerVersion
mkServerVersion [majorVer, minorVer, patchVer] =
  Just (ServerVersion majorVer minorVer patchVer)
mkServerVersion _                 = Nothing

getServerVersion :: IO (Maybe ServerVersion)
getServerVersion = liftM extractVersion (withTestEnv getStatus)
  where
    version'                    = T.splitOn "." . number . version
    toInt                       = read . T.unpack
    parseVersion v              = map toInt (version' v)
    extractVersion              = join . liftM (mkServerVersion . parseVersion)

testServerBranch :: IO (Maybe ServerVersion)
testServerBranch = getServerVersion >>= \v -> return $ liftM serverBranch v

atleast :: ServerVersion -> IO Bool
atleast v = testServerBranch >>= \x -> return $ x >= Just (serverBranch v)

atmost :: ServerVersion -> IO Bool
atmost v = testServerBranch >>= \x -> return $ x <= Just (serverBranch v)

is :: ServerVersion -> IO Bool
is v = testServerBranch >>= \x -> return $ x == Just (serverBranch v)

when' :: Monad m => m Bool -> m () -> m ()
when' b f = b >>= \x -> when x f

(==~) :: (ApproxEq a, Show a) => a -> a -> Property
a ==~ b = counterexample (show a ++ " !=~ " ++ show b) (a =~ b)

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
    object ["parent" .= Null ]

data ChildMapping = ChildMapping deriving (Eq, Show)

instance ToJSON ChildMapping where
  toJSON ChildMapping =
    object ["child" .=
      object ["_parent" .= object ["type" .= ("parent" :: Text)]]
    ]

data TweetMapping = TweetMapping deriving (Eq, Show)

instance ToJSON TweetMapping where
  toJSON TweetMapping =
    object ["tweet" .=
      object ["properties" .=
        object [ "user"     .= object ["type"    .= ("string" :: Text)]
               -- Serializing the date as a date is breaking other tests, mysteriously.
               -- , "postDate" .= object [ "type"   .= ("date" :: Text)
               --                        , "format" .= ("YYYY-MM-dd`T`HH:mm:ss.SSSZZ" :: Text)]
               , "message"  .= object ["type" .= ("string" :: Text)]
               , "age"      .= object ["type" .= ("integer" :: Text)]
               , "location" .= object ["type" .= ("geo_point" :: Text)]
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

instance ApproxEq NominalDiffTime where (=~) = (==)
instance ApproxEq UTCTime where (=~) = (==)
instance ApproxEq Text where (=~) = (==)
instance ApproxEq Bool where (=~) = (==)
instance ApproxEq Int where (=~) = (==)
instance ApproxEq Double where (=~) = (==)
instance ApproxEq a => ApproxEq (NonEmpty a)
instance ApproxEq a => ApproxEq (Maybe a)
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
instance ApproxEq FilteredQuery
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
instance ApproxEq Char
instance ApproxEq a => ApproxEq [a] where
  as =~ bs = and (zipWith (=~) as bs)
instance (ApproxEq l, ApproxEq r) => ApproxEq (Either l r) where
  Left a =~ Left b = a =~ b
  Right a =~ Right b = a =~ b
  _ =~ _ = False
instance ApproxEq NodeAttrFilter
instance ApproxEq NodeAttrName

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


noDuplicates :: Eq a => [a] -> Bool
noDuplicates xs = nub xs == xs

instance Arbitrary NominalDiffTime where
  arbitrary = fromInteger <$> arbitrary

instance (Arbitrary k, Ord k, Arbitrary v) => Arbitrary (M.Map k v) where
  arbitrary = M.fromList <$> arbitrary

instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary UTCTime where
  arbitrary = UTCTime
          <$> arbitrary
          <*> (fromRational . toRational <$> choose (0::Double, 86400))

instance Arbitrary Day where
    arbitrary = ModifiedJulianDay <$> (2000 +) <$> arbitrary
    shrink    = (ModifiedJulianDay <$>) . shrink . toModifiedJulianDay

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = liftA2 (:|) arbitrary arbitrary

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
    (Left e) -> Left e
    (Right Nothing) -> Left (EsError 500 "Source was missing")
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
                                 , ConstantScoreFilter <$> arbitrary <*> arbitrary
                                 , ConstantScoreQuery <$> arbitrary <*> arbitrary
                                 , QueryDisMaxQuery <$> arbitrary
                                 , QueryFilteredQuery <$> arbitrary
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
                                 ]
  shrink = genericShrink

instance Arbitrary Filter where
  arbitrary = reduceSize $ oneof [ AndFilter <$> arbitrary <*> arbitrary
                                 , OrFilter <$> arbitrary <*> arbitrary
                                 , NotFilter <$> arbitrary <*> arbitrary
                                 , pure IdentityFilter
                                 , BoolFilter <$> arbitrary
                                 , ExistsFilter <$> arbitrary
                                 , GeoBoundingBoxFilter <$> arbitrary
                                 , GeoDistanceFilter <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                                 , GeoDistanceRangeFilter <$> arbitrary <*> arbitrary
                                 , GeoPolygonFilter <$> arbitrary <*> arbitrary
                                 , IdsFilter <$> arbitrary <*> arbitrary
                                 , LimitFilter <$> arbitrary
                                 , MissingFilter <$> arbitrary <*> arbitrary <*> arbitrary
                                 , PrefixFilter <$> arbitrary <*> arbitrary <*> arbitrary
                                 , QueryFilter <$> arbitrary <*> arbitrary
                                 , RangeFilter <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                                 , RegexpFilter <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                                 , TermFilter <$> arbitrary <*> arbitrary]
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
  arbitrary = NodeAttrName . T.pack . getNonEmpty <$> arbitrary


instance Arbitrary NodeAttrFilter where
  arbitrary = do
    n <- arbitrary
    s:ss <- listOf1 (listOf1 arbitraryAlphaNum)
    let ts = T.pack <$> s :| ss
    return (NodeAttrFilter n ts)

$(derive makeArbitrary ''IndexName)
$(derive makeArbitrary ''MappingName)
$(derive makeArbitrary ''DocId)
$(derive makeArbitrary ''Version)
$(derive makeArbitrary ''IndexAliasRouting)
$(derive makeArbitrary ''ShardCount)
$(derive makeArbitrary ''ReplicaCount)
$(derive makeArbitrary ''TemplateName)
$(derive makeArbitrary ''TemplatePattern)
$(derive makeArbitrary ''QueryString)
$(derive makeArbitrary ''CacheName)
$(derive makeArbitrary ''CacheKey)
$(derive makeArbitrary ''Existence)
$(derive makeArbitrary ''CutoffFrequency)
$(derive makeArbitrary ''Analyzer)
$(derive makeArbitrary ''MaxExpansions)
$(derive makeArbitrary ''Lenient)
$(derive makeArbitrary ''Tiebreaker)
$(derive makeArbitrary ''Boost)
$(derive makeArbitrary ''BoostTerms)
$(derive makeArbitrary ''MinimumMatch)
$(derive makeArbitrary ''DisableCoord)
$(derive makeArbitrary ''IgnoreTermFrequency)
$(derive makeArbitrary ''MinimumTermFrequency)
$(derive makeArbitrary ''MaxQueryTerms)
$(derive makeArbitrary ''Fuzziness)
$(derive makeArbitrary ''PrefixLength)
$(derive makeArbitrary ''TypeName)
$(derive makeArbitrary ''PercentMatch)
$(derive makeArbitrary ''StopWord)
$(derive makeArbitrary ''QueryPath)
$(derive makeArbitrary ''AllowLeadingWildcard)
$(derive makeArbitrary ''LowercaseExpanded)
$(derive makeArbitrary ''EnablePositionIncrements)
$(derive makeArbitrary ''AnalyzeWildcard)
$(derive makeArbitrary ''GeneratePhraseQueries)
$(derive makeArbitrary ''Locale)
$(derive makeArbitrary ''MaxWordLength)
$(derive makeArbitrary ''MinWordLength)
$(derive makeArbitrary ''PhraseSlop)
$(derive makeArbitrary ''MinDocFrequency)
$(derive makeArbitrary ''MaxDocFrequency)
$(derive makeArbitrary ''Regexp)
$(derive makeArbitrary ''SimpleQueryStringQuery)
$(derive makeArbitrary ''FieldOrFields)
$(derive makeArbitrary ''SimpleQueryFlag)
$(derive makeArbitrary ''RegexpQuery)
$(derive makeArbitrary ''QueryStringQuery)
$(derive makeArbitrary ''RangeQuery)
$(derive makeArbitrary ''RangeValue)
$(derive makeArbitrary ''PrefixQuery)
$(derive makeArbitrary ''NestedQuery)
$(derive makeArbitrary ''MoreLikeThisFieldQuery)
$(derive makeArbitrary ''MoreLikeThisQuery)
$(derive makeArbitrary ''IndicesQuery)
$(derive makeArbitrary ''HasParentQuery)
$(derive makeArbitrary ''HasChildQuery)
$(derive makeArbitrary ''FuzzyQuery)
$(derive makeArbitrary ''FuzzyLikeFieldQuery)
$(derive makeArbitrary ''FuzzyLikeThisQuery)
$(derive makeArbitrary ''FilteredQuery)
$(derive makeArbitrary ''DisMaxQuery)
$(derive makeArbitrary ''CommonTermsQuery)
$(derive makeArbitrary ''DistanceRange)
$(derive makeArbitrary ''MultiMatchQuery)
$(derive makeArbitrary ''LessThanD)
$(derive makeArbitrary ''LessThanEqD)
$(derive makeArbitrary ''GreaterThanD)
$(derive makeArbitrary ''GreaterThanEqD)
$(derive makeArbitrary ''LessThan)
$(derive makeArbitrary ''LessThanEq)
$(derive makeArbitrary ''GreaterThan)
$(derive makeArbitrary ''GreaterThanEq)
$(derive makeArbitrary ''GeoPoint)
$(derive makeArbitrary ''NullValue)
$(derive makeArbitrary ''MinimumMatchHighLow)
$(derive makeArbitrary ''CommonMinimumMatch)
$(derive makeArbitrary ''BoostingQuery)
$(derive makeArbitrary ''BoolQuery)
$(derive makeArbitrary ''MatchQuery)
$(derive makeArbitrary ''MultiMatchQueryType)
$(derive makeArbitrary ''BooleanOperator)
$(derive makeArbitrary ''ZeroTermsQuery)
$(derive makeArbitrary ''MatchQueryType)
$(derive makeArbitrary ''SearchAliasRouting)
$(derive makeArbitrary ''ScoreType)
$(derive makeArbitrary ''Distance)
$(derive makeArbitrary ''DistanceUnit)
$(derive makeArbitrary ''DistanceType)
$(derive makeArbitrary ''OptimizeBbox)
$(derive makeArbitrary ''GeoBoundingBoxConstraint)
$(derive makeArbitrary ''GeoFilterType)
$(derive makeArbitrary ''GeoBoundingBox)
$(derive makeArbitrary ''LatLon)
$(derive makeArbitrary ''RangeExecution)
$(derive makeArbitrary ''RegexpFlag)
$(derive makeArbitrary ''BoolMatch)
$(derive makeArbitrary ''Term)
$(derive makeArbitrary ''IndexSettings)
$(derive makeArbitrary ''UpdatableIndexSetting)
$(derive makeArbitrary ''Bytes)
$(derive makeArbitrary ''AllocationPolicy)
$(derive makeArbitrary ''InitialShardCount)
$(derive makeArbitrary ''FSType)
$(derive makeArbitrary ''CompoundFormat)


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

  describe "error parsing" $ do
    it "can parse EsErrors" $ withTestEnv $ do
      res <- getDocument (IndexName "bogus") (MappingName "also_bogus") (DocId "bogus_as_well")
      let errorResp = eitherDecode (responseBody res)
      liftIO (errorResp `shouldBe` Right (EsError 404 "IndexMissingException[[bogus] missing]"))

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
      _ <- putMapping testIndex (MappingName "parent") ParentMapping
      _ <- putMapping testIndex (MappingName "child") ChildMapping
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
      let firstDoc = BulkIndex testIndex
                     testMapping (DocId "2") (toJSON firstTest)
      let secondDoc = BulkCreate testIndex
                     testMapping (DocId "3") (toJSON secondTest)
      let stream = V.fromList [firstDoc, secondDoc]
      _ <- bulk stream
      _ <- refreshIndex testIndex
      fDoc <- getDocument testIndex testMapping (DocId "2")
      sDoc <- getDocument testIndex testMapping (DocId "3")
      let maybeFirst  = eitherDecode $ responseBody fDoc :: Either String (EsResult BulkTest)
      let maybeSecond = eitherDecode $ responseBody sDoc :: Either String (EsResult BulkTest)
      liftIO $ do
        fmap getSource maybeFirst `shouldBe` Right (Just firstTest)
        fmap getSource maybeSecond `shouldBe` Right (Just secondTest)


  describe "query API" $ do
    it "returns document for term query and identity filter" $ withTestEnv $ do
      _ <- insertData
      let query = TermQuery (Term "user" "bitemyapp") Nothing
      let filter = IdentityFilter <&&> IdentityFilter
      let search = mkSearch (Just query) (Just filter)
      myTweet <- searchTweet search
      liftIO $
        myTweet `shouldBe` Right exampleTweet

    it "handles constant score queries" $ withTestEnv $ do
      _ <- insertData
      let query = TermsQuery "user" ("bitemyapp" :| [])
      let cfQuery = ConstantScoreQuery query (Boost 1.0)
      let filter = IdentityFilter
      let search = mkSearch (Just cfQuery) (Just filter)
      myTweet <- searchTweet search
      liftIO $
        myTweet `shouldBe` Right exampleTweet
    it "handles constant score filters" $ withTestEnv $ do
      _ <- insertData
      let query = TermsQuery "user" ("bitemyapp" :| [])
      let cfFilter = ConstantScoreFilter IdentityFilter (Boost 1.0)
      let boolQuery = mkBoolQuery [query, cfFilter] [] []
      let search = mkSearch (Just (QueryBoolQuery boolQuery)) Nothing
      myTweet <- searchTweet search
      liftIO $
        myTweet `shouldBe` Right exampleTweet


    it "returns document for terms query and identity filter" $ withTestEnv $ do
      _ <- insertData
      let query = TermsQuery "user" ("bitemyapp" :| [])
      let filter = IdentityFilter <&&> IdentityFilter
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

    it "returns document for bool query" $ withTestEnv $ do
      _ <- insertData
      let innerQuery = QueryMatchQuery $
                       mkMatchQuery (FieldName "user") (QueryString "bitemyapp")
      let query = QueryBoolQuery $
                  mkBoolQuery [innerQuery] [] []
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


  describe "sorting" $ do
    it "returns documents in the right order" $ withTestEnv $ do
      _ <- insertData
      _ <- insertOther
      let sortSpec = DefaultSortSpec $ mkSort (FieldName "age") Ascending
      let search = Search Nothing
                   (Just IdentityFilter) (Just [sortSpec]) Nothing Nothing
                   False (From 0) (Size 10) SearchTypeQueryThenFetch Nothing Nothing
      result <- searchTweets search
      let myTweet = grabFirst result
      liftIO $
        myTweet `shouldBe` Right otherTweet


  describe "filtering API" $ do
    it "returns document for composed boolmatch and identity" $ withTestEnv $ do
      _ <- insertData
      let queryFilter = BoolFilter (MustMatch (Term "user" "bitemyapp") False)
                        <&&> IdentityFilter
      let search = mkSearch Nothing (Just queryFilter)
      myTweet <- searchTweet search
      liftIO $
        myTweet `shouldBe` Right exampleTweet

    it "returns document for term filter" $ withTestEnv $ do
      _ <- insertData
      let termFilter = TermFilter (Term "user" "bitemyapp") False
      let search = mkSearch Nothing (Just termFilter)
      myTweet <- searchTweet search
      liftIO $
        myTweet `shouldBe` Right exampleTweet

    it "returns document for existential filter" $ withTestEnv $ do
      _ <- insertData
      let search = mkSearch Nothing (Just (ExistsFilter (FieldName "user")))
      myTweet <- searchTweet search
      liftIO $
        myTweet `shouldBe` Right exampleTweet

    it "returns document for geo boundingbox filter" $ withTestEnv $ do
      _ <- insertData
      let box = GeoBoundingBox (LatLon 40.73 (-74.1)) (LatLon 40.10 (-71.12))
      let bbConstraint = GeoBoundingBoxConstraint (FieldName "tweet.location") box False GeoFilterMemory
      let geoFilter = GeoBoundingBoxFilter bbConstraint
      let search = mkSearch Nothing (Just geoFilter)
      myTweet <- searchTweet search
      liftIO $
        myTweet `shouldBe` Right exampleTweet

    it "doesn't return document for nonsensical boundingbox filter" $ withTestEnv $ do
      _ <- insertData
      let box          = GeoBoundingBox (LatLon 0.73 (-4.1)) (LatLon 0.10 (-1.12))
      let bbConstraint = GeoBoundingBoxConstraint (FieldName "tweet.location") box False GeoFilterMemory
      let geoFilter    = GeoBoundingBoxFilter bbConstraint
      let search       = mkSearch Nothing (Just geoFilter)
      searchExpectNoResults search

    it "returns document for geo distance filter" $ withTestEnv $ do
      _ <- insertData
      let geoPoint = GeoPoint (FieldName "tweet.location") (LatLon 40.12 (-71.34))
      let distance = Distance 10.0 Miles
      let optimizeBbox = OptimizeGeoFilterType GeoFilterMemory
      let geoFilter = GeoDistanceFilter geoPoint distance SloppyArc optimizeBbox False
      let search = mkSearch Nothing (Just geoFilter)
      myTweet <- searchTweet search
      liftIO $
        myTweet `shouldBe` Right exampleTweet

    it "returns document for geo distance range filter" $ withTestEnv $ do
      _ <- insertData
      let geoPoint = GeoPoint (FieldName "tweet.location") (LatLon 40.12 (-71.34))
      let distanceRange = DistanceRange (Distance 0.0 Miles) (Distance 10.0 Miles)
      let geoFilter = GeoDistanceRangeFilter geoPoint distanceRange
      let search = mkSearch Nothing (Just geoFilter)
      myTweet <- searchTweet search
      liftIO $
        myTweet `shouldBe` Right exampleTweet

    it "doesn't return document for wild geo distance range filter" $ withTestEnv $ do
      _ <- insertData
      let geoPoint = GeoPoint (FieldName "tweet.location") (LatLon 40.12 (-71.34))
      let distanceRange = DistanceRange (Distance 100.0 Miles) (Distance 1000.0 Miles)
      let geoFilter = GeoDistanceRangeFilter geoPoint distanceRange
      let search = mkSearch Nothing (Just geoFilter)
      searchExpectNoResults search

    it "returns document for geo polygon filter" $ withTestEnv $ do
      _ <- insertData
      let points = [LatLon 40.0 (-70.00),
                    LatLon 40.0 (-72.00),
                    LatLon 41.0 (-70.00),
                    LatLon 41.0 (-72.00)]
      let geoFilter = GeoPolygonFilter (FieldName "tweet.location") points
      let search = mkSearch Nothing (Just geoFilter)
      myTweet <- searchTweet search
      liftIO $
        myTweet `shouldBe` Right exampleTweet

    it "doesn't return document for bad geo polygon filter" $ withTestEnv $ do
      _ <- insertData
      let points = [LatLon 40.0 (-70.00),
                    LatLon 40.0 (-71.00),
                    LatLon 41.0 (-70.00),
                    LatLon 41.0 (-71.00)]
      let geoFilter = GeoPolygonFilter (FieldName "tweet.location") points
      let search = mkSearch Nothing (Just geoFilter)
      searchExpectNoResults search

    it "returns document for ids filter" $ withTestEnv $ do
      _ <- insertData
      let filter = IdsFilter (MappingName "tweet") [DocId "1"]
      let search = mkSearch Nothing (Just filter)
      myTweet <- searchTweet search
      liftIO $
        myTweet `shouldBe` Right exampleTweet

    it "returns document for Double range filter" $ withTestEnv $ do
      _ <- insertData
      let filter = RangeFilter (FieldName "age")
                   (RangeDoubleGtLt (GreaterThan 1000.0) (LessThan 100000.0))
                   RangeExecutionIndex False
      let search = mkSearch Nothing (Just filter)
      myTweet <- searchTweet search
      liftIO $
        myTweet `shouldBe` Right exampleTweet

    it "returns document for UTCTime date filter" $ withTestEnv $ do
      _ <- insertData
      let filter = RangeFilter (FieldName "postDate")
                   (RangeDateGtLt
                    (GreaterThanD (UTCTime
                                (ModifiedJulianDay 54000)
                                (secondsToDiffTime 0)))
                    (LessThanD (UTCTime
                                (ModifiedJulianDay 55000)
                                (secondsToDiffTime 11))))
                   RangeExecutionIndex False
      let search = mkSearch Nothing (Just filter)
      myTweet <- searchTweet search
      liftIO $
        myTweet `shouldBe` Right exampleTweet

    it "returns document for regexp filter" $ withTestEnv $ do
      _ <- insertData
      let filter = RegexpFilter (FieldName "user") (Regexp "bite.*app")
                   AllRegexpFlags (CacheName "test") False (CacheKey "key")
      let search = mkSearch Nothing (Just filter)
      myTweet <- searchTweet search
      liftIO $
        myTweet `shouldBe` Right exampleTweet

    it "doesn't return document for non-matching regexp filter" $ withTestEnv $ do
      _ <- insertData
      let filter = RegexpFilter (FieldName "user")
                   (Regexp "boy") AllRegexpFlags
                   (CacheName "test") False (CacheKey "key")
      let search = mkSearch Nothing (Just filter)
      searchExpectNoResults search

    it "returns document for query filter, uncached" $ withTestEnv $ do
      _ <- insertData
      let filter = QueryFilter (TermQuery (Term "user" "bitemyapp") Nothing) True
          search = mkSearch Nothing (Just filter)
      myTweet <- searchTweet search
      liftIO $ myTweet `shouldBe` Right exampleTweet

    it "returns document for query filter, cached" $ withTestEnv $ do
      _ <- insertData
      let filter = QueryFilter (TermQuery (Term "user" "bitemyapp") Nothing) False
          search = mkSearch Nothing (Just filter)
      myTweet <- searchTweet search
      liftIO $ myTweet `shouldBe` Right exampleTweet

  describe "Aggregation API" $ do
    it "returns term aggregation results" $ withTestEnv $ do
      _ <- insertData
      let terms = TermsAgg $ mkTermsAggregation "user"
      let search = mkAggregateSearch Nothing $ mkAggregations "users" terms
      searchExpectAggs search
      searchValidBucketAgg search "users" toTerms

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

    it "can execute filter aggregations" $ withTestEnv $ do
      _ <- insertData
      _ <- insertOther
      let ags = mkAggregations "bitemyapps" (FilterAgg (FilterAggregation (TermFilter (Term "user" "bitemyapp") defaultCache) Nothing)) <>
                mkAggregations "notmyapps" (FilterAgg (FilterAggregation (TermFilter (Term "user" "notmyapp") defaultCache) Nothing))
      let search = mkAggregateSearch Nothing ags
      let docCountPair k n = (k, object ["doc_count" .= Number n])
      res <- searchTweets search
      liftIO $
        fmap aggregations res `shouldBe` Right (Just (M.fromList [ docCountPair "bitemyapps" 1
                                                                 , docCountPair "notmyapps" 1
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

    it "returns date histogram using fractional date" $ withTestEnv $ do
      _ <- insertData
      let periods            = [Year, Quarter, Month, Week, Day, Hour, Minute, Second]
      let fractionals        = map (FractionalInterval 1.5) [Weeks, Days, Hours, Minutes, Seconds]
      let intervals          = periods ++ fractionals
      let histogram          = mkDateHistogram (FieldName "postDate")
      let search interval    = mkAggregateSearch Nothing $ mkAggregations "byDate" $ DateHistogramAgg (histogram interval)
      let expect interval    = searchExpectAggs (search interval)
      let valid interval     = searchValidBucketAgg (search interval) "byDate" toDateHistogram
      forM_ intervals expect
      forM_ intervals valid

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
      let query = QueryMatchQuery $ mkMatchQuery (FieldName "_all") (QueryString "haskell")
      let testHighlight = Highlights Nothing [FieldHighlight (FieldName "message") Nothing]

      let search = mkHighlightSearch (Just query) testHighlight
      myHighlight <- searchTweetHighlight search
      liftIO $
        myHighlight `shouldBe` Right (Just (M.fromList [("message",["Use <em>haskell</em>!"])]))

    it "doesn't return highlight from a query when it shouldn't" $ withTestEnv $ do
      _ <- insertData
      _ <- insertOther
      let query = QueryMatchQuery $ mkMatchQuery (FieldName "_all") (QueryString "haskell")
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
    it "handles the simple case of aliasing an existing index" $ do
      let alias = IndexAlias (testIndex) (IndexAliasName (IndexName "bloodhound-tests-twitter-1-alias"))
      let create = IndexAliasCreate Nothing Nothing
      let action = AddAlias alias create

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

    it "handles an alias with routing and a filter" $ do
      let alias = IndexAlias (testIndex) (IndexAliasName (IndexName "bloodhound-tests-twitter-1-alias"))
      let sar = SearchAliasRouting (RoutingValue "search val" :| [])
      let iar = IndexAliasRouting (RoutingValue "index val")
      let routing = GranularAliasRouting (Just sar) (Just iar)
      let filter = LimitFilter 42
      let create = IndexAliasCreate (Just routing) (Just filter)
      let action = AddAlias alias create

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

  describe "Index Optimization" $ do
    it "returns a successful response upon completion" $ withTestEnv $ do
      _ <- createExampleIndex
      resp <- optimizeIndex (IndexList (testIndex :| [])) defaultIndexOptimizationSettings
      liftIO $ validateStatus resp 200

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
    propJSON (Proxy :: Proxy FilteredQuery)
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
    propJSON (Proxy :: Proxy UpdatableIndexSetting)
    propJSON (Proxy :: Proxy ReplicaBounds)
    propJSON (Proxy :: Proxy Bytes)
    propJSON (Proxy :: Proxy AllocationPolicy)
    propJSON (Proxy :: Proxy InitialShardCount)
    propJSON (Proxy :: Proxy FSType)
    propJSON (Proxy :: Proxy CompoundFormat)
