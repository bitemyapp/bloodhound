{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Test.JSONSpec (spec) where

import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import TestsUtils.ApproxEq
import TestsUtils.Generators
import TestsUtils.Import

propJSON ::
  forall a.
  ( Arbitrary a,
    ToJSON a,
    FromJSON a,
    Show a,
    Eq a,
    Typeable a
  ) =>
  Proxy a ->
  Spec
propJSON _ = prop testName $ \(a :: a) ->
  let jsonStr = "via " <> BL8.unpack (encode a)
   in counterexample
        jsonStr
        ( parseEither parseJSON (toJSON a)
            === Right a
        )
  where
    testName = show ty <> " FromJSON/ToJSON roundtrips"
    ty = typeOf (undefined :: a)

propApproxJSON ::
  forall a.
  ( Arbitrary a,
    ToJSON a,
    FromJSON a,
    Show a,
    ApproxEq a,
    Typeable a
  ) =>
  Proxy a ->
  Spec
propApproxJSON _ = prop testName $ \(a :: a) ->
  let jsonStr = "via " <> BL8.unpack (encode a)
   in counterexample
        jsonStr
        ( parseEither parseJSON (toJSON a)
            ==~ Right a
        )
  where
    testName = show ty <> " FromJSON/ToJSON roundtrips"
    ty = typeOf (undefined :: a)

spec :: Spec
spec = do
  describe "ToJSON RegexpFlags" $ do
    it "generates the correct JSON for AllRegexpFlags" $
      toJSON AllRegexpFlags `shouldBe` String "ALL"

    it "generates the correct JSON for NoRegexpFlags" $
      toJSON NoRegexpFlags `shouldBe` String "NONE"

    it "generates the correct JSON for SomeRegexpFlags" $
      let flags =
            AnyString
              :| [ Automaton,
                   Complement,
                   Empty,
                   Intersection,
                   Interval
                 ]
       in toJSON (SomeRegexpFlags flags) `shouldBe` String "ANYSTRING|AUTOMATON|COMPLEMENT|EMPTY|INTERSECTION|INTERVAL"

    prop "removes duplicates from flags" $ \(flags :: RegexpFlags) ->
      let String str = toJSON flags
          flagStrs = T.splitOn "|" str
       in noDuplicates flagStrs

  describe "FromJSON EsError" $ do
    it "should parse ElasticSearch 7 responses (status & error)" $ do
      let mbJson = decode "{\"status\": 209, \"error\": \"Error message\"}"
      mbJson `shouldBe` Just (EsError (Just 209) "Error message")

    it "should parse ElasticSearch 7 responses (error.reason)" $ do
      let mbJson = decode "{\"error\": {\"reason\": \"Error message\"}}"
      mbJson `shouldBe` Just (EsError Nothing "Error message")

    it "should parse OpenSearch 1.3 responses" $ do
      -- This is a real error response for a version conflict
      let mbJson = decode "{\"took\":9,\"timed_out\":false,\"total\":3,\"updated\":1,\"deleted\":0,\"batches\":1,\"version_conflicts\":2,\"noops\":0,\"retries\":{\"bulk\":0,\"search\":0},\"throttled_millis\":0,\"requests_per_second\":-1.0,\"throttled_until_millis\":0,\"failures\":[{\"index\":\"directory_test\",\"type\":\"_doc\",\"id\":\"9fda4188-2afd-490d-8796-e023df61a4e9\",\"cause\":{\"type\":\"version_conflict_engine_exception\",\"reason\":\"[9fda4188-2afd-490d-8796-e023df61a4e9]: version conflict, required seqNo [11], primary term [1]. current document has seqNo [16] and primary term [1]\",\"index\":\"directory_test\",\"shard\":\"0\",\"index_uuid\":\"Y3RpVY_DQEW9ULn8oGulrg\"},\"status\":409},{\"index\":\"directory_test\",\"type\":\"_doc\",\"id\":\"d70b631d-966a-4951-a94c-35ddc210f28a\",\"cause\":{\"type\":\"version_conflict_engine_exception\",\"reason\":\"[d70b631d-966a-4951-a94c-35ddc210f28a]: version conflict, required seqNo [13], primary term [1]. current document has seqNo [15] and primary term [1]\",\"index\":\"directory_test\",\"shard\":\"0\",\"index_uuid\":\"Y3RpVY_DQEW9ULn8oGulrg\"},\"status\":409}]}"
      mbJson `shouldBe` Just (EsError (Just 409) "[9fda4188-2afd-490d-8796-e023df61a4e9]: version conflict, required seqNo [11], primary term [1]. current document has seqNo [16] and primary term [1]")

  describe "Exact isomorphism JSON instances" $ do
    propJSON (Proxy :: Proxy IndexName)
    propJSON (Proxy :: Proxy DocId)
    propJSON (Proxy :: Proxy IndexAliasRouting)
    propJSON (Proxy :: Proxy RoutingValue)
    propJSON (Proxy :: Proxy ShardCount)
    propJSON (Proxy :: Proxy ReplicaCount)
    propJSON (Proxy :: Proxy TemplateName)
    propJSON (Proxy :: Proxy IndexPattern)
    propJSON (Proxy :: Proxy QueryString)
    propJSON (Proxy :: Proxy FieldName)
    propJSON (Proxy :: Proxy Script)
    propJSON (Proxy :: Proxy ScriptLanguage)
    propJSON (Proxy :: Proxy ScriptParams)
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
    propJSON (Proxy :: Proxy RelationName)
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
    propJSON (Proxy :: Proxy IgnoreUnmapped)
    propJSON (Proxy :: Proxy MinChildren)
    propJSON (Proxy :: Proxy MaxChildren)
    propJSON (Proxy :: Proxy AggregateParentScore)
    propJSON (Proxy :: Proxy HasParentQuery)
    propJSON (Proxy :: Proxy HasChildQuery)
    propJSON (Proxy :: Proxy FuzzyQuery)
    propJSON (Proxy :: Proxy FuzzyLikeFieldQuery)
    propJSON (Proxy :: Proxy FuzzyLikeThisQuery)
    propJSON (Proxy :: Proxy FunctionScoreQuery)
    propJSON (Proxy :: Proxy BoostMode)
    propJSON (Proxy :: Proxy ScoreMode)
    propJSON (Proxy :: Proxy ComponentFunctionScoreFunction)
    propJSON (Proxy :: Proxy FieldValueFactor)
    propJSON (Proxy :: Proxy FactorModifier)
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
            SomeRegexpFlags fs -> SomeRegexpFlags (NE.fromList (L.nub (NE.toList fs)))
            x -> x
       in parseEither parseJSON (toJSON rfs) === Right expected
    propJSON (Proxy :: Proxy BoolMatch)
    propJSON (Proxy :: Proxy Term)
    propJSON (Proxy :: Proxy MultiMatchQuery)
    -- propJSON (Proxy :: Proxy IndexSettings)
    propJSON (Proxy :: Proxy CompoundFormat)
    propJSON (Proxy :: Proxy Suggest)
    propJSON (Proxy :: Proxy DirectGenerators)
    propJSON (Proxy :: Proxy DirectGeneratorSuggestModeTypes)

  describe "Approximate isomorphism JSON instances" $ do
    propApproxJSON (Proxy :: Proxy UpdatableIndexSetting')
    propApproxJSON (Proxy :: Proxy ReplicaCount)
    propApproxJSON (Proxy :: Proxy ReplicaBounds)
    propApproxJSON (Proxy :: Proxy Bytes)
    propApproxJSON (Proxy :: Proxy AllocationPolicy)
    propApproxJSON (Proxy :: Proxy InitialShardCount)
    propApproxJSON (Proxy :: Proxy FSType)
