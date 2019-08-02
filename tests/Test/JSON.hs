{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.JSON (spec) where

import Test.Import

import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.List as L
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V

import Test.ApproxEq
import Test.Generators

propJSON :: forall a
          . ( Arbitrary a
            , ToJSON a
            , FromJSON a
            , Show a
            , Eq a
            , Typeable a
            )
         => Proxy a -> Spec
propJSON _ = prop testName $ \(a :: a) ->
  let jsonStr = "via " <> BL8.unpack (encode a)
  in counterexample jsonStr (parseEither parseJSON (toJSON a)
                             === Right a)
  where testName = show ty <> " FromJSON/ToJSON roundtrips"
        ty = typeOf (undefined :: a)

propApproxJSON :: forall a
                . ( Arbitrary a
                  , ToJSON a
                  , FromJSON a
                  , Show a
                  , ApproxEq a
                  , Typeable a
                  )
               => Proxy a -> Spec
propApproxJSON _ = prop testName $ \(a :: a) ->
  let jsonStr = "via " <> BL8.unpack (encode a)
  in counterexample jsonStr (parseEither parseJSON (toJSON a)
                             ==~ Right a)
  where testName = show ty <> " FromJSON/ToJSON roundtrips"
        ty = typeOf (undefined :: a)

spec :: Spec
spec = do
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

  describe "Exact isomorphism JSON instances" $ do
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
    propJSON (Proxy :: Proxy Script)
    propJSON (Proxy :: Proxy ScriptLanguage)
    propJSON (Proxy :: Proxy ScriptInline)
    propJSON (Proxy :: Proxy ScriptId)
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
    propJSON (Proxy :: Proxy IndexSettings)
    propJSON (Proxy :: Proxy CompoundFormat)
    propJSON (Proxy :: Proxy TemplateQueryInline)
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
