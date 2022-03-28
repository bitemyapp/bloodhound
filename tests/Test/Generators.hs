{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Test.Generators where

import           Database.Bloodhound

import           Test.Import

import qualified Data.Aeson.KeyMap   as X
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.SemVer as SemVer

import           Generic.Random
import           Test.ApproxEq

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
    arbitrary =
      ModifiedJulianDay . (2000 +) <$> arbitrary
    shrink =
      (ModifiedJulianDay <$>) . shrink . toModifiedJulianDay

#if !MIN_VERSION_QuickCheck(2,9,0)
instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = liftA2 (:|) arbitrary arbitrary
#endif

arbitraryScore :: Gen Score
arbitraryScore = fmap getPositive <$> arbitrary

instance (Arbitrary a, Typeable a) => Arbitrary (Hit a) where
  arbitrary = Hit <$> arbitrary
                  <*> arbitrary
                  <*> arbitraryScore
                  <*> arbitrary
                  <*> return Nothing
                  <*> arbitrary
                  <*> arbitrary
                  <*> return Nothing

instance Arbitrary HitFields where
  arbitrary = pure (HitFields M.empty)
  shrink = const []

instance Arbitrary HitsTotalRelation where
  arbitrary = oneof [pure HTR_EQ, pure HTR_GTE]

instance Arbitrary HitsTotal where
  arbitrary = do
    tot <- getPositive <$> arbitrary
    relation_ <- arbitrary
    return $ HitsTotal tot relation_

instance (Arbitrary a, Typeable a) => Arbitrary (SearchHits a) where
  arbitrary = reduceSize $ do
    tot <- arbitrary
    score <- arbitraryScore
    hs <- arbitrary
    return $ SearchHits tot score hs


reduceSize :: Gen a -> Gen a
reduceSize f = sized $ \n -> resize (n `div` 2) f

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
                    ,both']
    where one = GranularAliasRouting
                <$> (Just <$> arbitrary)
                <*> pure Nothing
          theOther = GranularAliasRouting Nothing
                     <$> (Just <$> arbitrary)
          both' = GranularAliasRouting
                 <$> (Just <$> arbitrary)
                 <*> (Just <$> arbitrary)
          allAlias = AllAliasRouting <$> arbitrary



instance Arbitrary FieldName where
  arbitrary =
        FieldName
      . T.pack
    <$> listOf1 arbitraryAlphaNum


#if MIN_VERSION_base(4,10,0)
-- Test.QuickCheck.Modifiers

qcNonEmptyToNonEmpty :: NonEmptyList a -> NonEmpty a
qcNonEmptyToNonEmpty (NonEmpty (a : xs)) = (a :| xs)
qcNonEmptyToNonEmpty (NonEmpty []) = error "NonEmpty was empty!"

instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary =
        qcNonEmptyToNonEmpty
    <$> arbitrary
#endif
instance Arbitrary ScriptFields where
  arbitrary =
    pure $ ScriptFields $
      X.fromList []

  shrink = const []

instance Arbitrary ScriptParams where
  arbitrary =
    pure $ ScriptParams $
      X.fromList [ ("a", Number 42)
                  , ("b", String "forty two")
                  ]

  shrink = const []

instance Arbitrary RegexpFlags where
  arbitrary = oneof [ pure AllRegexpFlags
                    , pure NoRegexpFlags
                    , SomeRegexpFlags <$> genUniqueFlags
                    ]
    where genUniqueFlags =
                NE.fromList . L.nub
            <$> listOf1 arbitrary

instance Arbitrary IndexAliasCreate where
  arbitrary =
        IndexAliasCreate
    <$> arbitrary
    <*> reduceSize arbitrary

instance Arbitrary Query where
  arbitrary =
    reduceSize
    $ oneof [ TermQuery <$> arbitrary <*> arbitrary
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
            , IdsQuery <$> arbitrary
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
  -- TODO: Implement shrink
  -- shrink = genericShrink

instance Arbitrary Filter where
  arbitrary =
    Filter <$> arbitrary
  shrink (Filter q) =
    Filter <$> shrink q

instance Arbitrary ReplicaBounds where
  arbitrary = oneof [ replicasBounded
                    , replicasLowerBounded
                    , pure ReplicasUnbounded
                    ]
    where replicasBounded = do
            Positive a <- arbitrary
            Positive b <- arbitrary
            return (ReplicasBounded a b)
          replicasLowerBounded = do
            Positive a <- arbitrary
            return (ReplicasLowerBounded a)

instance Arbitrary NodeAttrName where
  arbitrary =
        NodeAttrName
      . T.pack
    <$> listOf1 arbitraryAlphaNum


instance Arbitrary NodeAttrFilter where
  arbitrary = do
    n <- arbitrary
    xs <- listOf1 (listOf1 arbitraryAlphaNum)
    let (s, ss) = unpackConsPartial xs
        ts = T.pack <$> s :| ss
    return (NodeAttrFilter n ts)
      where -- listOf1 means this shouldn't blow up.
            unpackConsPartial (x : xs) = (x, xs)
            unpackConsPartial _ = error "unpackConsPartial failed but shouldn't have"

instance Arbitrary VersionNumber where
  arbitrary = do
    major <- posInt
    minor <- posInt
    patch <- posInt
    return $ VersionNumber $ SemVer.version major minor patch [] []
    where
      posInt = getPositive <$> arbitrary

instance Arbitrary TemplateQueryKeyValuePairs where
  arbitrary = TemplateQueryKeyValuePairs . X.fromList <$> arbitrary
  shrink (TemplateQueryKeyValuePairs x) = map (TemplateQueryKeyValuePairs . X.fromList) . shrink $ X.toList x

instance Arbitrary IndexName where arbitrary = genericArbitraryU
instance Arbitrary DocId where arbitrary = genericArbitraryU
instance Arbitrary Version where arbitrary = genericArbitraryU
instance Arbitrary BuildHash where arbitrary = genericArbitraryU
instance Arbitrary IndexAliasRouting where arbitrary = genericArbitraryU
instance Arbitrary ShardCount where arbitrary = genericArbitraryU
instance Arbitrary ReplicaCount where arbitrary = genericArbitraryU
instance Arbitrary TemplateName where arbitrary = genericArbitraryU
instance Arbitrary IndexPattern where arbitrary = genericArbitraryU
instance Arbitrary QueryString where arbitrary = genericArbitraryU
instance Arbitrary CacheName where arbitrary = genericArbitraryU
instance Arbitrary CacheKey where arbitrary = genericArbitraryU
instance Arbitrary Existence where arbitrary = genericArbitraryU
instance Arbitrary CutoffFrequency where arbitrary = genericArbitraryU
instance Arbitrary Analyzer where arbitrary = genericArbitraryU
instance Arbitrary MaxExpansions where arbitrary = genericArbitraryU
instance Arbitrary Lenient where arbitrary = genericArbitraryU
instance Arbitrary Tiebreaker where arbitrary = genericArbitraryU
instance Arbitrary Boost where arbitrary = genericArbitraryU
instance Arbitrary BoostTerms where arbitrary = genericArbitraryU
instance Arbitrary MinimumMatch where arbitrary = genericArbitraryU
instance Arbitrary DisableCoord where arbitrary = genericArbitraryU
instance Arbitrary IgnoreTermFrequency where arbitrary = genericArbitraryU
instance Arbitrary MinimumTermFrequency where arbitrary = genericArbitraryU
instance Arbitrary MaxQueryTerms where arbitrary = genericArbitraryU
instance Arbitrary Fuzziness where arbitrary = genericArbitraryU
instance Arbitrary PrefixLength where arbitrary = genericArbitraryU
instance Arbitrary RelationName where arbitrary = genericArbitraryU
instance Arbitrary PercentMatch where arbitrary = genericArbitraryU
instance Arbitrary StopWord where arbitrary = genericArbitraryU
instance Arbitrary QueryPath where arbitrary = genericArbitraryU
instance Arbitrary AllowLeadingWildcard where arbitrary = genericArbitraryU
instance Arbitrary LowercaseExpanded where arbitrary = genericArbitraryU
instance Arbitrary EnablePositionIncrements where arbitrary = genericArbitraryU
instance Arbitrary AnalyzeWildcard where arbitrary = genericArbitraryU
instance Arbitrary GeneratePhraseQueries where arbitrary = genericArbitraryU
instance Arbitrary Locale where arbitrary = genericArbitraryU
instance Arbitrary MaxWordLength where arbitrary = genericArbitraryU
instance Arbitrary MinWordLength where arbitrary = genericArbitraryU
instance Arbitrary PhraseSlop where arbitrary = genericArbitraryU
instance Arbitrary MinDocFrequency where arbitrary = genericArbitraryU
instance Arbitrary MaxDocFrequency where arbitrary = genericArbitraryU
instance Arbitrary Regexp where arbitrary = genericArbitraryU
instance Arbitrary SimpleQueryStringQuery where arbitrary = genericArbitraryU
instance Arbitrary FieldOrFields where arbitrary = genericArbitraryU
instance Arbitrary SimpleQueryFlag where arbitrary = genericArbitraryU
instance Arbitrary RegexpQuery where arbitrary = genericArbitraryU
instance Arbitrary QueryStringQuery where arbitrary = genericArbitraryU
instance Arbitrary RangeQuery where arbitrary = genericArbitraryU
instance Arbitrary RangeValue where arbitrary = genericArbitraryU
instance Arbitrary PrefixQuery where arbitrary = genericArbitraryU
instance Arbitrary NestedQuery where arbitrary = genericArbitraryU
instance Arbitrary MoreLikeThisFieldQuery where arbitrary = genericArbitraryU
instance Arbitrary MoreLikeThisQuery where arbitrary = genericArbitraryU
instance Arbitrary IndicesQuery where arbitrary = genericArbitraryU
instance Arbitrary IgnoreUnmapped where arbitrary = genericArbitraryU
instance Arbitrary MinChildren where arbitrary = genericArbitraryU
instance Arbitrary MaxChildren where arbitrary = genericArbitraryU
instance Arbitrary AggregateParentScore where arbitrary = genericArbitraryU
instance Arbitrary HasParentQuery where arbitrary = genericArbitraryU
instance Arbitrary HasChildQuery where arbitrary = genericArbitraryU
instance Arbitrary FuzzyQuery where arbitrary = genericArbitraryU
instance Arbitrary FuzzyLikeFieldQuery where arbitrary = genericArbitraryU
instance Arbitrary FuzzyLikeThisQuery where arbitrary = genericArbitraryU
instance Arbitrary DisMaxQuery where arbitrary = genericArbitraryU
instance Arbitrary CommonTermsQuery where arbitrary = genericArbitraryU
instance Arbitrary DistanceRange where arbitrary = genericArbitraryU
instance Arbitrary MultiMatchQuery where arbitrary = genericArbitraryU
instance Arbitrary LessThanD where arbitrary = genericArbitraryU
instance Arbitrary LessThanEqD where arbitrary = genericArbitraryU
instance Arbitrary GreaterThanD where arbitrary = genericArbitraryU
instance Arbitrary GreaterThanEqD where arbitrary = genericArbitraryU
instance Arbitrary LessThan where arbitrary = genericArbitraryU
instance Arbitrary LessThanEq where arbitrary = genericArbitraryU
instance Arbitrary GreaterThan where arbitrary = genericArbitraryU
instance Arbitrary GreaterThanEq where arbitrary = genericArbitraryU
instance Arbitrary GeoPoint where arbitrary = genericArbitraryU
instance Arbitrary NullValue where arbitrary = genericArbitraryU
instance Arbitrary MinimumMatchHighLow where arbitrary = genericArbitraryU
instance Arbitrary CommonMinimumMatch where arbitrary = genericArbitraryU
instance Arbitrary BoostingQuery where arbitrary = genericArbitraryU
instance Arbitrary BoolQuery where arbitrary = genericArbitraryU
instance Arbitrary MatchQuery where arbitrary = genericArbitraryU
instance Arbitrary MultiMatchQueryType where arbitrary = genericArbitraryU
instance Arbitrary BooleanOperator where arbitrary = genericArbitraryU
instance Arbitrary ZeroTermsQuery where arbitrary = genericArbitraryU
instance Arbitrary MatchQueryType where arbitrary = genericArbitraryU
instance Arbitrary SearchAliasRouting where arbitrary = genericArbitraryU
instance Arbitrary ScoreType where arbitrary = genericArbitraryU
instance Arbitrary Distance where arbitrary = genericArbitraryU
instance Arbitrary DistanceUnit where arbitrary = genericArbitraryU
instance Arbitrary DistanceType where arbitrary = genericArbitraryU
instance Arbitrary OptimizeBbox where arbitrary = genericArbitraryU
instance Arbitrary GeoBoundingBoxConstraint where arbitrary = genericArbitraryU
instance Arbitrary GeoFilterType where arbitrary = genericArbitraryU
instance Arbitrary GeoBoundingBox where arbitrary = genericArbitraryU
instance Arbitrary LatLon where arbitrary = genericArbitraryU
instance Arbitrary RangeExecution where arbitrary = genericArbitraryU
instance Arbitrary RegexpFlag where arbitrary = genericArbitraryU
instance Arbitrary BoolMatch where arbitrary = genericArbitraryU
instance Arbitrary Term where arbitrary = genericArbitraryU
instance Arbitrary IndexMappingsLimits where arbitrary = genericArbitraryU
instance Arbitrary IndexSettings where arbitrary = genericArbitraryU
instance Arbitrary TokenChar where arbitrary = genericArbitraryU
instance Arbitrary Ngram where arbitrary = genericArbitraryU
instance Arbitrary TokenizerDefinition where arbitrary = genericArbitraryU
instance Arbitrary TokenFilter where arbitrary = genericArbitraryU
instance Arbitrary TokenFilterDefinition where arbitrary = genericArbitraryU
instance Arbitrary Language where arbitrary = genericArbitraryU
instance Arbitrary Shingle where arbitrary = genericArbitraryU

instance Arbitrary CharFilter where arbitrary = genericArbitraryU
instance Arbitrary AnalyzerDefinition where arbitrary = genericArbitraryU

-- TODO: This should have a proper generator that doesn't
-- create garbage that has to be filtered out.
instance Arbitrary CharFilterDefinition where
  arbitrary =
    oneof [   CharFilterDefinitionMapping
            . chomp <$> arbitrary
          , CharFilterDefinitionPatternReplace
            <$> arbitrary <*> arbitrary <*> arbitrary
                    ]
    where chomp =
              M.map T.strip
            . M.mapKeys (T.replace "=>" "" . T.strip)

instance Arbitrary Analysis where arbitrary = genericArbitraryU
instance Arbitrary Tokenizer where arbitrary = genericArbitraryU
instance Arbitrary Compression where arbitrary = genericArbitraryU
instance Arbitrary Bytes where arbitrary = genericArbitraryU
instance Arbitrary AllocationPolicy where arbitrary = genericArbitraryU
instance Arbitrary InitialShardCount where arbitrary = genericArbitraryU
instance Arbitrary FSType where arbitrary = genericArbitraryU
instance Arbitrary CompoundFormat where arbitrary = genericArbitraryU
instance Arbitrary FsSnapshotRepo where arbitrary = genericArbitraryU
instance Arbitrary SnapshotRepoName where arbitrary = genericArbitraryU
instance Arbitrary DirectGeneratorSuggestModeTypes where arbitrary = genericArbitraryU
instance Arbitrary DirectGenerators where arbitrary = genericArbitraryU
instance Arbitrary PhraseSuggesterCollate where arbitrary = genericArbitraryU
instance Arbitrary PhraseSuggesterHighlighter where arbitrary = genericArbitraryU
instance Arbitrary Size where arbitrary = genericArbitraryU
instance Arbitrary PhraseSuggester where arbitrary = genericArbitraryU
instance Arbitrary SuggestType where arbitrary = genericArbitraryU
instance Arbitrary Suggest where arbitrary = genericArbitraryU

instance Arbitrary FunctionScoreQuery where arbitrary = genericArbitraryU

instance Arbitrary FunctionScoreFunction where arbitrary = genericArbitraryU
instance Arbitrary FunctionScoreFunctions where arbitrary = genericArbitraryU
instance Arbitrary ComponentFunctionScoreFunction where arbitrary = genericArbitraryU
instance Arbitrary Script where arbitrary = genericArbitraryU
instance Arbitrary ScriptLanguage where arbitrary = genericArbitraryU
instance Arbitrary ScriptSource where arbitrary = genericArbitraryU
instance Arbitrary ScoreMode where arbitrary = genericArbitraryU
instance Arbitrary BoostMode where arbitrary = genericArbitraryU
instance Arbitrary Seed where arbitrary = genericArbitraryU
instance Arbitrary FieldValueFactor where arbitrary = genericArbitraryU
instance Arbitrary Weight where arbitrary = genericArbitraryU
instance Arbitrary Factor where arbitrary = genericArbitraryU
instance Arbitrary FactorMissingFieldValue where arbitrary = genericArbitraryU
instance Arbitrary FactorModifier where arbitrary = genericArbitraryU

instance Arbitrary UpdatableIndexSetting where
  arbitrary = resize 10 genericArbitraryU

newtype UpdatableIndexSetting' =
  UpdatableIndexSetting' UpdatableIndexSetting
  deriving (Show, Eq, ToJSON, FromJSON, ApproxEq, Typeable)

instance Arbitrary UpdatableIndexSetting' where
  arbitrary = do
    settings <- arbitrary
    return $ UpdatableIndexSetting' $ case settings of
      RoutingAllocationInclude xs ->
        RoutingAllocationInclude (dropDuplicateAttrNames xs)
      RoutingAllocationExclude xs ->
        RoutingAllocationExclude (dropDuplicateAttrNames xs)
      RoutingAllocationRequire xs ->
        RoutingAllocationRequire (dropDuplicateAttrNames xs)
      x -> x
    where
      dropDuplicateAttrNames =
        NE.fromList . L.nubBy sameAttrName . NE.toList
      sameAttrName a b =
        nodeAttrFilterName a == nodeAttrFilterName b
  shrink (UpdatableIndexSetting' x) = map UpdatableIndexSetting' (shrink x)

instance Arbitrary InnerHits where arbitrary = genericArbitraryU
