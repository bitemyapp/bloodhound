{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module TestsUtils.Generators where

import qualified Data.Aeson.KeyMap as X
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Versions as Versions
import Database.Bloodhound
import Generic.Random
import TestsUtils.ApproxEq
import TestsUtils.Import

instance Arbitrary NominalDiffTime where
  arbitrary = fromInteger <$> arbitrary

#if !MIN_VERSION_QuickCheck(2,8,0)
instance (Arbitrary k, Ord k, Arbitrary v) => Arbitrary (M.Map k v) where
  arbitrary = M.fromList <$> arbitrary
#endif

instance Arbitrary Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary UTCTime where
  arbitrary =
    UTCTime
      <$> arbitrary
      <*> (fromRational . toRational <$> choose (0 :: Double, 86400))

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
  arbitrary =
    Hit
      <$> arbitrary
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
arbitraryAlphaNum =
  oneof
    [ choose ('a', 'z'),
      choose ('A', 'Z'),
      choose ('0', '9')
    ]

instance Arbitrary RoutingValue where
  arbitrary = RoutingValue . T.pack <$> listOf1 arbitraryAlphaNum

instance Arbitrary AliasRouting where
  arbitrary =
    oneof
      [ allAlias,
        one,
        theOther,
        both'
      ]
    where
      one =
        GranularAliasRouting
          <$> (Just <$> arbitrary)
          <*> pure Nothing
      theOther =
        GranularAliasRouting Nothing
          <$> (Just <$> arbitrary)
      both' =
        GranularAliasRouting
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
    pure $
      ScriptFields $
        X.fromList []

  shrink = const []

instance Arbitrary ScriptParams where
  arbitrary =
    pure $
      ScriptParams $
        X.fromList
          [ ("a", Number 42),
            ("b", String "forty two")
          ]

  shrink = const []

instance Arbitrary RegexpFlags where
  arbitrary =
    oneof
      [ pure AllRegexpFlags,
        pure NoRegexpFlags,
        SomeRegexpFlags <$> genUniqueFlags
      ]
    where
      genUniqueFlags =
        NE.fromList . L.nub
          <$> listOf1 arbitrary

instance Arbitrary IndexAliasCreate where
  arbitrary =
    IndexAliasCreate
      <$> arbitrary
      <*> reduceSize arbitrary

instance Arbitrary Query where
  arbitrary =
    reduceSize $
      oneof
        [ TermQuery <$> arbitrary <*> arbitrary,
          TermsQuery <$> arbitrary <*> arbitrary,
          QueryMatchQuery <$> arbitrary,
          QueryMultiMatchQuery <$> arbitrary,
          QueryBoolQuery <$> arbitrary,
          QueryBoostingQuery <$> arbitrary,
          QueryCommonTermsQuery <$> arbitrary,
          ConstantScoreQuery <$> arbitrary <*> arbitrary,
          QueryDisMaxQuery <$> arbitrary,
          QueryFuzzyLikeThisQuery <$> arbitrary,
          QueryFuzzyLikeFieldQuery <$> arbitrary,
          QueryFuzzyQuery <$> arbitrary,
          QueryHasChildQuery <$> arbitrary,
          QueryHasParentQuery <$> arbitrary,
          IdsQuery <$> arbitrary,
          QueryIndicesQuery <$> arbitrary,
          MatchAllQuery <$> arbitrary,
          QueryMoreLikeThisQuery <$> arbitrary,
          QueryMoreLikeThisFieldQuery <$> arbitrary,
          QueryNestedQuery <$> arbitrary,
          QueryPrefixQuery <$> arbitrary,
          QueryQueryStringQuery <$> arbitrary,
          QuerySimpleQueryStringQuery <$> arbitrary,
          QueryRangeQuery <$> arbitrary,
          QueryRegexpQuery <$> arbitrary
        ]

-- TODO: Implement shrink
-- shrink = genericShrink

instance Arbitrary Filter where
  arbitrary =
    Filter <$> arbitrary
  shrink (Filter q) =
    Filter <$> shrink q

instance Arbitrary ReplicaBounds where
  arbitrary =
    oneof
      [ replicasBounded,
        replicasLowerBounded,
        pure ReplicasUnbounded
      ]
    where
      replicasBounded = do
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
    where
      -- listOf1 means this shouldn't blow up.
      unpackConsPartial (x : xs) = (x, xs)
      unpackConsPartial _ = error "unpackConsPartial failed but shouldn't have"

#if MIN_VERSION_versions(6,0,0)
instance Arbitrary VersionNumber where
  arbitrary = do
    chunks <- Versions.Chunks <$> arbitrary
    return $ VersionNumber $ Versions.Version Nothing chunks Nothing Nothing

instance Arbitrary Versions.Chunk where
  arbitrary =
    oneof
      [ Versions.Numeric <$> arbitrary,
        do
          n <- chooseInt (1, 5)
          Versions.Alphanum . T.pack <$> replicateM n (chooseEnum ('a', 'z'))]
#else
instance Arbitrary VersionNumber where
  arbitrary = do
    chunks <- arbitrary
    return $
      VersionNumber $
        Versions.Version {
          Versions._vEpoch = Nothing,
          Versions._vChunks = chunks, Versions._vRel = [], Versions._vMeta = Nothing}

instance Arbitrary Versions.VUnit where
  arbitrary =
    oneof
      [ Versions.Digits . fromIntegral <$> chooseInt (0, 1000),
        do
          n <- chooseInt (1, 5)
          Versions.Str . T.pack <$> replicateM n (chooseEnum ('a', 'z'))]
#endif

instance Arbitrary TemplateQueryKeyValuePairs where
  arbitrary = TemplateQueryKeyValuePairs . X.fromList <$> arbitrary
  shrink (TemplateQueryKeyValuePairs x) = map (TemplateQueryKeyValuePairs . X.fromList) . shrink $ X.toList x

instance Arbitrary IndexName where
  arbitrary = do
    n <- chooseInt (5, 15)
    indewName <- T.pack <$> replicateM n (chooseEnum ('a', 'z'))
    return $ either (\e -> error $ "Invalid generated IndexName " <> show indewName <> ":" <> T.unpack e) id $ mkIndexName indewName

deriving newtype instance Arbitrary DocId

instance Arbitrary Version where arbitrary = genericArbitraryU

deriving newtype instance Arbitrary BuildHash

deriving newtype instance Arbitrary IndexAliasRouting

deriving newtype instance Arbitrary ShardCount

deriving newtype instance Arbitrary ReplicaCount

deriving newtype instance Arbitrary TemplateName

deriving newtype instance Arbitrary IndexPattern

deriving newtype instance Arbitrary QueryString

deriving newtype instance Arbitrary CacheName

deriving newtype instance Arbitrary CacheKey

deriving newtype instance Arbitrary Existence

deriving newtype instance Arbitrary CutoffFrequency

deriving newtype instance Arbitrary Analyzer

deriving newtype instance Arbitrary MaxExpansions

deriving newtype instance Arbitrary Lenient

deriving newtype instance Arbitrary Tiebreaker

deriving newtype instance Arbitrary Boost

deriving newtype instance Arbitrary BoostTerms

deriving newtype instance Arbitrary MinimumMatch

deriving newtype instance Arbitrary DisableCoord

deriving newtype instance Arbitrary IgnoreTermFrequency

deriving newtype instance Arbitrary MinimumTermFrequency

deriving newtype instance Arbitrary MaxQueryTerms

instance Arbitrary Fuzziness where arbitrary = genericArbitraryU

deriving newtype instance Arbitrary PrefixLength

deriving newtype instance Arbitrary RelationName

deriving newtype instance Arbitrary PercentMatch

deriving newtype instance Arbitrary StopWord

deriving newtype instance Arbitrary QueryPath

deriving newtype instance Arbitrary AllowLeadingWildcard

deriving newtype instance Arbitrary LowercaseExpanded

deriving newtype instance Arbitrary EnablePositionIncrements

deriving newtype instance Arbitrary AnalyzeWildcard

deriving newtype instance Arbitrary GeneratePhraseQueries

deriving newtype instance Arbitrary Locale

deriving newtype instance Arbitrary MaxWordLength

deriving newtype instance Arbitrary MinWordLength

deriving newtype instance Arbitrary PhraseSlop

deriving newtype instance Arbitrary MinDocFrequency

deriving newtype instance Arbitrary MaxDocFrequency

deriving newtype instance Arbitrary Regexp

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

deriving newtype instance Arbitrary IgnoreUnmapped

deriving newtype instance Arbitrary MinChildren

deriving newtype instance Arbitrary MaxChildren

deriving newtype instance Arbitrary AggregateParentScore

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

deriving newtype instance Arbitrary NullValue

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

deriving newtype instance Arbitrary TokenFilter

instance Arbitrary NgramFilter where arbitrary = genericArbitraryU

instance Arbitrary EdgeNgramFilterSide where arbitrary = genericArbitraryU

instance Arbitrary TokenFilterDefinition where arbitrary = genericArbitraryU

instance Arbitrary Language where arbitrary = genericArbitraryU

instance Arbitrary Shingle where arbitrary = genericArbitraryU

deriving newtype instance Arbitrary CharFilter

instance Arbitrary AnalyzerDefinition where arbitrary = genericArbitraryU

-- TODO: This should have a proper generator that doesn't
-- create garbage that has to be filtered out.
instance Arbitrary CharFilterDefinition where
  arbitrary =
    oneof
      [ CharFilterDefinitionMapping
          . chomp
          <$> arbitrary,
        CharFilterDefinitionPatternReplace
          <$> arbitrary
          <*> arbitrary
          <*> arbitrary
      ]
    where
      chomp =
        M.map T.strip
          . M.mapKeys (T.replace "=>" "" . T.strip)

instance Arbitrary Analysis where arbitrary = genericArbitraryU

deriving newtype instance Arbitrary Tokenizer

instance Arbitrary Compression where arbitrary = genericArbitraryU

deriving newtype instance Arbitrary Bytes

instance Arbitrary AllocationPolicy where arbitrary = genericArbitraryU

instance Arbitrary InitialShardCount where arbitrary = genericArbitraryU

instance Arbitrary FSType where arbitrary = genericArbitraryU

instance Arbitrary CompoundFormat where arbitrary = genericArbitraryU

instance Arbitrary FsSnapshotRepo where arbitrary = genericArbitraryU

deriving newtype instance Arbitrary SnapshotRepoName

instance Arbitrary DirectGeneratorSuggestModeTypes where arbitrary = genericArbitraryU

instance Arbitrary DirectGenerators where arbitrary = genericArbitraryU

instance Arbitrary PhraseSuggesterCollate where arbitrary = genericArbitraryU

instance Arbitrary PhraseSuggesterHighlighter where arbitrary = genericArbitraryU

deriving newtype instance Arbitrary Size

instance Arbitrary PhraseSuggester where arbitrary = genericArbitraryU

instance Arbitrary SuggestType where arbitrary = genericArbitraryU

instance Arbitrary Suggest where arbitrary = genericArbitraryU

instance Arbitrary FunctionScoreQuery where arbitrary = genericArbitraryU

instance Arbitrary FunctionScoreFunction where arbitrary = genericArbitraryU

instance Arbitrary FunctionScoreFunctions where arbitrary = genericArbitraryU

instance Arbitrary ComponentFunctionScoreFunction where arbitrary = genericArbitraryU

instance Arbitrary Script where arbitrary = genericArbitraryU

deriving newtype instance Arbitrary ScriptLanguage

instance Arbitrary ScriptSource where arbitrary = genericArbitraryU

instance Arbitrary ScoreMode where arbitrary = genericArbitraryU

instance Arbitrary BoostMode where arbitrary = genericArbitraryU

deriving newtype instance Arbitrary Seed

instance Arbitrary FieldValueFactor where arbitrary = genericArbitraryU

deriving newtype instance Arbitrary Weight

deriving newtype instance Arbitrary Factor

deriving newtype instance Arbitrary FactorMissingFieldValue

instance Arbitrary FactorModifier where arbitrary = genericArbitraryU

instance Arbitrary UpdatableIndexSetting where
  arbitrary = resize 10 genericArbitraryU

newtype UpdatableIndexSetting'
  = UpdatableIndexSetting' UpdatableIndexSetting
  deriving stock (Eq, Show, Typeable)
  deriving newtype (ToJSON, FromJSON, ApproxEq)

instance Arbitrary UpdatableIndexSetting' where
  arbitrary = do
    settings <- arbitrary
    return $
      UpdatableIndexSetting' $ case settings of
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
