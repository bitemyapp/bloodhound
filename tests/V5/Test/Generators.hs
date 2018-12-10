{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Test.Generators where

import           Database.V5.Bloodhound

import           Test.Import

import qualified Data.HashMap.Strict as HM
import qualified Data.List as L
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.SemVer as SemVer
import           Test.QuickCheck.TH.Generators

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
                  <*> arbitrary
                  <*> arbitraryScore
                  <*> arbitrary
                  <*> arbitrary
                  <*> arbitrary

instance Arbitrary HitFields where
  arbitrary = pure (HitFields M.empty)
  shrink = const []

instance (Arbitrary a, Typeable a) => Arbitrary (SearchHits a) where
  arbitrary = reduceSize $ do
    tot <- getPositive <$> arbitrary
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
      HM.fromList []

  shrink = const []

instance Arbitrary ScriptParams where
  arbitrary =
    pure $ ScriptParams $
      HM.fromList [ ("a", Number 42)
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
    s:ss <- listOf1 (listOf1 arbitraryAlphaNum)
    let ts = T.pack <$> s :| ss
    return (NodeAttrFilter n ts)

instance Arbitrary VersionNumber where
  arbitrary = do
    major <- posInt
    minor <- posInt
    patch <- posInt
    return $ VersionNumber $ SemVer.version major minor patch [] []
    where
      posInt = getPositive <$> arbitrary

instance Arbitrary TemplateQueryKeyValuePairs where
  arbitrary = TemplateQueryKeyValuePairs . HM.fromList <$> arbitrary
  shrink (TemplateQueryKeyValuePairs x) = map (TemplateQueryKeyValuePairs . HM.fromList) . shrink $ HM.toList x

makeArbitrary ''IndexName
instance Arbitrary IndexName where arbitrary = arbitraryIndexName
makeArbitrary ''MappingName
instance Arbitrary MappingName where arbitrary = arbitraryMappingName
makeArbitrary ''DocId
instance Arbitrary DocId where arbitrary = arbitraryDocId
makeArbitrary ''Version
instance Arbitrary Version where arbitrary = arbitraryVersion
makeArbitrary ''BuildHash
instance Arbitrary BuildHash where arbitrary = arbitraryBuildHash
makeArbitrary ''IndexAliasRouting
instance Arbitrary IndexAliasRouting where arbitrary = arbitraryIndexAliasRouting
makeArbitrary ''ShardCount
instance Arbitrary ShardCount where arbitrary = arbitraryShardCount
makeArbitrary ''ReplicaCount
instance Arbitrary ReplicaCount where arbitrary = arbitraryReplicaCount
makeArbitrary ''TemplateName
instance Arbitrary TemplateName where arbitrary = arbitraryTemplateName
makeArbitrary ''TemplatePattern
instance Arbitrary TemplatePattern where arbitrary = arbitraryTemplatePattern
makeArbitrary ''QueryString
instance Arbitrary QueryString where arbitrary = arbitraryQueryString
makeArbitrary ''CacheName
instance Arbitrary CacheName where arbitrary = arbitraryCacheName
makeArbitrary ''CacheKey
instance Arbitrary CacheKey where arbitrary = arbitraryCacheKey
makeArbitrary ''Existence
instance Arbitrary Existence where arbitrary = arbitraryExistence
makeArbitrary ''CutoffFrequency
instance Arbitrary CutoffFrequency where arbitrary = arbitraryCutoffFrequency
makeArbitrary ''Analyzer
instance Arbitrary Analyzer where arbitrary = arbitraryAnalyzer
makeArbitrary ''MaxExpansions
instance Arbitrary MaxExpansions where arbitrary = arbitraryMaxExpansions
makeArbitrary ''Lenient
instance Arbitrary Lenient where arbitrary = arbitraryLenient
makeArbitrary ''Tiebreaker
instance Arbitrary Tiebreaker where arbitrary = arbitraryTiebreaker
makeArbitrary ''Boost
instance Arbitrary Boost where arbitrary = arbitraryBoost
makeArbitrary ''BoostTerms
instance Arbitrary BoostTerms where arbitrary = arbitraryBoostTerms
makeArbitrary ''MinimumMatch
instance Arbitrary MinimumMatch where arbitrary = arbitraryMinimumMatch
makeArbitrary ''DisableCoord
instance Arbitrary DisableCoord where arbitrary = arbitraryDisableCoord
makeArbitrary ''IgnoreTermFrequency
instance Arbitrary IgnoreTermFrequency where arbitrary = arbitraryIgnoreTermFrequency
makeArbitrary ''MinimumTermFrequency
instance Arbitrary MinimumTermFrequency where arbitrary = arbitraryMinimumTermFrequency
makeArbitrary ''MaxQueryTerms
instance Arbitrary MaxQueryTerms where arbitrary = arbitraryMaxQueryTerms
makeArbitrary ''Fuzziness
instance Arbitrary Fuzziness where arbitrary = arbitraryFuzziness
makeArbitrary ''PrefixLength
instance Arbitrary PrefixLength where arbitrary = arbitraryPrefixLength
makeArbitrary ''TypeName
instance Arbitrary TypeName where arbitrary = arbitraryTypeName
makeArbitrary ''PercentMatch
instance Arbitrary PercentMatch where arbitrary = arbitraryPercentMatch
makeArbitrary ''StopWord
instance Arbitrary StopWord where arbitrary = arbitraryStopWord
makeArbitrary ''QueryPath
instance Arbitrary QueryPath where arbitrary = arbitraryQueryPath
makeArbitrary ''AllowLeadingWildcard
instance Arbitrary AllowLeadingWildcard where arbitrary = arbitraryAllowLeadingWildcard
makeArbitrary ''LowercaseExpanded
instance Arbitrary LowercaseExpanded where arbitrary = arbitraryLowercaseExpanded
makeArbitrary ''EnablePositionIncrements
instance Arbitrary EnablePositionIncrements where arbitrary = arbitraryEnablePositionIncrements
makeArbitrary ''AnalyzeWildcard
instance Arbitrary AnalyzeWildcard where arbitrary = arbitraryAnalyzeWildcard
makeArbitrary ''GeneratePhraseQueries
instance Arbitrary GeneratePhraseQueries where arbitrary = arbitraryGeneratePhraseQueries
makeArbitrary ''Locale
instance Arbitrary Locale where arbitrary = arbitraryLocale
makeArbitrary ''MaxWordLength
instance Arbitrary MaxWordLength where arbitrary = arbitraryMaxWordLength
makeArbitrary ''MinWordLength
instance Arbitrary MinWordLength where arbitrary = arbitraryMinWordLength
makeArbitrary ''PhraseSlop
instance Arbitrary PhraseSlop where arbitrary = arbitraryPhraseSlop
makeArbitrary ''MinDocFrequency
instance Arbitrary MinDocFrequency where arbitrary = arbitraryMinDocFrequency
makeArbitrary ''MaxDocFrequency
instance Arbitrary MaxDocFrequency where arbitrary = arbitraryMaxDocFrequency
makeArbitrary ''Regexp
instance Arbitrary Regexp where arbitrary = arbitraryRegexp
makeArbitrary ''SimpleQueryStringQuery
instance Arbitrary SimpleQueryStringQuery where arbitrary = arbitrarySimpleQueryStringQuery
makeArbitrary ''FieldOrFields
instance Arbitrary FieldOrFields where arbitrary = arbitraryFieldOrFields
makeArbitrary ''SimpleQueryFlag
instance Arbitrary SimpleQueryFlag where arbitrary = arbitrarySimpleQueryFlag
makeArbitrary ''RegexpQuery
instance Arbitrary RegexpQuery where arbitrary = arbitraryRegexpQuery
makeArbitrary ''QueryStringQuery
instance Arbitrary QueryStringQuery where arbitrary = arbitraryQueryStringQuery
makeArbitrary ''RangeQuery
instance Arbitrary RangeQuery where arbitrary = arbitraryRangeQuery
makeArbitrary ''RangeValue
instance Arbitrary RangeValue where arbitrary = arbitraryRangeValue
makeArbitrary ''PrefixQuery
instance Arbitrary PrefixQuery where arbitrary = arbitraryPrefixQuery
makeArbitrary ''NestedQuery
instance Arbitrary NestedQuery where arbitrary = arbitraryNestedQuery
makeArbitrary ''MoreLikeThisFieldQuery
instance Arbitrary MoreLikeThisFieldQuery where arbitrary = arbitraryMoreLikeThisFieldQuery
makeArbitrary ''MoreLikeThisQuery
instance Arbitrary MoreLikeThisQuery where arbitrary = arbitraryMoreLikeThisQuery
makeArbitrary ''IndicesQuery
instance Arbitrary IndicesQuery where arbitrary = arbitraryIndicesQuery
makeArbitrary ''HasParentQuery
instance Arbitrary HasParentQuery where arbitrary = arbitraryHasParentQuery
makeArbitrary ''HasChildQuery
instance Arbitrary HasChildQuery where arbitrary = arbitraryHasChildQuery
makeArbitrary ''FuzzyQuery
instance Arbitrary FuzzyQuery where arbitrary = arbitraryFuzzyQuery
makeArbitrary ''FuzzyLikeFieldQuery
instance Arbitrary FuzzyLikeFieldQuery where arbitrary = arbitraryFuzzyLikeFieldQuery
makeArbitrary ''FuzzyLikeThisQuery
instance Arbitrary FuzzyLikeThisQuery where arbitrary = arbitraryFuzzyLikeThisQuery
makeArbitrary ''DisMaxQuery
instance Arbitrary DisMaxQuery where arbitrary = arbitraryDisMaxQuery
makeArbitrary ''CommonTermsQuery
instance Arbitrary CommonTermsQuery where arbitrary = arbitraryCommonTermsQuery
makeArbitrary ''DistanceRange
instance Arbitrary DistanceRange where arbitrary = arbitraryDistanceRange
makeArbitrary ''MultiMatchQuery
instance Arbitrary MultiMatchQuery where arbitrary = arbitraryMultiMatchQuery
makeArbitrary ''LessThanD
instance Arbitrary LessThanD where arbitrary = arbitraryLessThanD
makeArbitrary ''LessThanEqD
instance Arbitrary LessThanEqD where arbitrary = arbitraryLessThanEqD
makeArbitrary ''GreaterThanD
instance Arbitrary GreaterThanD where arbitrary = arbitraryGreaterThanD
makeArbitrary ''GreaterThanEqD
instance Arbitrary GreaterThanEqD where arbitrary = arbitraryGreaterThanEqD
makeArbitrary ''LessThan
instance Arbitrary LessThan where arbitrary = arbitraryLessThan
makeArbitrary ''LessThanEq
instance Arbitrary LessThanEq where arbitrary = arbitraryLessThanEq
makeArbitrary ''GreaterThan
instance Arbitrary GreaterThan where arbitrary = arbitraryGreaterThan
makeArbitrary ''GreaterThanEq
instance Arbitrary GreaterThanEq where arbitrary = arbitraryGreaterThanEq
makeArbitrary ''GeoPoint
instance Arbitrary GeoPoint where arbitrary = arbitraryGeoPoint
makeArbitrary ''NullValue
instance Arbitrary NullValue where arbitrary = arbitraryNullValue
makeArbitrary ''MinimumMatchHighLow
instance Arbitrary MinimumMatchHighLow where arbitrary = arbitraryMinimumMatchHighLow
makeArbitrary ''CommonMinimumMatch
instance Arbitrary CommonMinimumMatch where arbitrary = arbitraryCommonMinimumMatch
makeArbitrary ''BoostingQuery
instance Arbitrary BoostingQuery where arbitrary = arbitraryBoostingQuery
makeArbitrary ''BoolQuery
instance Arbitrary BoolQuery where arbitrary = arbitraryBoolQuery
makeArbitrary ''MatchQuery
instance Arbitrary MatchQuery where arbitrary = arbitraryMatchQuery
makeArbitrary ''MultiMatchQueryType
instance Arbitrary MultiMatchQueryType where arbitrary = arbitraryMultiMatchQueryType
makeArbitrary ''BooleanOperator
instance Arbitrary BooleanOperator where arbitrary = arbitraryBooleanOperator
makeArbitrary ''ZeroTermsQuery
instance Arbitrary ZeroTermsQuery where arbitrary = arbitraryZeroTermsQuery
makeArbitrary ''MatchQueryType
instance Arbitrary MatchQueryType where arbitrary = arbitraryMatchQueryType
makeArbitrary ''SearchAliasRouting
instance Arbitrary SearchAliasRouting where arbitrary = arbitrarySearchAliasRouting
makeArbitrary ''ScoreType
instance Arbitrary ScoreType where arbitrary = arbitraryScoreType
makeArbitrary ''Distance
instance Arbitrary Distance where arbitrary = arbitraryDistance
makeArbitrary ''DistanceUnit
instance Arbitrary DistanceUnit where arbitrary = arbitraryDistanceUnit
makeArbitrary ''DistanceType
instance Arbitrary DistanceType where arbitrary = arbitraryDistanceType
makeArbitrary ''OptimizeBbox
instance Arbitrary OptimizeBbox where arbitrary = arbitraryOptimizeBbox
makeArbitrary ''GeoBoundingBoxConstraint
instance Arbitrary GeoBoundingBoxConstraint where arbitrary = arbitraryGeoBoundingBoxConstraint
makeArbitrary ''GeoFilterType
instance Arbitrary GeoFilterType where arbitrary = arbitraryGeoFilterType
makeArbitrary ''GeoBoundingBox
instance Arbitrary GeoBoundingBox where arbitrary = arbitraryGeoBoundingBox
makeArbitrary ''LatLon
instance Arbitrary LatLon where arbitrary = arbitraryLatLon
makeArbitrary ''RangeExecution
instance Arbitrary RangeExecution where arbitrary = arbitraryRangeExecution
makeArbitrary ''RegexpFlag
instance Arbitrary RegexpFlag where arbitrary = arbitraryRegexpFlag
makeArbitrary ''BoolMatch
instance Arbitrary BoolMatch where arbitrary = arbitraryBoolMatch
makeArbitrary ''Term
instance Arbitrary Term where arbitrary = arbitraryTerm
makeArbitrary ''IndexSettings
instance Arbitrary IndexSettings where arbitrary = arbitraryIndexSettings
makeArbitrary ''TokenChar
instance Arbitrary TokenChar where arbitrary = arbitraryTokenChar
makeArbitrary ''Ngram
instance Arbitrary Ngram where arbitrary = arbitraryNgram
makeArbitrary ''TokenizerDefinition
instance Arbitrary TokenizerDefinition where arbitrary = arbitraryTokenizerDefinition
makeArbitrary ''TokenFilter
instance Arbitrary TokenFilter where arbitrary = arbitraryTokenFilter
makeArbitrary ''TokenFilterDefinition
instance Arbitrary TokenFilterDefinition where arbitrary = arbitraryTokenFilterDefinition
makeArbitrary ''Language
instance Arbitrary Language where arbitrary = arbitraryLanguage
makeArbitrary ''Shingle
instance Arbitrary Shingle where arbitrary = arbitraryShingle

makeArbitrary ''CharFilter
instance Arbitrary CharFilter where arbitrary = arbitraryCharFilter
makeArbitrary ''AnalyzerDefinition
instance Arbitrary AnalyzerDefinition where arbitrary = arbitraryAnalyzerDefinition

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

makeArbitrary ''Analysis
instance Arbitrary Analysis where arbitrary = arbitraryAnalysis
makeArbitrary ''Tokenizer
instance Arbitrary Tokenizer where arbitrary = arbitraryTokenizer
makeArbitrary ''Compression
instance Arbitrary Compression where arbitrary = arbitraryCompression
makeArbitrary ''Bytes
instance Arbitrary Bytes where arbitrary = arbitraryBytes
makeArbitrary ''AllocationPolicy
instance Arbitrary AllocationPolicy where arbitrary = arbitraryAllocationPolicy
makeArbitrary ''InitialShardCount
instance Arbitrary InitialShardCount where arbitrary = arbitraryInitialShardCount
makeArbitrary ''FSType
instance Arbitrary FSType where arbitrary = arbitraryFSType
makeArbitrary ''CompoundFormat
instance Arbitrary CompoundFormat where arbitrary = arbitraryCompoundFormat
makeArbitrary ''FsSnapshotRepo
instance Arbitrary FsSnapshotRepo where arbitrary = arbitraryFsSnapshotRepo
makeArbitrary ''SnapshotRepoName
instance Arbitrary SnapshotRepoName where arbitrary = arbitrarySnapshotRepoName
makeArbitrary ''TemplateQueryInline
instance Arbitrary TemplateQueryInline where arbitrary = arbitraryTemplateQueryInline
makeArbitrary ''DirectGeneratorSuggestModeTypes
instance Arbitrary DirectGeneratorSuggestModeTypes where arbitrary = arbitraryDirectGeneratorSuggestModeTypes
makeArbitrary ''DirectGenerators
instance Arbitrary DirectGenerators where arbitrary = arbitraryDirectGenerators
makeArbitrary ''PhraseSuggesterCollate
instance Arbitrary PhraseSuggesterCollate where arbitrary = arbitraryPhraseSuggesterCollate
makeArbitrary ''PhraseSuggesterHighlighter
instance Arbitrary PhraseSuggesterHighlighter where arbitrary = arbitraryPhraseSuggesterHighlighter
makeArbitrary ''Size
instance Arbitrary Size where arbitrary = arbitrarySize
makeArbitrary ''PhraseSuggester
instance Arbitrary PhraseSuggester where arbitrary = arbitraryPhraseSuggester
makeArbitrary ''SuggestType
instance Arbitrary SuggestType where arbitrary = arbitrarySuggestType
makeArbitrary ''Suggest
instance Arbitrary Suggest where arbitrary = arbitrarySuggest

makeArbitrary ''FunctionScoreQuery
instance Arbitrary FunctionScoreQuery where arbitrary = arbitraryFunctionScoreQuery

makeArbitrary ''FunctionScoreFunction
instance Arbitrary FunctionScoreFunction where arbitrary = arbitraryFunctionScoreFunction
makeArbitrary ''FunctionScoreFunctions
instance Arbitrary FunctionScoreFunctions where arbitrary = arbitraryFunctionScoreFunctions
makeArbitrary ''ComponentFunctionScoreFunction
instance Arbitrary ComponentFunctionScoreFunction where arbitrary = arbitraryComponentFunctionScoreFunction
makeArbitrary ''Script
instance Arbitrary Script where arbitrary = arbitraryScript
makeArbitrary ''ScriptLanguage
instance Arbitrary ScriptLanguage where arbitrary = arbitraryScriptLanguage
makeArbitrary ''ScriptInline
instance Arbitrary ScriptInline where arbitrary = arbitraryScriptInline
makeArbitrary ''ScriptId
instance Arbitrary ScriptId where arbitrary = arbitraryScriptId
makeArbitrary ''ScoreMode
instance Arbitrary ScoreMode where arbitrary = arbitraryScoreMode
makeArbitrary ''BoostMode
instance Arbitrary BoostMode where arbitrary = arbitraryBoostMode
makeArbitrary ''Seed
instance Arbitrary Seed where arbitrary = arbitrarySeed
makeArbitrary ''FieldValueFactor
instance Arbitrary FieldValueFactor where arbitrary = arbitraryFieldValueFactor
makeArbitrary ''Weight
instance Arbitrary Weight where arbitrary = arbitraryWeight
makeArbitrary ''Factor
instance Arbitrary Factor where arbitrary = arbitraryFactor
makeArbitrary ''FactorMissingFieldValue
instance Arbitrary FactorMissingFieldValue where arbitrary = arbitraryFactorMissingFieldValue
makeArbitrary ''FactorModifier
instance Arbitrary FactorModifier where arbitrary = arbitraryFactorModifier

makeArbitrary ''UpdatableIndexSetting
instance Arbitrary UpdatableIndexSetting where
  arbitrary = resize 10 arbitraryUpdatableIndexSetting

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
