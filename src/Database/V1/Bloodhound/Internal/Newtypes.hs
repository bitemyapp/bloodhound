{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Database.V1.Bloodhound.Internal.Newtypes where

import           Bloodhound.Import

newtype From = From Int deriving (Eq, Show, ToJSON)
newtype Size = Size Int deriving (Eq, Show, ToJSON, FromJSON)


{-| 'FieldName' is used all over the place wherever a specific field within
     a document needs to be specified, usually in 'Query's or 'Filter's.
-}
newtype FieldName =
  FieldName Text
  deriving (Eq, Show, ToJSON, FromJSON)

newtype Boost =
  Boost Double
  deriving (Eq, Show, ToJSON, FromJSON)

newtype BoostTerms =
  BoostTerms Double
  deriving (Eq, Show, ToJSON, FromJSON)

{-| 'ReplicaCount' is part of 'IndexSettings' -}
newtype ReplicaCount =
  ReplicaCount Int
  deriving (Eq, Show, ToJSON)

{-| 'ShardCount' is part of 'IndexSettings' -}
newtype ShardCount =
  ShardCount Int
  deriving (Eq, Show, ToJSON)


{-| 'TemplateName' is used to describe which template to query/create/delete
-}
newtype TemplateName = TemplateName Text deriving (Eq, Show, ToJSON, FromJSON)

{-| 'TemplatePattern' represents a pattern which is matched against index names
-}
newtype TemplatePattern = TemplatePattern Text deriving (Eq, Show, ToJSON, FromJSON)

{-| 'MappingName' is part of mappings which are how ES describes and schematizes
    the data in the indices.
-}
newtype MappingName = MappingName Text deriving (Eq, Show, ToJSON, FromJSON)

{-| 'DocId' is a generic wrapper value for expressing unique Document IDs.
    Can be set by the user or created by ES itself. Often used in client
    functions for poking at specific documents.
-}
newtype DocId = DocId Text deriving (Eq, Show, ToJSON, FromJSON)

{-| 'QueryString' is used to wrap query text bodies, be they human written or not.
-}
newtype QueryString = QueryString Text deriving (Eq, Show, ToJSON, FromJSON)

{-| 'Script' is often used in place of 'FieldName' to specify more
complex ways of extracting a value from a document.
-}
newtype Script = Script { scriptText :: Text } deriving (Eq, Show)

{-| 'CacheName' is used in 'RegexpFilter' for describing the
    'CacheKey' keyed caching behavior.
-}
newtype CacheName = CacheName Text deriving (Eq, Show, ToJSON, FromJSON)

{-| 'CacheKey' is used in 'RegexpFilter' to key regex caching.
-}
newtype CacheKey =
  CacheKey Text deriving (Eq, Show, ToJSON, FromJSON)
newtype Existence =
  Existence Bool deriving (Eq, Show, ToJSON, FromJSON)
newtype NullValue =
  NullValue Bool deriving (Eq, Show, ToJSON, FromJSON)
newtype CutoffFrequency =
  CutoffFrequency Double deriving (Eq, Show, ToJSON, FromJSON)
newtype Analyzer =
  Analyzer Text deriving (Eq, Show, ToJSON, FromJSON)
newtype MaxExpansions =
  MaxExpansions Int deriving (Eq, Show, ToJSON, FromJSON)

{-| 'Lenient', if set to true, will cause format based failures to be
    ignored. I don't know what the bloody default is, Elasticsearch
    documentation didn't say what it was. Let me know if you figure it out.
-}
newtype Lenient =
  Lenient Bool deriving (Eq, Show, ToJSON, FromJSON)
newtype Tiebreaker =
  Tiebreaker Double deriving (Eq, Show, ToJSON, FromJSON)

{-| 'MinimumMatch' controls how many should clauses in the bool query should
     match. Can be an absolute value (2) or a percentage (30%) or a
     combination of both.
-}
newtype MinimumMatch =
  MinimumMatch Int deriving (Eq, Show, ToJSON, FromJSON)
newtype DisableCoord =
  DisableCoord Bool deriving (Eq, Show, ToJSON, FromJSON)
newtype IgnoreTermFrequency =
  IgnoreTermFrequency Bool deriving (Eq, Show, ToJSON, FromJSON)
newtype MinimumTermFrequency =
  MinimumTermFrequency Int deriving (Eq, Show, ToJSON, FromJSON)
newtype MaxQueryTerms =
  MaxQueryTerms Int deriving (Eq, Show, ToJSON, FromJSON)
newtype Fuzziness =
  Fuzziness Double deriving (Eq, Show, ToJSON, FromJSON)

{-| 'PrefixLength' is the prefix length used in queries, defaults to 0. -}
newtype PrefixLength =
  PrefixLength Int deriving (Eq, Show, ToJSON, FromJSON)
newtype TypeName =
  TypeName Text deriving (Eq, Show, ToJSON, FromJSON)
newtype PercentMatch =
  PercentMatch Double deriving (Eq, Show, ToJSON, FromJSON)
newtype StopWord =
  StopWord Text deriving (Eq, Show, ToJSON, FromJSON)
newtype QueryPath =
  QueryPath Text deriving (Eq, Show, ToJSON, FromJSON)

{-| Allowing a wildcard at the beginning of a word (eg "*ing") is particularly
    heavy, because all terms in the index need to be examined, just in case
    they match. Leading wildcards can be disabled by setting
    'AllowLeadingWildcard' to false. -}
newtype AllowLeadingWildcard =
  AllowLeadingWildcard     Bool deriving (Eq, Show, ToJSON, FromJSON)
newtype LowercaseExpanded =
  LowercaseExpanded        Bool deriving (Eq, Show, ToJSON, FromJSON)
newtype EnablePositionIncrements =
  EnablePositionIncrements Bool deriving (Eq, Show, ToJSON, FromJSON)

{-| By default, wildcard terms in a query are not analyzed.
    Setting 'AnalyzeWildcard' to true enables best-effort analysis.
-}
newtype AnalyzeWildcard = AnalyzeWildcard Bool deriving (Eq, Show, ToJSON, FromJSON)

{-| 'GeneratePhraseQueries' defaults to false.
-}
newtype GeneratePhraseQueries =
  GeneratePhraseQueries Bool deriving (Eq, Show, ToJSON, FromJSON)

{-| 'Locale' is used for string conversions - defaults to ROOT.
-}
newtype Locale        = Locale        Text deriving (Eq, Show, ToJSON, FromJSON)
newtype MaxWordLength = MaxWordLength Int  deriving (Eq, Show, ToJSON, FromJSON)
newtype MinWordLength = MinWordLength Int  deriving (Eq, Show, ToJSON, FromJSON)

{-| 'PhraseSlop' sets the default slop for phrases, 0 means exact
     phrase matches. Default is 0.
-}
newtype PhraseSlop      = PhraseSlop      Int deriving (Eq, Show, ToJSON, FromJSON)
newtype MinDocFrequency = MinDocFrequency Int deriving (Eq, Show, ToJSON, FromJSON)
newtype MaxDocFrequency = MaxDocFrequency Int deriving (Eq, Show, ToJSON, FromJSON)

-- | Newtype wrapper to parse ES's concerning tendency to in some APIs return a floating point number of milliseconds since epoch ಠ_ಠ
newtype POSIXMS = POSIXMS { posixMS :: UTCTime }

instance FromJSON POSIXMS where
  parseJSON = withScientific "POSIXMS" (return . parse)
    where parse n = let n' = truncate n :: Integer
                    in POSIXMS (posixSecondsToUTCTime (fromInteger (n' `div` 1000)))

{-| 'IndexName' is used to describe which index to query/create/delete
-}
newtype IndexName = IndexName Text deriving (Eq, Show, ToJSON, FromJSON)

newtype IndexAliasName = IndexAliasName { indexAliasName :: IndexName } deriving (Eq, Show, ToJSON)

type Score = Maybe Double

newtype ShardId = ShardId { shardId :: Int }
                deriving (Eq, Show, FromJSON)

-- | Milliseconds
newtype MS = MS NominalDiffTime


-- keeps the unexported constructor warnings at bay
unMS :: MS -> NominalDiffTime
unMS (MS t) = t

instance FromJSON MS where
  parseJSON = withScientific "MS" (return . MS . parse)
    where
      parse n = fromInteger ((truncate n) * 1000)

newtype MaybeNA a = MaybeNA { unMaybeNA :: Maybe a }
  deriving (Show, Eq)

instance FromJSON a => FromJSON (MaybeNA a) where
  parseJSON (String "NA") = pure $ MaybeNA Nothing
  parseJSON o             = MaybeNA . Just <$> parseJSON o

newtype SnapshotName = SnapshotName { snapshotName :: Text }
                     deriving (Show, Eq, Ord, ToJSON, FromJSON)

instance FromJSON ShardCount where
  parseJSON v = parseAsInt v
            <|> parseAsString v
    where parseAsInt = fmap ShardCount . parseJSON
          parseAsString = withText "ShardCount" (fmap ShardCount . parseReadText)


instance FromJSON ReplicaCount where
  parseJSON v = parseAsInt v
            <|> parseAsString v
    where parseAsInt = fmap ReplicaCount . parseJSON
          parseAsString = withText "ReplicaCount" (fmap ReplicaCount . parseReadText)
