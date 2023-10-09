{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Database.Bloodhound.Internal.Newtypes
  ( From (..),
    Size (..),
    HitFields (..),
    Score,
    ShardId (..),
    DocId (..),
    FieldName (..),
    RelationName (..),
    QueryString (..),
    CacheName (..),
    CacheKey (..),
    Existence (..),
    NullValue (..),
    CutoffFrequency (..),
    Analyzer (..),
    MaxExpansions (..),
    Lenient (..),
    Tiebreaker (..),
    MinimumMatch (..),
    DisableCoord (..),
    IgnoreTermFrequency (..),
    MinimumTermFrequency (..),
    MaxQueryTerms (..),
    PrefixLength (..),
    PercentMatch (..),
    StopWord (..),
    QueryPath (..),
    AllowLeadingWildcard (..),
    LowercaseExpanded (..),
    EnablePositionIncrements (..),
    AnalyzeWildcard (..),
    GeneratePhraseQueries (..),
    Locale (..),
    MaxWordLength (..),
    MinWordLength (..),
    PhraseSlop (..),
    MinDocFrequency (..),
    MaxDocFrequency (..),
    AggregateParentScore (..),
    IgnoreUnmapped (..),
    MinChildren (..),
    MaxChildren (..),
    POSIXMS (..),
    Boost (..),
    BoostTerms (..),
    ReplicaCount (..),
    ShardCount (..),
    IndexName,
    unIndexName,
    mkIndexName,
    IndexAliasName (..),
    MaybeNA (..),
    SnapshotName (..),
    MS (..),
    unMS,
    TokenFilter (..),
    CharFilter (..),
  )
where

import Bloodhound.Import
import Control.Exception (throwIO)
import qualified Data.ByteString as BS
import Data.Char (isLetter, isLower)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Generics

newtype From = From Int deriving (Eq, Show, ToJSON)

newtype Size = Size Int deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- Used with scripts
newtype HitFields
  = HitFields (M.Map Text [Value])
  deriving (Eq, Show)

instance FromJSON HitFields where
  parseJSON x =
    HitFields <$> parseJSON x

-- Slight misnomer.
type Score = Maybe Double

newtype ShardId = ShardId {shardId :: Int}
  deriving (Eq, Show, FromJSON)

-- | 'DocId' is a generic wrapper value for expressing unique Document IDs.
--    Can be set by the user or created by ES itself. Often used in client
--    functions for poking at specific documents.
newtype DocId
  = DocId Text
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | 'FieldName' is used all over the place wherever a specific field within
--     a document needs to be specified, usually in 'Query's or 'Filter's.
newtype FieldName
  = FieldName Text
  deriving (Eq, Read, Show, ToJSON, FromJSON)

-- | 'RelationName' describes a relation role between parend and child Documents
--     in a Join relarionship: https://www.elastic.co/guide/en/elasticsearch/reference/current/parent-join.html
newtype RelationName
  = RelationName Text
  deriving (Eq, Read, Show, Generic, ToJSON, FromJSON)

-- | 'QueryString' is used to wrap query text bodies, be they human written or not.
newtype QueryString
  = QueryString Text
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- {-| 'Script' is often used in place of 'FieldName' to specify more
-- complex ways of extracting a value from a document.
-- -}
-- newtype Script =
--   Script { scriptText :: Text }
--   deriving (Eq, Show)

-- | 'CacheName' is used in 'RegexpFilter' for describing the
--    'CacheKey' keyed caching behavior.
newtype CacheName
  = CacheName Text
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | 'CacheKey' is used in 'RegexpFilter' to key regex caching.
newtype CacheKey
  = CacheKey Text
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype Existence
  = Existence Bool
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype NullValue
  = NullValue Bool
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype CutoffFrequency
  = CutoffFrequency Double
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype Analyzer
  = Analyzer Text
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype MaxExpansions
  = MaxExpansions Int
  deriving (Eq, Generic, Show, FromJSON, ToJSON)

-- | 'Lenient', if set to true, will cause format based failures to be
--    ignored. I don't know what the bloody default is, Elasticsearch
--    documentation didn't say what it was. Let me know if you figure it out.
newtype Lenient
  = Lenient Bool
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype Tiebreaker
  = Tiebreaker Double
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | 'MinimumMatch' controls how many should clauses in the bool query should
--     match. Can be an absolute value (2) or a percentage (30%) or a
--     combination of both.
newtype MinimumMatch
  = MinimumMatch Int
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype DisableCoord
  = DisableCoord Bool
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype IgnoreTermFrequency
  = IgnoreTermFrequency Bool
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype MinimumTermFrequency
  = MinimumTermFrequency Int
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype MaxQueryTerms
  = MaxQueryTerms Int
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | 'PrefixLength' is the prefix length used in queries, defaults to 0.
newtype PrefixLength
  = PrefixLength Int
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype PercentMatch
  = PercentMatch Double
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype StopWord
  = StopWord Text
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype QueryPath
  = QueryPath Text
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | Allowing a wildcard at the beginning of a word (eg "*ing") is particularly
--    heavy, because all terms in the index need to be examined, just in case
--    they match. Leading wildcards can be disabled by setting
--    'AllowLeadingWildcard' to false.
newtype AllowLeadingWildcard
  = AllowLeadingWildcard Bool
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype LowercaseExpanded
  = LowercaseExpanded Bool
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype EnablePositionIncrements
  = EnablePositionIncrements Bool
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | By default, wildcard terms in a query are not analyzed.
--    Setting 'AnalyzeWildcard' to true enables best-effort analysis.
newtype AnalyzeWildcard = AnalyzeWildcard Bool deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | 'GeneratePhraseQueries' defaults to false.
newtype GeneratePhraseQueries
  = GeneratePhraseQueries Bool
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | 'Locale' is used for string conversions - defaults to ROOT.
newtype Locale = Locale Text deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype MaxWordLength = MaxWordLength Int deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype MinWordLength = MinWordLength Int deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | 'PhraseSlop' sets the default slop for phrases, 0 means exact
--     phrase matches. Default is 0.
newtype PhraseSlop = PhraseSlop Int deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype MinDocFrequency = MinDocFrequency Int deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype MaxDocFrequency = MaxDocFrequency Int deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | Indicates whether the relevance score of a matching parent document is aggregated into its child documents.
newtype AggregateParentScore = AggregateParentScore Bool deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | Indicates whether to ignore an unmapped parent_type and not return any documents instead of an error.
newtype IgnoreUnmapped = IgnoreUnmapped Bool deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | Maximum number of child documents that match the query allowed for a returned parent document.
--    If the parent document exceeds this limit, it is excluded from the search results.
newtype MinChildren = MinChildren Int deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | Minimum number of child documents that match the query required to match the query for a returned parent document.
--    If the parent document does not meet this limit, it is excluded from the search results.
newtype MaxChildren = MaxChildren Int deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | Newtype wrapper to parse ES's concerning tendency to in some APIs return a floating point number of milliseconds since epoch ಠ_ಠ
newtype POSIXMS = POSIXMS {posixMS :: UTCTime}

instance FromJSON POSIXMS where
  parseJSON = withScientific "POSIXMS" (return . parse)
    where
      parse n =
        let n' = truncate n :: Integer
         in POSIXMS (posixSecondsToUTCTime (fromInteger (n' `div` 1000)))

newtype Boost
  = Boost Double
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype BoostTerms
  = BoostTerms Double
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | 'ReplicaCount' is part of 'IndexSettings'
newtype ReplicaCount
  = ReplicaCount Int
  deriving (Eq, Show, Generic, ToJSON)

-- | 'ShardCount' is part of 'IndexSettings'
newtype ShardCount
  = ShardCount Int
  deriving (Eq, Show, Generic, ToJSON)

-- This insanity is because ES *sometimes* returns Replica/Shard counts as strings
instance FromJSON ReplicaCount where
  parseJSON v =
    parseAsInt v
      <|> parseAsString v
    where
      parseAsInt = fmap ReplicaCount . parseJSON
      parseAsString = withText "ReplicaCount" (fmap ReplicaCount . parseReadText)

instance FromJSON ShardCount where
  parseJSON v =
    parseAsInt v
      <|> parseAsString v
    where
      parseAsInt = fmap ShardCount . parseJSON
      parseAsString = withText "ShardCount" (fmap ShardCount . parseReadText)

-- | 'IndexName' is used to describe which index to query/create/delete
newtype IndexName
  = IndexName Text
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Hashable, Semigroup, ToJSON, ToJSONKey, FromJSONKey)

instance FromJSON IndexName where
  parseJSON x =
    parseJSON x >>= either (fail . T.unpack) return . mkIndexNameSystem

unIndexName :: IndexName -> Text
unIndexName (IndexName x) = x

mkIndexName :: Text -> Either Text IndexName
mkIndexName name = do
  let check explanation p = if p then Right () else Left explanation
  check "Is empty" $ not $ T.null name
  check "Is longer than 255 bytes" $ BS.length (T.encodeUtf8 name) < 256
  check "Contains uppercase letter(s)" $ T.all (\x -> not (isLetter x) || isLower x) name
  check "Includes [\\/*?\"<>| ,#:]" $ T.all (flip @_ @String notElem "\\/*?\"<>| ,#:") name
  check "Starts with [-_+.]" $ maybe False (flip @_ @String notElem "-_+." . fst) $ T.uncons name
  return $ IndexName name

mkIndexNameSystem :: Text -> Either Text IndexName
mkIndexNameSystem name = do
  let check explanation p = if p then Right () else Left explanation
  check "Is empty" $ not $ T.null name
  check "Is longer than 255 bytes" $ BS.length (T.encodeUtf8 name) < 256
  check "Contains uppercase letter(s)" $ T.all (\x -> not (isLetter x) || isLower x) name
  check "Includes [\\/*?\"<>| ,#:]" $ T.all (flip @_ @String notElem "\\/*?\"<>| ,#:") name
  check "Is (.|..)" $ notElem name [".", ".."]
  check "Starts with [-_+]" $ maybe False (flip @_ @String notElem "-_+" . fst) $ T.uncons name
  return $ IndexName name

newtype IndexAliasName = IndexAliasName {indexAliasName :: IndexName}
  deriving (Eq, Show, ToJSON)

newtype MaybeNA a = MaybeNA {unMaybeNA :: Maybe a}
  deriving newtype (Show, Eq, Functor, Applicative, Monad)

instance FromJSON a => FromJSON (MaybeNA a) where
  parseJSON (String "NA") = pure $ MaybeNA Nothing
  parseJSON o = MaybeNA . Just <$> parseJSON o

newtype SnapshotName = SnapshotName {snapshotName :: Text}
  deriving (Eq, Show, ToJSON, FromJSON)

-- | Milliseconds
newtype MS = MS NominalDiffTime

-- keeps the unexported constructor warnings at bay
unMS :: MS -> NominalDiffTime
unMS (MS t) = t

instance FromJSON MS where
  parseJSON = withScientific "MS" (return . MS . parse)
    where
      parse n = fromInteger (truncate n * 1000)

newtype TokenFilter
  = TokenFilter Text
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

newtype CharFilter
  = CharFilter Text
  deriving (Eq, Show, Generic, FromJSON, ToJSON)
