module Database.Bloodhound.Internal.Client.Doc
  ( DocVersion (..),
    ExternalDocVersion (..),
    mkDocVersion,
    VersionControl (..),
  )
where

import Data.Aeson
import Data.Maybe
import GHC.Enum

-- | 'DocVersion' is an integer version number for a document between 1
-- and 9.2e+18 used for <<https://www.elastic.co/guide/en/elasticsearch/guide/current/optimistic-concurrency-control.html optimistic concurrency control>>.
newtype DocVersion = DocVersion
  { docVersionNumber :: Int
  }
  deriving stock (Eq, Show, Ord)
  deriving newtype (ToJSON)

-- | Smart constructor for in-range doc version
mkDocVersion :: Int -> Maybe DocVersion
mkDocVersion i
  | i >= docVersionNumber minBound
      && i <= docVersionNumber maxBound =
      Just $ DocVersion i
  | otherwise = Nothing

instance Bounded DocVersion where
  minBound = DocVersion 1
  maxBound = DocVersion 9200000000000000000 -- 9.2e+18

instance Enum DocVersion where
  succ x
    | x /= maxBound = DocVersion (succ $ docVersionNumber x)
    | otherwise = succError "DocVersion"
  pred x
    | x /= minBound = DocVersion (pred $ docVersionNumber x)
    | otherwise = predError "DocVersion"
  toEnum i =
    fromMaybe (error $ show i ++ " out of DocVersion range") $ mkDocVersion i
  fromEnum = docVersionNumber
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

instance FromJSON DocVersion where
  parseJSON v = do
    i <- parseJSON v
    maybe (fail "DocVersion out of range") return $ mkDocVersion i

-- | 'ExternalDocVersion' is a convenience wrapper if your code uses its
-- own version numbers instead of ones from ES.
newtype ExternalDocVersion = ExternalDocVersion DocVersion
  deriving newtype (Eq, Ord, Show, Bounded, Enum, ToJSON)

-- | 'VersionControl' is specified when indexing documents as a
-- optimistic concurrency control.
data VersionControl
  = -- | Don't send a version. This is a pure overwrite.
    NoVersionControl
  | -- | Use the default ES versioning scheme. Only
    -- index the document if the version is the same
    -- as the one specified. Only applicable to
    -- updates, as you should be getting Version from
    -- a search result.
    InternalVersion DocVersion
  | -- | Use your own version numbering. Only index
    -- the document if the version is strictly higher
    -- OR the document doesn't exist. The given
    -- version will be used as the new version number
    -- for the stored document. N.B. All updates must
    -- increment this number, meaning there is some
    -- global, external ordering of updates.
    ExternalGT ExternalDocVersion
  | -- | Use your own version numbering. Only index
    -- the document if the version is equal or higher
    -- than the stored version. Will succeed if there
    -- is no existing document. The given version will
    -- be used as the new version number for the
    -- stored document. Use with care, as this could
    -- result in data loss.
    ExternalGTE ExternalDocVersion
  | -- | The document will always be indexed and the
    -- given version will be the new version. This is
    -- typically used for correcting errors. Use with
    -- care, as this could result in data loss.
    ForceVersion ExternalDocVersion
  deriving stock (Eq, Show, Ord)
