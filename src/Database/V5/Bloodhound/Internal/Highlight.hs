{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Database.V5.Bloodhound.Internal.Highlight where

import           Bloodhound.Import

-- import           Data.Aeson
-- import           Data.Aeson.Types (Parser)
import qualified Data.Map.Strict as M

import           Database.V5.Bloodhound.Internal.Newtypes
import           Database.V5.Bloodhound.Internal.Query

type HitHighlight = M.Map Text [Text]

data Highlights = Highlights
  { globalsettings  :: Maybe HighlightSettings
  , highlightFields :: [FieldHighlight]
  } deriving (Eq, Show)

data FieldHighlight =
  FieldHighlight FieldName (Maybe HighlightSettings)
  deriving (Eq, Show)


data HighlightSettings =
    Plain PlainHighlight
  | Postings PostingsHighlight
  | FastVector FastVectorHighlight
  deriving (Eq, Show)

data PlainHighlight =
  PlainHighlight { plainCommon  :: Maybe CommonHighlight
                 , plainNonPost :: Maybe NonPostings }
  deriving (Eq, Show)

 -- This requires that index_options are set to 'offset' in the mapping.
data PostingsHighlight =
  PostingsHighlight (Maybe CommonHighlight)
  deriving (Eq, Show)

-- This requires that term_vector is set to 'with_positions_offsets' in the mapping.
data FastVectorHighlight = FastVectorHighlight
  { fvCommon          :: Maybe CommonHighlight
  , fvNonPostSettings :: Maybe NonPostings
  , boundaryChars     :: Maybe Text
  , boundaryMaxScan   :: Maybe Int
  , fragmentOffset    :: Maybe Int
  , matchedFields     :: [Text]
  , phraseLimit       :: Maybe Int
  } deriving (Eq, Show)

data CommonHighlight = CommonHighlight
  { order             :: Maybe Text
  , forceSource       :: Maybe Bool
  , tag               :: Maybe HighlightTag
  , encoder           :: Maybe HighlightEncoder
  , noMatchSize       :: Maybe Int
  , highlightQuery    :: Maybe Query
  , requireFieldMatch :: Maybe Bool
  } deriving (Eq, Show)

-- Settings that are only applicable to FastVector and Plain highlighters.
data NonPostings =
    NonPostings { fragmentSize      :: Maybe Int
                , numberOfFragments :: Maybe Int
                } deriving (Eq, Show)

data HighlightEncoder = DefaultEncoder
                      | HTMLEncoder
                      deriving (Eq, Show)

instance ToJSON HighlightEncoder where
    toJSON DefaultEncoder = String "default"
    toJSON HTMLEncoder    = String "html"

-- NOTE: Should the tags use some kind of HTML type, rather than Text?
data HighlightTag =
    TagSchema Text
    -- Only uses more than the first value in the lists if fvh
  | CustomTags ([Text], [Text]) 
  deriving (Eq, Show)
