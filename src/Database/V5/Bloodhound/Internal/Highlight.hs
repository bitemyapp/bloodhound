{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Database.V5.Bloodhound.Internal.Highlight where

import           Bloodhound.Import

import qualified Data.Map.Strict as M

import           Database.V5.Bloodhound.Internal.Newtypes
import           Database.V5.Bloodhound.Internal.Query

type HitHighlight = M.Map Text [Text]

data Highlights = Highlights
  { globalsettings  :: Maybe HighlightSettings
  , highlightFields :: [FieldHighlight]
  } deriving (Eq, Show)

instance ToJSON Highlights where
    toJSON (Highlights global fields) =
        omitNulls (("fields" .= fields)
                  : highlightSettingsPairs global)

data FieldHighlight =
  FieldHighlight FieldName (Maybe HighlightSettings)
  deriving (Eq, Show)


instance ToJSON FieldHighlight where
    toJSON (FieldHighlight (FieldName fName) (Just fSettings)) =
        object [ fName .= fSettings ]
    toJSON (FieldHighlight (FieldName fName) Nothing) =
        object [ fName .= emptyObject ]

data HighlightSettings =
    Plain PlainHighlight
  | Postings PostingsHighlight
  | FastVector FastVectorHighlight
  deriving (Eq, Show)


instance ToJSON HighlightSettings where
    toJSON hs = omitNulls (highlightSettingsPairs (Just hs))

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

highlightSettingsPairs :: Maybe HighlightSettings -> [Pair]
highlightSettingsPairs Nothing = []
highlightSettingsPairs (Just (Plain plh)) = plainHighPairs (Just plh)
highlightSettingsPairs (Just (Postings ph)) = postHighPairs (Just ph)
highlightSettingsPairs (Just (FastVector fvh)) = fastVectorHighPairs (Just fvh)


plainHighPairs :: Maybe PlainHighlight -> [Pair]
plainHighPairs Nothing = []
plainHighPairs (Just (PlainHighlight plCom plNonPost)) =
    [ "type" .= String "plain"]
    ++ commonHighlightPairs plCom
    ++ nonPostingsToPairs plNonPost

postHighPairs :: Maybe PostingsHighlight -> [Pair]
postHighPairs Nothing = []
postHighPairs (Just (PostingsHighlight pCom)) =
    ("type" .= String "postings")
    : commonHighlightPairs pCom

fastVectorHighPairs :: Maybe FastVectorHighlight -> [Pair]
fastVectorHighPairs Nothing = []
fastVectorHighPairs
  (Just
    (FastVectorHighlight fvCom fvNonPostSettings' fvBoundChars
                         fvBoundMaxScan fvFragOff fvMatchedFields
                                          fvPhraseLim)) =
                        [ "type" .= String "fvh"
                        , "boundary_chars" .= fvBoundChars
                        , "boundary_max_scan" .= fvBoundMaxScan
                        , "fragment_offset" .= fvFragOff
                        , "matched_fields" .= fvMatchedFields
                        , "phraseLimit" .= fvPhraseLim]
                        ++ commonHighlightPairs fvCom
                        ++ nonPostingsToPairs fvNonPostSettings'

commonHighlightPairs :: Maybe CommonHighlight -> [Pair]
commonHighlightPairs Nothing = []
commonHighlightPairs (Just (CommonHighlight chScore chForceSource
                            chTag chEncoder chNoMatchSize
                            chHighlightQuery chRequireFieldMatch)) =
    [ "order" .= chScore
    , "force_source" .= chForceSource
    , "encoder" .= chEncoder
    , "no_match_size" .= chNoMatchSize
    , "highlight_query" .= chHighlightQuery
    , "require_fieldMatch" .= chRequireFieldMatch
    ]
    ++ highlightTagToPairs chTag


nonPostingsToPairs :: Maybe NonPostings -> [Pair]
nonPostingsToPairs Nothing = []
nonPostingsToPairs (Just (NonPostings npFragSize npNumOfFrags)) =
    [ "fragment_size" .= npFragSize
    , "number_of_fragments" .= npNumOfFrags
    ]

highlightTagToPairs :: Maybe HighlightTag -> [Pair]
highlightTagToPairs (Just (TagSchema _)) =
  [ "scheme" .= String "default"
  ]
highlightTagToPairs (Just (CustomTags (pre, post))) =
  [ "pre_tags"  .= pre
  , "post_tags" .= post
  ]
highlightTagToPairs Nothing = []
