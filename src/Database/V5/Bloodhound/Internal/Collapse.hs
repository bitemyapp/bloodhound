{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Database.V5.Bloodhound.Internal.Collapse where

import           Bloodhound.Import

import           Database.V5.Bloodhound.Internal.Client
import           Database.V5.Bloodhound.Internal.Highlight
import           Database.V5.Bloodhound.Internal.Newtypes
import           Database.V5.Bloodhound.Internal.Sort

data Collapse = Collapse { collapseField         :: FieldName
                         , collapseInnerHit      :: Maybe [InnerHit]
                         , collapseMaxConcurrent :: Maybe Int 
                         } deriving (Eq, Show)
                         
instance ToJSON Collapse where
  toJSON Collapse{..} = 
    omitNulls [ "field"      .= collapseField
              , "inner_hits" .= fmap toJSON collapseInnerHit
              , "max_concurrent_group_searches" .= collapseMaxConcurrent
              ]                         
                         
data InnerHit = InnerHit { innerHitName      :: Text
                         , innerHitFrom      :: Maybe From
                         , innerHitSize      :: Maybe Size
                         , innerHitSort      :: Maybe Sort
                         , innerHitHighlight :: Maybe Highlights
                         , innerHitSource    :: Maybe Source
                         } deriving (Eq, Show)

instance ToJSON InnerHit where
  toJSON InnerHit{..} = omitNulls [ "name"      .= innerHitName
                                  , "from"      .= innerHitFrom
                                  , "size"      .= innerHitSize
                                  , "sort"      .= innerHitSort
                                  , "highlight" .= innerHitHighlight
                                  , "_source"   .= innerHitSource
                                  ]
  
mkInnerHit :: Text -> InnerHit
mkInnerHit name = InnerHit name Nothing Nothing Nothing Nothing Nothing 
  