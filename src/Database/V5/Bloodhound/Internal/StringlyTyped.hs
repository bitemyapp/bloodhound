{-# LANGUAGE OverloadedStrings #-}

module Database.V5.Bloodhound.Internal.StringlyTyped where

import           Bloodhound.Import

import qualified Data.Text     as T


-- This whole module is a sin bucket to deal with Elasticsearch badness.
newtype StringlyTypedDouble = StringlyTypedDouble
  { unStringlyTypedDouble :: Double }

instance FromJSON StringlyTypedDouble where
  parseJSON =
      fmap StringlyTypedDouble
    . parseJSON
    . unStringlyTypeJSON

newtype StringlyTypedInt = StringlyTypedInt
  { unStringlyTypedInt :: Int }

instance FromJSON StringlyTypedInt where
  parseJSON =
      fmap StringlyTypedInt
    . parseJSON
    . unStringlyTypeJSON

newtype StringlyTypedBool = StringlyTypedBool { unStringlyTypedBool :: Bool }

instance FromJSON StringlyTypedBool where
  parseJSON =
      fmap StringlyTypedBool
    . parseJSON
    . unStringlyTypeJSON

-- | For some reason in several settings APIs, all leaf values get returned
-- as strings. This function attempts to recover from this for all
-- non-recursive JSON types. If nothing can be done, the value is left alone.
unStringlyTypeJSON :: Value -> Value
unStringlyTypeJSON (String "true") =
  Bool True
unStringlyTypeJSON (String "false") =
  Bool False
unStringlyTypeJSON (String "null") =
  Null
unStringlyTypeJSON v@(String t) =
  case readMay (T.unpack t) of
    Just n  -> Number n
    Nothing -> v
unStringlyTypeJSON v = v
