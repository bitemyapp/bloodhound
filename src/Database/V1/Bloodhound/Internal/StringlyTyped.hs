{-# LANGUAGE OverloadedStrings #-}

module Database.V1.Bloodhound.Internal.StringlyTyped where

import           Bloodhound.Import

import qualified Data.Text         as T

newtype StringlyTypedDouble = StringlyTypedDouble { unStringlyTypedDouble :: Double }


instance FromJSON StringlyTypedDouble where
  parseJSON = fmap StringlyTypedDouble . parseJSON . unStringlyTypeJSON


-- | For some reason in several settings APIs, all leaf values get returned
-- as strings. This function attepmts to recover from this for all
-- non-recursive JSON types. If nothing can be done, the value is left alone.
unStringlyTypeJSON :: Value -> Value
unStringlyTypeJSON (String "true") = Bool True
unStringlyTypeJSON (String "false") = Bool False
unStringlyTypeJSON (String "null") = Null
unStringlyTypeJSON v@(String t) = case readMay (T.unpack t) of
                                  Just n  -> Number n
                                  Nothing -> v
unStringlyTypeJSON v = v
