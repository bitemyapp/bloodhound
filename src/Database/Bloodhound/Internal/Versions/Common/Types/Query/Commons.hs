{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Bloodhound.Internal.Versions.Common.Types.Query.Commons
  ( BooleanOperator (..),
    Fuzziness (..),
    ZeroTermsQuery (..),
    fieldTagged,
  )
where

import qualified Data.Aeson.KeyMap as X
import Database.Bloodhound.Internal.Utils.Imports
import Database.Bloodhound.Internal.Versions.Common.Types.Newtypes
import GHC.Generics

data ZeroTermsQuery
  = ZeroTermsNone
  | ZeroTermsAll
  deriving (Eq, Show, Generic)

instance ToJSON ZeroTermsQuery where
  toJSON ZeroTermsNone = String "none"
  toJSON ZeroTermsAll = String "all"

instance FromJSON ZeroTermsQuery where
  parseJSON = withText "ZeroTermsQuery" parse
    where
      parse "none" = pure ZeroTermsNone
      parse "all" = pure ZeroTermsAll
      parse q = fail ("Unexpected ZeroTermsQuery: " <> show q)

-- | 'BooleanOperator' is the usual And/Or operators with an ES compatible
--    JSON encoding baked in. Used all over the place.
data BooleanOperator = And | Or deriving (Eq, Show, Generic)

instance ToJSON BooleanOperator where
  toJSON And = String "and"
  toJSON Or = String "or"

instance FromJSON BooleanOperator where
  parseJSON = withText "BooleanOperator" parse
    where
      parse "and" = pure And
      parse "or" = pure Or
      parse o = fail ("Unexpected BooleanOperator: " <> show o)

-- | Fuzziness value as a number or 'AUTO'.
-- See:
-- https://www.elastic.co/guide/en/elasticsearch/reference/current/common-options.html#fuzziness
data Fuzziness = Fuzziness Double | FuzzinessAuto
  deriving (Eq, Show, Generic)

instance ToJSON Fuzziness where
  toJSON (Fuzziness n) = toJSON n
  toJSON FuzzinessAuto = String "AUTO"

instance FromJSON Fuzziness where
  parseJSON (String "AUTO") = return FuzzinessAuto
  parseJSON v = Fuzziness <$> parseJSON v

fieldTagged :: (Monad m, MonadFail m) => (FieldName -> Object -> m a) -> Object -> m a
fieldTagged f o = case X.toList o of
  [(k, Object o')] -> f (FieldName $ toText k) o'
  _ -> fail "Expected object with 1 field-named key"
