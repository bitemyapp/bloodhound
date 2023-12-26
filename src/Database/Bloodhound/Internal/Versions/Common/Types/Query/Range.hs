{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Bloodhound.Internal.Versions.Common.Types.Query.Range
  ( GreaterThan (..),
    GreaterThanD (..),
    GreaterThanEq (..),
    GreaterThanEqD (..),
    LessThan (..),
    LessThanD (..),
    LessThanEq (..),
    LessThanEqD (..),
    RangeQuery (..),
    RangeValue (..),
    mkRangeQuery,
  )
where

import Database.Bloodhound.Internal.Utils.Imports
import Database.Bloodhound.Internal.Versions.Common.Types.Newtypes
import Database.Bloodhound.Internal.Versions.Common.Types.Query.Commons
import GHC.Generics

data RangeQuery = RangeQuery
  { rangeQueryField :: FieldName,
    rangeQueryRange :: RangeValue,
    rangeQueryBoost :: Boost
  }
  deriving (Eq, Show, Generic)

instance ToJSON RangeQuery where
  toJSON (RangeQuery (FieldName fieldName) range boost) =
    object [fromText fieldName .= object conjoined]
    where
      conjoined = ("boost" .= boost) : rangeValueToPair range

instance FromJSON RangeQuery where
  parseJSON = withObject "RangeQuery" parse
    where
      parse = fieldTagged $ \fn o ->
        RangeQuery fn
          <$> parseJSON (Object o)
          <*> o .: "boost"

mkRangeQuery :: FieldName -> RangeValue -> RangeQuery
mkRangeQuery f r = RangeQuery f r (Boost 1.0)

newtype LessThan = LessThan Double deriving (Eq, Show, Generic)

newtype LessThanEq = LessThanEq Double deriving (Eq, Show, Generic)

newtype GreaterThan = GreaterThan Double deriving (Eq, Show, Generic)

newtype GreaterThanEq = GreaterThanEq Double deriving (Eq, Show, Generic)

newtype LessThanD = LessThanD UTCTime deriving (Eq, Show, Generic)

newtype LessThanEqD = LessThanEqD UTCTime deriving (Eq, Show, Generic)

newtype GreaterThanD = GreaterThanD UTCTime deriving (Eq, Show, Generic)

newtype GreaterThanEqD = GreaterThanEqD UTCTime deriving (Eq, Show, Generic)

data RangeValue
  = RangeDateLte LessThanEqD
  | RangeDateLt LessThanD
  | RangeDateGte GreaterThanEqD
  | RangeDateGt GreaterThanD
  | RangeDateGtLt GreaterThanD LessThanD
  | RangeDateGteLte GreaterThanEqD LessThanEqD
  | RangeDateGteLt GreaterThanEqD LessThanD
  | RangeDateGtLte GreaterThanD LessThanEqD
  | RangeDoubleLte LessThanEq
  | RangeDoubleLt LessThan
  | RangeDoubleGte GreaterThanEq
  | RangeDoubleGt GreaterThan
  | RangeDoubleGtLt GreaterThan LessThan
  | RangeDoubleGteLte GreaterThanEq LessThanEq
  | RangeDoubleGteLt GreaterThanEq LessThan
  | RangeDoubleGtLte GreaterThan LessThanEq
  deriving (Eq, Show, Generic)

parseRangeValue ::
  ( FromJSON t4,
    FromJSON t3,
    FromJSON t2,
    FromJSON t1
  ) =>
  (t3 -> t5) ->
  (t1 -> t6) ->
  (t4 -> t7) ->
  (t2 -> t8) ->
  (t5 -> t6 -> b) ->
  (t7 -> t6 -> b) ->
  (t5 -> t8 -> b) ->
  (t7 -> t8 -> b) ->
  (t5 -> b) ->
  (t6 -> b) ->
  (t7 -> b) ->
  (t8 -> b) ->
  Parser b ->
  Object ->
  Parser b
parseRangeValue
  mkGt
  mkLt
  mkGte
  mkLte
  fGtLt
  fGteLt
  fGtLte
  fGteLte
  fGt
  fLt
  fGte
  fLte
  nada
  o = do
    lt <- o .:? "lt"
    lte <- o .:? "lte"
    gt <- o .:? "gt"
    gte <- o .:? "gte"
    case (lt, lte, gt, gte) of
      (Just a, _, Just b, _) ->
        return (fGtLt (mkGt b) (mkLt a))
      (Just a, _, _, Just b) ->
        return (fGteLt (mkGte b) (mkLt a))
      (_, Just a, Just b, _) ->
        return (fGtLte (mkGt b) (mkLte a))
      (_, Just a, _, Just b) ->
        return (fGteLte (mkGte b) (mkLte a))
      (_, _, Just a, _) ->
        return (fGt (mkGt a))
      (Just a, _, _, _) ->
        return (fLt (mkLt a))
      (_, _, _, Just a) ->
        return (fGte (mkGte a))
      (_, Just a, _, _) ->
        return (fLte (mkLte a))
      (Nothing, Nothing, Nothing, Nothing) ->
        nada

instance FromJSON RangeValue where
  parseJSON = withObject "RangeValue" parse
    where
      parse o =
        parseDate o
          <|> parseDouble o
      parseDate o =
        parseRangeValue
          GreaterThanD
          LessThanD
          GreaterThanEqD
          LessThanEqD
          RangeDateGtLt
          RangeDateGteLt
          RangeDateGtLte
          RangeDateGteLte
          RangeDateGt
          RangeDateLt
          RangeDateGte
          RangeDateLte
          mzero
          o
      parseDouble o =
        parseRangeValue
          GreaterThan
          LessThan
          GreaterThanEq
          LessThanEq
          RangeDoubleGtLt
          RangeDoubleGteLt
          RangeDoubleGtLte
          RangeDoubleGteLte
          RangeDoubleGt
          RangeDoubleLt
          RangeDoubleGte
          RangeDoubleLte
          mzero
          o

rangeValueToPair :: RangeValue -> [Pair]
rangeValueToPair rv = case rv of
  RangeDateLte (LessThanEqD t) -> ["lte" .= t]
  RangeDateGte (GreaterThanEqD t) -> ["gte" .= t]
  RangeDateLt (LessThanD t) -> ["lt" .= t]
  RangeDateGt (GreaterThanD t) -> ["gt" .= t]
  RangeDateGteLte (GreaterThanEqD l) (LessThanEqD g) -> ["gte" .= l, "lte" .= g]
  RangeDateGtLte (GreaterThanD l) (LessThanEqD g) -> ["gt" .= l, "lte" .= g]
  RangeDateGteLt (GreaterThanEqD l) (LessThanD g) -> ["gte" .= l, "lt" .= g]
  RangeDateGtLt (GreaterThanD l) (LessThanD g) -> ["gt" .= l, "lt" .= g]
  RangeDoubleLte (LessThanEq t) -> ["lte" .= t]
  RangeDoubleGte (GreaterThanEq t) -> ["gte" .= t]
  RangeDoubleLt (LessThan t) -> ["lt" .= t]
  RangeDoubleGt (GreaterThan t) -> ["gt" .= t]
  RangeDoubleGteLte (GreaterThanEq l) (LessThanEq g) -> ["gte" .= l, "lte" .= g]
  RangeDoubleGtLte (GreaterThan l) (LessThanEq g) -> ["gt" .= l, "lte" .= g]
  RangeDoubleGteLt (GreaterThanEq l) (LessThan g) -> ["gte" .= l, "lt" .= g]
  RangeDoubleGtLt (GreaterThan l) (LessThan g) -> ["gt" .= l, "lt" .= g]
