{-# LANGUAGE OverloadedStrings #-}

module Database.Bloodhound.Internal.Versions.Common.Types.Units
  ( Bytes (..),
    Interval (..),
    TimeInterval (..),
    FixedInterval (..),
    TimeZoneOffset (..),
    TimeOffset (..),
    ExtendedBounds (..),
    gigabytes,
    kilobytes,
    megabytes,
    parseStringInterval,
    fixedIntervalDurationLens,
    fixedIntervalUnitLens,
    timeZoneOffsetHoursLens,
    timeZoneOffsetMinutesLens,
    extendedBoundsMaxLens,
    extendedBoundsMinLens,
  )
where

import Data.Int
import qualified Data.Text as T
import Data.Word
import Database.Bloodhound.Internal.Utils.Imports
import Text.Printf (printf)
import Text.Read (Read (..))
import qualified Text.Read as TR

-- | A measure of bytes used for various configurations. You may want
-- to use smart constructors like 'gigabytes' for larger values.
--
-- >>> gigabytes 9
-- Bytes 9000000000
--
-- >>> megabytes 9
-- Bytes 9000000
--
-- >>> kilobytes 9
-- Bytes 9000
newtype Bytes
  = Bytes Int
  deriving newtype (Eq, Show, Ord, ToJSON, FromJSON)

gigabytes :: Int -> Bytes
gigabytes n = megabytes (1000 * n)

megabytes :: Int -> Bytes
megabytes n = kilobytes (1000 * n)

kilobytes :: Int -> Bytes
kilobytes n = Bytes (1000 * n)

data TimeInterval
  = Weeks
  | Days
  | Hours
  | Minutes
  | Seconds
  deriving stock (Eq)

instance Show TimeInterval where
  show Weeks = "w"
  show Days = "d"
  show Hours = "h"
  show Minutes = "m"
  show Seconds = "s"

instance Read TimeInterval where
  readPrec = f =<< TR.get
    where
      f 'w' = return Weeks
      f 'd' = return Days
      f 'h' = return Hours
      f 'm' = return Minutes
      f 's' = return Seconds
      f _ = fail "TimeInterval expected one of w, d, h, m, s"

data Interval
  = Year
  | Quarter
  | Month
  | Week
  | Day
  | Hour
  | Minute
  | Second
  | IntervalFixed FixedInterval
  deriving stock (Eq, Show)

instance ToJSON Interval where
  toJSON x =
    case x of
      Year -> "year"
      Quarter -> "quarter"
      Month -> "month"
      Week -> "week"
      Day -> "day"
      Hour -> "hour"
      Minute -> "minute"
      Second -> "second"
      IntervalFixed i -> toJSON i

parseStringInterval :: (Monad m, MonadFail m) => String -> m NominalDiffTime
parseStringInterval s = case span isNumber s of
  ("", _) -> fail "Invalid interval"
  (nS, unitS) -> case (readMay nS, readMay unitS) of
    (Just n, Just unit) -> return (fromInteger (n * unitNDT unit))
    (Nothing, _) -> fail "Invalid interval number"
    (_, Nothing) -> fail "Invalid interval unit"
  where
    unitNDT Seconds = 1
    unitNDT Minutes = 60
    unitNDT Hours = 60 * 60
    unitNDT Days = 24 * 60 * 60
    unitNDT Weeks = 7 * 24 * 60 * 60

data FixedInterval = FixedInterval
  { fixedIntervalDuration :: Word32,
    fixedIntervalUnit :: TimeInterval
  }
  deriving stock (Eq, Show)

fixedIntervalDurationLens :: Lens' FixedInterval Word32
fixedIntervalDurationLens = lens fixedIntervalDuration (\x y -> x {fixedIntervalDuration = y})

fixedIntervalUnitLens :: Lens' FixedInterval TimeInterval
fixedIntervalUnitLens = lens fixedIntervalUnit (\x y -> x {fixedIntervalUnit = y})

instance ToJSON FixedInterval where
  toJSON (FixedInterval duration unit) = String (showText duration <> T.pack (show unit))

data TimeZoneOffset = TimeZoneOffset
  { timeZoneOffsetHours :: Int8,
    timeZoneOffsetMinutes :: Word8
  }
  deriving stock (Eq, Show)

timeZoneOffsetHoursLens :: Lens' TimeZoneOffset Int8
timeZoneOffsetHoursLens = lens timeZoneOffsetHours (\x y -> x {timeZoneOffsetHours = y})

timeZoneOffsetMinutesLens :: Lens' TimeZoneOffset Word8
timeZoneOffsetMinutesLens = lens timeZoneOffsetMinutes (\x y -> x {timeZoneOffsetMinutes = y})

instance ToJSON TimeZoneOffset where
  toJSON (TimeZoneOffset hours minutes) =
    String $
      (if hours >= 0 then "+" else "")
        <> T.pack (printf "%02d" hours)
        <> ":"
        <> T.pack (printf "%02d" minutes)

newtype TimeOffset = TimeOffset Int8
  deriving stock (Eq, Show)
  deriving newtype (FromJSON)

instance ToJSON TimeOffset where
  toJSON (TimeOffset offset) =
    String $
      (if offset >= 0 then "+" else "")
        <> showText offset
        <> "h"

data ExtendedBounds = ExtendedBounds
  { extendedBoundsMin :: Value,
    extendedBoundsMax :: Value
  }
  deriving stock (Eq, Show)

extendedBoundsMinLens :: Lens' ExtendedBounds Value
extendedBoundsMinLens = lens extendedBoundsMin (\x y -> x {extendedBoundsMin = y})

extendedBoundsMaxLens :: Lens' ExtendedBounds Value
extendedBoundsMaxLens = lens extendedBoundsMax (\x y -> x {extendedBoundsMax = y})

instance ToJSON ExtendedBounds where
  toJSON (ExtendedBounds minVal maxVal) =
    object
      [ "min" .= minVal,
        "max" .= maxVal
      ]
