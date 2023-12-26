{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Bloodhound.Internal.Versions.Common.Types.Units
  ( Bytes (..),
    Interval (..),
    TimeInterval (..),
    gigabytes,
    kilobytes,
    megabytes,
    parseStringInterval,
  )
where

import Database.Bloodhound.Internal.Utils.Imports
import GHC.Generics
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
  deriving (Eq, Show, Generic, Ord, ToJSON, FromJSON)

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
  deriving (Eq)

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
  deriving (Eq, Show)

instance ToJSON Interval where
  toJSON Year = "year"
  toJSON Quarter = "quarter"
  toJSON Month = "month"
  toJSON Week = "week"
  toJSON Day = "day"
  toJSON Hour = "hour"
  toJSON Minute = "minute"
  toJSON Second = "second"

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
