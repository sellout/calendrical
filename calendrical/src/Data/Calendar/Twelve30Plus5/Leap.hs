{-# LANGUAGE Safe #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
--
-- This is very similar to "Data.Calendar.Twelve30Plus5", except that it
-- includes leap years.
module Data.Calendar.Twelve30Plus5.Leap
  ( module Data.Calendar.Twelve30Plus5,
    date',
    isLeapYear,
    operationsForEpoch,
  )
where

import "base" Control.Applicative (pure)
import "base" Control.Category ((.))
import "base" Data.Bool (Bool, (&&), (||))
import "base" Data.Eq (Eq, (==))
import "base" Data.Function (($))
import "base" Data.Maybe (Maybe (Nothing))
import "base" Data.Ord ((<))
import "numeric-tangle" Numeric.Widen (widen)
import "this" Data.Calendar (FixedDate (RD), Moment (Moment), mod, offset)
import "this" Data.Calendar.Twelve30Plus5
  ( Date (Date),
    Day,
    Operations (Operations),
    Year,
    day,
    epoch,
    fixedFrom,
    fromFixed,
    fromMoment,
    month,
    year,
  )
import "base" Prelude
  ( Bounded,
    Enum,
    div,
    floor,
    fromEnum,
    fromIntegral,
    maxBound,
    toEnum,
    (*),
    (+),
    (-),
  )

isLeapYear :: Year -> Bool
isLeapYear year = year `mod` 4 == 3

-- | This is a synonym for the data constructor. It’s necessary, because we
--   can’t import the data constructor without the type constructor, but the
--   type constructor conflicts with a synonym in many other modules.
date' :: (Bounded month, Eq month) => Year -> month -> Day -> Maybe (Date month)
date' y m d =
  if d == 0 || m == maxBound && (if isLeapYear y then 6 else 5) < d
    then Nothing
    else pure $ Date y m d

operationsForEpoch :: (Enum month) => FixedDate -> Operations month
operationsForEpoch epoch =
  let fixedFrom Date {year, month, day} =
        RD $
          offset epoch
            - 1
            + 365 * (year - 1)
            + year `div` 4
            + 30 * widen (fromEnum month)
            + widen day
      fromFixed (RD date) =
        let year = (4 * (date - offset epoch) + 1463) `div` 1461
            month =
              toEnum . fromIntegral $
                (date - offset (fixedFrom $ Date year (toEnum 0) 1)) `div` 30
            day =
              fromIntegral $ date + 1 - offset (fixedFrom $ Date year month 1)
         in Date {year, month, day}
   in Operations
        { epoch,
          fixedFrom,
          fromFixed,
          fromMoment = \(Moment t) -> fromFixed . RD $ floor t
        }
