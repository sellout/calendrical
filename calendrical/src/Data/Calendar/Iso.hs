{-# LANGUAGE Safe #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
module Data.Calendar.Iso
  ( Date (Date),
    Week,
    Year,
    day,
    isLongYear,
    week,
    year,
  )
where

import "base" Control.Category ((.))
import "base" Data.Bool (Bool, (||))
import "base" Data.Eq (Eq, (==))
import "base" Data.Function (($))
import "base" Data.Kind (Type)
import "base" Data.Ord (Ord, (<=))
import "base" Data.Proxy (Proxy (Proxy))
import "fin" Data.Fin (Fin)
import "fin" Data.Type.Nat qualified as Nat
import "numeric-tangle" Numeric.Widen (widen)
import "this" Data.Calendar
  ( Calendar,
    CyclicCalendar,
    DayOfWeek (Monday, Sunday, Thursday),
    FixedDate (RD),
    Moment (Moment),
    epoch,
    fixedFrom,
    fromFixed,
    fromMoment,
    mod1,
    offset,
  )
import "this" Data.Calendar.Gregorian (Year)
import "this" Data.Calendar.Gregorian qualified as Gregorian
import "base" Prelude (div, floor, fromEnum, fromIntegral, toEnum, (+), (-))

type Week :: Type
type Week = Fin (Nat.FromGHC 54)

type Date :: Type
data Date = Date {year :: Year, week :: Week, day :: DayOfWeek}
  deriving stock (Eq, Ord)

instance CyclicCalendar Date where
  epoch _ = epoch (Proxy :: Proxy Gregorian.Date)
  fromFixed date = Date {year, week, day}
    where
      approx = Gregorian.yearFromFixed $ date - 3
      year =
        if fixedFrom (Date (approx + 1) 1 Monday) <= date
          then approx + 1
          else approx
      week =
        fromIntegral $
          offset (date - fixedFrom (Date year 1 Monday)) `div` 7 + 1
      day = toEnum . fromIntegral $ offset (date - RD 0) `mod1` 7
  fromMoment (Moment t) = fromFixed . RD $ floor t

instance Calendar Date where
  fixedFrom Date {year, week, day} =
    Gregorian.nthKday
      (widen week)
      Sunday
      (Gregorian.Date (year - 1) Gregorian.December 28)
      + fromIntegral (fromEnum day)

isLongYear :: Year -> Bool
isLongYear iYear = jan1 == Thursday || dec31 == Thursday
  where
    jan1 = fromFixed $ Gregorian.newYear iYear
    dec31 = fromFixed $ Gregorian.yearEnd iYear
