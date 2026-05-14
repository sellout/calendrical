{-# LANGUAGE Safe #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -fplugin-opt=NoRecursion:ignore-decls:summer,winter #-}

-- |
-- Copyright: 2024 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-commercial
module Data.Calendar.Icelandic
  ( Date (Date),
    Season (Summer, Winter),
    Week,
    Year,
    angleFromSeason,
    isLeapYear,
    month,
    season,
    seasonFromAngle,
    summer,
    week,
    weekday,
    winter,
    year,
  )
where

import "base" Control.Applicative (pure)
import "base" Control.Category ((.))
import "base" Data.Bool (Bool (True))
import "base" Data.Eq (Eq, (/=), (==))
import "base" Data.Function (($))
import "base" Data.Kind (Type)
import "base" Data.Maybe (Maybe (Nothing))
import "base" Data.Ord (Ord, (<), (<=))
import "base" Data.Proxy (Proxy (Proxy))
import "base" Text.Read (Read)
import "base" Text.Show (Show)
import "fin" Data.Fin (Fin)
import "fin" Data.Type.Nat qualified as Nat
import "numeric-tangle" Numeric.Widen (widen)
import "numeric-tangle-fin" Numeric.Widen.Instances.Fin ()
import "this" Data.Calendar
  ( Calendar,
    CyclicCalendar,
    DayOfWeek (Saturday, Thursday),
    FixedDate (RD),
    Moment (Moment),
    epoch,
    fixedFrom,
    fromFixed,
    fromMoment,
    kdayOnOrAfter,
    mod,
    offset,
  )
import "this" Data.Calendar.Gregorian qualified as Gregorian
import "this" Data.Calendar.Types (Angle, Integer)
import "base" Prelude
  ( Bounded,
    div,
    floor,
    fromEnum,
    fromInteger,
    fromIntegral,
    (*),
    (+),
    (-),
  )

type Year :: Type
type Year = Integer

type Season :: Type
data Season
  = Summer
  | Winter
  deriving stock (Bounded, Eq, Ord, Read, Show)

angleFromSeason :: Season -> Angle
angleFromSeason = \case
  -- (3.19)
  Summer -> 90
  -- (3.21)
  Winter -> 270

seasonFromAngle :: Angle -> Season
seasonFromAngle angle =
  let a = angle `mod` 360
   in if a < 90 then Winter else if a < 270 then Summer else Winter

type Week :: Type
type Week = Fin (Nat.FromGHC 29)

type Date :: Type
data Date = Date
  { year :: Year,
    season :: Season,
    week :: Week,
    weekday :: DayOfWeek
  }
  deriving stock (Eq, Ord, Show)

summer :: Year -> FixedDate
summer iYear =
  kdayOnOrAfter Thursday $
    fixedFrom (Gregorian.Date (fromInteger iYear) Gregorian.April 19)

winter :: Year -> FixedDate
winter iYear = summer (iYear + 1) - 180

instance CyclicCalendar Date where
  epoch _ = fixedFrom $ Gregorian.Date 1 Gregorian.April 19
  fromFixed date =
    let approx =
          (400 * offset (date - epoch (Proxy :: Proxy Date) + 369)) `div` 146097
        year = if summer approx <= date then approx else approx - 1
        season = if date < winter year then Summer else Winter
        start = case season of
          Summer -> summer year
          Winter -> winter year
     in Date
          { year,
            season,
            week = fromIntegral $ offset (date - start) `div` 7 + 1,
            weekday = fromFixed date
          }
  fromMoment (Moment t) = fromFixed . RD $ floor t

instance Calendar Date where
  fixedFrom Date {year, season, week, weekday} =
    start
      + RD (7 * (widen week - 1))
      + RD (widen (fromEnum weekday - fromEnum shift) `mod` 7)
    where
      (start, shift) = case season of
        Summer -> (summer year, Thursday)
        Winter -> (winter year, Saturday)

isLeapYear :: Year -> Bool
isLeapYear iYear = summer (iYear + 1) - summer iYear /= 364

-- | In the range [1..6].
--
--  __NB__: This is not unique per year – it must be combined with the `Season`.
type Month :: Type
type Month = Fin (Nat.FromGHC 7)

-- | For leap days (which occur between the third and fourth months of summer),
--   this returns `Nothing`.
month :: Date -> Maybe Month
month iDate@Date {year, season} =
  let date = fixedFrom iDate
      midsummer = winter year - 90
      start = case season of
        Summer
          | midsummer <= date -> midsummer - 90
          | date < summer year + 90 -> summer year
          | True -> midsummer
        Winter -> winter year
      month = fromIntegral $ offset (date - start) `div` 30 + 1
   in if month == 0 then Nothing else pure month
